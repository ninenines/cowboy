%% Copyright (c) 2018-2024, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(security_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [raw_open/1]).
-import(cowboy_test, [raw_send/2]).
-import(cowboy_test, [raw_recv_head/1]).
-import(cowboy_test, [raw_recv/3]).

%% ct.

all() ->
	cowboy_test:common_all().

groups() ->
	Tests = [nc_rand, nc_zero],
	H1Tests = [slowloris, slowloris_chunks],
	H2CTests = [
		http2_cancel_flood,
		http2_data_dribble,
		http2_empty_frame_flooding_data,
		http2_empty_frame_flooding_headers_continuation,
		http2_empty_frame_flooding_push_promise,
		http2_infinite_continuations,
		http2_ping_flood,
		http2_reset_flood,
		http2_settings_flood,
		http2_zero_length_header_leak
	],
	[
		{http, [parallel], Tests ++ H1Tests},
		{https, [parallel], Tests ++ H1Tests},
		{h2, [parallel], Tests},
		{h2c, [parallel], Tests ++ H2CTests},
		{http_compress, [parallel], Tests ++ H1Tests},
		{https_compress, [parallel], Tests ++ H1Tests},
		{h2_compress, [parallel], Tests},
		{h2c_compress, [parallel], Tests ++ H2CTests}
	].

init_per_suite(Config) ->
	ct_helper:create_static_dir(config(priv_dir, Config) ++ "/static"),
	Config.

end_per_suite(Config) ->
	ct_helper:delete_static_dir(config(priv_dir, Config) ++ "/static").

init_per_group(Name, Config) ->
	cowboy_test:init_common_groups(Name, Config, ?MODULE).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

%% Routes.

init_dispatch(_) ->
	cowboy_router:compile([{"localhost", [
		{"/", hello_h, []},
		{"/echo/:key", echo_h, []},
		{"/delay_hello", delay_hello_h, 1000},
		{"/long_polling", long_polling_h, []},
		{"/resp/:key[/:arg]", resp_h, []}
	]}]).

%% Tests.

http2_cancel_flood(Config) ->
	doc("Confirm that Cowboy detects the rapid reset attack. (CVE-2023-44487)"),
	do_http2_cancel_flood(Config, 1, 500),
	do_http2_cancel_flood(Config, 10, 50),
	do_http2_cancel_flood(Config, 500, 1),
	ok.

do_http2_cancel_flood(Config, NumStreamsPerBatch, NumBatches) ->
	{ok, Socket} = rfc7540_SUITE:do_handshake(Config),
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/delay_hello">>}
	]),
	AllStreamIDs = lists:seq(1, NumBatches * NumStreamsPerBatch * 2, 2),
	_ = lists:foldl(
		fun (_BatchNumber, AvailableStreamIDs) ->
			%% Take a bunch of IDs from the available stream IDs.
			%% Send HEADERS for all these and then cancel them.
			{IDs, RemainingStreamIDs} = lists:split(NumStreamsPerBatch, AvailableStreamIDs),
			_ = gen_tcp:send(Socket, [cow_http2:headers(ID, fin, HeadersBlock) || ID <- IDs]),
			_ = gen_tcp:send(Socket, [<<4:24, 3:8, 0:8, ID:32, 8:32>> || ID <- IDs]),
			RemainingStreamIDs
		end,
		AllStreamIDs,
		lists:seq(1, NumBatches, 1)),
	%% When Cowboy detects a flood it must close the connection.
	case gen_tcp:recv(Socket, 17, 6000) of
		{ok, <<_:24, 7:8, 0:8, 0:32, _LastStreamId:32, 11:32>>} ->
			%% GOAWAY with error code 11 = ENHANCE_YOUR_CALM.
			ok;
		%% We also accept the connection being closed immediately,
		%% which may happen because we send the GOAWAY right before closing.
		{error, closed} ->
			ok
	end.

http2_data_dribble(Config) ->
	doc("Request a very large response then update the window 1 byte at a time. (CVE-2019-9511)"),
	{ok, Socket} = rfc7540_SUITE:do_handshake(Config),
	%% Send a GET request for a very large response.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/resp/stream_body/loop">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a response with a few DATA frames draining the window.
	{ok, <<SkipLen:24, 1:8, _:8, 1:32>>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{ok, <<16384:24, 0:8, 0:8, 1:32, _:16384/unit:8>>} = gen_tcp:recv(Socket, 9 + 16384, 1000),
	{ok, <<16384:24, 0:8, 0:8, 1:32, _:16384/unit:8>>} = gen_tcp:recv(Socket, 9 + 16384, 1000),
	{ok, <<16384:24, 0:8, 0:8, 1:32, _:16384/unit:8>>} = gen_tcp:recv(Socket, 9 + 16384, 1000),
	{ok, <<16383:24, 0:8, 0:8, 1:32, _:16383/unit:8>>} = gen_tcp:recv(Socket, 9 + 16383, 1000),
	%% Send WINDOW_UPDATE frames with a value of 1. The server should
	%% not attempt to send data until the window is over a configurable threshold.
	ok = gen_tcp:send(Socket, [
		cow_http2:window_update(1),
		cow_http2:window_update(1, 1)
	]),
	{error, timeout} = gen_tcp:recv(Socket, 0, 1000),
	ok.

http2_empty_frame_flooding_data(Config) ->
	doc("Confirm that Cowboy detects empty DATA frame flooding. (CVE-2019-9518)"),
	{ok, Socket} = rfc7540_SUITE:do_handshake(Config),
	%% Send a POST request followed by many empty DATA frames.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, HeadersBlock)),
	_ = [gen_tcp:send(Socket, cow_http2:data(1, nofin, <<>>)) || _ <- lists:seq(1, 20000)],
	%% When Cowboy detects a flood it must close the connection.
	%% We skip WINDOW_UPDATE frames sent when Cowboy starts to read the body.
	case gen_tcp:recv(Socket, 43, 6000) of
		{ok, <<_:26/unit:8, _:24, 7:8, _:72, 11:32>>} ->
			ok;
		%% We also accept the connection being closed immediately,
		%% which may happen because we send the GOAWAY right before closing.
		{error, closed} ->
			ok;
		%% At least on Windows this might also occur.
		{error, enotconn} ->
			ok
	end.

http2_empty_frame_flooding_headers_continuation(Config) ->
	doc("Confirm that Cowboy detects empty HEADERS/CONTINUATION frame flooding. (CVE-2019-9518)"),
	{ok, Socket} = rfc7540_SUITE:do_handshake(Config),
	%% Send many empty HEADERS/CONTINUATION frames before the headers.
	ok = gen_tcp:send(Socket, <<0:24, 1:8, 0:9, 1:31>>),
	_ = [gen_tcp:send(Socket, <<0:24, 9:8, 0:9, 1:31>>) || _ <- lists:seq(1, 20000)],
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	Len = iolist_size(HeadersBlock),
	_ = gen_tcp:send(Socket, [<<Len:24, 9:8, 0:5, 1:1, 0:1, 1:1, 0:1, 1:31>>, HeadersBlock]),
	%% When Cowboy detects a flood it must close the connection.
	case gen_tcp:recv(Socket, 17, 6000) of
		{ok, <<_:24, 7:8, _:72, 11:32>>} ->
			ok;
		%% We also accept the connection being closed immediately,
		%% which may happen because we send the GOAWAY right before closing.
		{error, closed} ->
			ok;
		%% At least on Windows this might also occur.
		{error, enotconn} ->
			ok
	end.

http2_empty_frame_flooding_push_promise(Config) ->
	doc("Confirm that Cowboy detects empty PUSH_PROMISE frame flooding. (CVE-2019-9518)"),
	{ok, Socket} = rfc7540_SUITE:do_handshake(Config),
	%% Send a HEADERS frame to which we will attach a PUSH_PROMISE.
	%% We use nofin in order to keep the stream alive.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, HeadersBlock)),
	%% Send nofin PUSH_PROMISE frame without any data.
	ok = gen_tcp:send(Socket, <<4:24, 5:8, 0:8, 0:1, 1:31, 0:1, 3:31>>),
	%% Receive a PROTOCOL_ERROR connection error.
	%%
	%% Cowboy rejects all PUSH_PROMISE frames therefore no flooding
	%% can take place.
	{ok, <<_:24, 7:8, _:72, 1:32>>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

http2_infinite_continuations(Config) ->
	doc("Confirm that Cowboy rejects CONTINUATION frames when the "
		"total size of HEADERS + CONTINUATION(s) exceeds the limit."),
	{ok, Socket} = rfc7540_SUITE:do_handshake(Config),
	%% Send a HEADERS frame followed by a large number
	%% of continuation frames.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	HeadersBlockLen = iolist_size(HeadersBlock),
	ok = gen_tcp:send(Socket, [
		%% HEADERS frame.
		<<
			HeadersBlockLen:24, 1:8, 0:5,
			0:1, %% END_HEADERS
			0:1,
			1:1, %% END_STREAM
			0:1,
			1:31 %% Stream ID.
		>>,
		HeadersBlock,
		%% CONTINUATION frames.
		[<<1024:24, 9:8, 0:8, 0:1, 1:31, 0:1024/unit:8>>
			|| _ <- lists:seq(1, 100)]
	]),
	%% Receive an ENHANCE_YOUR_CALM connection error.
	{ok, <<_:24, 7:8, _:72, 11:32>>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% @todo http2_internal_data_buffering(Config) -> I do not know how to test this.
%	doc("Request many very large responses, with a larger than necessary window size, "
%		"but do not attempt to read from the socket. (CVE-2019-9517)"),

http2_ping_flood(Config) ->
	doc("Confirm that Cowboy detects PING floods. (CVE-2019-9512)"),
	{ok, Socket} = rfc7540_SUITE:do_handshake(Config),
	%% Flood the server with PING frames.
	_ = [gen_tcp:send(Socket, cow_http2:ping(0)) || _ <- lists:seq(1, 20000)],
	%% Receive a number of PING ACK frames in return, following by the closing of the connection.
	try
		[case gen_tcp:recv(Socket, 17, 6000) of
			{ok, <<8:24, 6:8, _:7, 1:1, _:32, 0:64>>} -> ok;
			{ok, <<_:24, 7:8, _:72, 11:32>>} -> throw(goaway);
			%% We also accept the connection being closed immediately,
			%% which may happen because we send the GOAWAY right before closing.
			{error, closed} -> throw(goaway)
		end || _ <- lists:seq(1, 20000)],
		error(flood_successful)
	catch throw:goaway ->
		ok
	end.

http2_reset_flood(Config) ->
	doc("Confirm that Cowboy detects reset floods. (CVE-2019-9514)"),
	{ok, Socket} = rfc7540_SUITE:do_handshake(Config),
	%% Flood the server with HEADERS frames without a :method pseudo-header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	_ = [gen_tcp:send(Socket, cow_http2:headers(ID, fin, HeadersBlock)) || ID <- lists:seq(1, 100, 2)],
	%% Receive a number of RST_STREAM frames in return, following by the closing of the connection.
	try
		[case gen_tcp:recv(Socket, 13, 6000) of
			{ok, <<_:24, 3:8, _:8, ID:32, 1:32>>} -> ok;
			{ok, <<_:24, 7:8, _:72>>} ->
				{ok, <<11:32>>} = gen_tcp:recv(Socket, 4, 1000),
				throw(goaway);
			%% We also accept the connection being closed immediately,
			%% which may happen because we send the GOAWAY right before closing.
			{error, closed} ->
				throw(goaway)
		end || ID <- lists:seq(1, 100, 2)],
		error(flood_successful)
	catch throw:goaway ->
		ok
	end.

%% @todo If we ever implement the PRIORITY mechanism, this test should
%% be implemented as well. CVE-2019-9513 https://www.kb.cert.org/vuls/id/605641/
%% http2_resource_loop

http2_settings_flood(Config) ->
	doc("Confirm that Cowboy detects SETTINGS floods. (CVE-2019-9515)"),
	{ok, Socket} = rfc7540_SUITE:do_handshake(Config),
	%% Flood the server with empty SETTINGS frames.
	_ = [gen_tcp:send(Socket, cow_http2:settings(#{})) || _ <- lists:seq(1, 20000)],
	%% Receive a number of SETTINGS ACK frames in return, following by the closing of the connection.
	try
		[case gen_tcp:recv(Socket, 9, 6000) of
			{ok, <<0:24, 4:8, 0:7, 1:1, 0:32>>} -> ok;
			{ok, <<_:24, 7:8, _:40>>} ->
				{ok, <<_:32, 11:32>>} = gen_tcp:recv(Socket, 8, 1000),
				throw(goaway);
			%% We also accept the connection being closed immediately,
			%% which may happen because we send the GOAWAY right before closing.
			{error, closed} ->
				throw(goaway)
		end || _ <- lists:seq(1, 20000)],
		error(flood_successful)
	catch throw:goaway ->
		ok
	end.

http2_zero_length_header_leak(Config) ->
	doc("Confirm that Cowboy rejects HEADERS frame with a 0-length header name. (CVE-2019-9516)"),
	{ok, Socket} = rfc7540_SUITE:do_handshake(Config),
	%% Send a GET request with a 0-length header name.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>},
		{<<>>, <<"CVE-2019-9516">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, <<_:24, 3:8, _:8, 1:32, 1:32>>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

nc_rand(Config) ->
	doc("Throw random garbage at the server, then check if it's still up."),
	do_nc(Config, "/dev/urandom").

nc_zero(Config) ->
	doc("Throw zeroes at the server, then check if it's still up."),
	do_nc(Config, "/dev/zero").

do_nc(Config, Input) ->
	Cat = os:find_executable("cat"),
	Nc = os:find_executable("nc"),
	case {Cat, Nc} of
		{false, _} ->
			{skip, "The cat executable was not found."};
		{_, false} ->
			{skip, "The nc executable was not found."};
		_ ->
			StrPort = integer_to_list(config(port, Config)),
			_ = [
				os:cmd("cat " ++ Input ++ " | nc localhost " ++ StrPort)
			|| _ <- lists:seq(1, 100)],
			ConnPid = gun_open(Config),
			Ref = gun:get(ConnPid, "/"),
			{response, _, 200, _} = gun:await(ConnPid, Ref),
			ok
	end.

slowloris(Config) ->
	doc("Send request headers one byte at a time. "
		"Confirm that the connection gets closed."),
	Client = raw_open(Config),
	try
		[begin
			ok = raw_send(Client, [C]),
			timer:sleep(250)
		end || C <- "GET / HTTP/1.1\r\nHost: localhost\r\n"
			"User-Agent: Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US)\r\n"
			"Cookie: name=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n\r\n"],
		error(failure)
	catch error:{badmatch, _} ->
		ok
	end.

slowloris_chunks(Config) ->
	doc("Send request headers one line at a time. "
		"Confirm that the connection gets closed."),
	Client = raw_open(Config),
	ok = raw_send(Client, "GET / HTTP/1.1\r\n"),
	timer:sleep(300),
	ok = raw_send(Client, "Host: localhost\r\n"),
	timer:sleep(300),
	Data = raw_recv_head(Client),
	{'HTTP/1.1', 408, _, Rest} = cow_http:parse_status_line(Data),
	{Headers, _} = cow_http:parse_headers(Rest),
	{_, <<"close">>} = lists:keyfind(<<"connection">>, 1, Headers),
	{error, closed} = raw_recv(Client, 0, 1000).
