%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
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

-module(http_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(ct_helper, [get_remote_pid_tcp/1]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [gun_down/1]).
-import(cowboy_test, [raw_open/1]).
-import(cowboy_test, [raw_send/2]).
-import(cowboy_test, [raw_recv_head/1]).
-import(cowboy_test, [raw_recv_rest/3]).
-import(cowboy_test, [raw_recv/3]).
-import(cowboy_test, [raw_expect_recv/2]).

-include_lib("stdlib/include/assert.hrl").

all() ->
	[{group, clear_no_parallel}, {group, clear}].

groups() ->
	[
		%% cowboy:stop_listener can be slow when called many times
		%% in parallel so we must run this test separately from the others.
		{clear_no_parallel, [], [graceful_shutdown_listener]},
		{clear, [parallel], ct_helper:all(?MODULE) -- [graceful_shutdown_listener]}
	].

init_per_group(Name, Config) ->
	cowboy_test:init_http(Name, #{
		env => #{dispatch => init_dispatch(Config)}
	}, Config).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

init_dispatch(_) ->
	cowboy_router:compile([{"localhost", [
		{"/", hello_h, []},
		{"/delay_hello", delay_hello_h, #{delay => 1000, notify_received => self()}},
		{"/echo/:key", echo_h, []},
		{"/resp/:key[/:arg]", resp_h, []},
		{"/set_options/:key", set_options_h, []},
		{"/streamed_result/:n/:interval", streamed_result_h, []}
	]}]).

chunked_false(Config) ->
	doc("Confirm the option chunked => false disables chunked "
		"transfer-encoding for HTTP/1.1 connections."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		chunked => false
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "GET /resp/stream_reply2/200 HTTP/1.1\r\nhost: localhost\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		Data = raw_recv_head(Client),
		%% Cowboy always advertises itself as HTTP/1.1.
		{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
		{Headers, Rest} = cow_http:parse_headers(Rest0),
		false = lists:keyfind(<<"content-length">>, 1, Headers),
		false = lists:keyfind(<<"transfer-encoding">>, 1, Headers),
		Bits = 8000000 - bit_size(Rest),
		raw_expect_recv(Client, <<0:Bits>>),
		{error, closed} = raw_recv(Client, 1, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

chunked_one_byte_at_a_time(Config) ->
	doc("Confirm that chunked transfer-encoding works when "
		"the body is received one byte at a time."),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	ChunkedBody = iolist_to_binary(do_chunked_body(50, Body, [])),
	Client = raw_open(Config),
	ok = raw_send(Client,
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n\r\n"),
	_ = [begin
		raw_send(Client, <<C>>),
		timer:sleep(1)
	end || <<C>> <= ChunkedBody],
	Data = raw_recv_head(Client),
	{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
	{_, Rest} = cow_http:parse_headers(Rest0),
	RestSize = byte_size(Rest),
	<<Rest:RestSize/binary, Expect/bits>> = Body,
	raw_expect_recv(Client, Expect).

chunked_one_chunk_at_a_time(Config) ->
	doc("Confirm that chunked transfer-encoding works when "
		"the body is received one chunk at a time."),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = do_chunked_body(50, Body, []),
	Client = raw_open(Config),
	ok = raw_send(Client,
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n\r\n"),
	_ = [begin
		raw_send(Client, Chunk),
		timer:sleep(10)
	end || Chunk <- Chunks],
	Data = raw_recv_head(Client),
	{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
	{_, Rest} = cow_http:parse_headers(Rest0),
	RestSize = byte_size(Rest),
	<<Rest:RestSize/binary, Expect/bits>> = Body,
	raw_expect_recv(Client, Expect).

chunked_split_delay_in_chunk_body(Config) ->
	doc("Confirm that chunked transfer-encoding works when "
		"the body is received with a delay inside the chunks."),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = do_chunked_body(50, Body, []),
	Client = raw_open(Config),
	ok = raw_send(Client,
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n\r\n"),
	_ = [begin
		case Chunk of
			<<"0\r\n\r\n">> ->
				raw_send(Client, Chunk);
			_ ->
				[Size, ChunkBody, <<>>] = binary:split(Chunk, <<"\r\n">>, [global]),
				PartASize = rand:uniform(byte_size(ChunkBody)),
				<<PartA:PartASize/binary, PartB/binary>> = ChunkBody,
				raw_send(Client, [Size, <<"\r\n">>, PartA]),
				timer:sleep(10),
				raw_send(Client, [PartB, <<"\r\n">>])
		end
	end || Chunk <- Chunks],
	Data = raw_recv_head(Client),
	{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
	{_, Rest} = cow_http:parse_headers(Rest0),
	RestSize = byte_size(Rest),
	<<Rest:RestSize/binary, Expect/bits>> = Body,
	raw_expect_recv(Client, Expect).

chunked_split_delay_in_chunk_crlf(Config) ->
	doc("Confirm that chunked transfer-encoding works when "
		"the body is received with a delay inside the chunks end CRLF."),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = do_chunked_body(50, Body, []),
	Client = raw_open(Config),
	ok = raw_send(Client,
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n\r\n"),
	_ = [begin
		Len = byte_size(Chunk) - (rand:uniform(2) - 1),
		<<Begin:Len/binary, End/binary>> = Chunk,
		raw_send(Client, Begin),
		timer:sleep(10),
		raw_send(Client, End)
	end || Chunk <- Chunks],
	Data = raw_recv_head(Client),
	{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
	{_, Rest} = cow_http:parse_headers(Rest0),
	RestSize = byte_size(Rest),
	<<Rest:RestSize/binary, Expect/bits>> = Body,
	raw_expect_recv(Client, Expect).

do_chunked_body(_, <<>>, Acc) ->
	lists:reverse([cow_http_te:last_chunk()|Acc]);
do_chunked_body(ChunkSize0, Data, Acc) ->
	ChunkSize = min(byte_size(Data), ChunkSize0),
	<<Chunk:ChunkSize/binary, Rest/binary>> = Data,
	do_chunked_body(ChunkSize, Rest,
		[iolist_to_binary(cow_http_te:chunk(Chunk))|Acc]).

chunked_trailers_skip(Config) ->
	doc("Confirm that request trailers are skipped and another "
		"request can be sent on the same connection."),
	Client = raw_open(Config),
	ok = raw_send(Client,
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-Encoding: chunked\r\n"
		"Trailer: x-foo\r\n"
		"\r\n"
		"5\r\nHello\r\n"
		"0\r\n"
		"X-Foo: bar\r\n"
		"\r\n"),
	Data = raw_recv_head(Client),
	{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
	{_, Rest} = cow_http:parse_headers(Rest0),
	RestSize = byte_size(Rest),
	<<Rest:RestSize/binary, Expect/bits>> = <<"Hello">>,
	raw_expect_recv(Client, Expect),
	%% Second request/response on the same connection.
	ok = raw_send(Client,
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	Data2 = raw_recv_head(Client),
	{'HTTP/1.1', 200, _, _} = cow_http:parse_status_line(Data2),
	ok.

chunked_trailers_skip_limit(Config) ->
	doc("Confirm that the trailer size limit is enforced and "
		"results in a 400 error and the closing of the connection."),
	Client = raw_open(Config),
	Large = binary:copy(<<"a">>, 5000),
	ok = raw_send(Client, [
		"POST /delay_hello HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-Encoding: chunked\r\n"
		"\r\n"
		"0\r\n"
		"X-Foo: ", Large
	]),
	Data = raw_recv_head(Client),
	{'HTTP/1.1', 400, _, _} = cow_http:parse_status_line(Data),
	{error, closed} = raw_recv(Client, 0, 1000).

disable_http1_tls(Config) ->
	doc("Ensure that we can disable HTTP/1.1 over TLS (force HTTP/2)."),
	TlsOpts = ct_helper:get_certs_from_ets(),
	{ok, _} = cowboy:start_tls(?FUNCTION_NAME, TlsOpts ++ [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		alpn_default_protocol => http2
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		{ok, Socket} = ssl:connect("localhost", Port,
			[binary, {active, false}|TlsOpts]),
		%% ALPN was not negotiated but we're still over HTTP/2.
		{error, protocol_not_negotiated} = ssl:negotiated_protocol(Socket),
		%% Send a valid preface.
		ok = ssl:send(Socket, [
			"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n",
			cow_http2:settings(#{})]),
		%% Receive the server preface.
		{ok, << Len:24 >>} = ssl:recv(Socket, 3, 1000),
		{ok, << 4:8, 0:40, _:Len/binary >>} = ssl:recv(Socket, 6 + Len, 1000),
		ok
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

disable_http2_prior_knowledge(Config) ->
	doc("Ensure that we can disable prior knowledge HTTP/2 upgrade."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		protocols => [http]
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
		%% Send a valid preface.
		ok = gen_tcp:send(Socket, [
			"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n",
			cow_http2:settings(#{})]),
		{ok, <<"HTTP/1.1 501">>} = gen_tcp:recv(Socket, 12, 1000),
		ok
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

disable_http2_upgrade(Config) ->
	doc("Ensure that we can disable HTTP/1.1 upgrade to HTTP/2."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		protocols => [http]
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
		%% Send a valid preface.
		ok = gen_tcp:send(Socket, [
			"GET / HTTP/1.1\r\n"
			"Host: localhost\r\n"
			"Connection: Upgrade, HTTP2-Settings\r\n"
			"Upgrade: h2c\r\n"
			"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
			"\r\n"]),
		{ok, <<"HTTP/1.1 200">>} = gen_tcp:recv(Socket, 12, 1000),
		ok
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

hibernate(Config) ->
	doc("Ensure that we can enable hibernation for HTTP/1.1 connections."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		hibernate => true
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		StreamRef1 = gun:get(ConnPid, "/"),
		StreamRef2 = gun:get(ConnPid, "/"),
		StreamRef3 = gun:get(ConnPid, "/"),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef1),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef2),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef3),
		gun:close(ConnPid)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

http10_keepalive_false(Config) ->
	doc("Confirm the option http10_keepalive => false disables keep-alive "
		"completely for HTTP/1.0 connections."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		http10_keepalive => false
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Keepalive = "GET / HTTP/1.0\r\nhost: localhost\r\nConnection: keep-alive\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Keepalive),
		Data = raw_recv_head(Client),
		%% Cowboy always advertises itself as HTTP/1.1.
		{'HTTP/1.1', 200, _, Rest} = cow_http:parse_status_line(Data),
		{Headers, _} = cow_http:parse_headers(Rest),
		{_, <<"close">>} = lists:keyfind(<<"connection">>, 1, Headers),
		ok = raw_send(Client, Keepalive),
		try raw_recv_head(Client) of
			_ -> error(not_closed)
		catch _:_ ->
			closed
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

idle_timeout_read_body(Config) ->
	doc("Ensure the idle_timeout drops connections when the "
		"connection is idle too long reading the request body."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		request_timeout => 60000,
		idle_timeout => 500
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		_StreamRef = gun:post(ConnPid, "/echo/read_body",
			#{<<"content-length">> => <<"12">>}),
		{error, {down, {shutdown, closed}}} = gun:await(ConnPid, undefined, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

idle_timeout_read_body_pipeline(Config) ->
	doc("Ensure the idle_timeout drops connections when the "
		"connection is idle too long reading the request body."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		request_timeout => 60000,
		idle_timeout => 500
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		StreamRef1 = gun:get(ConnPid, "/"),
		StreamRef2 = gun:get(ConnPid, "/"),
		_StreamRef3 = gun:post(ConnPid, "/echo/read_body",
			#{<<"content-length">> => <<"12">>}),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef1),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef2),
		{error, {down, {shutdown, closed}}} = gun:await(ConnPid, undefined, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

idle_timeout_skip_body(Config) ->
	doc("Ensure the idle_timeout drops connections when the "
		"connection is idle too long skipping the request body."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		request_timeout => 60000,
		idle_timeout => 500
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		StreamRef = gun:post(ConnPid, "/",
			#{<<"content-length">> => <<"12">>}),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef),
		{error, {down, {shutdown, closed}}} = gun:await(ConnPid, undefined, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

idle_timeout_infinity(Config) ->
	doc("Ensure the idle_timeout option accepts the infinity value."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		idle_timeout => infinity
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		timer:sleep(500),
		#{socket := Socket} = gun:info(ConnPid),
		Pid = get_remote_pid_tcp(Socket),
		_ = gun:post(ConnPid, "/echo/read_body",
			[{<<"content-type">>, <<"text/plain">>}]),
		Ref = erlang:monitor(process, Pid),
		receive
			{'DOWN', Ref, process, Pid, Reason} ->
				error(Reason)
		after 1000 ->
			gun:close(ConnPid)
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

idle_timeout_on_send(Config) ->
	doc("Ensure the idle timeout is not reset when sending (by default)."),
	do_idle_timeout_on_send(Config, http).

%% Also used by http2_SUITE.
do_idle_timeout_on_send(Config, Protocol) ->
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		idle_timeout => 1000
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, Protocol}, {port, Port}|Config]),
		{ok, Protocol} = gun:await_up(ConnPid),
		timer:sleep(500),
		#{socket := Socket} = gun:info(ConnPid),
		Pid = get_remote_pid_tcp(Socket),
		StreamRef = gun:get(ConnPid, "/streamed_result/10/250"),
		Ref = erlang:monitor(process, Pid),
		receive
			{gun_response, ConnPid, StreamRef, nofin, _Status, _Headers} ->
				do_idle_timeout_recv_loop(Ref, Pid, ConnPid, StreamRef, false)
		after 2000 ->
		      error(timeout)
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

idle_timeout_reset_on_send(Config) ->
	doc("Ensure the reset_idle_timeout_on_send results in the "
		"idle timeout resetting when sending ."),
	do_idle_timeout_reset_on_send(Config, http).

%% Also used by http2_SUITE.
do_idle_timeout_reset_on_send(Config, Protocol) ->
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		idle_timeout => 1000,
		reset_idle_timeout_on_send => true
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, Protocol}, {port, Port}|Config]),
		{ok, Protocol} = gun:await_up(ConnPid),
		timer:sleep(500),
		#{socket := Socket} = gun:info(ConnPid),
		Pid = get_remote_pid_tcp(Socket),
		StreamRef = gun:get(ConnPid, "/streamed_result/10/250"),
		Ref = erlang:monitor(process, Pid),
		receive
			{gun_response, ConnPid, StreamRef, nofin, _Status, _Headers} ->
				do_idle_timeout_recv_loop(Ref, Pid, ConnPid, StreamRef, true)
		after 2000 ->
		      error(timeout)
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

do_idle_timeout_recv_loop(Ref, Pid, ConnPid, StreamRef, ExpectCompletion) ->
	receive
		{gun_data, ConnPid, StreamRef, nofin, _Data} ->
			do_idle_timeout_recv_loop(Ref, Pid, ConnPid, StreamRef, ExpectCompletion);
		{gun_data, ConnPid, StreamRef, fin, _Data} when ExpectCompletion ->
			gun:close(ConnPid);
		{gun_data, ConnPid, StreamRef, fin, _Data} ->
			gun:close(ConnPid),
			error(completed);
		{'DOWN', Ref, process, Pid, _} when ExpectCompletion ->
			gun:close(ConnPid),
			error(exited);
		{'DOWN', Ref, process, Pid, _} ->
			 ok
	after 2000 ->
	      error(timeout)
	end.

invalid_response_headers_inform(Config) ->
	doc("Ensure invalid response headers are rejected by default."),
	Dispatch = cowboy_router:compile([{'_', [
		{"/inform", resp_invalid_headers_h, []}
	]}]),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => Dispatch}
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "GET /inform HTTP/1.1\r\nhost: localhost\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		{'HTTP/1.1', 500, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
		{_, _} = cow_http:parse_headers(Rest),
		ok
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

invalid_response_headers_inform_ignore(Config) ->
	doc("Ensure invalid response headers are sent "
		"when allowed by configuration."),
	Dispatch = cowboy_router:compile([{'_', [
		{"/inform", resp_invalid_headers_h, []}
	]}]),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => Dispatch},
		invalid_response_headers => ignore
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "GET /inform HTTP/1.1\r\nhost: localhost\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		{'HTTP/1.1', 100, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
		?assertError(function_clause, cow_http:parse_headers(Rest),
			"Invalid header goes through."),
		{'HTTP/1.1', 200, _, _} = cow_http:parse_status_line(raw_recv_head(Client)),
		ok
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

invalid_response_headers_reply(Config) ->
	doc("Ensure invalid response headers are rejected by default."),
	Dispatch = cowboy_router:compile([{'_', [
		{"/reply", resp_invalid_headers_h, []}
	]}]),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => Dispatch}
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "GET /reply HTTP/1.1\r\nhost: localhost\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		{'HTTP/1.1', 500, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
		{_, _} = cow_http:parse_headers(Rest),
		ok
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

invalid_response_headers_reply_ignore(Config) ->
	doc("Ensure invalid response headers are sent "
		"when allowed by configuration."),
	Dispatch = cowboy_router:compile([{'_', [
		{"/reply", resp_invalid_headers_h, []}
	]}]),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => Dispatch},
		invalid_response_headers => ignore
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "GET /reply HTTP/1.1\r\nhost: localhost\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		{'HTTP/1.1', 200, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
		?assertError(function_clause, cow_http:parse_headers(Rest),
			"Invalid header goes through.")
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

invalid_response_headers_stream_reply(Config) ->
	doc("Ensure invalid response headers are rejected by default."),
	Dispatch = cowboy_router:compile([{'_', [
		{"/stream_reply", resp_invalid_headers_h, []}
	]}]),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => Dispatch},
		invalid_response_headers => error_terminate
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "GET /stream_reply HTTP/1.1\r\nhost: localhost\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		{'HTTP/1.1', 500, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
		{_, _} = cow_http:parse_headers(Rest),
		ok
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

invalid_response_headers_stream_reply_ignore(Config) ->
	doc("Ensure invalid response headers are sent "
		"when allowed by configuration."),
	Dispatch = cowboy_router:compile([{'_', [
		{"/stream_reply", resp_invalid_headers_h, []}
	]}]),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => Dispatch},
		invalid_response_headers => ignore
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "GET /stream_reply HTTP/1.1\r\nhost: localhost\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		{'HTTP/1.1', 200, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
		?assertError(function_clause, cow_http:parse_headers(Rest),
			"Invalid header goes through.")
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

invalid_response_headers_stream_trailers(Config) ->
	doc("Ensure invalid response headers are rejected by default."),
	Dispatch = cowboy_router:compile([{'_', [
		{"/stream_trailers", resp_invalid_headers_h, []}
	]}]),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => Dispatch},
		invalid_response_headers => error_terminate
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "GET /stream_trailers HTTP/1.1\r\nhost: localhost\r\n"
			"te: trailers\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(raw_recv_head(Client)),
		{Headers, <<>>} = cow_http:parse_headers(Rest0),
		{_, _} = lists:keyfind(<<"trailer">>, 1, Headers),
		{ok, <<"2\r\nOK\r\n">>} = raw_recv(Client, 0, 1000),
		%% Connection should be closed without trailers.
		{error, closed} = raw_recv(Client, 0, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

invalid_response_headers_stream_trailers_ignore(Config) ->
	doc("Ensure invalid response headers are sent "
		"when allowed by configuration."),
	Dispatch = cowboy_router:compile([{'_', [
		{"/stream_trailers", resp_invalid_headers_h, []}
	]}]),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => Dispatch},
		invalid_response_headers => ignore
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "GET /stream_trailers HTTP/1.1\r\nhost: localhost\r\n"
			"te: trailers\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(raw_recv_head(Client)),
		{Headers, <<>>} = cow_http:parse_headers(Rest0),
		{_, _} = lists:keyfind(<<"trailer">>, 1, Headers),
		{ok, <<"2\r\nOK\r\n">>} = raw_recv(Client, 0, 1000),
		{ok, Rest1} = raw_recv(Client, 0, 1000),
		?assertError(function_clause, cow_http:parse_headers(Rest1),
			"Invalid header goes through.")
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

invalid_response_headers_switch_protocol(Config) ->
	doc("Ensure invalid response headers are rejected by default."),
	Dispatch = cowboy_router:compile([{'_', [
		{"/switch_protocol", resp_invalid_headers_h, []}
	]}]),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => Dispatch}
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "GET /switch_protocol HTTP/1.1\r\nhost: localhost\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		{'HTTP/1.1', 500, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
		{_, _} = cow_http:parse_headers(Rest),
		ok
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

invalid_response_headers_switch_protocol_ignore(Config) ->
	doc("Ensure invalid response headers are sent "
		"when allowed by configuration."),
	Dispatch = cowboy_router:compile([{'_', [
		{"/switch_protocol", resp_invalid_headers_h, []}
	]}]),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => Dispatch},
		invalid_response_headers => ignore
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "GET /switch_protocol HTTP/1.1\r\nhost: localhost\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		{'HTTP/1.1', 101, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
		?assertError(function_clause, cow_http:parse_headers(Rest),
			"Invalid header goes through.")
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

invalid_response_headers_early_error(Config) ->
	doc("Ensure invalid response headers are rejected by default."),
	Dispatch = cowboy_router:compile([{'_', []}]),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => Dispatch},
		stream_handlers => [stream_handler_invalid_headers_h]
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "TRACE / HTTP/1.1\r\nhost: localhost\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		%% We sent the initial response, ignoring the
		%% modified response from stream handlers.
		{'HTTP/1.1', 501, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
		{_, _} = cow_http:parse_headers(Rest),
		ok
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

invalid_response_headers_early_error_ignore(Config) ->
	doc("Ensure invalid response headers are sent "
		"when allowed by configuration."),
	Dispatch = cowboy_router:compile([{'_', []}]),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => Dispatch},
		invalid_response_headers => ignore,
		stream_handlers => [stream_handler_invalid_headers_h]
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "TRACE /switch_protocol HTTP/1.1\r\nhost: localhost\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		{'HTTP/1.1', 501, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
		?assertError(function_clause, cow_http:parse_headers(Rest),
			"Invalid header goes through.")
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

max_authority_length_host_header(Config) ->
	doc("Confirm the max_authority_length option correctly limits "
		"the length of the authority component in the host header."),
	Dispatch = cowboy_router:compile([{'_', [{"/", hello_h, []}]}]),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => Dispatch},
		max_authority_length => 10
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		RawConfig = [{port, Port}, {type, tcp}, {opts, []}|Config],
		%% At limit.
		#{code := 200} = rfc7230_SUITE:do_raw(RawConfig,
			"GET / HTTP/1.1\r\n"
			"Host: example.co\r\n"
			"\r\n"),
		%% Over limit.
		#{code := 414, client := Client} = rfc7230_SUITE:do_raw(RawConfig,
			"GET / HTTP/1.1\r\n"
			"Host: example.com\r\n"
			"\r\n"),
		{error, closed} = raw_recv(Client, 0, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

max_authority_length_request_line(Config) ->
	doc("Confirm the max_authority_length option correctly limits "
		"the length of the authority component in the absolute-form "
		"target of the request line."),
	Dispatch = cowboy_router:compile([{'_', [{"/", hello_h, []}]}]),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => Dispatch},
		max_authority_length => 10
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		RawConfig = [{port, Port}, {type, tcp}, {opts, []}|Config],
		%% At limit.
		#{code := 200} = rfc7230_SUITE:do_raw(RawConfig,
			"GET http://example.co/ HTTP/1.1\r\n"
			"Host: example.co\r\n"
			"\r\n"),
		%% Over limit.
		#{code := 414, client := Client} = rfc7230_SUITE:do_raw(RawConfig,
			"GET http://example.com/ HTTP/1.1\r\n"
			"Host: example.com\r\n"
			"\r\n"),
		{error, closed} = raw_recv(Client, 0, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

max_authorization_header_value_length(Config) ->
	doc("Confirm the max_authorization_header_value_length option "
		"correctly limits the length of authorization header values."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		max_authorization_header_value_length => 2048
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		do_max_header_value_length(Config, Port,
			<<"authorization">>, 2048),
		%% Confirm that other headers still use the default limit.
		do_max_header_value_length(Config, Port,
			<<"my-header">>, 4096)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

max_cookie_header_value_length(Config) ->
	doc("Confirm the max_cookie_header_value_length option "
		"correctly limits the length of cookie header values."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		max_cookie_header_value_length => 2048
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		do_max_header_value_length(Config, Port,
			<<"cookie">>, 2048),
		%% Confirm that other headers still use the default limit.
		do_max_header_value_length(Config, Port,
			<<"my-header">>, 4096)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

max_header_value_length(Config) ->
	doc("Confirm the max_header_value_length option "
		"correctly limits the length of header values."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		max_header_value_length => 2048
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		do_max_header_value_length(Config, Port,
			<<"my-header">>, 2048)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

max_header_value_length_default(Config) ->
	doc("Confirm the max_header_value_length option "
		"correctly limits the length of header values."),
	do_max_header_value_length(Config, config(port, Config),
		<<"my-header">>, 4096).

do_max_header_value_length(Config, Port, Name, MaxLen) ->
	ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
	{ok, http} = gun:await_up(ConnPid),
	StreamRef1 = gun:get(ConnPid, "/", #{Name => lists:duplicate(MaxLen, $a)}),
	{response, nofin, 200, _} = gun:await(ConnPid, StreamRef1),
	%% We * 2 because this is a soft limit.
	StreamRef2 = gun:get(ConnPid, "/", #{Name => lists:duplicate(MaxLen * 2, $a)}),
	{response, fin, 431, _} = gun:await(ConnPid, StreamRef2),
	gun:close(ConnPid).

persistent_term_router(Config) ->
	doc("The router can retrieve the routes from persistent_term storage."),
	case erlang:function_exported(persistent_term, get, 1) of
		true -> do_persistent_term_router(Config);
		false -> {skip, "This test uses the persistent_term functionality added in Erlang/OTP 21.2."}
	end.

do_persistent_term_router(Config) ->
	persistent_term:put(?FUNCTION_NAME, init_dispatch(Config)),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => {persistent_term, ?FUNCTION_NAME}}
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		StreamRef = gun:get(ConnPid, "/"),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef),
		gun:close(ConnPid)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

request_timeout(Config) ->
	doc("Ensure the request_timeout drops connections when requests "
		"fail to come in fast enough."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		request_timeout => 500
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		{error, {down, {shutdown, closed}}} = gun:await(ConnPid, undefined, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

request_timeout_pipeline(Config) ->
	doc("Ensure the request_timeout drops connections when requests "
		"fail to come in fast enough after pipelined requests went through."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		request_timeout => 500
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		StreamRef1 = gun:get(ConnPid, "/"),
		StreamRef2 = gun:get(ConnPid, "/"),
		StreamRef3 = gun:get(ConnPid, "/"),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef1),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef2),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef3),
		{error, {down, {shutdown, closed}}} = gun:await(ConnPid, undefined, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

request_timeout_pipeline_delay(Config) ->
	doc("Ensure the request_timeout does not trigger on requests "
		"coming in after a large request body."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		request_timeout => 500
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		StreamRef1 = gun:post(ConnPid, "/", #{}, <<0:8000000>>),
		StreamRef2 = gun:get(ConnPid, "/delay_hello"),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef1),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef2),
		{error, {down, {shutdown, closed}}} = gun:await(ConnPid, undefined, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

request_timeout_skip_body(Config) ->
	doc("Ensure the request_timeout drops connections when requests "
		"fail to come in fast enough after skipping a request body."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		request_timeout => 500
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, <<
			"POST / HTTP/1.1\r\n"
			"host: localhost\r\n"
			"content-length: 12\r\n\r\n"
		>>),
		Data = raw_recv_head(Client),
		{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
		{Headers, Rest} = cow_http:parse_headers(Rest0),
		{_, Len} = lists:keyfind(<<"content-length">>, 1, Headers),
		<<"Hello world!">> = raw_recv_rest(Client, binary_to_integer(Len), Rest),
		%% We then send the request data that should be skipped by Cowboy.
		timer:sleep(100),
		raw_send(Client, <<"Hello world!">>),
		%% Connection should be closed by the request_timeout after that.
		{error, closed} = raw_recv(Client, 1, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

request_timeout_skip_body_more(Config) ->
	doc("Ensure the request_timeout drops connections when requests "
		"fail to come in fast enough after skipping a request body."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		request_timeout => 500
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, <<
			"POST / HTTP/1.1\r\n"
			"host: localhost\r\n"
			"content-length: 12\r\n\r\n"
		>>),
		Data = raw_recv_head(Client),
		{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
		{Headers, Rest} = cow_http:parse_headers(Rest0),
		{_, Len} = lists:keyfind(<<"content-length">>, 1, Headers),
		<<"Hello world!">> = raw_recv_rest(Client, binary_to_integer(Len), Rest),
		%% We then send the request data that should be skipped by Cowboy.
		timer:sleep(100),
		raw_send(Client, <<"Hello world!">>),
		%% Send the start of another request.
		ok = raw_send(Client, <<
			"GET / HTTP/1.1\r\n"
			"host: localhost\r\n"
			%% Missing final \r\n on purpose.
		>>),
		%% Connection should be closed by the request_timeout after that.
		%% We attempt to send a 408 response on a best effort basis so
		%% that is accepted as well.
		case raw_recv(Client, 13, 1000) of
			{error, closed} -> ok;
			{ok, <<"HTTP/1.1 408 ", _/bits>>} -> ok
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

request_timeout_infinity(Config) ->
	doc("Ensure the request_timeout option accepts the infinity value."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		request_timeout => infinity
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		timer:sleep(500),
		#{socket := Socket} = gun:info(ConnPid),
		Pid = get_remote_pid_tcp(Socket),
		Ref = erlang:monitor(process, Pid),
		receive
			{'DOWN', Ref, process, Pid, Reason} ->
				error(Reason)
		after 1000 ->
			gun:close(ConnPid)
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

set_options_chunked_false(Config) ->
	doc("Confirm the option chunked can be dynamically set to disable "
		"chunked transfer-encoding. This results in the closing of the "
		"connection after the current request."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		chunked => true
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "GET /set_options/chunked_false HTTP/1.1\r\nhost: localhost\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		Data = raw_recv_head(Client),
		%% Cowboy always advertises itself as HTTP/1.1.
		{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
		{Headers, Rest} = cow_http:parse_headers(Rest0),
		false = lists:keyfind(<<"content-length">>, 1, Headers),
		false = lists:keyfind(<<"transfer-encoding">>, 1, Headers),
		Bits = 8000000 - bit_size(Rest),
		raw_expect_recv(Client, <<0:Bits>>),
		{error, closed} = raw_recv(Client, 1, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

set_options_chunked_false_ignored(Config) ->
	doc("Confirm the option chunked can be dynamically set to disable "
		"chunked transfer-encoding, and that it is ignored if the "
		"response is not streamed."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		chunked => true
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		%% We do a first request setting the option but not
		%% using chunked transfer-encoding in the response.
		StreamRef1 = gun:get(ConnPid, "/set_options/chunked_false_ignored"),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef1),
		{ok, <<"Hello world!">>} = gun:await_body(ConnPid, StreamRef1),
		%% We then do a second request to confirm that chunked
		%% is not disabled for that second request.
		StreamRef2 = gun:get(ConnPid, "/resp/stream_reply2/200"),
		{response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef2),
		{_, <<"chunked">>} = lists:keyfind(<<"transfer-encoding">>, 1, Headers),
		gun:close(ConnPid)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

set_options_idle_timeout(Config) ->
	doc("Confirm that the idle_timeout option can be dynamically "
		"set to change how long Cowboy will wait before it closes the connection."),
	%% We start with a long timeout and then cut it short.
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		idle_timeout => 60000
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		timer:sleep(500),
		#{socket := Socket} = gun:info(ConnPid),
		Pid = get_remote_pid_tcp(Socket),
		_ = gun:post(ConnPid, "/set_options/idle_timeout_short",
			[{<<"content-type">>, <<"text/plain">>}]),
		Ref = erlang:monitor(process, Pid),
		receive
			{'DOWN', Ref, process, Pid, _} ->
				ok
		after 2000 ->
			error(timeout)
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

set_options_idle_timeout_only_applies_to_current_request(Config) ->
	doc("Confirm that changes to the idle_timeout option only apply to the current stream."),
	%% We start with a long timeout and then cut it short.
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		idle_timeout => 500
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		timer:sleep(500),
		#{socket := Socket} = gun:info(ConnPid),
		Pid = get_remote_pid_tcp(Socket),
		StreamRef = gun:post(ConnPid, "/set_options/idle_timeout_long",
			[{<<"content-type">>, <<"text/plain">>}]),
		Ref = erlang:monitor(process, Pid),
		receive
			{'DOWN', Ref, process, Pid, Reason} ->
				error(Reason)
		after 2000 ->
			ok
		end,
		%% Finish the first request and start a second one to confirm
		%% the idle_timeout option is back to normal.
		gun:data(ConnPid, StreamRef, fin, <<"Hello!">>),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef),
		{ok, <<"Hello!">>} = gun:await_body(ConnPid, StreamRef),
		_ = gun:post(ConnPid, "/echo/read_body",
			[{<<"content-type">>, <<"text/plain">>}]),
		receive
			{'DOWN', Ref, process, Pid, _} ->
				ok
		after 2000 ->
			error(timeout)
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

switch_protocol_flush(Config) ->
	doc("Confirm that switch_protocol does not flush unrelated messages."),
	ProtoOpts = #{
		env => #{dispatch => init_dispatch(Config)},
		stream_handlers => [switch_protocol_flush_h]
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Self = self(),
		ConnPid = gun_open([{port, Port}, {type, tcp}, {protocol, http}|Config]),
		_ = gun:get(ConnPid, "/", [
			{<<"x-test-pid">>, pid_to_list(Self)}
		]),
		receive
			{Self, Events} ->
				switch_protocol_flush_h:validate(Events)
		after 5000 ->
			error(timeout)
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

graceful_shutdown_connection(Config) ->
	doc("Check that the current request is handled before gracefully "
	    "shutting down a connection."),
	Dispatch = cowboy_router:compile([{"localhost", [
		{"/hello", hello_h, #{}},
		{"/delay_hello", delay_hello_h,
			#{delay => 1000, notify_received => self()}}
	]}]),
	ProtoOpts = #{
		env => #{dispatch => Dispatch}
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client,
			"GET /delay_hello HTTP/1.1\r\n"
			"Host: localhost\r\n\r\n"
			"GET /hello HTTP/1.1\r\n"
			"Host: localhost\r\n\r\n"),
		receive {request_received, <<"/delay_hello">>} -> ok end,
		CowboyConnPid = get_remote_pid_tcp(element(2, Client)),
		CowboyConnRef = erlang:monitor(process, CowboyConnPid),
		ok = sys:terminate(CowboyConnPid, system_is_going_down),
		Data = raw_recv_head(Client),
		{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
		{Headers, Rest} = cow_http:parse_headers(Rest0),
		<<"close">> = proplists:get_value(<<"connection">>, Headers),
		<<"Hello world!">> = raw_recv_rest(Client, byte_size(<<"Hello world!">>), Rest),
		{error, closed} = raw_recv(Client, 0, 1000),
		receive
			{'DOWN', CowboyConnRef, process, CowboyConnPid, _Reason} ->
				ok
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

graceful_shutdown_listener(Config) ->
	doc("Check that connections are shut down gracefully when stopping a listener."),
	TransOpts = #{
		socket_opts => [{port, 0}],
		shutdown => 1000 %% Shorter timeout to make the test case faster.
	},
	Dispatch = cowboy_router:compile([{"localhost", [
		{"/delay_hello", delay_hello_h,
			#{delay => 500, notify_received => self()}},
		{"/long_delay_hello", delay_hello_h,
			#{delay => 10000, notify_received => self()}}
	]}]),
	ProtoOpts = #{
		env => #{dispatch => Dispatch}
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, TransOpts, ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	ConnPid1 = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
	Ref1 = gun:get(ConnPid1, "/delay_hello"),
	ConnPid2 = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
	Ref2 = gun:get(ConnPid2, "/long_delay_hello"),
	%% Shutdown listener while the handlers are working.
	receive {request_received, <<"/delay_hello">>} -> ok end,
	receive {request_received, <<"/long_delay_hello">>} -> ok end,
	%% Note: This call does not complete quickly and will
	%% prevent other cowboy:stop_listener/1 calls to complete.
	ok = cowboy:stop_listener(?FUNCTION_NAME),
	%% Check that the 1st request is handled before shutting down.
	{response, nofin, 200, RespHeaders} = gun:await(ConnPid1, Ref1),
	<<"close">> = proplists:get_value(<<"connection">>, RespHeaders),
	{ok, RespBody} = gun:await_body(ConnPid1, Ref1),
	<<"Hello world!">> = iolist_to_binary(RespBody),
	gun:close(ConnPid1),
	%% Check that the 2nd (very slow) request is not handled.
	{error, {stream_error, closed}} = gun:await(ConnPid2, Ref2),
	gun:close(ConnPid2).

send_timeout_close(_Config) ->
	doc("Check that connections are closed on send timeout."),
	TransOpts = #{
		socket_opts => [
			{port, 0},
			{send_timeout, 100},
			{send_timeout_close, true},
			{sndbuf, 10}
		]
	},
	Dispatch = cowboy_router:compile([{"localhost", [
		{"/endless", loop_handler_endless_h, #{delay => 100}}
	]}]),
	ProtoOpts = #{
		env => #{dispatch => Dispatch},
		idle_timeout => infinity
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, TransOpts, ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		%% Connect a client that sends a request and waits indefinitely.
		{ok, ClientSocket} = gen_tcp:connect("localhost", Port,
			[{recbuf, 10}, {buffer, 10}, {active, false}, {packet, 0}]),
		ok = gen_tcp:send(ClientSocket, [
			"GET /endless HTTP/1.1\r\n",
			"Host: localhost:", integer_to_list(Port), "\r\n",
			"x-test-pid: ", pid_to_list(self()), "\r\n\r\n"
		]),
		%% Wait for the handler to start then get its pid,
		%% the remote connection's pid and socket.
		StreamPid = receive
			{Self, StreamPid0, init} when Self =:= self() ->
				StreamPid0
		after 1000 ->
			error(timeout)
		end,
		ServerPid = ct_helper:get_remote_pid_tcp(ClientSocket),
		{links, ServerLinks} = process_info(ServerPid, links),
		[ServerSocket] = [PidOrPort || PidOrPort <- ServerLinks, is_port(PidOrPort)],
		%% Poll the socket repeatedly until it is closed by the server.
		WaitClosedFun =
			fun F(T) when T =< 0 ->
					error({status, prim_inet:getstatus(ServerSocket)});
				F(T) ->
					Snooze = 100,
					case inet:sockname(ServerSocket) of
						{error, _} ->
							timer:sleep(Snooze);
						{ok, _} ->
							timer:sleep(Snooze),
							F(T - Snooze)
					end
			end,
		ok = WaitClosedFun(2000),
		false = erlang:is_process_alive(StreamPid),
		false = erlang:is_process_alive(ServerPid)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.
