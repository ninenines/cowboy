%% Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
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

-module(rfc7540_SUITE).
-compile(export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [raw_open/1]).
-import(cowboy_test, [raw_send/2]).
-import(cowboy_test, [raw_recv_head/1]).
-import(cowboy_test, [raw_recv/3]).

all() -> [{group, clear}, {group, tls}].

groups() ->
	Modules = ct_helper:all(?MODULE),
	Clear = [M || M <- Modules, lists:sublist(atom_to_list(M), 4) =/= "alpn"] -- [prior_knowledge_reject_tls],
	TLS = [M || M <- Modules, lists:sublist(atom_to_list(M), 4) =:= "alpn"] ++ [prior_knowledge_reject_tls],
	[{clear, [parallel], Clear}, {tls, [parallel], TLS}].

init_per_group(Name = clear, Config) ->
	cowboy_test:init_http(Name = clear, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))}
	}, Config);
init_per_group(Name = tls, Config) ->
	cowboy_test:init_http2(Name = tls, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))}
	}, Config).

end_per_group(Name, _) ->
	ok = cowboy:stop_listener(Name).

init_routes(_) -> [
	{"localhost", [
		{"/", hello_h, []}
	]}
].

%% Starting HTTP/2 for "http" URIs.

http_upgrade_ignore_h2(Config) ->
	doc("An h2 token in an Upgrade field must be ignored. (RFC7540 3.2)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	{ok, <<"HTTP/1.1 200">>} = gen_tcp:recv(Socket, 12, 1000),
	ok.

http_upgrade_ignore_if_http_10(Config) ->
	doc("The Upgrade header must be ignored if part of an HTTP/1.0 request. (RFC7230 6.7)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.0\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	{ok, <<"HTTP/1.1 200">>} = gen_tcp:recv(Socket, 12, 1000),
	ok.

http_upgrade_ignore_missing_upgrade_in_connection(Config) ->
	doc("The Upgrade header must be listed in the "
		"Connection header field. (RFC7230 6.7)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	{ok, <<"HTTP/1.1 200">>} = gen_tcp:recv(Socket, 12, 1000),
	ok.

http_upgrade_ignore_missing_http2_settings_in_connection(Config) ->
	doc("The HTTP2-Settings header must be listed in the "
		"Connection header field. (RFC7540 3.2.1, RFC7230 6.7)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	{ok, <<"HTTP/1.1 200">>} = gen_tcp:recv(Socket, 12, 1000),
	ok.

http_upgrade_reject_zero_http2_settings_header(Config) ->
	doc("The HTTP Upgrade request must include "
		"exactly one HTTP2-Settings header field (RFC7540 3.2, RFC7540 3.2.1)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"\r\n"]),
	{ok, <<"HTTP/1.1 400">>} = gen_tcp:recv(Socket, 12, 1000),
	ok.

http_upgrade_reject_two_http2_settings_header(Config) ->
	doc("The HTTP Upgrade request must include "
		"exactly one HTTP2-Settings header field (RFC7540 3.2, RFC7540 3.2.1)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	{ok, <<"HTTP/1.1 400">>} = gen_tcp:recv(Socket, 12, 1000),
	ok.

http_upgrade_reject_bad_http2_settings_header(Config) ->
	doc("The HTTP Upgrade request must include "
		"a valid HTTP2-Settings header field (RFC7540 3.2, RFC7540 3.2.1)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		%% We send a full SETTINGS frame on purpose.
		"HTTP2-Settings: ", base64:encode(cow_http2:settings(#{})), "\r\n",
		"\r\n"]),
	{ok, <<"HTTP/1.1 400">>} = gen_tcp:recv(Socket, 12, 1000),
	ok.

%% Match directly for now.
do_recv_101(Socket) ->
	{ok, <<
		"HTTP/1.1 101 Switching Protocols\r\n"
		"connection: Upgrade\r\n"
		"upgrade: h2c\r\n"
		"\r\n"
	>>} = gen_tcp:recv(Socket, 71, 1000),
	ok.

http_upgrade_101(Config) ->
	doc("A 101 response must be sent on successful upgrade "
		"to HTTP/2 when using the HTTP Upgrade mechanism. (RFC7540 3.2, RFC7230 6.7)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	ok = do_recv_101(Socket),
	ok.

http_upgrade_server_preface(Config) ->
	doc("The first frame after the upgrade must be a "
		"SETTINGS frame for the server connection preface. (RFC7540 3.2, RFC7540 3.5, RFC7540 6.5)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	ok = do_recv_101(Socket),
	%% Receive the server preface.
	{ok, << _:24, 4:8, 0:40 >>} = gen_tcp:recv(Socket, 9, 1000),
	ok.

http_upgrade_client_preface_timeout(Config) ->
	doc("Clients negotiating HTTP/2 and not sending a preface in "
		"a timely manner must be disconnected."),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	ok = do_recv_101(Socket),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% Do not send the preface. Wait for the server to disconnect us.
	{error, closed} = gen_tcp:recv(Socket, 9, 6000),
	ok.

http_upgrade_reject_missing_client_preface(Config) ->
	doc("Servers must treat an invalid connection preface as a "
		"connection error of type PROTOCOL_ERROR. (RFC7540 3.2, RFC7540 3.5)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	ok = do_recv_101(Socket),
	%% Send a SETTINGS frame directly instead of the proper preface.
	ok = gen_tcp:send(Socket, cow_http2:settings(#{})),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% We expect the server to close the connection when it receives a bad preface.
	{error, closed} = gen_tcp:recv(Socket, 9, 1000),
	ok.

http_upgrade_reject_invalid_client_preface(Config) ->
	doc("Servers must treat an invalid connection preface as a "
		"connection error of type PROTOCOL_ERROR. (RFC7540 3.2, RFC7540 3.5)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	ok = do_recv_101(Socket),
	%% Send a slightly incorrect preface.
	ok = gen_tcp:send(Socket, "PRI * HTTP/2.0\r\n\r\nSM: Value\r\n\r\n"),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% We expect the server to close the connection when it receives a bad preface.
	%% The server may however have already started sending the response to the
	%% initial HTTP/1.1 request.
	Received = lists:reverse(lists:foldl(fun(_, Acc) ->
		case gen_tcp:recv(Socket, 9, 1000) of
			{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} ->
				{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
				[headers|Acc];
			{ok, << SkipLen:24, 0:8, _:8, 1:32 >>} ->
				{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
				[data|Acc];
			{error, _} ->
				[closed|Acc]
		end
	end, [], [1, 2, 3])),
	case Received of
		[closed|_] -> ok;
		[headers, closed|_] -> ok;
		[headers, data, closed] -> ok
	end.

http_upgrade_reject_missing_client_preface_settings(Config) ->
	doc("Servers must treat an invalid connection preface as a "
		"connection error of type PROTOCOL_ERROR. (RFC7540 3.2, RFC7540 3.5)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	ok = do_recv_101(Socket),
	%% Send a valid preface sequence except followed by a PING instead of a SETTINGS frame.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:ping(0)]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% We expect the server to close the connection when it receives a bad preface.
	{error, closed} = gen_tcp:recv(Socket, 9, 1000),
	ok.

http_upgrade_reject_invalid_client_preface_settings(Config) ->
	doc("Servers must treat an invalid connection preface as a "
		"connection error of type PROTOCOL_ERROR. (RFC7540 3.2, RFC7540 3.5)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	ok = do_recv_101(Socket),
	%% Send a valid preface sequence except followed by a badly formed SETTINGS frame.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", << 0:24, 4:8, 0:9, 1:31 >>]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% We expect the server to close the connection when it receives a bad preface.
	%% The server may however have already started sending the response to the
	%% initial HTTP/1.1 request.
	Received = lists:reverse(lists:foldl(fun(_, Acc) ->
		case gen_tcp:recv(Socket, 9, 1000) of
			{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} ->
				{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
				[headers|Acc];
			{ok, << SkipLen:24, 0:8, _:8, 1:32 >>} ->
				{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
				[data|Acc];
			{error, _} ->
				[closed|Acc]
		end
	end, [], [1, 2, 3])),
	case Received of
		[closed|_] -> ok;
		[headers, closed|_] -> ok;
		[headers, data, closed] -> ok
	end.

http_upgrade_accept_client_preface_empty_settings(Config) ->
	doc("The SETTINGS frame in the client preface may be empty. (RFC7540 3.2, RFC7540 3.5)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	ok = do_recv_101(Socket),
	%% Send a valid preface sequence except followed by an empty SETTINGS frame.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	ok.

http_upgrade_client_preface_settings_ack_timeout(Config) ->
	doc("The SETTINGS frames sent by the client must be acknowledged. (RFC7540 3.5, RFC7540 6.5.3)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	ok = do_recv_101(Socket),
	%% Send a valid preface.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	%% Do not ack the server preface. Expect a GOAWAY with reason SETTINGS_TIMEOUT.
	{ok, << _:24, 7:8, _:72, 4:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% @todo We need a successful test with actual options in HTTP2-Settings.

%% @todo We need to test an upgrade with a request body (small and too large).

%% @todo Also assigned default priority values but not sure how to test that.
http_upgrade_response(Config) ->
	doc("A response must be sent to the initial HTTP/1.1 request "
		"after switching to HTTP/2. The response must use "
		"the stream identifier 1. (RFC7540 3.2)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	ok = do_recv_101(Socket),
	%% Send a valid preface.
	%% @todo Use non-empty SETTINGS here. Just because.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% Send the SETTINGS ack.
	ok = gen_tcp:send(Socket, cow_http2:settings_ack()),
	%% Receive the SETTINGS ack, and the response HEADERS and DATA (streamid 1).
	Received = lists:reverse(lists:foldl(fun(_, Acc) ->
		case gen_tcp:recv(Socket, 9, 1000) of
			{ok, << 0:24, 4:8, 1:8, 0:32 >>} ->
				[settings_ack|Acc];
			{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} ->
				{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
				[headers|Acc];
			{ok, << SkipLen:24, 0:8, _:8, 1:32 >>} ->
				{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
				[data|Acc]
		end
	end, [], [1, 2, 3])),
	case Received of
		[settings_ack, headers, data] -> ok;
		[headers, settings_ack, data] -> ok;
		[headers, data, settings_ack] -> ok
	end.

http_upgrade_response_half_closed(Config) ->
	doc("The stream for the initial HTTP/1.1 request is half-closed. (RFC7540 3.2)"),
	%% Try sending more data after the upgrade and get an error.
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(cow_http2:settings_payload(#{})), "\r\n",
		"\r\n"]),
	ok = do_recv_101(Socket),
	%% Send a valid preface.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Send more data on the stream to trigger an error.
	ok = gen_tcp:send(Socket, cow_http2:data(1, fin, <<>>)),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% Receive the SETTINGS ack.
	%% @todo It's possible that we receive the response before the SETTINGS ack or RST_STREAM.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	%% The server resets the stream with reason STREAM_CLOSED.
	{ok, << 4:24, 3:8, 0:8, 1:32, 5:32 >>} = gen_tcp:recv(Socket, 13, 1000),
	ok.

%% Starting HTTP/2 for "https" URIs.

alpn_ignore_h2c(Config) ->
	doc("An h2c ALPN protocol identifier must be ignored. (RFC7540 3.3)"),
	{ok, Socket} = ssl:connect("localhost", config(port, Config),
		[{alpn_advertised_protocols, [<<"h2c">>, <<"http/1.1">>]}, binary, {active, false}]),
	{ok, <<"http/1.1">>} = ssl:negotiated_protocol(Socket),
	ok.

alpn_server_preface(Config) ->
	doc("The first frame must be a SETTINGS frame "
		"for the server connection preface. (RFC7540 3.3, RFC7540 3.5, RFC7540 6.5)"),
	{ok, Socket} = ssl:connect("localhost", config(port, Config),
		[{alpn_advertised_protocols, [<<"h2">>]}, binary, {active, false}]),
	{ok, <<"h2">>} = ssl:negotiated_protocol(Socket),
	%% Receive the server preface.
	{ok, << _:24, 4:8, 0:40 >>} = ssl:recv(Socket, 9, 1000),
	ok.

alpn_client_preface_timeout(Config) ->
	doc("Clients negotiating HTTP/2 and not sending a preface in "
		"a timely manner must be disconnected."),
	{ok, Socket} = ssl:connect("localhost", config(port, Config),
		[{alpn_advertised_protocols, [<<"h2">>]}, binary, {active, false}]),
	{ok, <<"h2">>} = ssl:negotiated_protocol(Socket),
	%% Receive the server preface.
	{ok, << Len:24 >>} = ssl:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = ssl:recv(Socket, 6 + Len, 1000),
	%% Do not send the preface. Wait for the server to disconnect us.
	{error, closed} = ssl:recv(Socket, 3, 6000),
	ok.

alpn_reject_missing_client_preface(Config) ->
	doc("Servers must treat an invalid connection preface as a "
		"connection error of type PROTOCOL_ERROR. (RFC7540 3.3, RFC7540 3.5)"),
	{ok, Socket} = ssl:connect("localhost", config(port, Config),
		[{alpn_advertised_protocols, [<<"h2">>]}, binary, {active, false}]),
	{ok, <<"h2">>} = ssl:negotiated_protocol(Socket),
	%% Send a SETTINGS frame directly instead of the proper preface.
	ok = ssl:send(Socket, cow_http2:settings(#{})),
	%% Receive the server preface.
	{ok, << Len:24 >>} = ssl:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = ssl:recv(Socket, 6 + Len, 1000),
	%% We expect the server to close the connection when it receives a bad preface.
	{error, closed} = ssl:recv(Socket, 3, 1000),
	ok.

alpn_reject_invalid_client_preface(Config) ->
	doc("Servers must treat an invalid connection preface as a "
		"connection error of type PROTOCOL_ERROR. (RFC7540 3.3, RFC7540 3.5)"),
	{ok, Socket} = ssl:connect("localhost", config(port, Config),
		[{alpn_advertised_protocols, [<<"h2">>]}, binary, {active, false}]),
	{ok, <<"h2">>} = ssl:negotiated_protocol(Socket),
	%% Send a slightly incorrect preface.
	ok = ssl:send(Socket, "PRI * HTTP/2.0\r\n\r\nSM: Value\r\n\r\n"),
	%% Receive the server preface.
	{ok, << Len:24 >>} = ssl:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = ssl:recv(Socket, 6 + Len, 1000),
	%% We expect the server to close the connection when it receives a bad preface.
	{error, closed} = ssl:recv(Socket, 3, 1000),
	ok.

alpn_reject_missing_client_preface_settings(Config) ->
	doc("Servers must treat an invalid connection preface as a "
		"connection error of type PROTOCOL_ERROR. (RFC7540 3.3, RFC7540 3.5)"),
	{ok, Socket} = ssl:connect("localhost", config(port, Config),
		[{alpn_advertised_protocols, [<<"h2">>]}, binary, {active, false}]),
	{ok, <<"h2">>} = ssl:negotiated_protocol(Socket),
	%% Send a valid preface sequence except followed by a PING instead of a SETTINGS frame.
	ok = ssl:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:ping(0)]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = ssl:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = ssl:recv(Socket, 6 + Len, 1000),
	%% We expect the server to close the connection when it receives a bad preface.
	{error, closed} = ssl:recv(Socket, 3, 1000),
	ok.

alpn_reject_invalid_client_preface_settings(Config) ->
	doc("Servers must treat an invalid connection preface as a "
		"connection error of type PROTOCOL_ERROR. (RFC7540 3.3, RFC7540 3.5)"),
	{ok, Socket} = ssl:connect("localhost", config(port, Config),
		[{alpn_advertised_protocols, [<<"h2">>]}, binary, {active, false}]),
	{ok, <<"h2">>} = ssl:negotiated_protocol(Socket),
	%% Send a valid preface sequence except followed by a badly formed SETTINGS frame.
	ok = ssl:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", << 0:24, 4:8, 0:9, 1:31 >>]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = ssl:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = ssl:recv(Socket, 6 + Len, 1000),
	%% We expect the server to close the connection when it receives a bad preface.
	{error, closed} = ssl:recv(Socket, 3, 1000),
	ok.

alpn_accept_client_preface_empty_settings(Config) ->
	doc("The SETTINGS frame in the client preface may be empty. (RFC7540 3.3, RFC7540 3.5)"),
	{ok, Socket} = ssl:connect("localhost", config(port, Config),
		[{alpn_advertised_protocols, [<<"h2">>]}, binary, {active, false}]),
	{ok, <<"h2">>} = ssl:negotiated_protocol(Socket),
	%% Send a valid preface sequence except followed by an empty SETTINGS frame.
	ok = ssl:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = ssl:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = ssl:recv(Socket, 6 + Len, 1000),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = ssl:recv(Socket, 9, 1000),
	ok.

alpn_client_preface_settings_ack_timeout(Config) ->
	doc("Failure to acknowledge the server's SETTINGS frame "
		"results in a SETTINGS_TIMEOUT connection error. (RFC7540 3.5, RFC7540 6.5.3)"),
	{ok, Socket} = ssl:connect("localhost", config(port, Config),
		[{alpn_advertised_protocols, [<<"h2">>]}, binary, {active, false}]),
	{ok, <<"h2">>} = ssl:negotiated_protocol(Socket),
	%% Send a valid preface.
	ok = ssl:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = ssl:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = ssl:recv(Socket, 6 + Len, 1000),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = ssl:recv(Socket, 9, 1000),
	%% Do not ack the server preface. Expect a GOAWAY with reason SETTINGS_TIMEOUT.
	{ok, << _:24, 7:8, _:72, 4:32 >>} = ssl:recv(Socket, 17, 6000),
	ok.

alpn(Config) ->
	doc("Successful ALPN negotiation. (RFC7540 3.3)"),
	{ok, Socket} = ssl:connect("localhost", config(port, Config),
		[{alpn_advertised_protocols, [<<"h2">>]}, binary, {active, false}]),
	{ok, <<"h2">>} = ssl:negotiated_protocol(Socket),
	%% Send a valid preface.
	%% @todo Use non-empty SETTINGS here. Just because.
	ok = ssl:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = ssl:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = ssl:recv(Socket, 6 + Len, 1000),
	%% Send the SETTINGS ack.
	ok = ssl:send(Socket, cow_http2:settings_ack()),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = ssl:recv(Socket, 9, 1000),
	%% Wait until after the SETTINGS ack timeout was supposed to trigger.
	receive after 6000 -> ok end,
	%% Send a PING.
	ok = ssl:send(Socket, cow_http2:ping(0)),
	%% Receive a PING ack back, indicating the connection is still up.
	{ok, << 8:24, 6:8, 0:7, 1:1, 0:96 >>} = ssl:recv(Socket, 17, 1000),
	ok.

%% Starting HTTP/2 with prior knowledge.

prior_knowledge_reject_tls(Config) ->
	doc("Implementations that support HTTP/2 over TLS must use ALPN. (RFC7540 3.4)"),
	{ok, Socket} = ssl:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a valid preface.
	ok = ssl:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% We expect the server to send an HTTP 400 error
	%% when trying to use HTTP/2 without going through ALPN negotiation.
	{ok, <<"HTTP/1.1 400">>} = ssl:recv(Socket, 12, 1000),
	ok.

prior_knowledge_server_preface(Config) ->
	doc("The first frame must be a SETTINGS frame "
		"for the server connection preface. (RFC7540 3.4, RFC7540 3.5, RFC7540 6.5)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a valid preface.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, << _:24, 4:8, 0:40 >>} = gen_tcp:recv(Socket, 9, 1000),
	ok.

%% Note: the client preface timeout doesn't apply in this case,
%% so we don't test it. An HTTP/1.1 client that does not send
%% a request in a timely manner will get disconnected by the
%% HTTP protocol code, not by HTTP/2's.

%% Note: the test that starts by sending a SETTINGS frame is
%% redundant with tests sending garbage on the connection.
%% From the point of view of an HTTP/1.1 connection, a
%% SETTINGS frame is indistinguishable from garbage.

prior_knowledge_reject_invalid_client_preface(Config) ->
	doc("An incorrect preface is an invalid HTTP/1.1 request. (RFC7540 3.4)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a slightly incorrect preface.
	ok = gen_tcp:send(Socket, "PRI * HTTP/2.0\r\n\r\nSM: Value\r\n\r\n"),
	%% We propagate to HTTP/2 after checking only the request-line.
	%% The server then sends its preface before checking the full client preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% We expect the server to close the connection when it receives a bad preface.
	{error, closed} = gen_tcp:recv(Socket, 9, 1000),
	ok.

prior_knowledge_reject_missing_client_preface_settings(Config) ->
	doc("Servers must treat an invalid connection preface as a "
		"connection error of type PROTOCOL_ERROR. (RFC7540 3.4, RFC7540 3.5)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a valid preface sequence except followed by a PING instead of a SETTINGS frame.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:ping(0)]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% We expect the server to close the connection when it receives a bad preface.
	{error, closed} = gen_tcp:recv(Socket, 9, 1000),
	ok.

prior_knowledge_reject_invalid_client_preface_settings(Config) ->
	doc("Servers must treat an invalid connection preface as a "
		"connection error of type PROTOCOL_ERROR. (RFC7540 3.4, RFC7540 3.5)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a valid preface sequence except followed by a badly formed SETTINGS frame.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", << 0:24, 4:8, 0:9, 1:31 >>]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% We expect the server to close the connection when it receives a bad preface.
	{error, closed} = gen_tcp:recv(Socket, 9, 1000),
	ok.

prior_knowledge_accept_client_preface_empty_settings(Config) ->
	doc("The SETTINGS frame in the client preface may be empty. (RFC7540 3.4, RFC7540 3.5)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a valid preface sequence except followed by an empty SETTINGS frame.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	ok.

prior_knowledge_client_preface_settings_ack_timeout(Config) ->
	doc("The SETTINGS frames sent by the client must be acknowledged. (RFC7540 3.5, RFC7540 6.5.3)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a valid preface.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	%% Do not ack the server preface. Expect a GOAWAY with reason SETTINGS_TIMEOUT.
	{ok, << _:24, 7:8, _:72, 4:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

prior_knowledge(Config) ->
	doc("Streams can be initiated after a successful HTTP/2 connection "
		"with prior knowledge of server capabilities. (RFC7540 3.4)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a valid preface.
	%% @todo Use non-empty SETTINGS here. Just because.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% Send the SETTINGS ack.
	ok = gen_tcp:send(Socket, cow_http2:settings_ack()),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	%% Wait until after the SETTINGS ack timeout was supposed to trigger.
	receive after 6000 -> ok end,
	%% Send a PING.
	ok = gen_tcp:send(Socket, cow_http2:ping(0)),
	%% Receive a PING ack back, indicating the connection is still up.
	{ok, << 8:24, 6:8, 0:7, 1:1, 0:96 >>} = gen_tcp:recv(Socket, 17, 1000),
	ok.



%% Tests still need to be added for the following points:

%% @todo how to test this?
%3.2.1
%   A server decodes and interprets these values as it would any other
%   SETTINGS frame.

%3.2
% @todo (maybe an option to disable HTTP/2?)
%   A server that does not support HTTP/2 can respond to the request as
%   though the Upgrade header field were absent

%% @todo Do we reject "http" requests over TLS and "https" requests over TCP?
%% Yes, see 421 status code. But maybe some configuration is in order.

%3.5
%% @todo yeah idk
%(if the upgrade failed and the connection send this before 101 then
% we should 400 and close the connection, but not sure how it can fail)
% @todo (maybe an option to disable HTTP/2?)
%	The client sends
%   the client connection preface immediately upon receipt of a 101
%   (Switching Protocols) response (indicating a successful upgrade)

%% @todo
%3.5
%(big mess)
%   To avoid unnecessary latency, clients are permitted to send
%   additional frames to the server immediately after sending the client
%   connection preface, without waiting to receive the server connection
%   preface.  It is important to note, however, that the server
%   connection preface SETTINGS frame might include parameters that
%   necessarily alter how a client is expected to communicate with the
%   server.  Upon receiving the SETTINGS frame, the client is expected to
%   honor any parameters established.  In some configurations, it is
%   possible for the server to transmit SETTINGS before the client sends
%   additional frames, providing an opportunity to avoid this issue.
