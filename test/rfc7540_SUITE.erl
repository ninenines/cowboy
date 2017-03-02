%% Copyright (c) 2016-2017, Loïc Hoguin <essen@ninenines.eu>
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
		{"/", hello_h, []},
		{"/echo/:key", echo_h, []}
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

http_upgrade_ignore_zero_http2_settings_header(Config) ->
	doc("The HTTP Upgrade request must include "
		"exactly one HTTP2-Settings header field (RFC7540 3.2, RFC7540 3.2.1)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"\r\n"]),
	{ok, <<"HTTP/1.1 200">>} = gen_tcp:recv(Socket, 12, 1000),
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
	%% Receive the response to the initial HTTP/1.1 request.
	{ok, << HeadersSkipLen:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, HeadersSkipLen, 1000),
	{ok, << DataSkipLen:24, 0:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, DataSkipLen, 1000),
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
%% SETTINGS_MAX_FRAME_SIZE is probably the easiest to test. The relevant
%% RFC quote is:
%%
%% 3.2.1
%%   A server decodes and interprets these values as it would any other
%%   SETTINGS frame. Explicit acknowledgement of these settings
%%   (Section 6.5.3) is not necessary, since a 101 response serves as
%%   implicit acknowledgement.

%% @todo We need to test an upgrade with a request body. It is probably
%% worth having a configuration value for how much we accept while still
%% upgrading (if too big, we would just stay on HTTP/1.1). We therefore
%% needs a test for when the body is small enough, and one for when the
%% body is larger than we accept. The relevant RFC quote is:
%%
%% 3.2
%%   Requests that contain a payload body MUST be sent in their entirety
%%   before the client can send HTTP/2 frames.  This means that a large
%%   request can block the use of the connection until it is completely
%%   sent.

%% @todo We should definitely have a test with OPTIONS. The relevant
%% RFC quote is:
%%
%% 3.2
%%   If concurrency of an initial request with subsequent requests is
%%   important, an OPTIONS request can be used to perform the upgrade to
%%   HTTP/2, at the cost of an additional round trip.

%% @todo If we ever handle priority, we need to check that the initial
%% HTTP/1.1 request has default priority. The relevant RFC quote is:
%%
%% 3.2
%%   The HTTP/1.1 request that is sent prior to upgrade is assigned a
%%   stream identifier of 1 (see Section 5.1.1) with default priority
%%   values (Section 5.3.5).

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
	%% Receive the SETTINGS ack, and the response HEADERS and DATA (Stream ID 1).
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
	%% Send a valid preface followed by an unexpected DATA frame.
	ok = gen_tcp:send(Socket, [
		"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n",
		cow_http2:settings(#{}),
		cow_http2:data(1, fin, <<"Unexpected DATA frame.">>)
	]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% Skip the SETTINGS ack, receive the response HEADERS, DATA and RST_STREAM (streamid 1).
	Received = lists:reverse(lists:foldl(fun(_, Acc) ->
		case gen_tcp:recv(Socket, 9, 1000) of
			{ok, << 0:24, 4:8, 1:8, 0:32 >>} ->
				Acc;
			{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} ->
				{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
				[headers|Acc];
			{ok, << SkipLen:24, 0:8, _:8, 1:32 >>} ->
				{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
				[data|Acc];
			{ok, << 4:24, 3:8, 0:8, 1:32 >>} ->
				%% We expect a STREAM_CLOSED reason.
				{ok, << 5:32 >>} = gen_tcp:recv(Socket, 4, 1000),
				[rst_stream|Acc];
			{error, _} ->
				%% Can be timeouts, ignore them.
				Acc
		end
	end, [], [1, 2, 3, 4])),
	case Received of
		[rst_stream] -> ok;
		[headers, rst_stream] -> ok;
		[headers, data, rst_stream] -> ok
	end.

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

%% Do a prior knowledge handshake.
do_handshake(Config) ->
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a valid preface.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% Send the SETTINGS ack.
	ok = gen_tcp:send(Socket, cow_http2:settings_ack()),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, Socket}.

prior_knowledge(Config) ->
	doc("Streams can be initiated after a successful HTTP/2 connection "
		"with prior knowledge of server capabilities. (RFC7540 3.4)"),
	%% @todo Use non-empty SETTINGS here. Just because.
	{ok, Socket} = do_handshake(Config),
	%% Wait until after the SETTINGS ack timeout was supposed to trigger.
	receive after 6000 -> ok end,
	%% Send a PING.
	ok = gen_tcp:send(Socket, cow_http2:ping(0)),
	%% Receive a PING ack back, indicating the connection is still up.
	{ok, << 8:24, 6:8, 0:7, 1:1, 0:96 >>} = gen_tcp:recv(Socket, 17, 1000),
	ok.

%% @todo If we ever add an option to disable HTTP/2, we need to check
%% the following things:
%% * HTTP/1.1 Upgrade returns an HTTP/1.1 response (3.2)
%% * HTTP/1.1 Upgrade errors out if the client sends HTTP/2 frames
%%   without waiting for the 101 response (3.2, 3.5)
%% * Prior knowledge handshake fails (3.4)
%% * ALPN selects HTTP/1.1 (3.3)

%% Frame format.

ignore_unknown_frames(Config) ->
	doc("Frames of unknown type must be ignored and discarded. (RFC7540 4.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request with a single DATA frame,
	%% and an unknown frame type interleaved.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		<< 10:24, 99:8, 0:40, 0:80 >>,
		cow_http2:data(1, fin, << 0:100/unit:8 >>)
	]),
	%% Receive a response with the same DATA frame.
	{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{ok, << 100:24, 0:8, 1:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, << 0:100/unit:8 >>} = gen_tcp:recv(Socket, 100, 1000),
	ok.

ignore_data_unknown_flags(Config) ->
	doc("Undefined DATA frame flags must be ignored. (RFC7540 4.1, RFC7540 6.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request with a DATA frame with unknown flags.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		<< 100:24, 0:8,
			1:1, 1:1, 1:1, 1:1, %% Undefined.
			0:1, %% PADDED.
			1:1, 1:1, %% Undefined.
			1:1, %% END_STREAM.
			0:1, 1:31, 0:100/unit:8 >>
	]),
	%% Receive a response with the same DATA frame.
	{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{ok, << 100:24, 0:8, 1:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, << 0:100/unit:8 >>} = gen_tcp:recv(Socket, 100, 1000),
	ok.

ignore_headers_unknown_flags(Config) ->
	doc("Undefined HEADERS frame flags must be ignored. (RFC7540 4.1, RFC7540 6.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request with a HEADERS frame with unknown flags.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	Len = iolist_size(HeadersBlock),
	ok = gen_tcp:send(Socket, [
		<< Len:24, 1:8,
			1:1, 1:1, %% Undefined.
			0:1, %% PRIORITY.
			1:1, %% Undefined.
			0:1, %% PADDED.
			1:1, %% END_HEADERS.
			1:1, %% Undefined.
			0:1, %% END_STREAM.
			0:1, 1:31 >>,
		HeadersBlock,
		cow_http2:data(1, fin, << 0:100/unit:8 >>)
	]),
	%% Receive a response with the same DATA frame.
	{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{ok, << 100:24, 0:8, 1:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, << 0:100/unit:8 >>} = gen_tcp:recv(Socket, 100, 1000),
	ok.

ignore_priority_unknown_flags(Config) ->
	doc("Undefined PRIORITY frame flags must be ignored. (RFC7540 4.1, RFC7540 6.3)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request with an interleaved PRIORITY frame with unknown flags.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		<< 5:24, 2:8,
			1:1, 1:1, 1:1, 1:1, 1:1, 1:1, 1:1, 1:1, %% Undefined.
			0:1, 1:31, 0:1, 3:31, 0:8 >>,
		cow_http2:data(1, fin, << 0:100/unit:8 >>)
	]),
	%% Receive a response with the same DATA frame.
	{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{ok, << 100:24, 0:8, 1:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, << 0:100/unit:8 >>} = gen_tcp:recv(Socket, 100, 1000),
	ok.

ignore_rst_stream_unknown_flags(Config) ->
	doc("Undefined RST_STREAM frame flags must be ignored. (RFC7540 4.1, RFC7540 6.4)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request then cancel it with an RST_STREAM frame with unknown flags.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		<< 4:24, 3:8,
			1:1, 1:1, 1:1, 1:1, 1:1, 1:1, 1:1, 1:1, %% Undefined.
			0:1, 1:31, 8:32 >>,
		cow_http2:headers(3, nofin, HeadersBlock),
		cow_http2:data(3, fin, << 0:100/unit:8 >>)
	]),
	%% Receive a response with the same DATA frame.
	{ok, << SkipLen:24, 1:8, _:8, 3:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{ok, << 100:24, 0:8, 1:8, 3:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, << 0:100/unit:8 >>} = gen_tcp:recv(Socket, 100, 1000),
	ok.

ignore_settings_unknown_flags(Config) ->
	doc("Undefined SETTINGS frame flags must be ignored. (RFC7540 4.1, RFC7540 6.5)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a SETTINGS frame with unknown flags.
	ok = gen_tcp:send(Socket, << 6:24, 4:8,
		1:1, 1:1, 1:1, 1:1, 1:1, 1:1, 1:1, %% Undefined.
		0:1, %% ACK.
		0:32, 2:16, 0:32 >>),
	%% Receive a SETTINGS ack.
	{ok, << 0:24, 4:8, 0:7, 1:1, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	ok.

ignore_push_promise_unknown_flags(Config) ->
	doc("Undefined PUSH_PROMISE frame flags must be ignored. (RFC7540 4.1, RFC7540 6.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a PUSH_PROMISE frame with unknown flags.
	ok = gen_tcp:send(Socket, << 4:24, 5:8,
		1:1, 1:1, 1:1, 1:1, %% Undefined.
		0:1, %% PADDED.
		1:1, %% END_HEADERS.
		1:1, 1:1, %% Undefined.
		0:1, 1:31, 0:1, 3:31 >>
	),
	%% Receive a PROTOCOL_ERROR connection error.
	%%
	%% Note that it is not possible to distinguish between the expected
	%% result and the server rejecting PUSH_PROMISE frames.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

ignore_ping_unknown_flags(Config) ->
	doc("Undefined PING frame flags must be ignored. (RFC7540 4.1, RFC7540 6.7)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a PING frame with unknown flags.
	ok = gen_tcp:send(Socket, << 8:24, 6:8,
		1:1, 1:1, 1:1, 1:1, 1:1, 1:1, 1:1, %% Undefined.
		0:1, %% ACK.
		0:32, 0:64 >>),
	%% Receive a PING ACK in return.
	{ok, << 8:24, 6:8, _:7, 1:1, _:32, 0:64 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

ignore_goaway_unknown_flags(Config) ->
	doc("Undefined GOAWAY frame flags must be ignored. (RFC7540 4.1, RFC7540 6.8)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a GOAWAY frame with unknown flags.
	ok = gen_tcp:send(Socket, << 8:24, 7:8,
		1:1, 1:1, 1:1, 1:1, 1:1, 1:1, 1:1, 1:1, %% Undefined.
		0:32, 0:64 >>),
	%% Receive a GOAWAY frame back.
	{ok, << _:24, 7:8, _:72, 0:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

ignore_window_update_unknown_flags(Config) ->
	doc("Undefined WINDOW_UPDATE frame flags must be ignored. (RFC7540 4.1, RFC7540 6.9)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a WINDOW_UPDATE frame with unknown flags.
	ok = gen_tcp:send(Socket, << 4:24, 8:8,
		1:1, 1:1, 1:1, 1:1, 1:1, 1:1, 1:1, 1:1, %% Undefined.
		0:32, 1000:32 >>),
	%% We expect no errors or replies, therefore we send a PING frame.
	ok = gen_tcp:send(Socket, cow_http2:ping(0)),
	%% And receive a PING ACK in return.
	{ok, << 8:24, 6:8, _:7, 1:1, _:32, 0:64 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

ignore_continuation_unknown_flags(Config) ->
	doc("Undefined CONTINUATION frame flags must be ignored. (RFC7540 4.1, RFC7540 6.10)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request with a CONTINUATION frame with unknown flags.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	Len = iolist_size(HeadersBlock),
	ok = gen_tcp:send(Socket, [
		<< 0:24, 1:8, 0:8, 0:1, 1:31 >>,
		<< Len:24, 9:8,
			1:1, 1:1, 1:1, 1:1, 1:1, %% Undefined.
			1:1, %% END_HEADERS.
			1:1, 1:1, %% Undefined.
			0:1, 1:31 >>,
		HeadersBlock,
		cow_http2:data(1, fin, << 0:100/unit:8 >>)
	]),
	%% Receive a response with the same DATA frame.
	{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{ok, << 100:24, 0:8, 1:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, << 0:100/unit:8 >>} = gen_tcp:recv(Socket, 100, 1000),
	ok.

%% @todo Flags that have no defined semantics for
%% a particular frame type MUST be left unset (0x0) when sending. (RFC7540 4.1)

ignore_data_reserved_bit(Config) ->
	doc("Reserved 1-bit field of DATA frame must be ignored. (RFC7540 4.1, RFC7540 6.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request with a DATA frame with the reserved bit set.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		<< 100:24, 0:8, 0:7, 1:1,
			1:1, %% Reserved bit.
			1:31, 0:100/unit:8 >>
	]),
	%% Receive a response with the same DATA frame.
	{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{ok, << 100:24, 0:8, 1:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, << 0:100/unit:8 >>} = gen_tcp:recv(Socket, 100, 1000),
	ok.

ignore_headers_reserved_bit(Config) ->
	doc("Reserved 1-bit field of HEADERS frame must be ignored. (RFC7540 4.1, RFC7540 6.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request with a HEADERS frame with the reserved bit set.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	Len = iolist_size(HeadersBlock),
	ok = gen_tcp:send(Socket, [
		<< Len:24, 1:8, 0:5, 1:1, 0:2,
			1:1, %% Reserved bit.
			1:31 >>,
		HeadersBlock,
		cow_http2:data(1, fin, << 0:100/unit:8 >>)
	]),
	%% Receive a response with the same DATA frame.
	{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{ok, << 100:24, 0:8, 1:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, << 0:100/unit:8 >>} = gen_tcp:recv(Socket, 100, 1000),
	ok.

ignore_priority_reserved_bit(Config) ->
	doc("Reserved 1-bit field of PRIORITY frame must be ignored. (RFC7540 4.1, RFC7540 6.3)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request with an interleaved PRIORITY frame with the reserved bit set.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		<< 5:24, 2:8, 0:8,
			1:1, %% Reserved bit.
			1:31, 0:1, 3:31, 0:8 >>,
		cow_http2:data(1, fin, << 0:100/unit:8 >>)
	]),
	%% Receive a response with the same DATA frame.
	{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{ok, << 100:24, 0:8, 1:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, << 0:100/unit:8 >>} = gen_tcp:recv(Socket, 100, 1000),
	ok.

ignore_rst_stream_reserved_bit(Config) ->
	doc("Reserved 1-bit field of RST_STREAM frame must be ignored. (RFC7540 4.1, RFC7540 6.4)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request then cancel it with an RST_STREAM frame with the reserved bit set.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		<< 4:24, 3:8, 0:8,
			1:1, %% Reserved bit.
			1:31, 8:32 >>,
		cow_http2:headers(3, nofin, HeadersBlock),
		cow_http2:data(3, fin, << 0:100/unit:8 >>)
	]),
	%% Receive a response with the same DATA frame.
	{ok, << SkipLen:24, 1:8, _:8, 3:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{ok, << 100:24, 0:8, 1:8, 3:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, << 0:100/unit:8 >>} = gen_tcp:recv(Socket, 100, 1000),
	ok.

ignore_settings_reserved_bit(Config) ->
	doc("Reserved 1-bit field of SETTINGS frame must be ignored. (RFC7540 4.1, RFC7540 6.5)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a SETTINGS frame with the reserved bit set.
	ok = gen_tcp:send(Socket, << 6:24, 4:8, 0:8,
		1:1, %% Reserved bit.
		0:31, 2:16, 0:32 >>),
	%% Receive a SETTINGS ack.
	{ok, << 0:24, 4:8, 0:7, 1:1, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	ok.

ignore_push_promise_reserved_bit(Config) ->
	doc("Reserved 1-bit field of PUSH_PROMISE frame must be ignored. (RFC7540 4.1, RFC7540 6.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a PUSH_PROMISE frame with the reserved bit set.
	ok = gen_tcp:send(Socket, << 4:24, 5:8, 0:5, 1:1, 0:2,
		1:1, %% Reserved bit.
		1:31, 0:1, 3:31 >>
	),
	%% Receive a PROTOCOL_ERROR connection error.
	%%
	%% Note that it is not possible to distinguish between the expected
	%% result and the server rejecting PUSH_PROMISE frames.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

ignore_ping_reserved_bit(Config) ->
	doc("Reserved 1-bit field of PING frame must be ignored. (RFC7540 4.1, RFC7540 6.7)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a PING frame with the reserved bit set.
	ok = gen_tcp:send(Socket, << 8:24, 6:8, 0:8,
		1:1, %% Reserved bit.
		0:31, 0:64 >>),
	%% Receive a PING ACK in return.
	{ok, << 8:24, 6:8, _:7, 1:1, _:32, 0:64 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

ignore_goaway_reserved_bit(Config) ->
	doc("Reserved 1-bit field of GOAWAY frame must be ignored. (RFC7540 4.1, RFC7540 6.8)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a GOAWAY frame with the reserved bit set.
	ok = gen_tcp:send(Socket, << 8:24, 7:8, 0:8,
		1:1, %% Reserved bit.
		0:31, 0:64 >>),
	%% Receive a GOAWAY frame back.
	{ok, << _:24, 7:8, _:72, 0:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

ignore_window_update_reserved_bit(Config) ->
	doc("Reserved 1-bit field of WINDOW_UPDATE frame must be ignored. (RFC7540 4.1, RFC7540 6.9)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a WINDOW_UPDATE frame with the reserved bit set.
	ok = gen_tcp:send(Socket, << 4:24, 8:8, 0:8,
		1:1, %% Reserved bit.
		0:31, 1000:32 >>),
	%% We expect no errors or replies, therefore we send a PING frame.
	ok = gen_tcp:send(Socket, cow_http2:ping(0)),
	%% And receive a PING ACK in return.
	{ok, << 8:24, 6:8, _:7, 1:1, _:32, 0:64 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

ignore_continuation_reserved_bit(Config) ->
	doc("Reserved 1-bit field of CONTINUATION frame must be ignored. (RFC7540 4.1, RFC7540 6.10)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request with a CONTINUATION frame with the reserved bit set.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	Len = iolist_size(HeadersBlock),
	ok = gen_tcp:send(Socket, [
		<< 0:24, 1:8, 0:8, 0:1, 1:31 >>,
		<< Len:24, 9:8, 0:5, 1:1, 0:2,
			1:1, %% Reserved bit.
			1:31 >>,
		HeadersBlock,
		cow_http2:data(1, fin, << 0:100/unit:8 >>)
	]),
	%% Receive a response with the same DATA frame.
	{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{ok, << 100:24, 0:8, 1:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, << 0:100/unit:8 >>} = gen_tcp:recv(Socket, 100, 1000),
	ok.

%% @todo The reserved 1-bit field MUST remain unset (0x0) when sending. (RFC7540 4.1)

%% Frame size.

max_frame_size_allow_exactly_default(Config) ->
	doc("All implementations must allow frame sizes of at least 16384. (RFC7540 4.1, RFC7540 4.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request with a DATA frame of exactly 16384 bytes.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		cow_http2:data(1, fin, << 0:16384/unit:8 >>)
	]),
	%% Receive a response with the same DATA frame.
	{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{ok, << 16384:24, 0:8, 1:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, << 0:16384/unit:8 >>} = gen_tcp:recv(Socket, 16384, 1000),
	ok.

max_frame_size_reject_larger_than_default(Config) ->
	doc("A FRAME_SIZE_ERROR connection error must be sent when receiving "
		"frames larger than the default 16384 length. (RFC7540 4.1, RFC7540 4.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request with a DATA frame larger than 16384 bytes.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		cow_http2:data(1, fin, << 0:16385/unit:8 >>)
	]),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% @todo We need configurable SETTINGS in Cowboy for these tests.
%%	max_frame_size_config_reject_too_small(Config) ->
%%		doc("SETTINGS_MAX_FRAME_SIZE configuration values smaller than "
%%			"16384 must be rejected. (RFC7540 6.5.2)"),
%%		%% @todo This requires us to have a configurable SETTINGS in Cowboy.
%%		todo.
%%
%%	max_frame_size_config_reject_too_large(Config) ->
%%		doc("SETTINGS_MAX_FRAME_SIZE configuration values larger than "
%%			"16777215 must be rejected. (RFC7540 6.5.2)"),
%%		%% @todo This requires us to have a configurable SETTINGS in Cowboy.
%%		todo.
%%
%%	max_frame_size_allow_exactly_custom(Config) ->
%%		doc("An endpoint that sets SETTINGS_MAX_FRAME_SIZE must allow frames "
%%			"of up to that size. (RFC7540 4.2, RFC7540 6.5.2)"),
%%		%% @todo This requires us to have a configurable SETTINGS in Cowboy.
%%		todo.
%%
%%	max_frame_size_reject_larger_than_custom(Config) ->
%%		doc("An endpoint that sets SETTINGS_MAX_FRAME_SIZE must reject frames "
%%			"of up to that size with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.5.2)"),
%%		%% @todo This requires us to have a configurable SETTINGS in Cowboy.
%%		todo.

%% @todo How do I test this?
%%
%%	max_frame_size_client_default_respect_limits(Config) ->
%%		doc("The server must not send frame sizes of more "
%%			"than 16384 by default. (RFC7540 4.1, RFC7540 4.2)"),

%% This is about the client sending a SETTINGS frame.
max_frame_size_client_override_reject_too_small(Config) ->
	doc("A SETTINGS_MAX_FRAME_SIZE smaller than 16384 must be rejected "
		"with a PROTOCOL_ERROR connection error. (RFC7540 6.5.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a SETTINGS frame with a SETTINGS_MAX_FRAME_SIZE lower than 16384.
	ok = gen_tcp:send(Socket, << 6:24, 4:8, 0:40, 5:16, 16383:32 >>),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% This is about the client sending a SETTINGS frame.
max_frame_size_client_override_reject_too_large(Config) ->
	doc("A SETTINGS_MAX_FRAME_SIZE larger than 16777215 must be rejected "
		"with a PROTOCOL_ERROR connection error. (RFC7540 6.5.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a SETTINGS frame with a SETTINGS_MAX_FRAME_SIZE larger than 16777215.
	ok = gen_tcp:send(Socket, << 6:24, 4:8, 0:40, 5:16, 16777216:32 >>),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% @todo How do I test this?
%%
%%	max_frame_size_client_custom_respect_limits(Config) ->
%%		doc("The server must not send frame sizes of more than "
%%			"client's advertised limits. (RFC7540 4.1, RFC7540 4.2)"),

%% I am using FRAME_SIZE_ERROR here because the information in the
%% frame header tells us this frame is at least 1 byte long, while
%% the given length is smaller; i.e. it is too small to contain
%% mandatory frame data (the pad length).

data_reject_frame_size_0_padded_flag(Config) ->
	doc("DATA frames of size 0 with the PADDED flag set must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request with an incorrect padded DATA frame size.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		<< 0:24, 0:8, 0:4, 1:1, 0:2, 1:1, 0:1, 1:31 >>
	]),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% This case on the other hand is noted specifically in the RFC
%% as being a PROTOCOL_ERROR. It can be thought of as the Pad Length
%% being incorrect, rather than the frame size.

data_reject_frame_size_too_small_padded_flag(Config) ->
	doc("DATA frames with Pad Length >= Length must be rejected "
		"with a PROTOCOL_ERROR connection error. (RFC7540 6.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a POST request with an incorrect padded DATA frame size.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		<< 10:24, 0:8, 0:4, 1:1, 0:2, 1:1, 0:1, 1:31, 10:8, 0:80  >>
	]),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

headers_reject_frame_size_0_padded_flag(Config) ->
	doc("HEADERS frames of size 0 with the PADDED flag set must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a padded HEADERS frame with an incorrect size.
	ok = gen_tcp:send(Socket, << 0:24, 1:8, 0:4, 1:1, 0:2, 1:1, 0:1, 1:31 >>),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

headers_reject_frame_size_too_small_padded_flag(Config) ->
	doc("HEADERS frames with no priority flag and Pad Length >= Length "
		"must be rejected with a PROTOCOL_ERROR connection error. (RFC7540 6.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a padded HEADERS frame with an incorrect size.
	ok = gen_tcp:send(Socket, << 10:24, 1:8, 0:4, 1:1, 0:2, 1:1, 0:1, 1:31, 10:8, 0:80 >>),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

headers_reject_frame_size_too_small_priority_flag(Config) ->
	doc("HEADERS frames of size smaller than 5 with the PRIORITY flag set must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with priority set and an incorrect size.
	ok = gen_tcp:send(Socket, << 4:24, 1:8,
		0:2, 1:1, 0:4, 1:1, 0:1, 1:31, 0:1, 3:31, 0:8 >>),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

headers_reject_frame_size_5_padded_and_priority_flags(Config) ->
	doc("HEADERS frames of size smaller than 6 with the PADDED "
		"and PRIORITY flags set must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a padded HEADERS frame with an incorrect size.
	ok = gen_tcp:send(Socket, << 5:24, 1:8,
		0:2, 1:1, 0:1, 1:1, 0:2, 1:1, 0:1, 1:31, 0:8, 0:1, 3:31, 0:8 >>),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

headers_reject_frame_size_too_small_padded_and_priority_flags(Config) ->
	doc("HEADERS frames of size smaller than Length+6 with the PADDED and PRIORITY flags set "
		"must be rejected with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a padded HEADERS frame with an incorrect size.
	ok = gen_tcp:send(Socket, << 15:24, 1:8,
		0:2, 1:1, 0:1, 1:1, 0:2, 1:1, 0:1, 1:31, 10:8, 0:1, 3:31, 0:8, 0:80 >>),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

priority_reject_frame_size_too_small(Config) ->
	doc("PRIORITY frames of size smaller than 5 must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.3)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a PRIORITY frame with an incorrect size.
	ok = gen_tcp:send(Socket, << 4:24, 2:8, 0:9, 1:31, 0:1, 3:31, 0:8 >>),
	%% Receive a FRAME_SIZE_ERROR stream error.
	{ok, << _:24, 3:8, _:40, 6:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

priority_reject_frame_size_too_large(Config) ->
	doc("PRIORITY frames of size larger than 5 must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.3)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a PRIORITY frame with an incorrect size.
	ok = gen_tcp:send(Socket, << 6:24, 2:8, 0:9, 1:31, 0:1, 3:31, 0:16 >>),
	%% Receive a FRAME_SIZE_ERROR stream error.
	{ok, << _:24, 3:8, _:40, 6:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

rst_stream_reject_frame_size_too_small(Config) ->
	doc("RST_STREAM frames of size smaller than 4 must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.4)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a request and reset it immediately.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, fin, HeadersBlock),
		<< 3:24, 3:8, 0:9, 1:31, 8:32 >>
	]),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

rst_stream_reject_frame_size_too_large(Config) ->
	doc("RST_STREAM frames of size larger than 4 must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.4)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a request and reset it immediately.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, fin, HeadersBlock),
		<< 5:24, 3:8, 0:9, 1:31, 8:32 >>
	]),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

settings_reject_bad_frame_size(Config) ->
	doc("SETTINGS frames must have a size multiple of 6 or be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.5)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a SETTINGS frame with an incorrect size.
	ok = gen_tcp:send(Socket, << 5:24, 4:8, 0:40, 1:16, 4096:32 >>),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

settings_ack_reject_non_empty_frame_size(Config) ->
	doc("SETTINGS frames with the ACK flag set and a non-empty payload "
		"must be rejected with a FRAME_SIZE_ERROR connection error (RFC7540 4.2, RFC7540 6.5)"),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a valid preface.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% Send a SETTINGS ack with a payload.
	ok = gen_tcp:send(Socket, << 6:24, 4:8, 0:7, 1:1, 0:32, 1:16, 4096:32 >>),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% Note that clients are not supposed to send PUSH_PROMISE frames.
%% However when they do, we need to be able to parse it in order
%% to reject it, and so these errors may still occur.

push_promise_reject_frame_size_too_small(Config) ->
	doc("PUSH_PROMISE frames of size smaller than 4 must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a PUSH_PROMISE frame with an incorrect size.
	ok = gen_tcp:send(Socket, << 3:24, 5:8, 0:5, 1:1, 0:3, 1:31, 0:1, 3:31 >>),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

push_promise_reject_frame_size_4_padded_flag(Config) ->
	doc("PUSH_PROMISE frames of size smaller than 5 with the PADDED flag set must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a PUSH_PROMISE frame with an incorrect size.
	ok = gen_tcp:send(Socket, << 4:24, 5:8, 0:4, 1:1, 1:1, 0:3, 1:31, 0:1, 0:8, 3:31 >>),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

push_promise_reject_frame_size_too_small_padded_flag(Config) ->
	doc("PUSH_PROMISE frames of size smaller than Length+5 with the PADDED flag set "
		"must be rejected with a PROTOCOL_ERROR connection error. (RFC7540 4.2, RFC7540 6.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a PUSH_PROMISE frame with an incorrect size.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	Len = 14 + iolist_size(HeadersBlock),
	ok = gen_tcp:send(Socket, [
		<< Len:24, 5:8, 0:4, 1:1, 1:1, 0:3, 1:31, 10:8, 0:1, 3:31 >>,
		HeadersBlock,
		<< 0:80 >>
	]),
	%% Receive a PROTOCOL_ERROR connection error.
	%%
	%% Note that it is not possible to distinguish between a Pad Length
	%% error and the server rejecting PUSH_PROMISE frames.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

ping_reject_frame_size_too_small(Config) ->
	doc("PING frames of size smaller than 8 must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.7)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a PING frame with an incorrect size.
	ok = gen_tcp:send(Socket, << 7:24, 6:8, 0:40, 0:56 >>),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

ping_reject_frame_size_too_large(Config) ->
	doc("PING frames of size larger than 8 must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.7)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a PING frame with an incorrect size.
	ok = gen_tcp:send(Socket, << 9:24, 6:8, 0:40, 0:72 >>),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

goaway_reject_frame_size_too_small(Config) ->
	doc("GOAWAY frames of size smaller than 8 must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.8)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a GOAWAY frame with an incorrect size.
	ok = gen_tcp:send(Socket, << 7:24, 7:8, 0:40, 0:56 >>),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

goaway_allow_frame_size_too_large(Config) ->
	doc("GOAWAY frames of size larger than 8 must be allowed. (RFC7540 6.8)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a GOAWAY frame with debug data.
	ok = gen_tcp:send(Socket, << 12:24, 7:8, 0:40, 0:64, 99999:32 >>),
	%% Receive a GOAWAY frame back.
	{ok, << _:24, 7:8, _:72, 0:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

window_update_reject_frame_size_too_small(Config) ->
	doc("WINDOW_UPDATE frames of size smaller than 4 must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.9)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a WINDOW_UPDATE frame with an incorrect size.
	ok = gen_tcp:send(Socket, << 3:24, 8:8, 0:40, 1000:24 >>),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

window_update_reject_frame_size_too_large(Config) ->
	doc("WINDOW_UPDATE frames of size larger than 4 must be rejected "
		"with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.9)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a WINDOW_UPDATE frame with an incorrect size.
	ok = gen_tcp:send(Socket, << 5:24, 8:8, 0:40, 1000:40 >>),
	%% Receive a FRAME_SIZE_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% Note: There is no particular limits on the size of CONTINUATION frames,
%% they can go from 0 to SETTINGS_MAX_FRAME_SIZE.

%% Header compression and decompression.

headers_compression_error(Config) ->
	doc("A decoding error in a HEADERS frame's header block must be rejected "
		"with a COMPRESSION_ERROR connection error. (RFC7540 4.3, RFC7540 6.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with an invalid header block.
	ok = gen_tcp:send(Socket, << 10:24, 1:8, 0:5, 1:1, 0:1, 1:1, 0:1, 1:31, 0:10/unit:8 >>),
	%% Receive a COMPRESSION_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 9:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

continuation_compression_error(Config) ->
	doc("A decoding error in a CONTINUATION frame's header block must be rejected "
		"with a COMPRESSION_ERROR connection error. (RFC7540 4.3, RFC7540 6.10)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a CONTINUATION frame with an invalid header block.
	ok = gen_tcp:send(Socket, [
		<< 0:24, 1:8, 0:7, 1:1, 0:1, 1:31 >>,
		<< 10:24, 9:8, 0:5, 1:1, 0:3, 1:31, 0:10/unit:8 >>
	]),
	%% Receive a COMPRESSION_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 9:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

continuation_with_frame_interleaved_error(Config) ->
	doc("Frames interleaved in a header block must be rejected "
		"with a PROTOCOL_ERROR connection error. (RFC7540 4.3, RFC7540 6.2, RFC7540 6.10)"),
	{ok, Socket} = do_handshake(Config),
	%% Send an unterminated HEADERS frame followed by a PING frame.
	ok = gen_tcp:send(Socket, [
		<< 0:24, 1:8, 0:7, 1:1, 0:1, 1:31 >>,
		cow_http2:ping(0)
	]),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

continuation_wrong_stream_error(Config) ->
	doc("CONTINUATION frames with an incorrect stream identifier must be rejected "
		"with a PROTOCOL_ERROR connection error. (RFC7540 4.3, RFC7540 6.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send an unterminated HEADERS frame followed by a CONTINUATION frame for another stream.
	ok = gen_tcp:send(Socket, [
		<< 0:24, 1:8, 0:7, 1:1, 0:1, 1:31 >>,
		<< 0:24, 9:8, 0:9, 3:31 >>
	]),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.
