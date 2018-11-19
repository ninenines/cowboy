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
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).
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
	cowboy_test:init_http(Name, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))}
	}, Config);
init_per_group(Name = tls, Config) ->
	cowboy_test:init_http2(Name, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))}
	}, Config).

end_per_group(Name, _) ->
	ok = cowboy:stop_listener(Name).

init_routes(_) -> [
	{"localhost", [
		{"/", hello_h, []},
		{"/echo/:key", echo_h, []},
		{"/long_polling", long_polling_h, []},
		{"/resp/:key[/:arg]", resp_h, []}
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
		"HTTP2-Settings: ", base64:encode(iolist_to_binary(cow_http2:settings(#{}))), "\r\n",
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
	%% Receive the SETTINGS ack. The response might arrive beforehand.
	Received = lists:reverse(lists:foldl(fun(_, Acc) ->
		case gen_tcp:recv(Socket, 9, 1000) of
			{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} ->
				{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
				[headers|Acc];
			{ok, << SkipLen:24, 0:8, _:8, 1:32 >>} ->
				{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
				[data|Acc];
			{ok, << 0:24, 4:8, 1:8, 0:32 >>} ->
				[settings_ack|Acc]
		end
	end, [], [1, 2, 3])),
	case Received of
		[settings_ack|_] -> ok;
		[headers, settings_ack|_] -> ok;
		[headers, data, settings_ack] -> ok
	end.

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
	%% Skip the SETTINGS ack. Receive a GOAWAY with reason SETTINGS_TIMEOUT,
	%% possibly following a HEADERS or HEADERS and DATA frames.
	Received = lists:reverse(lists:foldl(fun(_, Acc) ->
		case gen_tcp:recv(Socket, 9, 6000) of
			{ok, << 0:24, 4:8, 1:8, 0:32 >>} ->
				Acc;
			{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} ->
				{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
				[headers|Acc];
			{ok, << SkipLen:24, 0:8, _:8, 1:32 >>} ->
				{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
				[data|Acc];
			{ok, << 8:24, 7:8, 0:40 >>} ->
				%% We expect a SETTINGS_TIMEOUT reason.
				{ok, << 1:32, 4:32 >>} = gen_tcp:recv(Socket, 8, 1000),
				[goaway|Acc];
			{error, _} ->
				%% Can be timeouts, ignore them.
				Acc
		end
	end, [], [1, 2, 3, 4])),
	case Received of
		[goaway] -> ok;
		[headers, goaway] -> ok;
		[headers, data, goaway] -> ok
	end.

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
		"GET /long_polling HTTP/1.1\r\n"
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
	%% Skip the SETTINGS ack. Receive an RST_STREAM possibly following by
	%% a HEADERS frame, or a GOAWAY following HEADERS and DATA. This
	%% corresponds to the stream being in half-closed and closed states.
	%% The reason must be STREAM_CLOSED.
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
			{ok, << 8:24, 7:8, 0:40 >>} ->
				%% We expect a STREAM_CLOSED reason.
				{ok, << 1:32, 5:32 >>} = gen_tcp:recv(Socket, 8, 1000),
				[goaway|Acc];
			{error, _} ->
				%% Can be timeouts, ignore them.
				Acc
		end
	end, [], [1, 2, 3, 4])),
	case Received of
		[rst_stream] -> ok;
		[headers, rst_stream] -> ok;
		[headers, data, goaway] -> ok
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
	{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} = case gen_tcp:recv(Socket, 9, 1000) of
		%% We received a WINDOW_UPDATE first. Skip it and the next.
		{ok, <<4:24, 8:8, 0:40>>} ->
			{ok, _} = gen_tcp:recv(Socket, 4 + 13, 1000),
			gen_tcp:recv(Socket, 9, 1000);
		Res ->
			Res
	end,
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

max_frame_size_allow_exactly_custom(Config0) ->
	doc("An endpoint that sets SETTINGS_MAX_FRAME_SIZE must allow frames "
		"of up to that size. (RFC7540 4.2, RFC7540 6.5.2)"),
	%% Create a new listener that sets the maximum frame size to 30000.
	Config = cowboy_test:init_http(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		max_frame_size_received => 30000
	}, Config0),
	try
		%% Do the handshake.
		{ok, Socket} = do_handshake(Config),
		%% Send a HEADERS frame initiating a stream followed by
		%% a single 30000 bytes DATA frame.
		Headers = [
			{<<":method">>, <<"POST">>},
			{<<":scheme">>, <<"http">>},
			{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
			{<<":path">>, <<"/long_polling">>}
		],
		{HeadersBlock, _} = cow_hpack:encode(Headers),
		ok = gen_tcp:send(Socket, [
			cow_http2:headers(1, nofin, HeadersBlock),
			cow_http2:data(1, fin, <<0:30000/unit:8>>)
		]),
		%% Receive a proper response.
		{ok, << Len2:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
		{ok, _} = gen_tcp:recv(Socket, Len2, 6000),
		%% No errors follow due to our sending of a 25000 bytes frame.
		{error, timeout} = gen_tcp:recv(Socket, 0, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

max_frame_size_reject_larger_than_custom(Config0) ->
	doc("An endpoint that sets SETTINGS_MAX_FRAME_SIZE must reject frames "
		"of up to that size with a FRAME_SIZE_ERROR connection error. (RFC7540 4.2, RFC7540 6.5.2)"),
	%% Create a new listener that sets the maximum frame size to 30000.
	Config = cowboy_test:init_http(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		max_frame_size_received => 30000
	}, Config0),
	try
		%% Do the handshake.
		{ok, Socket} = do_handshake(Config),
		%% Send a HEADERS frame initiating a stream followed by
		%% a single DATA frame larger than 30000 bytes.
		Headers = [
			{<<":method">>, <<"POST">>},
			{<<":scheme">>, <<"http">>},
			{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
			{<<":path">>, <<"/long_polling">>}
		],
		{HeadersBlock, _} = cow_hpack:encode(Headers),
		ok = gen_tcp:send(Socket, [
			cow_http2:headers(1, nofin, HeadersBlock),
			cow_http2:data(1, fin, <<0:30001/unit:8>>)
		]),
		%% Receive a FRAME_SIZE_ERROR connection error.
		{ok, << _:24, 7:8, _:72, 6:32 >>} = gen_tcp:recv(Socket, 17, 6000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

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

%% Stream states.

idle_stream_reject_data(Config) ->
	doc("DATA frames received on an idle stream must be rejected "
		"with a PROTOCOL_ERROR connection error. (RFC7540 5.1, RFC7540 6.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a DATA frame on an idle stream.
	ok = gen_tcp:send(Socket, cow_http2:data(1, fin, <<"Unexpected DATA frame.">>)),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

idle_stream_accept_headers(Config) ->
	doc("HEADERS frames received on an idle stream must be accepted. (RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame on an idle stream.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a HEADERS frame as a response.
	{ok, << _:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	ok.

idle_stream_accept_priority(Config) ->
	doc("PRIORITY frames received on an idle stream must be accepted. (RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a PRIORITY frame on an idle stream.
	ok = gen_tcp:send(Socket, cow_http2:priority(1, shared, 3, 123)),
	%% Receive no error.
	{error, timeout} = gen_tcp:recv(Socket, 7, 1000),
	%% Send a HEADERS frame on the same stream.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a HEADERS frame as a response.
	{ok, << _:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	ok.

idle_stream_reject_rst_stream(Config) ->
	doc("RST_STREAM frames received on an idle stream must be rejected "
		"with a PROTOCOL_ERROR connection error. (RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send an RST_STREAM frame on an idle stream.
	ok = gen_tcp:send(Socket, cow_http2:rst_stream(1, no_error)),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

idle_stream_reject_push_promise(Config) ->
	doc("PUSH_PROMISE frames received on an idle stream must be rejected "
		"with a PROTOCOL_ERROR connection error. (RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a PUSH_PROMISE frame on an idle stream.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:push_promise(1, 3, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

idle_stream_reject_window_update(Config) ->
	doc("WINDOW_UPDATE frames received on an idle stream must be rejected "
		"with a PROTOCOL_ERROR connection error. (RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a WINDOW_UPDATE frame on an idle stream.
	ok = gen_tcp:send(Socket, cow_http2:window_update(1, 12345)),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%reserved (local) - after sending PUSH_PROMISE:
%      An endpoint MUST NOT send any type of frame other than HEADERS,
%      RST_STREAM, or PRIORITY in this state.
%%% how to test this?
%
%      A PRIORITY or WINDOW_UPDATE frame MAY be received in this state.
%      Receiving any type of frame other than RST_STREAM, PRIORITY, or
%      WINDOW_UPDATE on a stream in this state MUST be treated as a
%      connection error (Section 5.4.1) of type PROTOCOL_ERROR.
%%% we need to use a large enough file for this
%
%reserved_local_reject_data
%reserved_local_reject_headers
%reserved_local_accept_priority
%reserved_local_accept_rst_stream
%reserved_local_reject_push_promise %% do we even care? we reject it always
%reserved_local_accept_window_update
%
%half-closed (remote):
%      If an endpoint receives additional frames, other than
%      WINDOW_UPDATE, PRIORITY, or RST_STREAM, for a stream that is in
%      this state, it MUST respond with a stream error (Section 5.4.2) of
%      type STREAM_CLOSED.

half_closed_remote_reject_data(Config) ->
	doc("DATA frames received on a half-closed (remote) stream must be rejected "
		"with a STREAM_CLOSED stream error. (RFC7540 5.1, RFC7540 6.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with the FIN flag set.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Send a DATA frame on that now half-closed (remote) stream.
	ok = gen_tcp:send(Socket, cow_http2:data(1, fin, <<"Unexpected DATA frame.">>)),
	%% Receive a STREAM_CLOSED stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 5:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

%% We reject all invalid HEADERS with a connection error because
%% we do not want to waste resources decoding them.
half_closed_remote_reject_headers(Config) ->
	doc("HEADERS frames received on a half-closed (remote) stream must be rejected "
		"with a STREAM_CLOSED connection error. (RFC7540 4.3, RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with the FIN flag set.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Send a HEADERS frame on that now half-closed (remote) stream.
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a STREAM_CLOSED connection error.
	{ok, << _:24, 7:8, _:72, 5:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

half_closed_remote_accept_priority(Config) ->
	doc("PRIORITY frames received on a half-closed stream must be accepted. (RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with the FIN flag set.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Send a PRIORITY frame on that now half-closed (remote) stream.
	ok = gen_tcp:send(Socket, cow_http2:priority(1, shared, 3, 123)),
	%% Receive a HEADERS frame as a response.
	{ok, << _:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	ok.

half_closed_remote_accept_rst_stream(Config) ->
	doc("RST_STREAM frames received on a half-closed stream must be accepted. (RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with the FIN flag set.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Send an RST_STREAM frame on that now half-closed (remote) stream.
	ok = gen_tcp:send(Socket, cow_http2:rst_stream(1, no_error)),
	%% Receive nothing back.
	{error, timeout} = gen_tcp:recv(Socket, 9, 6000),
	ok.

%% half_closed_remote_reject_push_promise
%%
%% We respond to all PUSH_PROMISE frames with a PROTOCOL_ERROR connection error
%% because PUSH is disabled in that direction. We therefore cannot test other
%% error conditions.

half_closed_remote_accept_window_update(Config) ->
	doc("WINDOW_UPDATE frames received on a half-closed stream must be accepted. (RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with the FIN flag set.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Send a WINDOW_UPDATE frame on that now half-closed (remote) stream.
	ok = gen_tcp:send(Socket, cow_http2:window_update(1, 12345)),
	%% Receive a HEADERS frame as a response.
	{ok, << _:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	ok.

%% We reject DATA frames sent on closed streams with a STREAM_CLOSED
%% connection error regardless of how the stream was closed to simplify
%% the implementation. This excludes the few frames we ignore from
%% lingering streams that we canceled.
rst_stream_closed_reject_data(Config) ->
	doc("DATA frames received on a stream closed via RST_STREAM must be rejected "
		"with a STREAM_CLOSED connection error. (RFC7540 5.1, RFC7540 6.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, HeadersBlock)),
	%% Send an RST_STREAM frame to close the stream.
	ok = gen_tcp:send(Socket, cow_http2:rst_stream(1, cancel)),
	%% Send a DATA frame on the now RST_STREAM closed stream.
	ok = gen_tcp:send(Socket, cow_http2:data(1, fin, <<"Unexpected DATA frame.">>)),
	%% Receive a STREAM_CLOSED connection error.
	{ok, << _:24, 7:8, _:72, 5:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% We reject all invalid HEADERS with a connection error because
%% we do not want to waste resources decoding them.
rst_stream_closed_reject_headers(Config) ->
	doc("HEADERS frames received on a stream closed via RST_STREAM must be rejected "
		"with a STREAM_CLOSED connection error. (RFC7540 4.3, RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, HeadersBlock)),
	%% Send an RST_STREAM frame to close the stream.
	ok = gen_tcp:send(Socket, cow_http2:rst_stream(1, cancel)),
	%% Send a HEADERS frame on the now RST_STREAM closed stream.
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, HeadersBlock)),
	%% Receive a STREAM_CLOSED connection error.
	{ok, << _:24, 7:8, _:72, 5:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

rst_stream_closed_accept_priority(Config) ->
	doc("PRIORITY frames received on a stream closed via RST_STREAM "
		"must be accepted. (RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, HeadersBlock)),
	%% Send an RST_STREAM frame to close the stream.
	ok = gen_tcp:send(Socket, cow_http2:rst_stream(1, cancel)),
	%% Send a PRIORITY frame on that now RST_STREAM closed stream.
	ok = gen_tcp:send(Socket, cow_http2:priority(1, shared, 3, 123)),
	%% Receive nothing back.
	{error, timeout} = gen_tcp:recv(Socket, 9, 6000),
	ok.

rst_stream_closed_ignore_rst_stream(Config) ->
	doc("RST_STREAM frames received on a stream closed via RST_STREAM "
		"must be ignored to avoid looping. (RFC7540 5.1, RFC7540 5.4.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, HeadersBlock)),
	%% Send an RST_STREAM frame to close the stream.
	ok = gen_tcp:send(Socket, cow_http2:rst_stream(1, cancel)),
	%% Send an extra RST_STREAM.
	ok = gen_tcp:send(Socket, cow_http2:rst_stream(1, cancel)),
	%% Receive nothing back.
	{error, timeout} = gen_tcp:recv(Socket, 9, 6000),
	ok.

%% rst_stream_closed_reject_push_promise
%%
%% We respond to all PUSH_PROMISE frames with a PROTOCOL_ERROR connection error
%% because PUSH is disabled in that direction. We therefore cannot test other
%% error conditions.

rst_stream_closed_reject_window_update(Config) ->
	doc("WINDOW_UPDATE frames received on a stream closed via RST_STREAM "
		"must be rejected with a STREAM_CLOSED stream error. (RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, HeadersBlock)),
	%% Send an RST_STREAM frame to close the stream.
	ok = gen_tcp:send(Socket, cow_http2:rst_stream(1, cancel)),
	%% Send a WINDOW_UPDATE frame on the now RST_STREAM closed stream.
	ok = gen_tcp:send(Socket, cow_http2:window_update(1, 12345)),
	%% Receive a STREAM_CLOSED stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 5:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

stream_closed_reject_data(Config) ->
	doc("DATA frames received on a stream closed normally must be rejected "
		"with a STREAM_CLOSED connection error. (RFC7540 5.1, RFC7540 6.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive the response.
	{ok, << Length1:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	{ok, _} = gen_tcp:recv(Socket, Length1, 6000),
	{ok, << Length2:24, 0:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	{ok, _} = gen_tcp:recv(Socket, Length2, 6000),
	%% Send a DATA frame on the now closed stream.
	ok = gen_tcp:send(Socket, cow_http2:data(1, fin, <<"Unexpected DATA frame.">>)),
	%% Receive a STREAM_CLOSED connection error.
	{ok, << _:24, 7:8, _:72, 5:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

stream_closed_reject_headers(Config) ->
	doc("HEADERS frames received on a stream closed normally must be rejected "
		"with a STREAM_CLOSED connection error. (RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive the response.
	{ok, << Length1:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	{ok, _} = gen_tcp:recv(Socket, Length1, 6000),
	{ok, << Length2:24, 0:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	{ok, _} = gen_tcp:recv(Socket, Length2, 6000),
	%% Send a HEADERS frame on the now closed stream.
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a STREAM_CLOSED connection error.
	{ok, << _:24, 7:8, _:72, 5:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

stream_closed_accept_priority(Config) ->
	doc("PRIORITY frames received on a stream closed normally must be accepted. (RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive the response.
	{ok, << Length1:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	{ok, _} = gen_tcp:recv(Socket, Length1, 6000),
	{ok, << Length2:24, 0:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	{ok, _} = gen_tcp:recv(Socket, Length2, 6000),
	%% Send a PRIORITY frame on the now closed stream.
	ok = gen_tcp:send(Socket, cow_http2:priority(1, shared, 3, 123)),
	%% Receive nothing back.
	{error, timeout} = gen_tcp:recv(Socket, 9, 6000),
	ok.

stream_closed_accept_rst_stream(Config) ->
	doc("RST_STREAM frames received on a stream closed normally "
		"must be accepted for a short period. (RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive the response.
	{ok, << Length1:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	{ok, _} = gen_tcp:recv(Socket, Length1, 6000),
	{ok, << Length2:24, 0:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	{ok, _} = gen_tcp:recv(Socket, Length2, 6000),
	%% Send an RST_STREAM frame on the now closed stream.
	ok = gen_tcp:send(Socket, cow_http2:rst_stream(1, cancel)),
	%% Receive nothing back.
	{error, timeout} = gen_tcp:recv(Socket, 9, 6000),
	ok.

%% stream_closed_reject_push_promise
%%
%% We respond to all PUSH_PROMISE frames with a PROTOCOL_ERROR connection error
%% because PUSH is disabled in that direction. We therefore cannot test other
%% error conditions.

stream_closed_accept_window_update(Config) ->
	doc("WINDOW_UPDATE frames received on a stream closed normally "
		"must be accepted for a short period. (RFC7540 5.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive the response.
	{ok, << Length1:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	{ok, _} = gen_tcp:recv(Socket, Length1, 6000),
	{ok, << Length2:24, 0:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	{ok, _} = gen_tcp:recv(Socket, Length2, 6000),
	%% Send a WINDOW_UPDATE frame on the now closed stream.
	ok = gen_tcp:send(Socket, cow_http2:window_update(1, 12345)),
	%% Receive nothing back.
	{error, timeout} = gen_tcp:recv(Socket, 9, 6000),
	ok.

%% @todo While we accept RST_STREAM and WINDOW_UPDATE for a short period
%% after the stream closed normally, we may want to reject the ones coming
%% a significant amount of time after that.

%% @todo Frames may arrive on a stream after we send an RST_STREAM for it.
%% They must be ignored for a short period of time:
%
%      If this state is reached as a result of sending a RST_STREAM
%      frame, the peer that receives the RST_STREAM might have already
%      sent -- or enqueued for sending -- frames on the stream that
%      cannot be withdrawn.  An endpoint MUST ignore frames that it
%      receives on closed streams after it has sent a RST_STREAM frame.
%      An endpoint MAY choose to limit the period over which it ignores
%      frames and treat frames that arrive after this time as being in
%      error.

%% @todo Ensure that rejected DATA frames result in the connection
%% flow-control window being updated. How to test this?
%
%      Flow-controlled frames (i.e., DATA) received after sending
%      RST_STREAM are counted toward the connection flow-control window.
%      Even though these frames might be ignored, because they are sent
%      before the sender receives the RST_STREAM, the sender will
%      consider the frames to count against the flow-control window.

%% Stream identifiers.

reject_streamid_even(Config) ->
	doc("HEADERS frames received with an even-numbered streamid "
		"must be rejected with a PROTOCOL_ERROR connection error. (RFC7540 5.1.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with an even-numbered streamid.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(2, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

reject_streamid_0(Config) ->
	doc("HEADERS frames received with streamid 0 (zero) "
		"must be rejected with a PROTOCOL_ERROR connection error. (RFC7540 5.1.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with an streamid 0.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(0, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% See the comment for reject_streamid_lower for the rationale behind
%% having a STREAM_CLOSED connection error.
http_upgrade_reject_reuse_streamid_1(Config) ->
	doc("Attempts to reuse streamid 1 after upgrading to HTTP/2 "
		"must be rejected with a STREAM_CLOSED connection error. (RFC7540 5.1.1)"),
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
	end,
	%% Send a HEADERS frame with streamid 1.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a STREAM_CLOSED connection error.
	{ok, << _:24, 7:8, _:72, 5:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% The RFC gives us various error codes to return for this case,
%% depending on whether the stream existed previously and how it
%% ended up being (half-)closed. Cowboy rejects all these HEADERS
%% frames the same way: with a STREAM_CLOSED connection error.
%% Making it a connection error is particularly important in the
%% cases where a stream error would be allowed because we avoid
%% having to decode the headers and save up resources.
reject_streamid_lower(Config) ->
	doc("HEADERS frames received with streamid lower than the previous stream "
		"must be rejected with a STREAM_CLOSED connection error. (RFC7540 5.1.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with streamid 5.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(5, fin, HeadersBlock)),
	%% Receive the response.
	{ok, << Length1:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	{ok, _} = gen_tcp:recv(Socket, Length1, 6000),
	{ok, << Length2:24, 0:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	{ok, _} = gen_tcp:recv(Socket, Length2, 6000),
	%% Send a HEADERS frame with streamid 3.
	ok = gen_tcp:send(Socket, cow_http2:headers(3, fin, HeadersBlock)),
	%% Receive a STREAM_CLOSED connection error.
	{ok, << _:24, 7:8, _:72, 5:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% @todo We need an option to limit the number of streams one can open
%% on a connection. And we need to enforce it. (RFC7540 5.1.1)
%
%   Stream identifiers cannot be reused.  Long-lived connections can
%   result in an endpoint exhausting the available range of stream
%   identifiers.  A server
%   that is unable to establish a new stream identifier can send a GOAWAY
%   frame so that the client is forced to open a new connection for new
%   streams.

%% (RFC7540 5.2.1)
%   3.  Flow control is directional with overall control provided by the
%       receiver.  A receiver MAY choose to set any window size that it
%       desires for each stream and for the entire connection.  A sender
%       MUST respect flow-control limits imposed by a receiver.  Clients,
%       servers, and intermediaries all independently advertise their
%       flow-control window as a receiver and abide by the flow-control
%       limits set by their peer when sending.
%
%   4.  The initial value for the flow-control window is 65,535 octets
%       for both new streams and the overall connection.
%
%   5.  The frame type determines whether flow control applies to a
%       frame.  Of the frames specified in this document, only DATA
%       frames are subject to flow control; all other frame types do not
%       consume space in the advertised flow-control window.  This
%       ensures that important control frames are not blocked by flow
%       control.
%
%   6.  Flow control cannot be disabled.

%% (RFC7540 5.2.2)
%   Even with full awareness of the current bandwidth-delay product,
%   implementation of flow control can be difficult.  When using flow
%   control, the receiver MUST read from the TCP receive buffer in a
%   timely fashion.  Failure to do so could lead to a deadlock when
%   critical frames, such as WINDOW_UPDATE, are not read and acted upon.

%% (RFC7540 5.3.1)
%   Inside the dependency tree, a dependent stream SHOULD only be
%   allocated resources if either all of the streams that it depends on
%   (the chain of parent streams up to 0x0) are closed or it is not
%   possible to make progress on them.

%% We reject all invalid HEADERS with a connection error because
%% we do not want to waste resources decoding them.
reject_self_dependent_stream_headers(Config) ->
	doc("HEADERS frames opening a stream that depends on itself "
		"must be rejected with a PROTOCOL_ERROR connection error. (RFC7540 5.3.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with priority set to depend on itself.
	ok = gen_tcp:send(Socket, << 5:24, 1:8,
		0:2, 1:1, 0:4, 1:1, 0:1, 1:31, 0:1, 1:31, 0:8 >>),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% We reject all invalid HEADERS with a connection error because
%% we do not want to waste resources decoding them.
reject_self_dependent_stream_headers_with_padding(Config) ->
	doc("HEADERS frames opening a stream that depends on itself "
		"must be rejected with a PROTOCOL_ERROR connection error. (RFC7540 5.3.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with priority set to depend on itself.
	ok = gen_tcp:send(Socket, << 6:24, 1:8,
		0:2, 1:1, 0:1, 1:1, 0:2, 1:1, 0:1, 1:31, 0:8, 0:1, 1:31, 0:8 >>),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

reject_self_dependent_stream_priority(Config) ->
	doc("PRIORITY frames making a stream depend on itself "
		"must be rejected with a PROTOCOL_ERROR stream error. (RFC7540 5.3.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a PRIORITY frame making a stream depend on itself.
	ok = gen_tcp:send(Socket, cow_http2:priority(1, shared, 1, 123)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

%% @todo Stream priorities. (RFC7540 5.3.2 5.3.3 5.3.4 5.3.5)

%% (RFC7540 5.4.1)
%   An endpoint that encounters a connection error SHOULD first send a
%   GOAWAY frame (Section 6.8) with the stream identifier of the last
%   stream that it successfully received from its peer.  The GOAWAY frame
%   includes an error code that indicates why the connection is
%   terminating.  After sending the GOAWAY frame for an error condition,
%   the endpoint MUST close the TCP connection.
%
%   An endpoint can end a connection at any time.  In particular, an
%   endpoint MAY choose to treat a stream error as a connection error.
%   Endpoints SHOULD send a GOAWAY frame when ending a connection,
%   providing that circumstances permit it.

%% (RFC7540 5.4.2)
%   A RST_STREAM is the last frame that an endpoint can send on a stream.
%   The peer that sends the RST_STREAM frame MUST be prepared to receive
%   any frames that were sent or enqueued for sending by the remote peer.
%   These frames can be ignored, except where they modify connection
%   state (such as the state maintained for header compression
%   (Section 4.3) or flow control).
%
%   Normally, an endpoint SHOULD NOT send more than one RST_STREAM frame
%   for any stream.  However, an endpoint MAY send additional RST_STREAM
%   frames if it receives frames on a closed stream after more than a
%   round-trip time.  This behavior is permitted to deal with misbehaving
%   implementations.
%
%   To avoid looping, an endpoint MUST NOT send a RST_STREAM in response
%   to a RST_STREAM frame.

%% (RFC7540 5.5)
%   Extensions are permitted to use new frame types (Section 4.1), new
%   settings (Section 6.5.2), or new error codes (Section 7).  Registries
%   are established for managing these extension points: frame types
%   (Section 11.2), settings (Section 11.3), and error codes
%   (Section 11.4).
%
%   Implementations MUST ignore unknown or unsupported values in all
%   extensible protocol elements.  Implementations MUST discard frames
%   that have unknown or unsupported types.  This means that any of these
%   extension points can be safely used by extensions without prior
%   arrangement or negotiation.  However, extension frames that appear in
%   the middle of a header block (Section 4.3) are not permitted; these
%   MUST be treated as a connection error (Section 5.4.1) of type
%   PROTOCOL_ERROR.

continuation_with_extension_frame_interleaved_error(Config) ->
	doc("Extension frames interleaved in a header block must be rejected "
		"with a PROTOCOL_ERROR connection error. "
		"(RFC7540 4.3, RFC7540 5.5, RFC7540 6.2, RFC7540 6.10)"),
	{ok, Socket} = do_handshake(Config),
	%% Send an unterminated HEADERS frame followed by an extension frame.
	ok = gen_tcp:send(Socket, [
		<< 0:24, 1:8, 0:7, 1:1, 0:1, 1:31 >>,
		<< 0:24, 128:8, 0:8, 0:32 >>
	]),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% (RFC7540 6.1) DATA
%   Padding:  Padding octets that contain no application semantic value.
%      Padding octets MUST be set to zero when sending.  A receiver is
%      not obligated to verify padding but MAY treat non-zero padding as
%      a connection error (Section 5.4.1) of type PROTOCOL_ERROR.
%
%   DATA frames MUST be associated with a stream.  If a DATA frame is
%   received whose stream identifier field is 0x0, the recipient MUST
%   respond with a connection error (Section 5.4.1) of type
%   PROTOCOL_ERROR.

%% (RFC7540 6.2) HEADERS
%   Padding:  Padding octets that contain no application semantic value.
%      Padding octets MUST be set to zero when sending.  A receiver is
%      not obligated to verify padding but MAY treat non-zero padding as
%      a connection error (Section 5.4.1) of type PROTOCOL_ERROR.
%
%      A HEADERS frame carries the END_STREAM flag that signals the end
%      of a stream.  However, a HEADERS frame with the END_STREAM flag
%      set can be followed by CONTINUATION frames on the same stream.
%      Logically, the CONTINUATION frames are part of the HEADERS frame.
%
%% @todo We probably need a test for the server sending HEADERS too large.
%   The payload of a HEADERS frame contains a header block fragment
%   (Section 4.3).  A header block that does not fit within a HEADERS
%   frame is continued in a CONTINUATION frame (Section 6.10).
%
%   HEADERS frames MUST be associated with a stream.  If a HEADERS frame
%   is received whose stream identifier field is 0x0, the recipient MUST
%   respond with a connection error (Section 5.4.1) of type
%   PROTOCOL_ERROR.

%% (RFC7540 6.3) PRIORITY
%   The PRIORITY frame always identifies a stream.  If a PRIORITY frame
%   is received with a stream identifier of 0x0, the recipient MUST
%   respond with a connection error (Section 5.4.1) of type
%   PROTOCOL_ERROR.

%% (RFC7540 6.4) RST_STREAM
%   The RST_STREAM frame fully terminates the referenced stream and
%   causes it to enter the "closed" state.  After receiving a RST_STREAM
%   on a stream, the receiver MUST NOT send additional frames for that
%   stream, with the exception of PRIORITY.  However, after sending the
%   RST_STREAM, the sending endpoint MUST be prepared to receive and
%   process additional frames sent on the stream that might have been
%   sent by the peer prior to the arrival of the RST_STREAM.
%
%   RST_STREAM frames MUST be associated with a stream.  If a RST_STREAM
%   frame is received with a stream identifier of 0x0, the recipient MUST
%   treat this as a connection error (Section 5.4.1) of type
%   PROTOCOL_ERROR.

%% (RFC7540 6.5) SETTINGS
%   A SETTINGS frame MUST be sent by both endpoints at the start of a
%   connection and MAY be sent at any other time by either endpoint over
%   the lifetime of the connection.  Implementations MUST support all of
%   the parameters defined by this specification.
%
%   SETTINGS frames always apply to a connection, never a single stream.
%   The stream identifier for a SETTINGS frame MUST be zero (0x0).  If an
%   endpoint receives a SETTINGS frame whose stream identifier field is
%   anything other than 0x0, the endpoint MUST respond with a connection
%   error (Section 5.4.1) of type PROTOCOL_ERROR.
%
%   The SETTINGS frame affects connection state.  A badly formed or
%   incomplete SETTINGS frame MUST be treated as a connection error
%   (Section 5.4.1) of type PROTOCOL_ERROR.

%% Settings.

settings_header_table_size_client(Config) ->
	doc("The SETTINGS_HEADER_TABLE_SIZE setting can be used to "
		"inform the server of the maximum header table size "
		"used by the client to decode header blocks. (RFC7540 6.5.2)"),
	HeaderTableSize = 128,
	%% Do the handhsake.
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a valid preface.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n",
		cow_http2:settings(#{header_table_size => HeaderTableSize})]),
	%% Receive the server preface.
	{ok, << Len0:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len0/binary >>} = gen_tcp:recv(Socket, 6 + Len0, 1000),
	%% Send the SETTINGS ack.
	ok = gen_tcp:send(Socket, cow_http2:settings_ack()),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	%% Initialize decoding/encoding states.
	DecodeState = cow_hpack:set_max_size(HeaderTableSize, cow_hpack:init()),
	EncodeState = cow_hpack:init(),
	%% Send a HEADERS frame as a request.
	{ReqHeadersBlock1, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	], EncodeState),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, ReqHeadersBlock1)),
	%% Receive a HEADERS frame as a response.
	{ok, << Len1:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	{ok, RespHeadersBlock1} = gen_tcp:recv(Socket, Len1, 6000),
	{RespHeaders, _} = cow_hpack:decode(RespHeadersBlock1, DecodeState),
	{_, <<"200">>} = lists:keyfind(<<":status">>, 1, RespHeaders),
	%% The decoding succeeded, confirming that the table size is
	%% lower than or equal to HeaderTableSize.
	ok.

settings_header_table_size_server(Config0) ->
	doc("The SETTINGS_HEADER_TABLE_SIZE setting can be used to "
		"inform the client of the maximum header table size "
		"used by the server to decode header blocks. (RFC7540 6.5.2)"),
	HeaderTableSize = 128,
	%% Create a new listener that allows larger header table sizes.
	Config = cowboy_test:init_http(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		max_decode_table_size => HeaderTableSize
	}, Config0),
	try
		%% Do the handhsake.
		{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
		%% Send a valid preface.
		ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n",
			cow_http2:settings(#{header_table_size => HeaderTableSize})]),
		%% Receive the server preface.
		{ok, << Len0:24 >>} = gen_tcp:recv(Socket, 3, 1000),
		{ok, Data = <<_:48, _:Len0/binary>>} = gen_tcp:recv(Socket, 6 + Len0, 1000),
		%% Confirm the server's SETTINGS_HEADERS_TABLE_SIZE uses HeaderTableSize.
		{ok, {settings, #{header_table_size := HeaderTableSize}}, <<>>}
			= cow_http2:parse(<<Len0:24, Data/binary>>),
		%% Send the SETTINGS ack.
		ok = gen_tcp:send(Socket, cow_http2:settings_ack()),
		%% Receive the SETTINGS ack.
		{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
		%% Initialize decoding/encoding states.
		DecodeState = cow_hpack:init(),
		EncodeState = cow_hpack:set_max_size(HeaderTableSize, cow_hpack:init()),
		%% Send a HEADERS frame as a request.
		{ReqHeadersBlock1, _} = cow_hpack:encode([
			{<<":method">>, <<"GET">>},
			{<<":scheme">>, <<"http">>},
			{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
			{<<":path">>, <<"/">>}
		], EncodeState),
		ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, ReqHeadersBlock1)),
		%% Receive a HEADERS frame as a response.
		{ok, << Len1:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
		{ok, RespHeadersBlock1} = gen_tcp:recv(Socket, Len1, 6000),
		{RespHeaders, _} = cow_hpack:decode(RespHeadersBlock1, DecodeState),
		{_, <<"200">>} = lists:keyfind(<<":status">>, 1, RespHeaders)
		%% The decoding succeeded on the server, confirming that
		%% the table size was updated to HeaderTableSize.
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

settings_max_concurrent_streams(Config0) ->
	doc("The SETTINGS_MAX_CONCURRENT_STREAMS setting can be used to "
		"restrict the number of concurrent streams. (RFC7540 5.1.2, RFC7540 6.5.2)"),
	%% Create a new listener that allows only a single concurrent stream.
	Config = cowboy_test:init_http(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		max_concurrent_streams => 1
	}, Config0),
	try
		{ok, Socket} = do_handshake(Config),
		%% Send two HEADERS frames as two separate streams.
		Headers = [
			{<<":method">>, <<"GET">>},
			{<<":scheme">>, <<"http">>},
			{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
			{<<":path">>, <<"/long_polling">>}
		],
		{ReqHeadersBlock1, EncodeState} = cow_hpack:encode(Headers),
		{ReqHeadersBlock2, _} = cow_hpack:encode(Headers, EncodeState),
		ok = gen_tcp:send(Socket, [
			cow_http2:headers(1, fin, ReqHeadersBlock1),
			cow_http2:headers(3, fin, ReqHeadersBlock2)
		]),
		%% Receive a REFUSED_STREAM stream error.
		{ok, << _:24, 3:8, _:8, 3:32, 7:32 >>} = gen_tcp:recv(Socket, 13, 6000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

settings_max_concurrent_streams_0(Config0) ->
	doc("The SETTINGS_MAX_CONCURRENT_STREAMS setting can be set to "
		"0 to refuse all incoming streams. (RFC7540 5.1.2, RFC7540 6.5.2)"),
	%% Create a new listener that allows only a single concurrent stream.
	Config = cowboy_test:init_http(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		max_concurrent_streams => 0
	}, Config0),
	try
		{ok, Socket} = do_handshake(Config),
		%% Send a HEADERS frame.
		{HeadersBlock, _} = cow_hpack:encode([
			{<<":method">>, <<"GET">>},
			{<<":scheme">>, <<"http">>},
			{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
			{<<":path">>, <<"/long_polling">>}
		]),
		ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
		%% Receive a REFUSED_STREAM stream error.
		{ok, << _:24, 3:8, _:8, 1:32, 7:32 >>} = gen_tcp:recv(Socket, 13, 6000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

%% @todo The client can limit the number of concurrent streams too. (RFC7540 5.1.2)
%
%   A peer can limit the number of concurrently active streams using the
%   SETTINGS_MAX_CONCURRENT_STREAMS parameter (see Section 6.5.2) within
%   a SETTINGS frame.  The maximum concurrent streams setting is specific
%   to each endpoint and applies only to the peer that receives the
%   setting.  That is, clients specify the maximum number of concurrent
%   streams the server can initiate, and servers specify the maximum
%   number of concurrent streams the client can initiate.
%
%   Endpoints MUST NOT exceed the limit set by their peer.  An endpoint
%   that receives a HEADERS frame that causes its advertised concurrent
%   stream limit to be exceeded MUST treat this as a stream error
%   (Section 5.4.2) of type PROTOCOL_ERROR or REFUSED_STREAM.  The choice
%   of error code determines whether the endpoint wishes to enable
%   automatic retry (see Section 8.1.4) for details).

settings_initial_window_size(Config0) ->
	doc("The SETTINGS_INITIAL_WINDOW_SIZE setting can be used to "
		"change the initial window size of streams. (RFC7540 6.5.2)"),
	%% Create a new listener that sets initial window sizes to 100000.
	Config = cowboy_test:init_http(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		initial_connection_window_size => 100000,
		initial_stream_window_size => 100000
	}, Config0),
	try
		%% We need to do the handshake manually because a WINDOW_UPDATE
		%% frame will be sent to update the connection window.
		{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
		%% Send a valid preface.
		ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
		%% Receive the server preface.
		{ok, << Len1:24 >>} = gen_tcp:recv(Socket, 3, 1000),
		{ok, << 4:8, 0:40, _:Len1/binary >>} = gen_tcp:recv(Socket, 6 + Len1, 1000),
		%% Send the SETTINGS ack.
		ok = gen_tcp:send(Socket, cow_http2:settings_ack()),
		%% Receive the WINDOW_UPDATE for the connection.
		{ok, << 4:24, 8:8, 0:40, _:32 >>} = gen_tcp:recv(Socket, 13, 1000),
		%% Receive the SETTINGS ack.
		{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
		%% Send a HEADERS frame initiating a stream followed by
		%% DATA frames totaling 90000 bytes of body.
		Headers = [
			{<<":method">>, <<"POST">>},
			{<<":scheme">>, <<"http">>},
			{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
			{<<":path">>, <<"/long_polling">>}
		],
		{HeadersBlock, _} = cow_hpack:encode(Headers),
		ok = gen_tcp:send(Socket, [
			cow_http2:headers(1, nofin, HeadersBlock),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, fin, <<0:15000/unit:8>>)
		]),
		%% Receive a proper response.
		{ok, << Len2:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
		{ok, _} = gen_tcp:recv(Socket, Len2, 6000),
		%% No errors follow due to our sending of more than 65535 bytes of data.
		{error, timeout} = gen_tcp:recv(Socket, 0, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

settings_initial_window_size_after_ack(Config0) ->
	doc("The SETTINGS_INITIAL_WINDOW_SIZE setting can be used to "
		"change the initial window size of streams. It is applied "
		"to all existing streams upon receipt of the SETTINGS ack. (RFC7540 6.5.2)"),
	%% Create a new listener that sets the initial stream window sizes to 0.
	Config = cowboy_test:init_http(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		initial_stream_window_size => 0
	}, Config0),
	try
		%% We need to do the handshake manually because we don't
		%% want to send the SETTINGS ack immediately.
		{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
		%% Send a valid preface.
		ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
		%% Receive the server preface.
		{ok, << Len1:24 >>} = gen_tcp:recv(Socket, 3, 1000),
		{ok, << 4:8, 0:40, _:Len1/binary >>} = gen_tcp:recv(Socket, 6 + Len1, 1000),
		%%
		%% Don't send the SETTINGS ack yet! We want to create a stream first.
		%%
		%% Receive the SETTINGS ack.
		{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
		%% Send a HEADERS frame initiating a stream, a SETTINGS ack
		%% and a small DATA frame despite no window available in the stream.
		Headers = [
			{<<":method">>, <<"POST">>},
			{<<":scheme">>, <<"http">>},
			{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
			{<<":path">>, <<"/long_polling">>}
		],
		{HeadersBlock, _} = cow_hpack:encode(Headers),
		ok = gen_tcp:send(Socket, [
			cow_http2:headers(1, nofin, HeadersBlock),
			cow_http2:settings_ack(),
			cow_http2:data(1, fin, <<0:32/unit:8>>)
		]),
		%% Receive a FLOW_CONTROL_ERROR stream error.
		{ok, << _:24, 3:8, _:8, 1:32, 3:32 >>} = gen_tcp:recv(Socket, 13, 6000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

settings_initial_window_size_before_ack(Config0) ->
	doc("The SETTINGS_INITIAL_WINDOW_SIZE setting can be used to "
		"change the initial window size of streams. It is only "
		"applied upon receipt of the SETTINGS ack. (RFC7540 6.5.2)"),
	%% Create a new listener that sets the initial stream window sizes to 0.
	Config = cowboy_test:init_http(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		initial_stream_window_size => 0
	}, Config0),
	try
		%% We need to do the handshake manually because we don't
		%% want to send the SETTINGS ack.
		{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
		%% Send a valid preface.
		ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
		%% Receive the server preface.
		{ok, << Len1:24 >>} = gen_tcp:recv(Socket, 3, 1000),
		{ok, << 4:8, 0:40, _:Len1/binary >>} = gen_tcp:recv(Socket, 6 + Len1, 1000),
		%%
		%% Don't send the SETTINGS ack! We want the server to keep the original settings.
		%%
		%% Receive the SETTINGS ack.
		{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
		%% Send a HEADERS frame initiating a stream followed by
		%% DATA frames totaling 60000 bytes of body.
		Headers = [
			{<<":method">>, <<"POST">>},
			{<<":scheme">>, <<"http">>},
			{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
			{<<":path">>, <<"/long_polling">>}
		],
		{HeadersBlock, _} = cow_hpack:encode(Headers),
		ok = gen_tcp:send(Socket, [
			cow_http2:headers(1, nofin, HeadersBlock),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, fin, <<0:15000/unit:8>>)
		]),
		%% Receive a proper response.
		{ok, << Len2:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
		{ok, _} = gen_tcp:recv(Socket, Len2, 6000),
		%% No errors follow due to our sending of more than 0 bytes of data.
		{error, timeout} = gen_tcp:recv(Socket, 0, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

settings_max_frame_size(Config0) ->
	doc("The SETTINGS_MAX_FRAME_SIZE setting can be used to "
		"change the maximum frame size allowed. (RFC7540 6.5.2)"),
	%% Create a new listener that sets the maximum frame size to 30000.
	Config = cowboy_test:init_http(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		max_frame_size_received => 30000
	}, Config0),
	try
		%% Do the handshake.
		{ok, Socket} = do_handshake(Config),
		%% Send a HEADERS frame initiating a stream followed by
		%% a single 25000 bytes DATA frame.
		Headers = [
			{<<":method">>, <<"POST">>},
			{<<":scheme">>, <<"http">>},
			{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
			{<<":path">>, <<"/long_polling">>}
		],
		{HeadersBlock, _} = cow_hpack:encode(Headers),
		ok = gen_tcp:send(Socket, [
			cow_http2:headers(1, nofin, HeadersBlock),
			cow_http2:data(1, fin, <<0:25000/unit:8>>)
		]),
		%% Receive a proper response.
		{ok, << Len2:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
		{ok, _} = gen_tcp:recv(Socket, Len2, 6000),
		%% No errors follow due to our sending of a 25000 bytes frame.
		{error, timeout} = gen_tcp:recv(Socket, 0, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

settings_max_frame_size_reject_too_small(Config) ->
	doc("A SETTINGS_MAX_FRAME_SIZE smaller than 16384 must be rejected "
		"with a PROTOCOL_ERROR connection error. (RFC7540 6.5.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a SETTINGS frame with a SETTINGS_MAX_FRAME_SIZE lower than 16384.
	ok = gen_tcp:send(Socket, << 6:24, 4:8, 0:40, 5:16, 16383:32 >>),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

settings_max_frame_size_reject_too_large(Config) ->
	doc("A SETTINGS_MAX_FRAME_SIZE larger than 16777215 must be rejected "
		"with a PROTOCOL_ERROR connection error. (RFC7540 6.5.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a SETTINGS frame with a SETTINGS_MAX_FRAME_SIZE larger than 16777215.
	ok = gen_tcp:send(Socket, << 6:24, 4:8, 0:40, 5:16, 16777216:32 >>),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%   SETTINGS_MAX_HEADER_LIST_SIZE (0x6):  This advisory setting informs a
%      peer of the maximum size of header list that the sender is
%      prepared to accept, in octets.  The value is based on the
%      uncompressed size of header fields, including the length of the
%      name and value in octets plus an overhead of 32 octets for each
%      header field.
%
%      For any given request, a lower limit than what is advertised MAY
%      be enforced.  The initial value of this setting is unlimited.
%
%   An endpoint that receives a SETTINGS frame with any unknown or
%   unsupported identifier MUST ignore that setting. (6.5.2 and 6.5.3)

%% (RFC7540 6.5.3)
%   Upon receiving a SETTINGS frame with the ACK flag set, the
%   sender of the altered parameters can rely on the setting having been
%   applied.
%
%   If the sender of a SETTINGS frame does not receive an acknowledgement
%   within a reasonable amount of time, it MAY issue a connection error
%   (Section 5.4.1) of type SETTINGS_TIMEOUT.

%% (RFC7540 6.6) PUSH_PROMISE
% @todo PUSH_PROMISE frames have a reserved bit in the payload that must be ignored.
%
%   Padding:  Padding octets that contain no application semantic value.
%      Padding octets MUST be set to zero when sending.  A receiver is
%      not obligated to verify padding but MAY treat non-zero padding as
%      a connection error (Section 5.4.1) of type PROTOCOL_ERROR.
%
%   PUSH_PROMISE frames MUST only be sent on a peer-initiated stream that
%   is in either the "open" or "half-closed (remote)" state.  The stream
%   identifier of a PUSH_PROMISE frame indicates the stream it is
%   associated with.  If the stream identifier field specifies the value
%   0x0, a recipient MUST respond with a connection error (Section 5.4.1)
%   of type PROTOCOL_ERROR.

client_settings_disable_push(Config) ->
	doc("PUSH_PROMISE frames must not be sent when the setting "
		"SETTINGS_ENABLE_PUSH is disabled. (RFC7540 6.5.2, RFC7540 6.6, RFC7540 8.2)"),
	%% Do a prior knowledge handshake.
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a valid preface.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{
		enable_push => false
	})]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% Send the SETTINGS ack.
	ok = gen_tcp:send(Socket, cow_http2:settings_ack()),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	%% Send a HEADERS frame on a resource that sends PUSH_PROMISE frames.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/resp/push">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a HEADERS frame as a response, no PUSH_PROMISE frames.
	{ok, << _:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	ok.

%   Since PUSH_PROMISE reserves a stream, ignoring a PUSH_PROMISE frame
%   causes the stream state to become indeterminate.  A receiver MUST
%   treat the receipt of a PUSH_PROMISE on a stream that is neither
%   "open" nor "half-closed (local)" as a connection error
%   (Section 5.4.1) of type PROTOCOL_ERROR.
%
%   A receiver MUST treat the receipt of a PUSH_PROMISE that promises an
%   illegal stream identifier (Section 5.1.1) as a connection error
%   (Section 5.4.1) of type PROTOCOL_ERROR.  Note that an illegal stream
%   identifier is an identifier for a stream that is not currently in the
%   "idle" state.

%% (RFC7540 6.7) PING
%   PING frames are not associated with any individual stream.  If a PING
%   frame is received with a stream identifier field value other than
%   0x0, the recipient MUST respond with a connection error
%   (Section 5.4.1) of type PROTOCOL_ERROR.

%% (RFC7540 6.8) GOAWAY
% @todo GOAWAY frames have a reserved bit in the payload that must be ignored.
%
%% @todo We should eventually implement the mechanism for gracefully
%% shutting down of the connection. (Send the GOAWAY, finish processing
%% the current set of streams, give up after a certain timeout.)
%
%% @todo If we graceful shutdown and receive a GOAWAY, we give up too.
%   A GOAWAY frame might not immediately precede closing of the
%   connection; a receiver of a GOAWAY that has no more use for the
%   connection SHOULD still send a GOAWAY frame before terminating the
%   connection.
%
%% @todo And it gets more complex when you think about h1 to h2 proxies.
%   A server that is attempting to gracefully shut down a
%   connection SHOULD send an initial GOAWAY frame with the last stream
%   identifier set to 2^31-1 and a NO_ERROR code.  This signals to the
%   client that a shutdown is imminent and that initiating further
%   requests is prohibited.  After allowing time for any in-flight stream
%   creation (at least one round-trip time), the server can send another
%   GOAWAY frame with an updated last stream identifier.  This ensures
%   that a connection can be cleanly shut down without losing requests.
%
%% @todo And of course even if we shutdown we need to be careful about
%% the connection state.
%   After sending a GOAWAY frame, the sender can discard frames for
%   streams initiated by the receiver with identifiers higher than the
%   identified last stream.  However, any frames that alter connection
%   state cannot be completely ignored.  For instance, HEADERS,
%   PUSH_PROMISE, and CONTINUATION frames MUST be minimally processed to
%   ensure the state maintained for header compression is consistent (see
%   Section 4.3); similarly, DATA frames MUST be counted toward the
%   connection flow-control window.  Failure to process these frames can
%   cause flow control or header compression state to become
%   unsynchronized.
%
%   The GOAWAY frame applies to the connection, not a specific stream.
%   An endpoint MUST treat a GOAWAY frame with a stream identifier other
%   than 0x0 as a connection error (Section 5.4.1) of type
%   PROTOCOL_ERROR.

%% (RFC7540 6.9) WINDOW_UPDATE
% @todo WINDOW_UPDATE frames have a reserved bit in the payload that must be ignored.

window_update_reject_0(Config) ->
	doc("WINDOW_UPDATE frames with an increment of 0 for the connection "
		"flow control window must be rejected with a "
		"PROTOCOL_ERROR connection error. (RFC7540 6.9.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send connection-wide WINDOW_UPDATE frame with a value of 0.
	ok = gen_tcp:send(Socket, [
		cow_http2:window_update(0)
	]),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

window_update_reject_0_stream(Config) ->
	doc("WINDOW_UPDATE frames with an increment of 0 for a stream "
		"flow control window must be rejected with a "
		"PROTOCOL_ERROR stream error. (RFC7540 6.9.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame immediately followed by
	%% a WINDOW_UPDATE frame with a value of 0.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, fin, HeadersBlock),
		cow_http2:window_update(1, 0)
	]),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

%   A receiver that receives a flow-controlled frame MUST always account
%   for its contribution against the connection flow-control window,
%   unless the receiver treats this as a connection error
%   (Section 5.4.1).  This is necessary even if the frame is in error.
%   The sender counts the frame toward the flow-control window, but if
%   the receiver does not, the flow-control window at the sender and
%   receiver can become different.

data_reject_overflow(Config0) ->
	doc("DATA frames that cause the connection flow control window "
		"to overflow must be rejected with a FLOW_CONTROL_ERROR "
		"connection error. (RFC7540 6.9.1)"),
	%% Create a new listener that allows only a single concurrent stream.
	Config = cowboy_test:init_http(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		initial_stream_window_size => 100000
	}, Config0),
	try
		{ok, Socket} = do_handshake(Config),
		%% Send a HEADERS frame initiating a stream followed by
		%% DATA frames totaling 90000 bytes of body.
		Headers = [
			{<<":method">>, <<"POST">>},
			{<<":scheme">>, <<"http">>},
			{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
			{<<":path">>, <<"/long_polling">>}
		],
		{HeadersBlock, _} = cow_hpack:encode(Headers),
		ok = gen_tcp:send(Socket, [
			cow_http2:headers(1, nofin, HeadersBlock),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, fin, <<0:15000/unit:8>>)
		]),
		%% Receive a FLOW_CONTROL_ERROR connection error.
		{ok, << _:24, 7:8, _:72, 3:32 >>} = gen_tcp:recv(Socket, 17, 6000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

data_reject_overflow_stream(Config0) ->
	doc("DATA frames that cause the stream flow control window "
		"to overflow must be rejected with a FLOW_CONTROL_ERROR "
		"stream error. (RFC7540 6.9.1)"),
	%% Create a new listener that allows only a single concurrent stream.
	Config = cowboy_test:init_http(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		initial_connection_window_size => 100000
	}, Config0),
	try
		%% We need to do the handshake manually because a WINDOW_UPDATE
		%% frame will be sent to update the connection window.
		{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
		%% Send a valid preface.
		ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
		%% Receive the server preface.
		{ok, << Len1:24 >>} = gen_tcp:recv(Socket, 3, 1000),
		{ok, << 4:8, 0:40, _:Len1/binary >>} = gen_tcp:recv(Socket, 6 + Len1, 1000),
		%% Send the SETTINGS ack.
		ok = gen_tcp:send(Socket, cow_http2:settings_ack()),
		%% Receive the WINDOW_UPDATE for the connection.
		{ok, << 4:24, 8:8, 0:40, _:32 >>} = gen_tcp:recv(Socket, 13, 1000),
		%% Receive the SETTINGS ack.
		{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
		%% Send a HEADERS frame initiating a stream followed by
		%% DATA frames totaling 90000 bytes of body.
		Headers = [
			{<<":method">>, <<"POST">>},
			{<<":scheme">>, <<"http">>},
			{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
			{<<":path">>, <<"/long_polling">>}
		],
		{HeadersBlock, _} = cow_hpack:encode(Headers),
		ok = gen_tcp:send(Socket, [
			cow_http2:headers(1, nofin, HeadersBlock),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, nofin, <<0:15000/unit:8>>),
			cow_http2:data(1, fin, <<0:15000/unit:8>>)
		]),
		%% Receive a FLOW_CONTROL_ERROR stream error.
		{ok, << _:24, 3:8, _:8, 1:32, 3:32 >>} = gen_tcp:recv(Socket, 13, 6000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

%% (RFC7540 6.9.1)
%   Frames with zero length with the END_STREAM flag set (that
%   is, an empty DATA frame) MAY be sent if there is no available space
%   in either flow-control window.

window_update_reject_overflow(Config) ->
	doc("WINDOW_UPDATE frames that cause the connection flow control "
		"window to exceed 2^31-1 must be rejected with a "
		"FLOW_CONTROL_ERROR connection error. (RFC7540 6.9.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a connection-wide WINDOW_UPDATE frame that causes the window to overflow.
	ok = gen_tcp:send(Socket, [
		cow_http2:window_update(16#7fffffff)
	]),
	%% Receive a FLOW_CONTROL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 3:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

window_update_reject_overflow_stream(Config) ->
	doc("WINDOW_UPDATE frames that cause a stream flow control "
		"window to exceed 2^31-1 must be rejected with a "
		"FLOW_CONTROL_ERROR stream error. (RFC7540 6.9.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame immediately followed by a WINDOW_UPDATE
	%% frame that causes the stream window to overflow.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, fin, HeadersBlock),
		cow_http2:window_update(1, 16#7fffffff)
	]),
	%% Receive a FLOW_CONTROL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 3:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

settings_initial_window_size_changes(Config) ->
	doc("When the value of SETTINGS_INITIAL_WINDOW_SIZE changes, the server "
		"must adjust the size of the flow control windows of the active "
		"streams. (RFC7540 6.9.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Set SETTINGS_INITIAL_WINDOW_SIZE to 0 to prevent sending of DATA.
	ok = gen_tcp:send(Socket, cow_http2:settings(#{initial_window_size => 0})),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	%% Send a HEADERS frame.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a response but no DATA frames are coming.
	{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{error, timeout} = gen_tcp:recv(Socket, 9, 1000),
	%% Set SETTINGS_INITIAL_WINDOW_SIZE to a larger value.
	ok = gen_tcp:send(Socket, cow_http2:settings(#{initial_window_size => 5})),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	%% Receive a DATA frame of that size and no other.
	{ok, << 5:24, 0:8, 0:8, 1:32, "Hello" >>} = gen_tcp:recv(Socket, 14, 1000),
	{error, timeout} = gen_tcp:recv(Socket, 9, 1000),
	%% Set SETTINGS_INITIAL_WINDOW_SIZE to exactly the size in the body.
	ok = gen_tcp:send(Socket, cow_http2:settings(#{initial_window_size => 12})),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	%% Receive the rest of the response.
	{ok, << 7:24, 0:8, 1:8, 1:32, " world!" >>} = gen_tcp:recv(Socket, 16, 1000),
	ok.

settings_initial_window_size_changes_negative(Config) ->
	doc("When the value of SETTINGS_INITIAL_WINDOW_SIZE changes, the server "
		"must adjust the size of the flow control windows of the active "
		"streams even if their window end up negative. (RFC7540 6.9.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Set SETTINGS_INITIAL_WINDOW_SIZE to 5.
	ok = gen_tcp:send(Socket, cow_http2:settings(#{initial_window_size => 5})),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	%% Send a HEADERS frame.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a response with a single DATA frame of the initial size we set.
	{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, _} = gen_tcp:recv(Socket, SkipLen, 1000),
	{ok, << 5:24, 0:8, 0:8, 1:32, "Hello" >>} = gen_tcp:recv(Socket, 14, 1000),
	{error, timeout} = gen_tcp:recv(Socket, 9, 1000),
	%% Set SETTINGS_INITIAL_WINDOW_SIZE to 0 to make the stream's window negative.
	ok = gen_tcp:send(Socket, cow_http2:settings(#{initial_window_size => 0})),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	%% Set SETTINGS_INITIAL_WINDOW_SIZE to exactly the size in the body.
	ok = gen_tcp:send(Socket, cow_http2:settings(#{initial_window_size => 12})),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	%% Receive the rest of the response.
	{ok, << 7:24, 0:8, 1:8, 1:32, " world!" >>} = gen_tcp:recv(Socket, 16, 1000),
	ok.

settings_initial_window_size_reject_overflow(Config) ->
	doc("A SETTINGS_INITIAL_WINDOW_SIZE that causes a flow control window "
		"to exceed 2^31-1 must be rejected with a FLOW_CONTROL_ERROR "
		"connection error. (RFC7540 6.5.2, RFC7540 6.9.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Set SETTINGS_INITIAL_WINDOW_SIZE to 2^31.
	ok = gen_tcp:send(Socket, cow_http2:settings(#{initial_window_size => 16#80000000})),
	%% Receive a FLOW_CONTROL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 3:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% (RFC7540 6.9.3)
%% @todo The right way to do this seems to be to wait for the SETTINGS ack
%% before we KNOW the flow control window was updated on the other side.
%   A receiver that wishes to use a smaller flow-control window than the
%   current size can send a new SETTINGS frame.  However, the receiver
%   MUST be prepared to receive data that exceeds this window size, since
%   the sender might send data that exceeds the lower limit prior to
%   processing the SETTINGS frame.

%% (RFC7540 6.10) CONTINUATION
%   CONTINUATION frames MUST be associated with a stream.  If a
%   CONTINUATION frame is received whose stream identifier field is 0x0,
%   the recipient MUST respond with a connection error (Section 5.4.1) of
%   type PROTOCOL_ERROR.
%
%   A CONTINUATION frame MUST be preceded by a HEADERS, PUSH_PROMISE or
%   CONTINUATION frame without the END_HEADERS flag set.  A recipient
%   that observes violation of this rule MUST respond with a connection
%   error (Section 5.4.1) of type PROTOCOL_ERROR.

%% (RFC7540 7) Error Codes
%   Unknown or unsupported error codes MUST NOT trigger any special
%   behavior.  These MAY be treated by an implementation as being
%   equivalent to INTERNAL_ERROR.

accept_trailers(Config) ->
	doc("Trailing HEADERS frames must be accepted. (RFC7540 8.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a request containing DATA and trailing HEADERS frames.
	{HeadersBlock, EncodeState} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>},
		{<<"trailer">>, <<"x-checksum">>}
	]),
	{TrailersBlock, _} = cow_hpack:encode([
		{<<"x-checksum">>, <<"md5:4cc909a007407f3706399b6496babec3">>}
	], EncodeState),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		cow_http2:data(1, nofin, <<0:10000/unit:8>>),
		cow_http2:headers(1, fin, TrailersBlock)
	]),
	%% Receive a HEADERS frame as a response.
	{ok, << _:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	ok.

accept_trailers_continuation(Config) ->
	doc("Trailing HEADERS and CONTINUATION frames must be accepted. (RFC7540 8.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a request containing DATA and trailing HEADERS and CONTINUATION frames.
	{HeadersBlock, EncodeState} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>},
		{<<"trailer">>, <<"x-checksum">>}
	]),
	{TrailersBlock, _} = cow_hpack:encode([
		{<<"x-checksum">>, <<"md5:4cc909a007407f3706399b6496babec3">>}
	], EncodeState),
	Len = iolist_size(TrailersBlock),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		cow_http2:data(1, nofin, <<0:10000/unit:8>>),
		<<0:24, 1:8, 0:7, 1:1, 0:1, 1:31>>,
		<<Len:24, 9:8, 0:5, 1:1, 0:3, 1:31>>,
		TrailersBlock
	]),
	%% Receive a HEADERS frame as a response.
	{ok, << _:24, 1:8, _:40 >>} = gen_tcp:recv(Socket, 9, 6000),
	ok.

%% We reject all invalid HEADERS with a connection error because
%% we do not want to waste resources decoding them.
reject_trailers_nofin(Config) ->
	doc("Trailing HEADERS frames received without the END_STREAM flag "
		"set must be rejected with a PROTOCOL_ERROR connection error. "
		"(RFC7540 8.1, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a request containing DATA and trailing HEADERS frames.
	%% The trailing HEADERS does not have the END_STREAM flag set.
	{HeadersBlock, EncodeState} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>},
		{<<"trailer">>, <<"x-checksum">>}
	]),
	{TrailersBlock, _} = cow_hpack:encode([
		{<<"x-checksum">>, <<"md5:4cc909a007407f3706399b6496babec3">>}
	], EncodeState),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		cow_http2:data(1, nofin, <<0:10000/unit:8>>),
		cow_http2:headers(1, nofin, TrailersBlock)
	]),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

%% We reject all invalid HEADERS with a connection error because
%% we do not want to waste resources decoding them.
reject_trailers_nofin_continuation(Config) ->
	doc("Trailing HEADERS frames received without the END_STREAM flag "
		"set must be rejected with a PROTOCOL_ERROR connection error. "
		"(RFC7540 8.1, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a request containing DATA and trailing HEADERS and CONTINUATION frames.
	%% The trailing HEADERS does not have the END_STREAM flag set.
	{HeadersBlock, EncodeState} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>},
		{<<"trailer">>, <<"x-checksum">>}
	]),
	{TrailersBlock, _} = cow_hpack:encode([
		{<<"x-checksum">>, <<"md5:4cc909a007407f3706399b6496babec3">>}
	], EncodeState),
	Len = iolist_size(TrailersBlock),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		cow_http2:data(1, nofin, <<0:10000/unit:8>>),
		<<0:24, 1:8, 0:9, 1:31>>,
		<<Len:24, 9:8, 0:5, 1:1, 0:3, 1:31>>,
		TrailersBlock
	]),
	%% Receive a PROTOCOL_ERROR connection error.
	{ok, << _:24, 7:8, _:72, 1:32 >>} = gen_tcp:recv(Socket, 17, 6000),
	ok.

headers_informational_nofin(Config) ->
	doc("Informational HEADERS frames must not have the END_STREAM flag set. (RFC7540 8.1)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame on an idle stream.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/echo/read_body">>},
		{<<"expect">>, <<"100-continue">>},
		{<<"content-length">>, <<"1000000">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, HeadersBlock)),
	%% Receive an informational HEADERS frame without the END_STREAM flag.
	{ok, << Len:24, 1:8, 0:5, 1:1, 0:2, _:32 >>} = gen_tcp:recv(Socket, 9, 6000),
	{ok, RespHeadersBlock} = gen_tcp:recv(Socket, Len, 6000),
	%% Confirm it has a 100 status code.
	{RespHeaders, _} = cow_hpack:decode(RespHeadersBlock),
	{_, <<"100">>} = lists:keyfind(<<":status">>, 1, RespHeaders),
	ok.

%% @todo This one is interesting to implement because Cowboy DOES this.
%   A server can
%   send a complete response prior to the client sending an entire
%   request if the response does not depend on any portion of the request
%   that has not been sent and received.  When this is true, a server MAY
%   request that the client abort transmission of a request without error
%   by sending a RST_STREAM with an error code of NO_ERROR after sending
%   a complete response (i.e., a frame with the END_STREAM flag).

headers_reject_uppercase_header_name(Config) ->
	doc("Requests containing uppercase header names must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a uppercase header name.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>},
		{<<"HELLO">>, <<"world">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_response_pseudo_headers(Config) ->
	doc("Requests containing response pseudo-headers must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.1, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a response pseudo-header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>},
		{<<":status">>, <<"200">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_unknown_pseudo_headers(Config) ->
	doc("Requests containing unknown pseudo-headers must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.1, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with an unknown pseudo-header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>},
		{<<":upgrade">>, <<"websocket">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_pseudo_headers_in_trailers(Config) ->
	doc("Requests containing pseudo-headers in trailers must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.1, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a request containing DATA and trailing HEADERS frames.
	%% The trailing HEADERS contains pseudo-headers.
	{HeadersBlock, EncodeState} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>},
		{<<"trailer">>, <<"x-checksum">>}
	]),
	{TrailersBlock, _} = cow_hpack:encode([
		{<<"x-checksum">>, <<"md5:4cc909a007407f3706399b6496babec3">>},
		{<<":path">>, <<"/">>}
	], EncodeState),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		cow_http2:data(1, nofin, <<0:10000/unit:8>>),
		cow_http2:headers(1, fin, TrailersBlock)
	]),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_pseudo_headers_after_regular_headers(Config) ->
	doc("Requests containing pseudo-headers after regular headers must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.1, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a pseudo-header after regular headers.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"content-length">>, <<"0">>},
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_connection_header(Config) ->
	doc("Requests containing a connection header must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.2, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a connection header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>},
		{<<"connection">>, <<"close">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_keep_alive_header(Config) ->
	doc("Requests containing a keep-alive header must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.2, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a keep-alive header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>},
		{<<"keep-alive">>, <<"timeout=5, max=1000">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_proxy_authenticate_header(Config) ->
	doc("Requests containing a connection header must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.2, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a proxy-authenticate header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>},
		{<<"proxy-authenticate">>, <<"Basic">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_proxy_authorization_header(Config) ->
	doc("Requests containing a connection header must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.2, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a proxy-authorization header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>},
		{<<"proxy-authorization">>, <<"Basic YWxhZGRpbjpvcGVuc2VzYW1l">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_transfer_encoding_header(Config) ->
	doc("Requests containing a connection header must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.2, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a transfer-encoding header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>},
		{<<"transfer-encoding">>, <<"chunked">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_upgrade_header(Config) ->
	doc("Requests containing a connection header must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.2, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a upgrade header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>},
		{<<"upgrade">>, <<"websocket">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

accept_te_header_value_trailers(Config) ->
	doc("Requests containing a TE header with a value of \"trailers\" "
		"must be accepted. (RFC7540 8.1.2.2)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a TE header with value "trailers".
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>},
		{<<"te">>, <<"trailers">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a response.
	{ok, << _:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	ok.

reject_te_header_other_values(Config) ->
	doc("Requests containing a TE header with a value other than \"trailers\" must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.2, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a TE header with a different value.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>},
		{<<"te">>, <<"trailers, deflate;q=0.5">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

%% (RFC7540 8.1.2.2)
%   This means that an intermediary transforming an HTTP/1.x message to
%   HTTP/2 will need to remove any header fields nominated by the
%   Connection header field, along with the Connection header field
%   itself.  Such intermediaries SHOULD also remove other connection-
%   specific header fields, such as Keep-Alive, Proxy-Connection,
%   Transfer-Encoding, and Upgrade, even if they are not nominated by the
%   Connection header field.

reject_userinfo(Config) ->
	doc("An authority containing a userinfo component must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.3, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a userinfo authority component.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"user@localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

%% (RFC7540 8.1.2.3)
%      To ensure that the HTTP/1.1 request line can be reproduced
%      accurately, this pseudo-header field MUST be omitted when
%      translating from an HTTP/1.1 request that has a request target in
%      origin or asterisk form (see [RFC7230], Section 5.3).  Clients
%      that generate HTTP/2 requests directly SHOULD use the ":authority"
%      pseudo-header field instead of the Host header field.  An
%      intermediary that converts an HTTP/2 request to HTTP/1.1 MUST
%      create a Host header field if one is not present in a request by
%      copying the value of the ":authority" pseudo-header field.

reject_empty_path(Config) ->
	doc("A request containing an empty path component must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.3, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with an empty path component.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<>>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_missing_pseudo_header_method(Config) ->
	doc("A request without a method component must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.3, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame without a :method pseudo-header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_many_pseudo_header_method(Config) ->
	doc("A request containing more than one method component must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.3, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with more than one :method pseudo-header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_missing_pseudo_header_scheme(Config) ->
	doc("A request without a scheme component must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.3, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame without a :scheme pseudo-header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_many_pseudo_header_scheme(Config) ->
	doc("A request containing more than one scheme component must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.3, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with more than one :scheme pseudo-header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_missing_pseudo_header_authority(Config) ->
	doc("A request without an authority component must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.3, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame without an :authority pseudo-header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_many_pseudo_header_authority(Config) ->
	doc("A request containing more than one authority component must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.3, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with more than one :authority pseudo-header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_missing_pseudo_header_path(Config) ->
	doc("A request without a path component must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.3, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame without a :path pseudo-header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>} %% @todo Correct port number.
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_many_pseudo_header_path(Config) ->
	doc("A request containing more than one path component must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.3, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with more than one :path pseudo-header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>},
		{<<":path">>, <<"/">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

%% (RFC7540 8.1.2.4)
%   For HTTP/2 responses, a single ":status" pseudo-header field is
%   defined that carries the HTTP status code field (see [RFC7231],
%   Section 6).  This pseudo-header field MUST be included in all
%   responses; otherwise, the response is malformed (Section 8.1.2.6).

%% (RFC7540 8.1.2.5)
%   To allow for better compression efficiency, the Cookie header field
%   MAY be split into separate header fields, each with one or more
%   cookie-pairs.  If there are multiple Cookie header fields after
%   decompression, these MUST be concatenated into a single octet string
%   using the two-octet delimiter of 0x3B, 0x20 (the ASCII string "; ")
%   before being passed into a non-HTTP/2 context, such as an HTTP/1.1
%   connection, or a generic HTTP server application.

reject_data_size_smaller_than_content_length(Config) ->
	doc("Requests that have a content-length header whose value does not "
		"match the total length of the DATA frames must be rejected with "
		"a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a content-length header different
	%% than the sum of the DATA frame sizes.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>},
		{<<"content-length">>, <<"12">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		cow_http2:data(1, fin, <<"Hello!">>)
	]),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_data_size_larger_than_content_length(Config) ->
	doc("Requests that have a content-length header whose value does not "
		"match the total length of the DATA frames must be rejected with "
		"a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a content-length header different
	%% than the sum of the DATA frame sizes.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>},
		{<<"content-length">>, <<"12">>}
	]),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		cow_http2:data(1, nofin, <<"Hello! World! Universe!">>),
		cow_http2:data(1, fin, <<"Multiverse!">>)
	]),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_content_length_without_data(Config) ->
	doc("Requests that have a content-length header whose value does not "
		"match the total length of the DATA frames must be rejected with "
		"a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a content-length header different
	%% than the sum of the DATA frame sizes.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>},
		{<<"content-length">>, <<"12">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, fin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_data_size_different_than_content_length_with_trailers(Config) ->
	doc("Requests that have a content-length header whose value does not "
		"match the total length of the DATA frames must be rejected with "
		"a PROTOCOL_ERROR stream error. (RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with a content-length header different
	%% than the sum of the DATA frame sizes.
	{HeadersBlock, EncodeState} = cow_hpack:encode([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/long_polling">>},
		{<<"content-length">>, <<"12">>},
		{<<"trailer">>, <<"x-checksum">>}
	]),
	{TrailersBlock, _} = cow_hpack:encode([
		{<<"x-checksum">>, <<"md5:4cc909a007407f3706399b6496babec3">>}
	], EncodeState),
	ok = gen_tcp:send(Socket, [
		cow_http2:headers(1, nofin, HeadersBlock),
		cow_http2:data(1, nofin, <<"Hello!">>),
		cow_http2:headers(1, fin, TrailersBlock)
	]),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_duplicate_content_length_header(Config) ->
	doc("A request with duplicate content-length headers must be rejected "
		"with a PROTOCOL_ERROR stream error. (RFC7230 3.3.2, RFC7540 8.1.2.6)"),
	{ok, Socket} = do_handshake(Config),
	%% Send a HEADERS frame with more than one content-length header.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"12">>},
		{<<"content-length">>, <<"12">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, HeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

%   Intermediaries that process HTTP requests or responses (i.e., any
%   intermediary not acting as a tunnel) MUST NOT forward a malformed
%   request or response.  Malformed requests or responses that are
%   detected MUST be treated as a stream error (Section 5.4.2) of type
%   PROTOCOL_ERROR.
%
%   For malformed requests, a server MAY send an HTTP response prior to
%   closing or resetting the stream.  Clients MUST NOT accept a malformed
%   response.  Note that these requirements are intended to protect
%   against several types of common attacks against HTTP; they are
%   deliberately strict because being permissive can expose
%   implementations to these vulnerabilities.

%% @todo It migh be worth reproducing the good examples. (RFC7540 8.1.3)

%% (RFC7540 8.1.4)
%   A server MUST NOT indicate that a stream has not been processed
%   unless it can guarantee that fact.  If frames that are on a stream
%   are passed to the application layer for any stream, then
%   REFUSED_STREAM MUST NOT be used for that stream, and a GOAWAY frame
%   MUST include a stream identifier that is greater than or equal to the
%   given stream identifier.

%% (RFC7540 8.2)
%   Promised requests MUST be cacheable (see [RFC7231], Section 4.2.3),
%   MUST be safe (see [RFC7231], Section 4.2.1), and MUST NOT include a
%   request body.
%
%   The server MUST include a value in the ":authority" pseudo-header
%   field for which the server is authoritative (see Section 10.1).
%
%   A client cannot push.  Thus, servers MUST treat the receipt of a
%   PUSH_PROMISE frame as a connection error (Section 5.4.1) of type
%   PROTOCOL_ERROR.

%% (RFC7540 8.2.1)
%   The header fields in PUSH_PROMISE and any subsequent CONTINUATION
%   frames MUST be a valid and complete set of request header fields
%   (Section 8.1.2.3).  The server MUST include a method in the ":method"
%   pseudo-header field that is safe and cacheable.  If a client receives
%   a PUSH_PROMISE that does not include a complete and valid set of
%   header fields or the ":method" pseudo-header field identifies a
%   method that is not safe, it MUST respond with a stream error
%   (Section 5.4.2) of type PROTOCOL_ERROR.
%
%% @todo This probably should be documented.
%   The server SHOULD send PUSH_PROMISE (Section 6.6) frames prior to
%   sending any frames that reference the promised responses.  This
%   avoids a race where clients issue requests prior to receiving any
%   PUSH_PROMISE frames.
%
%   PUSH_PROMISE frames MUST NOT be sent by the client.
%
%   PUSH_PROMISE frames can be sent by the server in response to any
%   client-initiated stream, but the stream MUST be in either the "open"
%   or "half-closed (remote)" state with respect to the server.
%   PUSH_PROMISE frames are interspersed with the frames that comprise a
%   response, though they cannot be interspersed with HEADERS and
%   CONTINUATION frames that comprise a single header block.

%% (RFC7540 8.2.2)
%   If the client determines, for any reason, that it does not wish to
%   receive the pushed response from the server or if the server takes
%   too long to begin sending the promised response, the client can send
%   a RST_STREAM frame, using either the CANCEL or REFUSED_STREAM code
%   and referencing the pushed stream's identifier.
%
%   A client can use the SETTINGS_MAX_CONCURRENT_STREAMS setting to limit
%   the number of responses that can be concurrently pushed by a server.
%   Advertising a SETTINGS_MAX_CONCURRENT_STREAMS value of zero disables
%   server push by preventing the server from creating the necessary
%   streams.  This does not prohibit a server from sending PUSH_PROMISE
%   frames; clients need to reset any promised streams that are not
%   wanted.

%% @todo Implement CONNECT. (RFC7540 8.3)

status_code_421(Config) ->
	doc("The 421 Misdirected Request status code can be sent. (RFC7540 9.1.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/421"),
	{response, fin, 421, _} = gun:await(ConnPid, Ref),
	ok.

%% @todo Review (RFC7540 9.2, 9.2.1, 9.2.2) TLS 1.2 usage.
%% We probably want different ways to enforce these to simplify the life
%% of users. A function cowboy:start_h2_tls could do the same as start_tls
%% but with the security requirements of HTTP/2 enforced. Another way is to
%% have an option at the establishment of the connection that checks that
%% the security of the connection is adequate.

%% (RFC7540 10.3)
%   The HTTP/2 header field encoding allows the expression of names that
%   are not valid field names in the Internet Message Syntax used by
%   HTTP/1.1.  Requests or responses containing invalid header field
%   names MUST be treated as malformed (Section 8.1.2.6).
%
%   Similarly, HTTP/2 allows header field values that are not valid.
%   While most of the values that can be encoded will not alter header
%   field parsing, carriage return (CR, ASCII 0xd), line feed (LF, ASCII
%   0xa), and the zero character (NUL, ASCII 0x0) might be exploited by
%   an attacker if they are translated verbatim.  Any request or response
%   that contains a character not permitted in a header field value MUST
%   be treated as malformed (Section 8.1.2.6).  Valid characters are
%   defined by the "field-content" ABNF rule in Section 3.2 of [RFC7230].

%% (RFC7540 10.5) Denial-of-Service Considerations
%   An endpoint that doesn't monitor this behavior exposes itself to a
%   risk of denial-of-service attack.  Implementations SHOULD track the
%   use of these features and set limits on their use.  An endpoint MAY
%   treat activity that is suspicious as a connection error
%   (Section 5.4.1) of type ENHANCE_YOUR_CALM.

%% (RFC7540 10.5.1)
%   A server that receives a larger header block than it is willing to
%   handle can send an HTTP 431 (Request Header Fields Too Large) status
%   code [RFC6585].  A client can discard responses that it cannot
%   process.  The header block MUST be processed to ensure a consistent
%   connection state, unless the connection is closed.

%% @todo Implement CONNECT and limit the number of CONNECT streams (RFC7540 10.5.2).

%% @todo This probably should be documented. (RFC7540 10.6)
%   Implementations communicating on a secure channel MUST NOT compress
%   content that includes both confidential and attacker-controlled data
%   unless separate compression dictionaries are used for each source of
%   data.  Compression MUST NOT be used if the source of data cannot be
%   reliably determined.  Generic stream compression, such as that
%   provided by TLS, MUST NOT be used with HTTP/2 (see Section 9.2).

%% (RFC7540 A)
%   An HTTP/2 implementation MAY treat the negotiation of any of the
%   following cipher suites with TLS 1.2 as a connection error
%   (Section 5.4.1) of type INADEQUATE_SECURITY.
