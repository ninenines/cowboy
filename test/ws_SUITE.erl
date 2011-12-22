%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
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

-module(ws_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
	init_per_group/2, end_per_group/2]). %% ct.
-export([ws0/1, ws8/1, ws8_single_bytes/1, ws8_init_shutdown/1,
	ws13/1, ws_timeout_hibernate/1]). %% ws.

%% ct.

all() ->
	[{group, ws}].

groups() ->
	BaseTests = [ws0, ws8, ws8_single_bytes, ws8_init_shutdown, ws13,
		ws_timeout_hibernate],
	[{ws, [], BaseTests}].

init_per_suite(Config) ->
	application:start(inets),
	application:start(cowboy),
	Config.

end_per_suite(_Config) ->
	application:stop(cowboy),
	application:stop(inets),
	ok.

init_per_group(ws, Config) ->
	Port = 33080,
	cowboy:start_listener(ws, 100,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [{dispatch, init_dispatch()}]
	),
	[{port, Port}|Config].

end_per_group(Listener, _Config) ->
	cowboy:stop_listener(Listener),
	ok.

%% Dispatch configuration.

init_dispatch() ->
	[
		{[<<"localhost">>], [
			{[<<"websocket">>], websocket_handler, []},
			{[<<"ws_timeout_hibernate">>], ws_timeout_hibernate_handler, []},
			{[<<"ws_init_shutdown">>], websocket_handler_init_shutdown, []}
		]}
	].

%% ws and wss.

%% This test makes sure the code works even if we wait for a reply
%% before sending the third challenge key in the GET body.
%%
%% This ensures that Cowboy will work fine with proxies on hixie.
ws0(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket,
		"GET /websocket HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: WebSocket\r\n"
		"Origin: http://localhost\r\n"
		"Sec-Websocket-Key1: Y\" 4 1Lj!957b8@0H756!i\r\n"
		"Sec-Websocket-Key2: 1711 M;4\\74  80<6\r\n"
		"\r\n"),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "WebSocket Protocol Handshake"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "WebSocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-location", "ws://localhost/websocket"}
		= lists:keyfind("sec-websocket-location", 1, Headers),
	{"sec-websocket-origin", "http://localhost"}
		= lists:keyfind("sec-websocket-origin", 1, Headers),
	ok = gen_tcp:send(Socket, <<15,245,8,18,2,204,133,33>>),
	{ok, Body} = gen_tcp:recv(Socket, 0, 6000),
	<<169,244,191,103,146,33,149,59,74,104,67,5,99,118,171,236>> = Body,
	ok = gen_tcp:send(Socket, << 0, "client_msg", 255 >>),
	{ok, << 0, "client_msg", 255 >>} = gen_tcp:recv(Socket, 0, 6000),
	{ok, << 0, "websocket_init", 255 >>} = gen_tcp:recv(Socket, 0, 6000),
	{ok, << 0, "websocket_handle", 255 >>} = gen_tcp:recv(Socket, 0, 6000),
	{ok, << 0, "websocket_handle", 255 >>} = gen_tcp:recv(Socket, 0, 6000),
	{ok, << 0, "websocket_handle", 255 >>} = gen_tcp:recv(Socket, 0, 6000),
	%% We try to send another HTTP request to make sure
	%% the server closed the request.
	ok = gen_tcp:send(Socket, [
		<< 255, 0 >>, %% Close websocket command.
		"GET / HTTP/1.1\r\nHost: localhost\r\n\r\n" %% Server should ignore it.
	]),
	{ok, << 255, 0 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws8(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /websocket HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	ok = gen_tcp:send(Socket, << 16#81, 16#85, 16#37, 16#fa, 16#21, 16#3d,
		16#7f, 16#9f, 16#4d, 16#51, 16#58 >>),
	{ok, << 1:1, 0:3, 1:4, 0:1, 5:7, "Hello" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 14:7, "websocket_init" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 9:4, 0:8 >>), %% ping
	{ok, << 1:1, 0:3, 10:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000), %% pong
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 0:8 >>), %% close
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws8_single_bytes(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /websocket HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	ok = gen_tcp:send(Socket, << 16#81 >>), %% send one byte
	ok = timer:sleep(100), %% sleep for a period
	ok = gen_tcp:send(Socket, << 16#85 >>), %% send another and so on
        ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#37 >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#fa >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#21 >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#3d >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#7f >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#9f >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#4d >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#51 >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#58 >>),
	{ok, << 1:1, 0:3, 1:4, 0:1, 14:7, "websocket_init" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 5:7, "Hello" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 9:4, 0:8 >>), %% ping
	{ok, << 1:1, 0:3, 10:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000), %% pong
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 0:8 >>), %% close
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_timeout_hibernate(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_timeout_hibernate HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws8_init_shutdown(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_init_shutdown HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 403, "Forbidden"}, _Rest}
		= erlang:decode_packet(http, Handshake, []),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws13(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /websocket HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 13\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"Upgrade: websocket\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	%% text
	ok = gen_tcp:send(Socket, << 16#81, 16#85, 16#37, 16#fa, 16#21, 16#3d,
		16#7f, 16#9f, 16#4d, 16#51, 16#58 >>),
	{ok, << 1:1, 0:3, 1:4, 0:1, 5:7, "Hello" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	%% binary (empty)
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 2:4, 0:8 >>),
	{ok, << 1:1, 0:3, 2:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	%% binary
	ok = gen_tcp:send(Socket, << 16#82, 16#85, 16#37, 16#fa, 16#21, 16#3d,
		16#7f, 16#9f, 16#4d, 16#51, 16#58 >>),
	{ok, << 1:1, 0:3, 2:4, 0:1, 5:7, "Hello" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	%% Receives.
	{ok, << 1:1, 0:3, 1:4, 0:1, 14:7, "websocket_init" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 9:4, 0:8 >>), %% ping
	{ok, << 1:1, 0:3, 10:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000), %% pong
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 0:8 >>), %% close
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

websocket_headers({ok, http_eoh, Rest}, Acc) ->
	[Acc, Rest];
websocket_headers({ok, {http_header, _I, Key, _R, Value}, Rest}, Acc) ->
	F = fun(S) when is_atom(S) -> S; (S) -> string:to_lower(S) end,
	websocket_headers(erlang:decode_packet(httph, Rest, []),
		[{F(Key), Value}|Acc]).
