%% Copyright (c) 2011-2017, Loïc Hoguin <essen@ninenines.eu>
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
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

%% ct.

all() ->
	[{group, ws}].

groups() ->
	[{ws, [parallel], ct_helper:all(?MODULE)}].

init_per_group(Name, Config) ->
	cowboy_test:init_http(Name, #{
		env => #{dispatch => init_dispatch()}
	}, Config).

end_per_group(Listener, _Config) ->
	cowboy:stop_listener(Listener).

%% Dispatch configuration.

init_dispatch() ->
	cowboy_router:compile([
		{"localhost", [
			{"/ws_echo", ws_echo, []},
			{"/ws_echo_timer", ws_echo_timer, []},
			{"/ws_init", ws_init_h, []},
			{"/ws_init_shutdown", ws_init_shutdown, []},
			{"/ws_send_many", ws_send_many, [
				{sequence, [
					{text, <<"one">>},
					{text, <<"two">>},
					{text, <<"seven!">>}]}
			]},
			{"/ws_send_close", ws_send_many, [
				{sequence, [
					{text, <<"send">>},
					close,
					{text, <<"won't be received">>}]}
			]},
			{"/ws_send_close_payload", ws_send_many, [
				{sequence, [
					{text, <<"send">>},
					{close, 1001, <<"some text!">>},
					{text, <<"won't be received">>}]}
			]},
			{"/ws_subprotocol", ws_subprotocol, []},
			{"/terminate", ws_terminate_h, []},
			{"/ws_timeout_hibernate", ws_timeout_hibernate, []},
			{"/ws_timeout_cancel", ws_timeout_cancel, []},
			{"/ws_max_frame_size", ws_max_frame_size, []},
			{"/ws_deflate_opts", ws_deflate_opts_h, []}
		]}
	]).

%% Tests.

unlimited_connections(Config) ->
	doc("Websocket connections are not limited. The connections "
		"are removed from the count after the handshake completes."),
	_ = [begin
		spawn_link(fun() -> do_connect_and_loop(Config) end),
		timer:sleep(1)
	end || _ <- lists:seq(1, 3000)],
	timer:sleep(1000),
	%% We have at least 3000 client and 3000 server sockets.
	true = length(erlang:ports()) > 6000,
	%% Ranch thinks we have no connections.
	0 = ranch_server:count_connections(ws),
	ok.

do_connect_and_loop(Config) ->
	{ok, Socket, _} = do_handshake("/ws_echo", Config),
	do_loop(Socket).

do_loop(Socket) ->
	%% Masked text hello echoed back clear by the server.
	Mask = 16#37fa213d,
	MaskedHello = do_mask(<<"Hello">>, Mask, <<>>),
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, MaskedHello/binary >>),
	{ok, << 1:1, 0:3, 1:4, 0:1, 5:7, "Hello" >>} = gen_tcp:recv(Socket, 0, 6000),
	timer:sleep(1000),
	do_loop(Socket).

ws0(Config) ->
	doc("Websocket version 0 (hixie-76 draft) is no longer supported."),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket,
		"GET /ws_echo_timer HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: WebSocket\r\n"
		"Origin: http://localhost\r\n"
		"Sec-Websocket-Key1: Y\" 4 1Lj!957b8@0H756!i\r\n"
		"Sec-Websocket-Key2: 1711 M;4\\74  80<6\r\n"
		"\r\n"),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 400, _}, _} = erlang:decode_packet(http, Handshake, []),
	ok.

ws7(Config) ->
	doc("Websocket version 7 (draft) is supported."),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_echo_timer HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 7\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, _}, Rest} = erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = do_decode_headers(erlang:decode_packet(httph, Rest, []), []),
	{_, "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{_, "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{_, "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="} = lists:keyfind("sec-websocket-accept", 1, Headers),
	do_ws_version(Socket).

ws8(Config) ->
	doc("Websocket version 8 (draft) is supported."),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_echo_timer HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, _}, Rest} = erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = do_decode_headers(erlang:decode_packet(httph, Rest, []), []),
	{_, "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{_, "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{_, "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="} = lists:keyfind("sec-websocket-accept", 1, Headers),
	do_ws_version(Socket).

ws13(Config) ->
	doc("Websocket version 13 (RFC) is supported."),
	{ok, Socket, _} = do_handshake("/ws_echo_timer", Config),
	do_ws_version(Socket).

do_ws_version(Socket) ->
	%% Masked text hello echoed back clear by the server.
	Mask = 16#37fa213d,
	MaskedHello = do_mask(<<"Hello">>, Mask, <<>>),
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, MaskedHello/binary >>),
	{ok, << 1:1, 0:3, 1:4, 0:1, 5:7, "Hello" >>} = gen_tcp:recv(Socket, 0, 6000),
	%% Empty binary frame echoed back.
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 2:4, 1:1, 0:7, 0:32 >>),
	{ok, << 1:1, 0:3, 2:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	%% Masked binary hello echoed back clear by the server.
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 2:4, 1:1, 5:7, Mask:32, MaskedHello/binary >>),
	{ok, << 1:1, 0:3, 2:4, 0:1, 5:7, "Hello" >>} = gen_tcp:recv(Socket, 0, 6000),
	%% Frames sent on timer by the handler.
	{ok, << 1:1, 0:3, 1:4, 0:1, 14:7, "websocket_init" >>} = gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>} = gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>} = gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>} = gen_tcp:recv(Socket, 0, 6000),
	%% Client-initiated ping/pong.
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 9:4, 1:1, 0:7, 0:32 >>),
	{ok, << 1:1, 0:3, 10:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	%% Client-initiated close.
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>),
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_deflate_opts_client_context_takeover(Config) ->
	doc("Handler is configured with client context takeover enabled."),
	{ok, _, Headers1} = do_handshake("/ws_deflate_opts?client_context_takeover",
		"Sec-WebSocket-Extensions: permessage-deflate\r\n", Config),
	{_, "permessage-deflate"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers1),
	{ok, _, Headers2} = do_handshake("/ws_deflate_opts?client_context_takeover",
		"Sec-WebSocket-Extensions: permessage-deflate; client_no_context_takeover\r\n", Config),
	{_, "permessage-deflate; client_no_context_takeover"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers2),
	ok.

ws_deflate_opts_client_no_context_takeover(Config) ->
	doc("Handler is configured with client context takeover disabled."),
	{ok, _, Headers1} = do_handshake("/ws_deflate_opts?client_no_context_takeover",
		"Sec-WebSocket-Extensions: permessage-deflate\r\n", Config),
	{_, "permessage-deflate; client_no_context_takeover"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers1),
	{ok, _, Headers2} = do_handshake("/ws_deflate_opts?client_no_context_takeover",
		"Sec-WebSocket-Extensions: permessage-deflate; client_no_context_takeover\r\n", Config),
	{_, "permessage-deflate; client_no_context_takeover"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers2),
	ok.

%% We must send client_max_window_bits to indicate we support it.
ws_deflate_opts_client_max_window_bits(Config) ->
	doc("Handler is configured with client max window bits."),
	{ok, _, Headers} = do_handshake("/ws_deflate_opts?client_max_window_bits",
		"Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits\r\n", Config),
	{_, "permessage-deflate; client_max_window_bits=9"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers),
	ok.

ws_deflate_opts_client_max_window_bits_override(Config) ->
	doc("Handler is configured with client max window bits."),
	{ok, _, Headers1} = do_handshake("/ws_deflate_opts?client_max_window_bits",
		"Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits=8\r\n", Config),
	{_, "permessage-deflate; client_max_window_bits=8"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers1),
	{ok, _, Headers2} = do_handshake("/ws_deflate_opts?client_max_window_bits",
		"Sec-WebSocket-Extensions: permessage-deflate; client_max_window_bits=12\r\n", Config),
	{_, "permessage-deflate; client_max_window_bits=9"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers2),
	ok.

ws_deflate_opts_server_context_takeover(Config) ->
	doc("Handler is configured with server context takeover enabled."),
	{ok, _, Headers1} = do_handshake("/ws_deflate_opts?server_context_takeover",
		"Sec-WebSocket-Extensions: permessage-deflate\r\n", Config),
	{_, "permessage-deflate"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers1),
	{ok, _, Headers2} = do_handshake("/ws_deflate_opts?server_context_takeover",
		"Sec-WebSocket-Extensions: permessage-deflate; server_no_context_takeover\r\n", Config),
	{_, "permessage-deflate; server_no_context_takeover"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers2),
	ok.

ws_deflate_opts_server_no_context_takeover(Config) ->
	doc("Handler is configured with server context takeover disabled."),
	{ok, _, Headers1} = do_handshake("/ws_deflate_opts?server_no_context_takeover",
		"Sec-WebSocket-Extensions: permessage-deflate\r\n", Config),
	{_, "permessage-deflate; server_no_context_takeover"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers1),
	{ok, _, Headers2} = do_handshake("/ws_deflate_opts?server_no_context_takeover",
		"Sec-WebSocket-Extensions: permessage-deflate; server_no_context_takeover\r\n", Config),
	{_, "permessage-deflate; server_no_context_takeover"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers2),
	ok.

ws_deflate_opts_server_max_window_bits(Config) ->
	doc("Handler is configured with server max window bits."),
	{ok, _, Headers} = do_handshake("/ws_deflate_opts?server_max_window_bits",
		"Sec-WebSocket-Extensions: permessage-deflate\r\n", Config),
	{_, "permessage-deflate; server_max_window_bits=9"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers),
	ok.

ws_deflate_opts_server_max_window_bits_override(Config) ->
	doc("Handler is configured with server max window bits."),
	{ok, _, Headers1} = do_handshake("/ws_deflate_opts?server_max_window_bits",
		"Sec-WebSocket-Extensions: permessage-deflate; server_max_window_bits=8\r\n", Config),
	{_, "permessage-deflate; server_max_window_bits=8"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers1),
	{ok, _, Headers2} = do_handshake("/ws_deflate_opts?server_max_window_bits",
		"Sec-WebSocket-Extensions: permessage-deflate; server_max_window_bits=12\r\n", Config),
	{_, "permessage-deflate; server_max_window_bits=9"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers2),
	ok.

ws_deflate_opts_zlevel(Config) ->
	doc("Handler is configured with zlib level."),
	do_ws_deflate_opts_z("/ws_deflate_opts?level", Config).

ws_deflate_opts_zmemlevel(Config) ->
	doc("Handler is configured with zlib mem_level."),
	do_ws_deflate_opts_z("/ws_deflate_opts?mem_level", Config).

ws_deflate_opts_zstrategy(Config) ->
	doc("Handler is configured with zlib strategy."),
	do_ws_deflate_opts_z("/ws_deflate_opts?strategy", Config).

do_ws_deflate_opts_z(Path, Config) ->
	{ok, Socket, Headers} = do_handshake(Path,
		"Sec-WebSocket-Extensions: permessage-deflate\r\n", Config),
	{_, "permessage-deflate"} = lists:keyfind("sec-websocket-extensions", 1, Headers),
	%% Send and receive a compressed "Hello" frame.
	Mask = 16#11223344,
	CompressedHello = << 242, 72, 205, 201, 201, 7, 0 >>,
	MaskedHello = do_mask(CompressedHello, Mask, <<>>),
	ok = gen_tcp:send(Socket, << 1:1, 1:1, 0:2, 1:4, 1:1, 7:7, Mask:32, MaskedHello/binary >>),
	{ok, << 1:1, 1:1, 0:2, 1:4, 0:1, 7:7, CompressedHello/binary >>} = gen_tcp:recv(Socket, 0, 6000),
	%% Client-initiated close.
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>),
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_init_return_ok(Config) ->
	doc("Handler does nothing."),
	{ok, Socket, _} = do_handshake("/ws_init?ok", Config),
	%% The handler does nothing; nothing should happen here.
	{error, timeout} = gen_tcp:recv(Socket, 0, 1000),
	ok.

ws_init_return_ok_hibernate(Config) ->
	doc("Handler does nothing; hibernates."),
	{ok, Socket, _} = do_handshake("/ws_init?ok_hibernate", Config),
	%% The handler does nothing; nothing should happen here.
	{error, timeout} = gen_tcp:recv(Socket, 0, 1000),
	ok.

ws_init_return_reply(Config) ->
	doc("Handler sends a text frame just after the handshake."),
	{ok, Socket, _} = do_handshake("/ws_init?reply", Config),
	{ok, << 1:1, 0:3, 1:4, 0:1, 5:7, "Hello" >>} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_init_return_reply_hibernate(Config) ->
	doc("Handler sends a text frame just after the handshake and then hibernates."),
	{ok, Socket, _} = do_handshake("/ws_init?reply_hibernate", Config),
	{ok, << 1:1, 0:3, 1:4, 0:1, 5:7, "Hello" >>} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_init_return_reply_close(Config) ->
	doc("Handler closes immediately after the handshake."),
	{ok, Socket, _} = do_handshake("/ws_init?reply_close", Config),
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_init_return_reply_close_hibernate(Config) ->
	doc("Handler closes immediately after the handshake, then attempts to hibernate."),
	{ok, Socket, _} = do_handshake("/ws_init?reply_close_hibernate", Config),
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_init_return_reply_many(Config) ->
	doc("Handler sends many frames just after the handshake."),
	{ok, Socket, _} = do_handshake("/ws_init?reply_many", Config),
	%% We catch all frames at once and check them directly.
	{ok, <<
		1:1, 0:3, 1:4, 0:1, 5:7, "Hello",
		1:1, 0:3, 2:4, 0:1, 5:7, "World" >>} = gen_tcp:recv(Socket, 14, 6000),
	ok.

ws_init_return_reply_many_hibernate(Config) ->
	doc("Handler sends many frames just after the handshake and then hibernates."),
	{ok, Socket, _} = do_handshake("/ws_init?reply_many_hibernate", Config),
	%% We catch all frames at once and check them directly.
	{ok, <<
		1:1, 0:3, 1:4, 0:1, 5:7, "Hello",
		1:1, 0:3, 2:4, 0:1, 5:7, "World" >>} = gen_tcp:recv(Socket, 14, 6000),
	ok.

ws_init_return_reply_many_close(Config) ->
	doc("Handler sends many frames including a close frame just after the handshake."),
	{ok, Socket, _} = do_handshake("/ws_init?reply_many_close", Config),
	%% We catch all frames at once and check them directly.
	{ok, <<
		1:1, 0:3, 1:4, 0:1, 5:7, "Hello",
		1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 9, 6000),
	ok.

ws_init_return_reply_many_close_hibernate(Config) ->
	doc("Handler sends many frames including a close frame just after the handshake and then hibernates."),
	{ok, Socket, _} = do_handshake("/ws_init?reply_many_close_hibernate", Config),
	%% We catch all frames at once and check them directly.
	{ok, <<
		1:1, 0:3, 1:4, 0:1, 5:7, "Hello",
		1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 9, 6000),
	ok.

ws_init_return_stop(Config) ->
	doc("Handler closes immediately after the handshake."),
	{ok, Socket, _} = do_handshake("/ws_init?stop", Config),
	{ok, << 1:1, 0:3, 8:4, 0:1, 2:7, 1000:16 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_init_shutdown_before_handshake(Config) ->
	doc("Handler stops before Websocket handshake."),
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_init_shutdown HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 13\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"Upgrade: websocket\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 403, _}, _Rest} = erlang:decode_packet(http, Handshake, []),
	ok.

ws_max_frame_size_close(Config) ->
	doc("Server closes connection when frame size exceeds max_frame_size option"),
	%% max_frame_size is set to 8 bytes in ws_max_frame_size.
	{ok, Socket, _} = do_handshake("/ws_max_frame_size", Config),
	Mask = 16#11223344,
	MaskedHello = do_mask(<<"HelloHello">>, Mask, <<>>),
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 2:4, 1:1, 10:7, Mask:32, MaskedHello/binary >>),
	{ok, << 1:1, 0:3, 8:4, 0:1, 2:7, 1009:16 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_max_frame_size_final_fragment_close(Config) ->
	doc("Server closes connection when final fragmented frame "
		"exceeds max_frame_size option"),
	%% max_frame_size is set to 8 bytes in ws_max_frame_size.
	{ok, Socket, _} = do_handshake("/ws_max_frame_size", Config),
	Mask = 16#11223344,
	MaskedHello = do_mask(<<"Hello">>, Mask, <<>>),
	ok = gen_tcp:send(Socket, << 0:1, 0:3, 2:4, 1:1, 5:7, Mask:32, MaskedHello/binary >>),
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 0:4, 1:1, 5:7, Mask:32, MaskedHello/binary >>),
	{ok, << 1:1, 0:3, 8:4, 0:1, 2:7, 1009:16 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_max_frame_size_intermediate_fragment_close(Config) ->
	doc("Server closes connection when intermediate fragmented frame "
		"exceeds max_frame_size option"),
	%% max_frame_size is set to 8 bytes in ws_max_frame_size.
	{ok, Socket, _} = do_handshake("/ws_max_frame_size", Config),
	Mask = 16#11223344,
	MaskedHello = do_mask(<<"Hello">>, Mask, <<>>),
	ok = gen_tcp:send(Socket, << 0:1, 0:3, 2:4, 1:1, 5:7, Mask:32, MaskedHello/binary >>),
	ok = gen_tcp:send(Socket, << 0:1, 0:3, 0:4, 1:1, 5:7, Mask:32, MaskedHello/binary >>),
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 0:4, 1:1, 5:7, Mask:32, MaskedHello/binary >>),
	{ok, << 1:1, 0:3, 8:4, 0:1, 2:7, 1009:16 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_send_close(Config) ->
	doc("Server-initiated close frame ends the connection."),
	{ok, Socket, _} = do_handshake("/ws_send_close", Config),
	%% We catch all frames at once and check them directly.
	{ok, <<
		1:1, 0:3, 1:4, 0:1, 4:7, "send",
		1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 8, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_send_close_payload(Config) ->
	doc("Server-initiated close frame with payload ends the connection."),
	{ok, Socket, _} = do_handshake("/ws_send_close_payload", Config),
	%% We catch all frames at once and check them directly.
	{ok, <<
		1:1, 0:3, 1:4, 0:1, 4:7, "send",
		1:1, 0:3, 8:4, 0:1, 12:7, 1001:16, "some text!" >>} = gen_tcp:recv(Socket, 20, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_send_many(Config) ->
	doc("Server sends many frames in a single reply."),
	{ok, Socket, _} = do_handshake("/ws_send_many", Config),
	%% We catch all frames at once and check them directly.
	{ok, <<
		1:1, 0:3, 1:4, 0:1, 3:7, "one",
		1:1, 0:3, 1:4, 0:1, 3:7, "two",
		1:1, 0:3, 1:4, 0:1, 6:7, "seven!" >>} = gen_tcp:recv(Socket, 18, 6000),
	ok.

ws_single_bytes(Config) ->
	doc("Client sends a text frame one byte at a time."),
	{ok, Socket, _} = do_handshake("/ws_echo", Config),
	%% We sleep between sends to make sure only one byte is sent.
	ok = gen_tcp:send(Socket, << 16#81 >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#85 >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#37 >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#fa >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#21 >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#3d >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#7f >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#9f >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#4d >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#51 >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#58 >>),
	{ok, << 1:1, 0:3, 1:4, 0:1, 5:7, "Hello" >>} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_subprotocol(Config) ->
	doc("Websocket sub-protocol negotiation."),
	{ok, _, Headers} = do_handshake("/ws_subprotocol",
		"Sec-WebSocket-Protocol: foo, bar\r\n", Config),
	{_, "foo"} = lists:keyfind("sec-websocket-protocol", 1, Headers),
	ok.

ws_terminate(Config) ->
	doc("The Req object is kept in a more compact form by default."),
	{ok, Socket, _} = do_handshake("/terminate",
		"x-test-pid: " ++ pid_to_list(self()) ++ "\r\n", Config),
	%% Send a close frame.
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>),
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	%% Confirm terminate/3 was called with a compacted Req.
	receive {terminate, _, Req} ->
		true = maps:is_key(path, Req),
		false = maps:is_key(headers, Req),
		ok
	after 1000 ->
		error(timeout)
	end.

ws_terminate_fun(Config) ->
	doc("A function can be given to filter the Req object."),
	{ok, Socket, _} = do_handshake("/terminate?req_filter",
		"x-test-pid: " ++ pid_to_list(self()) ++ "\r\n", Config),
	%% Send a close frame.
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>),
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	%% Confirm terminate/3 was called with a compacted Req.
	receive {terminate, _, Req} ->
		filtered = Req,
		ok
	after 1000 ->
		error(timeout)
	end.

ws_text_fragments(Config) ->
	doc("Client sends fragmented text frames."),
	{ok, Socket, _} = do_handshake("/ws_echo", Config),
	%% Send two "Hello" over two fragments and two sends.
	Mask = 16#37fa213d,
	MaskedHello = do_mask(<<"Hello">>, Mask, <<>>),
	ok = gen_tcp:send(Socket, << 0:1, 0:3, 1:4, 1:1, 5:7, Mask:32, MaskedHello/binary >>),
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 0:4, 1:1, 5:7, Mask:32, MaskedHello/binary >>),
	{ok, << 1:1, 0:3, 1:4, 0:1, 10:7, "HelloHello" >>} = gen_tcp:recv(Socket, 0, 6000),
	%% Send three "Hello" over three fragments and one send.
	ok = gen_tcp:send(Socket, [
		<< 0:1, 0:3, 1:4, 1:1, 5:7, Mask:32, MaskedHello/binary >>,
		<< 0:1, 0:3, 0:4, 1:1, 5:7, Mask:32, MaskedHello/binary >>,
		<< 1:1, 0:3, 0:4, 1:1, 5:7, Mask:32, MaskedHello/binary >>]),
	{ok, << 1:1, 0:3, 1:4, 0:1, 15:7, "HelloHelloHello" >>} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_timeout_hibernate(Config) ->
	doc("Server-initiated close on timeout with hibernating process."),
	{ok, Socket, _} = do_handshake("/ws_timeout_hibernate", Config),
	{ok, << 1:1, 0:3, 8:4, 0:1, 2:7, 1000:16 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_timeout_no_cancel(Config) ->
	doc("Server-initiated timeout is not influenced by reception of Erlang messages."),
	{ok, Socket, _} = do_handshake("/ws_timeout_cancel", Config),
	{ok, << 1:1, 0:3, 8:4, 0:1, 2:7, 1000:16 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_timeout_reset(Config) ->
	doc("Server-initiated timeout is reset when client sends more data."),
	{ok, Socket, _} = do_handshake("/ws_timeout_cancel", Config),
	%% Send and receive back a frame a few times.
	Mask = 16#37fa213d,
	MaskedHello = do_mask(<<"Hello">>, Mask, <<>>),
	[begin
		ok = gen_tcp:send(Socket, << 1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, MaskedHello/binary >>),
		{ok, << 1:1, 0:3, 1:4, 0:1, 5:7, "Hello" >>} = gen_tcp:recv(Socket, 0, 6000),
		timer:sleep(500)
	end || _ <- [1, 2, 3, 4]],
	%% Timeout will occur after we stop sending data.
	{ok, << 1:1, 0:3, 8:4, 0:1, 2:7, 1000:16 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_webkit_deflate(Config) ->
	doc("x-webkit-deflate-frame compression."),
	{ok, Socket, Headers} = do_handshake("/ws_echo",
		"Sec-WebSocket-Extensions: x-webkit-deflate-frame\r\n", Config),
	{_, "x-webkit-deflate-frame"} = lists:keyfind("sec-websocket-extensions", 1, Headers),
	%% Send and receive a compressed "Hello" frame.
	Mask = 16#11223344,
	CompressedHello = << 242, 72, 205, 201, 201, 7, 0 >>,
	MaskedHello = do_mask(CompressedHello, Mask, <<>>),
	ok = gen_tcp:send(Socket, << 1:1, 1:1, 0:2, 1:4, 1:1, 7:7, Mask:32, MaskedHello/binary >>),
	{ok, << 1:1, 1:1, 0:2, 1:4, 0:1, 7:7, CompressedHello/binary >>} = gen_tcp:recv(Socket, 0, 6000),
	%% Client-initiated close.
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>),
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_webkit_deflate_fragments(Config) ->
	doc("Client sends an x-webkit-deflate-frame compressed and fragmented text frame."),
	{ok, Socket, Headers} = do_handshake("/ws_echo",
		"Sec-WebSocket-Extensions: x-webkit-deflate-frame\r\n", Config),
	{_, "x-webkit-deflate-frame"} = lists:keyfind("sec-websocket-extensions", 1, Headers),
	%% Send a compressed "Hello" over two fragments and two sends.
	Mask = 16#11223344,
	CompressedHello = << 242, 72, 205, 201, 201, 7, 0 >>,
	MaskedHello1 = do_mask(binary:part(CompressedHello, 0, 4), Mask, <<>>),
	MaskedHello2 = do_mask(binary:part(CompressedHello, 4, 3), Mask, <<>>),
	ok = gen_tcp:send(Socket, << 0:1, 1:1, 0:2, 1:4, 1:1, 4:7, Mask:32, MaskedHello1/binary >>),
	ok = gen_tcp:send(Socket, << 1:1, 1:1, 0:2, 0:4, 1:1, 3:7, Mask:32, MaskedHello2/binary >>),
	{ok, << 1:1, 1:1, 0:2, 1:4, 0:1, 7:7, CompressedHello/binary >>} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_webkit_deflate_single_bytes(Config) ->
	doc("Client sends an x-webkit-deflate-frame compressed text frame one byte at a time."),
	{ok, Socket, Headers} = do_handshake("/ws_echo",
		"Sec-WebSocket-Extensions: x-webkit-deflate-frame\r\n", Config),
	{_, "x-webkit-deflate-frame"} = lists:keyfind("sec-websocket-extensions", 1, Headers),
	%% We sleep between sends to make sure only one byte is sent.
	Mask = 16#11223344,
	CompressedHello = << 242, 72, 205, 201, 201, 7, 0 >>,
	MaskedHello = do_mask(CompressedHello, Mask, <<>>),
	ok = gen_tcp:send(Socket, << 16#c1 >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#87 >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#11 >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#22 >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#33 >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#44 >>), timer:sleep(100),
	ok = gen_tcp:send(Socket, [binary:at(MaskedHello, 0)]), timer:sleep(100),
	ok = gen_tcp:send(Socket, [binary:at(MaskedHello, 1)]), timer:sleep(100),
	ok = gen_tcp:send(Socket, [binary:at(MaskedHello, 2)]), timer:sleep(100),
	ok = gen_tcp:send(Socket, [binary:at(MaskedHello, 3)]), timer:sleep(100),
	ok = gen_tcp:send(Socket, [binary:at(MaskedHello, 4)]), timer:sleep(100),
	ok = gen_tcp:send(Socket, [binary:at(MaskedHello, 5)]), timer:sleep(100),
	ok = gen_tcp:send(Socket, [binary:at(MaskedHello, 6)]),
	{ok, << 1:1, 1:1, 0:2, 1:4, 0:1, 7:7, CompressedHello/binary >>} = gen_tcp:recv(Socket, 0, 6000),
	ok.

%% Internal.

do_handshake(Path, Config) ->
	do_handshake(Path, "", Config).

do_handshake(Path, ExtraHeaders, Config) ->
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config),
		[binary, {active, false}]),
	ok = gen_tcp:send(Socket, [
		"GET ", Path, " HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 13\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"Upgrade: websocket\r\n",
		ExtraHeaders,
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, _}, Rest} = erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = do_decode_headers(erlang:decode_packet(httph, Rest, []), []),
	{_, "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{_, "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{_, "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="} = lists:keyfind("sec-websocket-accept", 1, Headers),
	{ok, Socket, Headers}.

do_decode_headers({ok, http_eoh, Rest}, Acc) ->
	[Acc, Rest];
do_decode_headers({ok, {http_header, _I, Key, _R, Value}, Rest}, Acc) ->
	F = fun(S) when is_atom(S) -> S; (S) -> string:to_lower(S) end,
	do_decode_headers(erlang:decode_packet(httph, Rest, []), [{F(Key), Value}|Acc]).

do_mask(<<>>, _, Acc) ->
	Acc;
do_mask(<< O:32, Rest/bits >>, MaskKey, Acc) ->
	T = O bxor MaskKey,
	do_mask(Rest, MaskKey, << Acc/binary, T:32 >>);
do_mask(<< O:24 >>, MaskKey, Acc) ->
	<< MaskKey2:24, _:8 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:24 >>;
do_mask(<< O:16 >>, MaskKey, Acc) ->
	<< MaskKey2:16, _:16 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:16 >>;
do_mask(<< O:8 >>, MaskKey, Acc) ->
	<< MaskKey2:8, _:24 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:8 >>.
