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

%% RFC 6455 server conformance.

-module(rfc6455_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).

suite() ->
	[{timetrap, 120000}].

all() ->
	[{group, ws}].

groups() ->
	[{ws, [parallel], ct_helper:all(?MODULE)}].

init_per_group(ws, Config) ->
	cowboy_test:init_http(rfc6455, #{
		env => #{dispatch => cowboy_router:compile(init_routes())}
	}, Config).

end_per_group(ws, _Config) ->
	ok = cowboy:stop_listener(rfc6455).

init_routes() ->
	[{"localhost", [
		{"/ws_echo", ws_echo, []}
	]}].

text_payload_0(Config) ->
	doc("Empty text frame is echoed. (RFC6455 5.6)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 0:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 0:7>>} = do_recv(Client, 2, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_payload_125(Config) ->
	doc("Text frame of 125 bytes uses the 7-bit length. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<"*">>, 125),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 125:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 125:7, Payload:125/binary>>} = do_recv(Client, 127, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_payload_126(Config) ->
	doc("Text frame of 126 bytes uses the 16-bit length. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<"*">>, 126),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 126:7, 126:16, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 126:7, 126:16, Payload:126/binary>>}
		= do_recv(Client, 130, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_payload_127(Config) ->
	doc("Text frame of 127 bytes uses the 16-bit length. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<"*">>, 127),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 126:7, 127:16, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 126:7, 127:16, Payload:127/binary>>}
		= do_recv(Client, 131, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_payload_128(Config) ->
	doc("Text frame of 128 bytes uses the 16-bit length. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<"*">>, 128),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 126:7, 128:16, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 126:7, 128:16, Payload:128/binary>>}
		= do_recv(Client, 132, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_payload_65535(Config) ->
	doc("Text frame of 65535 bytes uses the 16-bit length. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<"*">>, 65535),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 126:7, 65535:16, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 126:7, 65535:16, Payload:65535/binary>>}
		= do_recv(Client, 65539, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_payload_65536(Config) ->
	doc("Text frame of 65536 bytes uses the 64-bit length. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<"*">>, 65536),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client,
		<<1:1, 0:3, 1:4, 1:1, 127:7, 0:1, 65536:63, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 127:7, 0:1, 65536:63, Payload:65536/binary>>}
		= do_recv(Client, 65546, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

binary_payload_0(Config) ->
	doc("Empty binary frame is echoed. (RFC6455 5.6)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 2:4, 1:1, 0:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 2:4, 0:1, 0:7>>} = do_recv(Client, 2, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

binary_payload_125(Config) ->
	doc("Binary frame of 125 bytes uses the 7-bit length. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<16#fe>>, 125),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 2:4, 1:1, 125:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 2:4, 0:1, 125:7, Payload:125/binary>>} = do_recv(Client, 127, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

binary_payload_126(Config) ->
	doc("Binary frame of 126 bytes uses the 16-bit length. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<16#fe>>, 126),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 2:4, 1:1, 126:7, 126:16, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 2:4, 0:1, 126:7, 126:16, Payload:126/binary>>}
		= do_recv(Client, 130, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

binary_payload_127(Config) ->
	doc("Binary frame of 127 bytes uses the 16-bit length. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<16#fe>>, 127),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 2:4, 1:1, 126:7, 127:16, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 2:4, 0:1, 126:7, 127:16, Payload:127/binary>>}
		= do_recv(Client, 131, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

binary_payload_128(Config) ->
	doc("Binary frame of 128 bytes uses the 16-bit length. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<16#fe>>, 128),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 2:4, 1:1, 126:7, 128:16, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 2:4, 0:1, 126:7, 128:16, Payload:128/binary>>}
		= do_recv(Client, 132, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

binary_payload_65535(Config) ->
	doc("Binary frame of 65535 bytes uses the 16-bit length. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<16#fe>>, 65535),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 2:4, 1:1, 126:7, 65535:16, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 2:4, 0:1, 126:7, 65535:16, Payload:65535/binary>>}
		= do_recv(Client, 65539, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

binary_payload_65536(Config) ->
	doc("Binary frame of 65536 bytes uses the 64-bit length. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<16#fe>>, 65536),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client,
		<<1:1, 0:3, 2:4, 1:1, 127:7, 0:1, 65536:63, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 2:4, 0:1, 127:7, 0:1, 65536:63, Payload:65536/binary>>}
		= do_recv(Client, 65546, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_payload_65536_chop(Config) ->
	doc("Text frame of 65536 bytes is accepted when delivered in 997-byte TCP chops. "
		"(RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<"*">>, 65536),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send_chop(Client,
		<<1:1, 0:3, 1:4, 1:1, 127:7, 0:1, 65536:63, Mask:32, Masked/binary>>, 997),
	{ok, <<1:1, 0:3, 1:4, 0:1, 127:7, 0:1, 65536:63, Payload:65536/binary>>}
		= do_recv(Client, 65546, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

binary_payload_65536_chop(Config) ->
	doc("Binary frame of 65536 bytes is accepted when delivered in 997-byte TCP chops. "
		"(RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<16#fe>>, 65536),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send_chop(Client,
		<<1:1, 0:3, 2:4, 1:1, 127:7, 0:1, 65536:63, Mask:32, Masked/binary>>, 997),
	{ok, <<1:1, 0:3, 2:4, 0:1, 127:7, 0:1, 65536:63, Payload:65536/binary>>}
		= do_recv(Client, 65546, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

ping_empty(Config) ->
	doc("Ping with an empty payload is answered by an empty pong. "
		"(RFC6455 5.5.2, RFC6455 5.5.3)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 9:4, 1:1, 0:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 10:4, 0:1, 0:7>>} = do_recv(Client, 2, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

ping_text(Config) ->
	doc("Ping payload is copied into the pong. (RFC6455 5.5.3)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"Hello, world!">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 9:4, 1:1, 13:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 10:4, 0:1, 13:7, Payload:13/binary>>} = do_recv(Client, 15, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

ping_binary(Config) ->
	doc("Ping with a non-UTF-8 payload is answered by a pong carrying the same bytes. "
		"(RFC6455 5.5.3)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<0, 16#ff, 16#fe, 16#fd, 16#fc, 16#fb, 0, 16#ff>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 9:4, 1:1, 8:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 10:4, 0:1, 8:7, Payload:8/binary>>} = do_recv(Client, 10, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

ping_payload_125(Config) ->
	doc("Ping of 125 bytes, the maximum control-frame payload, is answered by a pong. "
		"(RFC6455 5.5)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<16#fe>>, 125),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 9:4, 1:1, 125:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 10:4, 0:1, 125:7, Payload:125/binary>>} = do_recv(Client, 127, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

ping_payload_126(Config) ->
	doc("Ping of 126 bytes is rejected. Control frames must be at most 125 bytes. "
		"(RFC6455 5.5)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<16#fe>>, 126),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 9:4, 1:1, 126:7, 126:16, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

ping_payload_125_chop(Config) ->
	doc("Ping of 125 bytes is answered when the frame arrives one byte at a time. "
		"(RFC6455 5.5)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<16#fe>>, 125),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send_chop(Client, <<1:1, 0:3, 9:4, 1:1, 125:7, Mask:32, Masked/binary>>, 1),
	{ok, <<1:1, 0:3, 10:4, 0:1, 125:7, Payload:125/binary>>} = do_recv(Client, 127, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

pong_unsolicited_empty(Config) ->
	doc("An unsolicited pong with an empty payload receives no reply. (RFC6455 5.5.3)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	Masked = do_mask(<<>>, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 10:4, 1:1, 0:7, Mask:32, Masked/binary>>),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

pong_unsolicited(Config) ->
	doc("An unsolicited pong with a payload receives no reply. (RFC6455 5.5.3)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"unsolicited pong payload">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 10:4, 1:1, 24:7, Mask:32, Masked/binary>>),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

pong_then_ping(Config) ->
	doc("An unsolicited pong is ignored, and a following ping is answered. "
		"(RFC6455 5.5.2, RFC6455 5.5.3)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	PongPayload = <<"unsolicited pong payload">>,
	PingPayload = <<"ping payload">>,
	Mask = 16#01020304,
	PongMasked = do_mask(PongPayload, Mask, <<>>),
	PingMasked = do_mask(PingPayload, Mask, <<>>),
	ok = do_send(Client, [
		<<1:1, 0:3, 10:4, 1:1, 24:7, Mask:32, PongMasked/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 12:7, Mask:32, PingMasked/binary>>
	]),
	{ok, <<1:1, 0:3, 10:4, 0:1, 12:7, PingPayload:12/binary>>}
		= do_recv(Client, 14, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

ping_ten(Config) ->
	doc("Ten pings are each answered with a pong carrying the same payload. "
		"(RFC6455 5.5.2, RFC6455 5.5.3)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	P0 = <<"payload-0">>,
	M0 = do_mask(P0, Mask, <<>>),
	P1 = <<"payload-1">>,
	M1 = do_mask(P1, Mask, <<>>),
	P2 = <<"payload-2">>,
	M2 = do_mask(P2, Mask, <<>>),
	P3 = <<"payload-3">>,
	M3 = do_mask(P3, Mask, <<>>),
	P4 = <<"payload-4">>,
	M4 = do_mask(P4, Mask, <<>>),
	P5 = <<"payload-5">>,
	M5 = do_mask(P5, Mask, <<>>),
	P6 = <<"payload-6">>,
	M6 = do_mask(P6, Mask, <<>>),
	P7 = <<"payload-7">>,
	M7 = do_mask(P7, Mask, <<>>),
	P8 = <<"payload-8">>,
	M8 = do_mask(P8, Mask, <<>>),
	P9 = <<"payload-9">>,
	M9 = do_mask(P9, Mask, <<>>),
	ok = do_send(Client, [
		<<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M0/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M1/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M2/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M3/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M4/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M5/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M6/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M7/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M8/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M9/binary>>
	]),
	{ok, <<
		1:1, 0:3, 10:4, 0:1, 9:7, P0:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P1:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P2:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P3:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P4:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P5:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P6:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P7:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P8:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P9:9/binary
	>>} = do_recv(Client, 110, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

ping_ten_chop(Config) ->
	doc("Ten pings are each answered when every frame arrives one byte at a time. "
		"(RFC6455 5.5.2, RFC6455 5.5.3)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	P0 = <<"payload-0">>,
	M0 = do_mask(P0, Mask, <<>>),
	P1 = <<"payload-1">>,
	M1 = do_mask(P1, Mask, <<>>),
	P2 = <<"payload-2">>,
	M2 = do_mask(P2, Mask, <<>>),
	P3 = <<"payload-3">>,
	M3 = do_mask(P3, Mask, <<>>),
	P4 = <<"payload-4">>,
	M4 = do_mask(P4, Mask, <<>>),
	P5 = <<"payload-5">>,
	M5 = do_mask(P5, Mask, <<>>),
	P6 = <<"payload-6">>,
	M6 = do_mask(P6, Mask, <<>>),
	P7 = <<"payload-7">>,
	M7 = do_mask(P7, Mask, <<>>),
	P8 = <<"payload-8">>,
	M8 = do_mask(P8, Mask, <<>>),
	P9 = <<"payload-9">>,
	M9 = do_mask(P9, Mask, <<>>),
	ok = do_send_chop(Client, <<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M0/binary>>, 1),
	ok = do_send_chop(Client, <<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M1/binary>>, 1),
	ok = do_send_chop(Client, <<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M2/binary>>, 1),
	ok = do_send_chop(Client, <<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M3/binary>>, 1),
	ok = do_send_chop(Client, <<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M4/binary>>, 1),
	ok = do_send_chop(Client, <<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M5/binary>>, 1),
	ok = do_send_chop(Client, <<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M6/binary>>, 1),
	ok = do_send_chop(Client, <<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M7/binary>>, 1),
	ok = do_send_chop(Client, <<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M8/binary>>, 1),
	ok = do_send_chop(Client, <<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M9/binary>>, 1),
	{ok, <<
		1:1, 0:3, 10:4, 0:1, 9:7, P0:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P1:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P2:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P3:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P4:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P5:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P6:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P7:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P8:9/binary,
		1:1, 0:3, 10:4, 0:1, 9:7, P9:9/binary
	>>} = do_recv(Client, 110, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_rsv_1(Config) ->
	doc("Text frame with RSV3 set, and no extension negotiated, fails the connection. "
		"(RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"Hello, world!">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 1:3, 1:4, 1:1, 13:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

text_then_rsv_2(Config) ->
	doc("A text frame is echoed, then a text frame with RSV2 set fails the connection "
		"before the following ping is answered. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"Hello, world!">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	Bad = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, [
		<<1:1, 0:3, 1:4, 1:1, 13:7, Mask:32, Masked/binary>>,
		<<1:1, 2:3, 1:4, 1:1, 13:7, Mask:32, Bad/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 0:7, Mask:32>>
	]),
	{closed, <<
		1:1, 0:3, 1:4, 0:1, 13:7, Payload:13/binary,
		1:1, 0:3, 8:4, 0:1, 2:7, 1002:16
	>>} = do_recv_until_closed(Client),
	ok.

text_then_rsv_3(Config) ->
	doc("A text frame is echoed, then a text frame with RSV2 and RSV3 set fails the "
		"connection. Each frame is a separate TCP send. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"Hello, world!">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	Bad = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 13:7, Mask:32, Masked/binary>>),
	ok = do_send(Client, <<1:1, 3:3, 1:4, 1:1, 13:7, Mask:32, Bad/binary>>),
	{closed, <<
		1:1, 0:3, 1:4, 0:1, 13:7, Payload:13/binary,
		1:1, 0:3, 8:4, 0:1, 2:7, 1002:16
	>>} = do_recv_until_closed(Client),
	ok.

text_then_rsv_4_chop(Config) ->
	doc("The first byte of a text frame with RSV1 set fails the connection. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	ok = do_send(Client, <<1:1, 4:3, 1:4>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

binary_rsv_5(Config) ->
	doc("Binary frame with RSV1 and RSV3 set, and no extension negotiated, fails the "
		"connection. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<0, 16#ff, 16#fe, 16#fd, 16#fc, 16#fb, 0, 16#ff>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 5:3, 2:4, 1:1, 8:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

binary_rsv_6(Config) ->
	doc("Binary frame with RSV1 and RSV2 set, and no extension negotiated, fails the "
		"connection. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"Hello, world!">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 6:3, 2:4, 1:1, 13:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

close_rsv_7(Config) ->
	doc("Close frame with RSV1, RSV2 and RSV3 set fails the connection. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	ok = do_send(Client, <<1:1, 7:3, 8:4, 1:1, 0:7, Mask:32>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

opcode_3(Config) ->
	doc("Reserved non-control opcode 3 fails the connection. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	Masked = do_mask(<<>>, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 3:4, 1:1, 0:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

opcode_4(Config) ->
	doc("Reserved non-control opcode 4 with a payload fails the connection. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"reserved opcode payload">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 4:4, 1:1, 24:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

opcode_11(Config) ->
	doc("Reserved control opcode 11 fails the connection. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	Masked = do_mask(<<>>, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 11:4, 1:1, 0:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

opcode_12(Config) ->
	doc("Reserved control opcode 12 with a payload fails the connection. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"reserved opcode payload">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 12:4, 1:1, 24:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

text_then_opcode_5(Config) ->
	doc("A text frame is echoed, then reserved opcode 5 fails the connection before the "
		"following ping is answered. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"Hello, world!">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, [
		<<1:1, 0:3, 1:4, 1:1, 13:7, Mask:32, Masked/binary>>,
		<<1:1, 0:3, 5:4, 1:1, 0:7, Mask:32>>,
		<<1:1, 0:3, 9:4, 1:1, 0:7, Mask:32>>]),
	{closed, <<
		1:1, 0:3, 1:4, 0:1, 13:7, Payload:13/binary,
		1:1, 0:3, 8:4, 0:1, 2:7, 1002:16
	>>} = do_recv_until_closed(Client),
	ok.

text_then_opcode_6(Config) ->
	doc("A text frame is echoed, then reserved opcode 6 with a payload fails the "
		"connection. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"Hello, world!">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	BadMasked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, [
		<<1:1, 0:3, 1:4, 1:1, 13:7, Mask:32, Masked/binary>>,
		<<1:1, 0:3, 6:4, 1:1, 13:7, Mask:32, BadMasked/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 0:7, Mask:32>>]),
	{closed, <<
		1:1, 0:3, 1:4, 0:1, 13:7, Payload:13/binary,
		1:1, 0:3, 8:4, 0:1, 2:7, 1002:16
	>>} = do_recv_until_closed(Client),
	ok.

text_then_opcode_7_chop(Config) ->
	doc("The first byte of reserved opcode 7 fails the connection. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	ok = do_send(Client, <<1:1, 0:3, 7:4>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

text_then_opcode_13(Config) ->
	doc("A text frame is echoed, then reserved opcode 13 fails the connection before the "
		"following ping is answered. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"Hello, world!">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, [
		<<1:1, 0:3, 1:4, 1:1, 13:7, Mask:32, Masked/binary>>,
		<<1:1, 0:3, 13:4, 1:1, 0:7, Mask:32>>,
		<<1:1, 0:3, 9:4, 1:1, 0:7, Mask:32>>]),
	{closed, <<
		1:1, 0:3, 1:4, 0:1, 13:7, Payload:13/binary,
		1:1, 0:3, 8:4, 0:1, 2:7, 1002:16
	>>} = do_recv_until_closed(Client),
	ok.

text_then_opcode_14(Config) ->
	doc("A text frame is echoed, then reserved opcode 14 with a payload fails the "
		"connection. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"Hello, world!">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	BadMasked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, [
		<<1:1, 0:3, 1:4, 1:1, 13:7, Mask:32, Masked/binary>>,
		<<1:1, 0:3, 14:4, 1:1, 13:7, Mask:32, BadMasked/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 0:7, Mask:32>>]),
	{closed, <<
		1:1, 0:3, 1:4, 0:1, 13:7, Payload:13/binary,
		1:1, 0:3, 8:4, 0:1, 2:7, 1002:16
	>>} = do_recv_until_closed(Client),
	ok.

text_then_opcode_15_chop(Config) ->
	doc("The first byte of reserved opcode 15 fails the connection. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	ok = do_send(Client, <<1:1, 0:3, 15:4>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

text_two_fragments(Config) ->
	doc("A text message split into two fragments is echoed as one message. (RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	A = <<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M1/binary>>,
	B = <<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M2/binary>>,
	ok = do_send(Client, [A, B]),
	{ok, <<1:1, 0:3, 1:4, 0:1, 18:7, "fragment1fragment2">>} = do_recv(Client, 20, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

binary_two_fragments(Config) ->
	doc("A binary message split into two fragments is echoed as one message. (RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<0, 1, 2>>,
	F2 = <<3, 4, 5>>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	A = <<0:1, 0:3, 2:4, 1:1, 3:7, Mask:32, M1/binary>>,
	B = <<1:1, 0:3, 0:4, 1:1, 3:7, Mask:32, M2/binary>>,
	ok = do_send(Client, [A, B]),
	{ok, <<1:1, 0:3, 2:4, 0:1, 6:7, 0, 1, 2, 3, 4, 5>>} = do_recv(Client, 8, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_two_fragments_separate(Config) ->
	doc("A text message split into two fragments is echoed when each fragment is a "
		"separate TCP send. (RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	A = <<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M1/binary>>,
	B = <<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M2/binary>>,
	ok = do_send(Client, A),
	ok = do_send(Client, B),
	{ok, <<1:1, 0:3, 1:4, 0:1, 18:7, "fragment1fragment2">>} = do_recv(Client, 20, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_two_fragments_chop(Config) ->
	doc("A text message split into two fragments is echoed when each frame arrives one "
		"byte at a time. (RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	A = <<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M1/binary>>,
	B = <<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M2/binary>>,
	ok = do_send_chop(Client, A, 1),
	ok = do_send_chop(Client, B, 1),
	{ok, <<1:1, 0:3, 1:4, 0:1, 18:7, "fragment1fragment2">>} = do_recv(Client, 20, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_fragment_ping(Config) ->
	doc("A ping between the fragments of a text message is answered, then the message is "
		"echoed. (RFC6455 5.4, RFC6455 5.5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	Ping = <<"ping payload">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	MP = do_mask(Ping, Mask, <<>>),
	A = <<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M1/binary>>,
	P = <<1:1, 0:3, 9:4, 1:1, 12:7, Mask:32, MP/binary>>,
	B = <<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M2/binary>>,
	ok = do_send(Client, [A, P, B]),
	{ok, <<
		1:1, 0:3, 10:4, 0:1, 12:7, Ping:12/binary,
		1:1, 0:3, 1:4, 0:1, 18:7, "fragment1fragment2"
	>>} = do_recv(Client, 34, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_fragment_ping_separate(Config) ->
	doc("A ping between text fragments is answered when each frame is a separate TCP send. "
		"(RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	Ping = <<"ping payload">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	MP = do_mask(Ping, Mask, <<>>),
	A = <<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M1/binary>>,
	P = <<1:1, 0:3, 9:4, 1:1, 12:7, Mask:32, MP/binary>>,
	B = <<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M2/binary>>,
	ok = do_send(Client, A),
	ok = do_send(Client, P),
	ok = do_send(Client, B),
	{ok, <<
		1:1, 0:3, 10:4, 0:1, 12:7, Ping:12/binary,
		1:1, 0:3, 1:4, 0:1, 18:7, "fragment1fragment2"
	>>} = do_recv(Client, 34, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_fragment_ping_chop(Config) ->
	doc("A ping between text fragments is answered when each frame arrives one byte at a "
		"time. (RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	Ping = <<"ping payload">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	MP = do_mask(Ping, Mask, <<>>),
	A = <<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M1/binary>>,
	P = <<1:1, 0:3, 9:4, 1:1, 12:7, Mask:32, MP/binary>>,
	B = <<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M2/binary>>,
	ok = do_send_chop(Client, A, 1),
	ok = do_send_chop(Client, P, 1),
	ok = do_send_chop(Client, B, 1),
	{ok, <<
		1:1, 0:3, 10:4, 0:1, 12:7, Ping:12/binary,
		1:1, 0:3, 1:4, 0:1, 18:7, "fragment1fragment2"
	>>} = do_recv(Client, 34, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

continuation_without_message(Config) ->
	doc("A continuation frame with nothing to continue fails the connection. "
		"(RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"non-continuation payload">>,
	Hello = <<"Hello, world!">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	HelloMasked = do_mask(Hello, Mask, <<>>),
	A = <<1:1, 0:3, 0:4, 1:1, 24:7, Mask:32, Masked/binary>>,
	B = <<1:1, 0:3, 1:4, 1:1, 13:7, Mask:32, HelloMasked/binary>>,
	ok = do_send(Client, [A, B]),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

continuation_without_message_complete(Config) ->
	doc("A complete continuation frame with nothing to continue fails the "
		"connection. (RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"non-continuation payload">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 0:4, 1:1, 24:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

continuation_without_message_chop(Config) ->
	doc("The first byte of a continuation frame with nothing to continue fails the "
		"connection. (RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	ok = do_send(Client, <<1:1, 0:3, 0:4>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

continuation_nofin_without_message(Config) ->
	doc("A non-final continuation frame with nothing to continue fails the connection. "
		"(RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"non-continuation payload">>,
	Hello = <<"Hello, world!">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	HelloMasked = do_mask(Hello, Mask, <<>>),
	A = <<0:1, 0:3, 0:4, 1:1, 24:7, Mask:32, Masked/binary>>,
	B = <<1:1, 0:3, 1:4, 1:1, 13:7, Mask:32, HelloMasked/binary>>,
	ok = do_send(Client, [A, B]),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

continuation_nofin_without_message_complete(Config) ->
	doc("A complete non-final continuation frame with nothing to continue fails "
		"the connection. (RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"non-continuation payload">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<0:1, 0:3, 0:4, 1:1, 24:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

continuation_nofin_without_message_chop(Config) ->
	doc("The first byte of a non-final continuation frame with nothing to continue fails "
		"the connection. (RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	ok = do_send(Client, <<0:1, 0:3, 0:4>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

text_fragment_then_continuation(Config) ->
	doc("A finished fragmented text message is echoed, then a further continuation fails "
		"the connection. (RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	F3 = <<"fragment3">>,
	F4 = <<"fragment4">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	M3 = do_mask(F3, Mask, <<>>),
	M4 = do_mask(F4, Mask, <<>>),
	ok = do_send(Client, [
		<<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M1/binary>>,
		<<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M2/binary>>,
		<<0:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M3/binary>>,
		<<1:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M4/binary>>
	]),
	{closed, <<
		1:1, 0:3, 1:4, 0:1, 18:7, "fragment1fragment2",
		1:1, 0:3, 8:4, 0:1, 2:7, 1002:16
	>>} = do_recv_until_closed(Client),
	ok.

continuation_nofin_repeated(Config) ->
	doc("A non-final continuation with nothing to continue fails the connection. The "
		"sequence is sent twice. (RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	F3 = <<"fragment3">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	M3 = do_mask(F3, Mask, <<>>),
	ok = do_send(Client, [
		<<0:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M1/binary>>,
		<<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M2/binary>>,
		<<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M3/binary>>,
		<<0:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M1/binary>>,
		<<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M2/binary>>,
		<<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M3/binary>>
	]),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

continuation_fin_repeated(Config) ->
	doc("A final continuation with nothing to continue fails the connection. The sequence "
		"is sent twice. (RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	F3 = <<"fragment3">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	M3 = do_mask(F3, Mask, <<>>),
	ok = do_send(Client, [
		<<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M1/binary>>,
		<<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M2/binary>>,
		<<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M3/binary>>,
		<<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M1/binary>>,
		<<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M2/binary>>,
		<<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M3/binary>>
	]),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

text_fragment_opcode_text(Config) ->
	doc("A second fragment that uses the text opcode instead of continuation fails the "
		"connection. (RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	ok = do_send(Client, [
		<<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M1/binary>>,
		<<1:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M2/binary>>
	]),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

text_fragments_two_pings(Config) ->
	doc("Pings sent between the fragments of one text message are answered before the "
		"message is complete. (RFC6455 5.4, RFC6455 5.5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	F3 = <<"fragment3">>,
	F4 = <<"fragment4">>,
	F5 = <<"fragment5">>,
	P1 = <<"pongme 1!">>,
	P2 = <<"pongme 2!">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	M3 = do_mask(F3, Mask, <<>>),
	M4 = do_mask(F4, Mask, <<>>),
	M5 = do_mask(F5, Mask, <<>>),
	MP1 = do_mask(P1, Mask, <<>>),
	MP2 = do_mask(P2, Mask, <<>>),
	A1 = <<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M1/binary>>,
	A2 = <<0:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M2/binary>>,
	A3 = <<0:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M3/binary>>,
	A4 = <<0:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M4/binary>>,
	A5 = <<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M5/binary>>,
	G1 = <<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, MP1/binary>>,
	G2 = <<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, MP2/binary>>,
	ok = do_send(Client, [A1, A2, G1]),
	{ok, <<1:1, 0:3, 10:4, 0:1, 9:7, "pongme 1!">>} = do_recv(Client, 11, 10000),
	ok = do_send(Client, [A3, A4, G2, A5]),
	{ok, <<
		1:1, 0:3, 10:4, 0:1, 9:7, "pongme 2!",
		1:1, 0:3, 1:4, 0:1, 45:7, "fragment1fragment2fragment3fragment4fragment5"
	>>} = do_recv(Client, 58, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_fragments_two_pings_separate(Config) ->
	doc("Pings between text fragments are answered when each frame is a separate TCP send. "
		"(RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	F3 = <<"fragment3">>,
	F4 = <<"fragment4">>,
	F5 = <<"fragment5">>,
	P1 = <<"pongme 1!">>,
	P2 = <<"pongme 2!">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	M3 = do_mask(F3, Mask, <<>>),
	M4 = do_mask(F4, Mask, <<>>),
	M5 = do_mask(F5, Mask, <<>>),
	MP1 = do_mask(P1, Mask, <<>>),
	MP2 = do_mask(P2, Mask, <<>>),
	A1 = <<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M1/binary>>,
	A2 = <<0:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M2/binary>>,
	A3 = <<0:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M3/binary>>,
	A4 = <<0:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M4/binary>>,
	A5 = <<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M5/binary>>,
	G1 = <<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, MP1/binary>>,
	G2 = <<1:1, 0:3, 9:4, 1:1, 9:7, Mask:32, MP2/binary>>,
	ok = do_send(Client, A1),
	ok = do_send(Client, A2),
	ok = do_send(Client, G1),
	{ok, <<1:1, 0:3, 10:4, 0:1, 9:7, "pongme 1!">>} = do_recv(Client, 11, 10000),
	ok = do_send(Client, A3),
	ok = do_send(Client, A4),
	ok = do_send(Client, G2),
	ok = do_send(Client, A5),
	{ok, <<
		1:1, 0:3, 10:4, 0:1, 9:7, "pongme 2!",
		1:1, 0:3, 1:4, 0:1, 45:7, "fragment1fragment2fragment3fragment4fragment5"
	>>} = do_recv(Client, 58, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

ping_fragmented(Config) ->
	doc("A fragmented ping fails the connection. Control frames must not be fragmented. "
		"(RFC6455 5.4, RFC6455 5.5)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	ok = do_send(Client, [
		<<0:1, 0:3, 9:4, 1:1, 9:7, Mask:32, M1/binary>>,
		<<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M2/binary>>
	]),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

pong_fragmented(Config) ->
	doc("A fragmented pong fails the connection. Control frames must not be fragmented. "
		"(RFC6455 5.4, RFC6455 5.5)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	ok = do_send(Client, [
		<<0:1, 0:3, 10:4, 1:1, 9:7, Mask:32, M1/binary>>,
		<<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M2/binary>>
	]),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

utf8_text_empty(Config) ->
	doc("An empty text message is echoed. (RFC6455 5.6, RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	Masked = do_mask(<<>>, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 0:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 0:7>>} = do_recv(Client, 2, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_empty_fragments(Config) ->
	doc("Three empty text fragments are echoed as one empty text message. "
		"(RFC6455 5.4, RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	Masked = do_mask(<<>>, Mask, <<>>),
	ok = do_send(Client, [
		<<0:1, 0:3, 1:4, 1:1, 0:7, Mask:32, Masked/binary>>,
		<<0:1, 0:3, 0:4, 1:1, 0:7, Mask:32, Masked/binary>>,
		<<1:1, 0:3, 0:4, 1:1, 0:7, Mask:32, Masked/binary>>
	]),
	{ok, <<1:1, 0:3, 1:4, 0:1, 0:7>>} = do_recv(Client, 2, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_empty_outer_fragments(Config) ->
	doc("Empty fragments around a non-empty text fragment are echoed as that middle "
		"payload. (RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"middle frame payload">>,
	Mask = 16#01020304,
	Empty = do_mask(<<>>, Mask, <<>>),
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, [
		<<0:1, 0:3, 1:4, 1:1, 0:7, Mask:32, Empty/binary>>,
		<<0:1, 0:3, 0:4, 1:1, 20:7, Mask:32, Masked/binary>>,
		<<1:1, 0:3, 0:4, 1:1, 0:7, Mask:32, Empty/binary>>
	]),
	{ok, <<1:1, 0:3, 1:4, 0:1, 20:7, Payload:20/binary>>} = do_recv(Client, 22, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_text_multibyte(Config) ->
	doc("A text message containing multi-byte UTF-8 code points is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"Ranch-é@çôûïëù-Cowboy!"/utf8>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 29:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 29:7, Payload:29/binary>>} = do_recv(Client, 31, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_fragment_on_code_point(Config) ->
	doc("A text message fragmented on a UTF-8 code point boundary is echoed. "
		"(RFC6455 5.4, RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	P1 = <<"Ranch-é@çôû"/utf8>>,
	P2 = <<"ïëù-Cowboy!"/utf8>>,
	Mask = 16#01020304,
	M1 = do_mask(P1, Mask, <<>>),
	M2 = do_mask(P2, Mask, <<>>),
	ok = do_send(Client, [
		<<0:1, 0:3, 1:4, 1:1, 15:7, Mask:32, M1/binary>>,
		<<1:1, 0:3, 0:4, 1:1, 14:7, Mask:32, M2/binary>>
	]),
	Message = <<P1/binary, P2/binary>>,
	{ok, <<1:1, 0:3, 1:4, 0:1, 29:7, Message:29/binary>>} = do_recv(Client, 31, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_fragment_per_octet(Config) ->
	doc("A valid UTF-8 text message fragmented into 1-byte frames is echoed. "
		"(RFC6455 5.4, RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"Ranch-é@çôûïëù-Cowboy!"/utf8>>,
	Mask = 16#01020304,
	Frames = [begin
		<<Byte:8>> = binary:part(Payload, I, 1),
		Masked = do_mask(<<Byte:8>>, Mask, <<>>),
		Fin = case I =:= byte_size(Payload) - 1 of true -> 1; false -> 0 end,
		Opcode = case I of 0 -> 1; _ -> 0 end,
		<<Fin:1, 0:3, Opcode:4, 1:1, 1:7, Mask:32, Masked/binary>>
	end || I <- lists:seq(0, byte_size(Payload) - 1)],
	ok = do_send(Client, Frames),
	{ok, <<1:1, 0:3, 1:4, 0:1, 29:7, Payload:29/binary>>} = do_recv(Client, 31, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_greek_per_octet(Config) ->
	doc("A valid multi-byte UTF-8 text message fragmented into 1-byte frames is echoed. "
		"(RFC6455 5.4, RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf, 16#83, 16#ce, 16#bc, 16#ce,
		16#b5>>,
	Mask = 16#01020304,
	Frames = [begin
		<<Byte:8>> = binary:part(Payload, I, 1),
		Masked = do_mask(<<Byte:8>>, Mask, <<>>),
		Fin = case I =:= byte_size(Payload) - 1 of true -> 1; false -> 0 end,
		Opcode = case I of 0 -> 1; _ -> 0 end,
		<<Fin:1, 0:3, Opcode:4, 1:1, 1:7, Mask:32, Masked/binary>>
	end || I <- lists:seq(0, byte_size(Payload) - 1)],
	ok = do_send(Client, Frames),
	{ok, <<1:1, 0:3, 1:4, 0:1, 11:7, Payload:11/binary>>} = do_recv(Client, 13, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_invalid(Config) ->
	doc("A text message that is not valid UTF-8 fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf, 16#83, 16#ce, 16#bc, 16#ce, 16#b5,
		16#ed, 16#a0, 16#80, <<"cowboy">>/binary>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 20:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_invalid_per_octet(Config) ->
	doc("An invalid UTF-8 text message fragmented into 1-byte frames fails the connection. "
		"(RFC6455 5.4, RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf, 16#83, 16#ce, 16#bc, 16#ce, 16#b5,
		16#ed, 16#a0, 16#80, <<"cowboy">>/binary>>,
	Mask = 16#01020304,
	Frames = [begin
		<<Byte:8>> = binary:part(Payload, I, 1),
		Masked = do_mask(<<Byte:8>>, Mask, <<>>),
		Fin = case I =:= byte_size(Payload) - 1 of true -> 1; false -> 0 end,
		Opcode = case I of 0 -> 1; _ -> 0 end,
		<<Fin:1, 0:3, Opcode:4, 1:1, 1:7, Mask:32, Masked/binary>>
	end || I <- lists:seq(0, byte_size(Payload) - 1)],
	ok = do_send(Client, Frames),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_fail_fast(Config) ->
	doc("Close on invalid UTF-8 in a later fragment before the message is "
		"finished. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	P0 = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf, 16#83, 16#ce, 16#bc, 16#ce, 16#b5>>,
	M0 = do_mask(P0, Mask, <<>>),
	P1 = <<16#f4, 16#90, 16#80, 16#80>>,
	M1 = do_mask(P1, Mask, <<>>),
	ok = do_send(Client, <<0:1, 0:3, 1:4, 1:1, 11:7, Mask:32, M0/binary>>),
	{error, timeout} = do_recv(Client, 1, 500),
	ok = do_send(Client, <<0:1, 0:3, 0:4, 1:1, 4:7, Mask:32, M1/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_fail_fast_on_bad_byte(Config) ->
	doc("Close on the fragment that completes an invalid UTF-8 sequence. "
		"(RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	P0 = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf, 16#83, 16#ce, 16#bc, 16#ce, 16#b5,
		16#f4>>,
	M0 = do_mask(P0, Mask, <<>>),
	P1 = <<16#90>>,
	M1 = do_mask(P1, Mask, <<>>),
	ok = do_send(Client, <<0:1, 0:3, 1:4, 1:1, 12:7, Mask:32, M0/binary>>),
	{error, timeout} = do_recv(Client, 1, 500),
	ok = do_send(Client, <<0:1, 0:3, 0:4, 1:1, 1:7, Mask:32, M1/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_fail_fast_chop(Config) ->
	doc("Close on invalid UTF-8 inside one text frame before the rest of the "
		"frame arrives. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf, 16#83, 16#ce, 16#bc, 16#ce, 16#b5,
		16#f4, 16#90, 16#80, 16#80, <<"cowboy">>/binary>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	Header = <<1:1, 0:3, 1:4, 1:1, 21:7, Mask:32>>,
	ok = do_send(Client, <<Header/binary, (binary:part(Masked, 0, 11))/binary>>),
	{error, timeout} = do_recv(Client, 1, 500),
	ok = do_send(Client, binary:part(Masked, 11, 4)),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_fail_fast_chop_bad_byte(Config) ->
	doc("Close on the byte that makes a text payload invalid UTF-8, before the "
		"rest of the frame arrives. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf, 16#83, 16#ce, 16#bc, 16#ce, 16#b5,
		16#f4, 16#90, 16#80, 16#80, <<"cowboy">>/binary>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	Header = <<1:1, 0:3, 1:4, 1:1, 21:7, Mask:32>>,
	ok = do_send(Client, <<Header/binary, (binary:part(Masked, 0, 12))/binary>>),
	{error, timeout} = do_recv(Client, 1, 500),
	ok = do_send(Client, binary:part(Masked, 12, 1)),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_valid_1(Config) ->
	doc("Valid UTF-8 text (11 bytes 0x68656c6c6f24776f...) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"hello$world">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 11:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 11:7, Payload:11/binary>>} = do_recv(Client, 13, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_valid_2(Config) ->
	doc("Valid UTF-8 text (12 bytes 0x68656c6c6fc2a277...) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#68, 16#65, 16#6c, 16#6c, 16#6f, 16#c2, 16#a2, 16#77, 16#6f, 16#72, 16#6c,
		16#64>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 12:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 12:7, Payload:12/binary>>} = do_recv(Client, 14, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_valid_3(Config) ->
	doc("Valid UTF-8 text (13 bytes 0x68656c6c6fe282ac...) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#68, 16#65, 16#6c, 16#6c, 16#6f, 16#e2, 16#82, 16#ac, 16#77, 16#6f, 16#72,
		16#6c, 16#64>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 13:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 13:7, Payload:13/binary>>} = do_recv(Client, 15, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_valid_4(Config) ->
	doc("Valid UTF-8 text (14 bytes 0x68656c6c6ff0a4ad...) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#68, 16#65, 16#6c, 16#6c, 16#6f, 16#f0, 16#a4, 16#ad, 16#a2, 16#77, 16#6f,
		16#72, 16#6c, 16#64>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 14:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 14:7, Payload:14/binary>>} = do_recv(Client, 16, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_valid_5(Config) ->
	doc("Valid UTF-8 text (11 bytes 0xcebae1bdb9cf83ce...) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf, 16#83, 16#ce, 16#bc, 16#ce,
		16#b5>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 11:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 11:7, Payload:11/binary>>} = do_recv(Client, 13, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_prefix_1(Config) ->
	doc("Invalid UTF-8 text (0xce) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 1:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_prefix_2(Config) ->
	doc("Valid UTF-8 text (0xceba) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 2:7, Payload:2/binary>>} = do_recv(Client, 4, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_prefix_3(Config) ->
	doc("Invalid UTF-8 text (0xcebae1) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_prefix_4(Config) ->
	doc("Invalid UTF-8 text (0xcebae1bd) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1, 16#bd>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_prefix_5(Config) ->
	doc("Valid UTF-8 text (0xcebae1bdb9) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 5:7, Payload:5/binary>>} = do_recv(Client, 7, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_prefix_6(Config) ->
	doc("Invalid UTF-8 text (0xcebae1bdb9cf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_prefix_7(Config) ->
	doc("Valid UTF-8 text (0xcebae1bdb9cf83) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf, 16#83>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 7:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 7:7, Payload:7/binary>>} = do_recv(Client, 9, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_prefix_8(Config) ->
	doc("Invalid UTF-8 text (0xcebae1bdb9cf83ce) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf, 16#83, 16#ce>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 8:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_prefix_9(Config) ->
	doc("Valid UTF-8 text (9 bytes 0xcebae1bdb9cf83ce...) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf, 16#83, 16#ce, 16#bc>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 9:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 9:7, Payload:9/binary>>} = do_recv(Client, 11, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_prefix_10(Config) ->
	doc("Invalid UTF-8 text (10 bytes 0xcebae1bdb9cf83ce...) fails the connection. "
		"(RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf, 16#83, 16#ce, 16#bc, 16#ce>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 10:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_prefix_11(Config) ->
	doc("Valid UTF-8 text (11 bytes 0xcebae1bdb9cf83ce...) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf, 16#83, 16#ce, 16#bc, 16#ce,
		16#b5>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 11:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 11:7, Payload:11/binary>>} = do_recv(Client, 13, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_first_1(Config) ->
	doc("Valid UTF-8 text (0x00) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#00>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 1:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 1:7, Payload:1/binary>>} = do_recv(Client, 3, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_first_2(Config) ->
	doc("Valid UTF-8 text (0xc280) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#c2, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 2:7, Payload:2/binary>>} = do_recv(Client, 4, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_first_3(Config) ->
	doc("Valid UTF-8 text (0xe0a080) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#e0, 16#a0, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 3:7, Payload:3/binary>>} = do_recv(Client, 5, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_first_4(Config) ->
	doc("Valid UTF-8 text (0xf0908080) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f0, 16#90, 16#80, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_first_long_1(Config) ->
	doc("Invalid UTF-8 text (0xf888808080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f8, 16#88, 16#80, 16#80, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_first_long_2(Config) ->
	doc("Invalid UTF-8 text (0xfc8480808080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#fc, 16#84, 16#80, 16#80, 16#80, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_last_1(Config) ->
	doc("Valid UTF-8 text (0x7f) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#7f>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 1:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 1:7, Payload:1/binary>>} = do_recv(Client, 3, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_last_2(Config) ->
	doc("Valid UTF-8 text (0xdfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#df, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 2:7, Payload:2/binary>>} = do_recv(Client, 4, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_last_3(Config) ->
	doc("Valid UTF-8 text (0xefbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ef, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 3:7, Payload:3/binary>>} = do_recv(Client, 5, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_last_4(Config) ->
	doc("Valid UTF-8 text (0xf48fbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f4, 16#8f, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_last_long_1(Config) ->
	doc("Invalid UTF-8 text (0xf7bfbfbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f7, 16#bf, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_last_long_2(Config) ->
	doc("Invalid UTF-8 text (0xfbbfbfbfbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#fb, 16#bf, 16#bf, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_last_long_3(Config) ->
	doc("Invalid UTF-8 text (0xfdbfbfbfbfbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#fd, 16#bf, 16#bf, 16#bf, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_boundary_1(Config) ->
	doc("Valid UTF-8 text (0xed9fbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#9f, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 3:7, Payload:3/binary>>} = do_recv(Client, 5, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_boundary_2(Config) ->
	doc("Valid UTF-8 text (0xee8080) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ee, 16#80, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 3:7, Payload:3/binary>>} = do_recv(Client, 5, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_boundary_3(Config) ->
	doc("Valid UTF-8 text (0xefbfbd) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ef, 16#bf, 16#bd>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 3:7, Payload:3/binary>>} = do_recv(Client, 5, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_boundary_4(Config) ->
	doc("Valid UTF-8 text (0xf48fbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f4, 16#8f, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_boundary_5(Config) ->
	doc("Invalid UTF-8 text (0xf4908080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f4, 16#90, 16#80, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_continuation_1(Config) ->
	doc("Invalid UTF-8 text (0x80) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 1:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_continuation_2(Config) ->
	doc("Invalid UTF-8 text (0xbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 1:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_continuation_3(Config) ->
	doc("Invalid UTF-8 text (0x80bf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#80, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_continuation_4(Config) ->
	doc("Invalid UTF-8 text (0x80bf80) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#80, 16#bf, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_continuation_5(Config) ->
	doc("Invalid UTF-8 text (0x80bf80bf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#80, 16#bf, 16#80, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_continuation_6(Config) ->
	doc("Invalid UTF-8 text (0x80bf80bf80) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#80, 16#bf, 16#80, 16#bf, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_continuation_7(Config) ->
	doc("Invalid UTF-8 text (0x80bf80bf80bf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#80, 16#bf, 16#80, 16#bf, 16#80, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_continuation_8(Config) ->
	doc("Invalid UTF-8 text (63 bytes 0x8081828384858687...) fails the connection. "
		"(RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#80, 16#81, 16#82, 16#83, 16#84, 16#85, 16#86, 16#87, 16#88, 16#89, 16#8a,
		16#8b, 16#8c, 16#8d, 16#8e, 16#8f, 16#90, 16#91, 16#92, 16#93, 16#94, 16#95, 16#96,
		16#97, 16#98, 16#99, 16#9a, 16#9b, 16#9c, 16#9d, 16#9e, 16#9f, 16#a0, 16#a1, 16#a2,
		16#a3, 16#a4, 16#a5, 16#a6, 16#a7, 16#a8, 16#a9, 16#aa, 16#ab, 16#ac, 16#ad, 16#ae,
		16#af, 16#b0, 16#b1, 16#b2, 16#b3, 16#b4, 16#b5, 16#b6, 16#b7, 16#b8, 16#b9, 16#ba,
		16#bb, 16#bc, 16#bd, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 63:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_lonely_start_1(Config) ->
	doc("Invalid UTF-8 text (62 bytes 0xc020c120c220c320...) fails the connection. "
		"(RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#c0, 16#20, 16#c1, 16#20, 16#c2, 16#20, 16#c3, 16#20, 16#c4, 16#20, 16#c5,
		16#20, 16#c6, 16#20, 16#c7, 16#20, 16#c8, 16#20, 16#c9, 16#20, 16#ca, 16#20, 16#cb,
		16#20, 16#cc, 16#20, 16#cd, 16#20, 16#ce, 16#20, 16#cf, 16#20, 16#d0, 16#20, 16#d1,
		16#20, 16#d2, 16#20, 16#d3, 16#20, 16#d4, 16#20, 16#d5, 16#20, 16#d6, 16#20, 16#d7,
		16#20, 16#d8, 16#20, 16#d9, 16#20, 16#da, 16#20, 16#db, 16#20, 16#dc, 16#20, 16#dd,
		16#20, 16#de, 16#20>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 62:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_lonely_start_2(Config) ->
	doc("Invalid UTF-8 text (30 bytes 0xe020e120e220e320...) fails the connection. "
		"(RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#e0, 16#20, 16#e1, 16#20, 16#e2, 16#20, 16#e3, 16#20, 16#e4, 16#20, 16#e5,
		16#20, 16#e6, 16#20, 16#e7, 16#20, 16#e8, 16#20, 16#e9, 16#20, 16#ea, 16#20, 16#eb,
		16#20, 16#ec, 16#20, 16#ed, 16#20, 16#ee, 16#20>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 30:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_lonely_start_3(Config) ->
	doc("Invalid UTF-8 text (14 bytes 0xf020f120f220f320...) fails the connection. "
		"(RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f0, 16#20, 16#f1, 16#20, 16#f2, 16#20, 16#f3, 16#20, 16#f4, 16#20, 16#f5,
		16#20, 16#f6, 16#20>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 14:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_lonely_start_4(Config) ->
	doc("Invalid UTF-8 text (0xf820f920fa20) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f8, 16#20, 16#f9, 16#20, 16#fa, 16#20>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_lonely_start_5(Config) ->
	doc("Invalid UTF-8 text (0xfc20) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#fc, 16#20>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_missing_continuation_1(Config) ->
	doc("Invalid UTF-8 text (0xc0) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#c0>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 1:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_missing_continuation_2(Config) ->
	doc("Invalid UTF-8 text (0xe080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#e0, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_missing_continuation_3(Config) ->
	doc("Invalid UTF-8 text (0xf08080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f0, 16#80, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_missing_continuation_4(Config) ->
	doc("Invalid UTF-8 text (0xf8808080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f8, 16#80, 16#80, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_missing_continuation_5(Config) ->
	doc("Invalid UTF-8 text (0xfc80808080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#fc, 16#80, 16#80, 16#80, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_missing_continuation_6(Config) ->
	doc("Invalid UTF-8 text (0xdf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#df>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 1:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_missing_continuation_7(Config) ->
	doc("Invalid UTF-8 text (0xefbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ef, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_missing_continuation_8(Config) ->
	doc("Invalid UTF-8 text (0xf7bfbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f7, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_missing_continuation_9(Config) ->
	doc("Invalid UTF-8 text (0xfbbfbfbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#fb, 16#bf, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_missing_continuation_10(Config) ->
	doc("Invalid UTF-8 text (0xfdbfbfbfbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#fd, 16#bf, 16#bf, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_incomplete_concat_1(Config) ->
	doc("Invalid UTF-8 text (30 bytes 0xc0e080f08080f880...) fails the connection. "
		"(RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#c0, 16#e0, 16#80, 16#f0, 16#80, 16#80, 16#f8, 16#80, 16#80, 16#80, 16#fc,
		16#80, 16#80, 16#80, 16#80, 16#df, 16#ef, 16#bf, 16#f7, 16#bf, 16#bf, 16#fb, 16#bf,
		16#bf, 16#bf, 16#fd, 16#bf, 16#bf, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 30:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_impossible_1(Config) ->
	doc("Invalid UTF-8 text (0xfe) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#fe>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 1:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_impossible_2(Config) ->
	doc("Invalid UTF-8 text (0xff) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ff>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 1:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_impossible_3(Config) ->
	doc("Invalid UTF-8 text (0xfefeffff) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#fe, 16#fe, 16#ff, 16#ff>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_ascii_1(Config) ->
	doc("Invalid UTF-8 text (0xc0af) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#c0, 16#af>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_ascii_2(Config) ->
	doc("Invalid UTF-8 text (0xe080af) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#e0, 16#80, 16#af>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_ascii_3(Config) ->
	doc("Invalid UTF-8 text (0xf08080af) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f0, 16#80, 16#80, 16#af>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_ascii_4(Config) ->
	doc("Invalid UTF-8 text (0xf8808080af) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f8, 16#80, 16#80, 16#80, 16#af>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_ascii_5(Config) ->
	doc("Invalid UTF-8 text (0xfc80808080af) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#fc, 16#80, 16#80, 16#80, 16#80, 16#af>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_max_1(Config) ->
	doc("Invalid UTF-8 text (0xc1bf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#c1, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_max_2(Config) ->
	doc("Invalid UTF-8 text (0xe09fbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#e0, 16#9f, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_max_3(Config) ->
	doc("Invalid UTF-8 text (0xf08fbfbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f0, 16#8f, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_max_4(Config) ->
	doc("Invalid UTF-8 text (0xf887bfbfbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f8, 16#87, 16#bf, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_max_5(Config) ->
	doc("Invalid UTF-8 text (0xfc83bfbfbfbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#fc, 16#83, 16#bf, 16#bf, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_nul_1(Config) ->
	doc("Invalid UTF-8 text (0xc080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#c0, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_nul_2(Config) ->
	doc("Invalid UTF-8 text (0xe08080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#e0, 16#80, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_nul_3(Config) ->
	doc("Invalid UTF-8 text (0xf0808080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f0, 16#80, 16#80, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_nul_4(Config) ->
	doc("Invalid UTF-8 text (0xf880808080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f8, 16#80, 16#80, 16#80, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_overlong_nul_5(Config) ->
	doc("Invalid UTF-8 text (0xfc8080808080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#fc, 16#80, 16#80, 16#80, 16#80, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_1(Config) ->
	doc("Invalid UTF-8 text (0xeda080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#a0, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_2(Config) ->
	doc("Invalid UTF-8 text (0xedadbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#ad, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_3(Config) ->
	doc("Invalid UTF-8 text (0xedae80) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#ae, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_4(Config) ->
	doc("Invalid UTF-8 text (0xedafbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#af, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_5(Config) ->
	doc("Invalid UTF-8 text (0xedb080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#b0, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_6(Config) ->
	doc("Invalid UTF-8 text (0xedbe80) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#be, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_7(Config) ->
	doc("Invalid UTF-8 text (0xedbfbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_pair_1(Config) ->
	doc("Invalid UTF-8 text (0xeda080edb080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#a0, 16#80, 16#ed, 16#b0, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_pair_2(Config) ->
	doc("Invalid UTF-8 text (0xeda080edbfbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#a0, 16#80, 16#ed, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_pair_3(Config) ->
	doc("Invalid UTF-8 text (0xedadbfedb080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#ad, 16#bf, 16#ed, 16#b0, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_pair_4(Config) ->
	doc("Invalid UTF-8 text (0xedadbfedbfbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#ad, 16#bf, 16#ed, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_pair_5(Config) ->
	doc("Invalid UTF-8 text (0xedae80edb080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#ae, 16#80, 16#ed, 16#b0, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_pair_6(Config) ->
	doc("Invalid UTF-8 text (0xedae80edbfbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#ae, 16#80, 16#ed, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_pair_7(Config) ->
	doc("Invalid UTF-8 text (0xedafbfedb080) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#af, 16#bf, 16#ed, 16#b0, 16#80>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_surrogate_pair_8(Config) ->
	doc("Invalid UTF-8 text (0xedafbfedbfbf) fails the connection. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ed, 16#af, 16#bf, 16#ed, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 6:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_1(Config) ->
	doc("Valid UTF-8 text (0xefbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ef, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 3:7, Payload:3/binary>>} = do_recv(Client, 5, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_2(Config) ->
	doc("Valid UTF-8 text (0xefbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ef, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 3:7, Payload:3/binary>>} = do_recv(Client, 5, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_3(Config) ->
	doc("Valid UTF-8 text (0xf09fbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f0, 16#9f, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_4(Config) ->
	doc("Valid UTF-8 text (0xf09fbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f0, 16#9f, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_5(Config) ->
	doc("Valid UTF-8 text (0xf0afbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f0, 16#af, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_6(Config) ->
	doc("Valid UTF-8 text (0xf0afbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f0, 16#af, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_7(Config) ->
	doc("Valid UTF-8 text (0xf0bfbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f0, 16#bf, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_8(Config) ->
	doc("Valid UTF-8 text (0xf0bfbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f0, 16#bf, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_9(Config) ->
	doc("Valid UTF-8 text (0xf18fbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f1, 16#8f, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_10(Config) ->
	doc("Valid UTF-8 text (0xf18fbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f1, 16#8f, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_11(Config) ->
	doc("Valid UTF-8 text (0xf19fbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f1, 16#9f, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_12(Config) ->
	doc("Valid UTF-8 text (0xf19fbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f1, 16#9f, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_13(Config) ->
	doc("Valid UTF-8 text (0xf1afbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f1, 16#af, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_14(Config) ->
	doc("Valid UTF-8 text (0xf1afbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f1, 16#af, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_15(Config) ->
	doc("Valid UTF-8 text (0xf1bfbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f1, 16#bf, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_16(Config) ->
	doc("Valid UTF-8 text (0xf1bfbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f1, 16#bf, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_17(Config) ->
	doc("Valid UTF-8 text (0xf28fbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f2, 16#8f, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_18(Config) ->
	doc("Valid UTF-8 text (0xf28fbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f2, 16#8f, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_19(Config) ->
	doc("Valid UTF-8 text (0xf29fbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f2, 16#9f, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_20(Config) ->
	doc("Valid UTF-8 text (0xf29fbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f2, 16#9f, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_21(Config) ->
	doc("Valid UTF-8 text (0xf2afbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f2, 16#af, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_22(Config) ->
	doc("Valid UTF-8 text (0xf2afbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f2, 16#af, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_23(Config) ->
	doc("Valid UTF-8 text (0xf2bfbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f2, 16#bf, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_24(Config) ->
	doc("Valid UTF-8 text (0xf2bfbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f2, 16#bf, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_25(Config) ->
	doc("Valid UTF-8 text (0xf38fbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f3, 16#8f, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_26(Config) ->
	doc("Valid UTF-8 text (0xf38fbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f3, 16#8f, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_27(Config) ->
	doc("Valid UTF-8 text (0xf39fbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f3, 16#9f, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_28(Config) ->
	doc("Valid UTF-8 text (0xf39fbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f3, 16#9f, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_29(Config) ->
	doc("Valid UTF-8 text (0xf3afbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f3, 16#af, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_30(Config) ->
	doc("Valid UTF-8 text (0xf3afbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f3, 16#af, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_31(Config) ->
	doc("Valid UTF-8 text (0xf3bfbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f3, 16#bf, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_32(Config) ->
	doc("Valid UTF-8 text (0xf3bfbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f3, 16#bf, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_33(Config) ->
	doc("Valid UTF-8 text (0xf48fbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f4, 16#8f, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_noncharacter_34(Config) ->
	doc("Valid UTF-8 text (0xf48fbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#f4, 16#8f, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 4:7, Payload:4/binary>>} = do_recv(Client, 6, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_special_1(Config) ->
	doc("Valid UTF-8 text (0xefbfb9) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ef, 16#bf, 16#b9>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 3:7, Payload:3/binary>>} = do_recv(Client, 5, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_special_2(Config) ->
	doc("Valid UTF-8 text (0xefbfba) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ef, 16#bf, 16#ba>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 3:7, Payload:3/binary>>} = do_recv(Client, 5, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_special_3(Config) ->
	doc("Valid UTF-8 text (0xefbfbb) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ef, 16#bf, 16#bb>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 3:7, Payload:3/binary>>} = do_recv(Client, 5, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_special_4(Config) ->
	doc("Valid UTF-8 text (0xefbfbc) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ef, 16#bf, 16#bc>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 3:7, Payload:3/binary>>} = do_recv(Client, 5, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_special_5(Config) ->
	doc("Valid UTF-8 text (0xefbfbd) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ef, 16#bf, 16#bd>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 3:7, Payload:3/binary>>} = do_recv(Client, 5, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_special_6(Config) ->
	doc("Valid UTF-8 text (0xefbfbe) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ef, 16#bf, 16#be>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 3:7, Payload:3/binary>>} = do_recv(Client, 5, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

utf8_special_7(Config) ->
	doc("Valid UTF-8 text (0xefbfbf) is echoed. (RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<16#ef, 16#bf, 16#bf>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 3:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 3:7, Payload:3/binary>>} = do_recv(Client, 5, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_then_close(Config) ->
	doc("A text message is echoed, then the client close is answered with 1000. "
		"(RFC6455 5.5.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"Hello World!">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 12:7, Mask:32, Masked/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 12:7, Payload:12/binary>>} = do_recv(Client, 14, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

close_then_close(Config) ->
	doc("A second close frame after a normal close is ignored. (RFC6455 5.5.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	Empty = do_mask(<<>>, Mask, <<>>),
	ok = do_send(Client, [
		<<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>,
		<<1:1, 0:3, 8:4, 1:1, 0:7, Mask:32, Empty/binary>>
	]),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

close_then_ping(Config) ->
	doc("A close frame is answered, and a ping sent after it receives no pong. "
		"(RFC6455 5.5.1, RFC6455 5.5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, [
		<<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 0:7, Mask:32>>
	]),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

close_then_text(Config) ->
	doc("A text frame sent after a close frame is ignored. (RFC6455 5.5.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	Payload = <<"Hello World!">>,
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, [
		<<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>,
		<<1:1, 0:3, 1:4, 1:1, 12:7, Mask:32, Masked/binary>>
	]),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

fragment_then_close(Config) ->
	doc("A close frame during a fragmented text message ends the connection without "
		"echoing the message. (RFC6455 5.5.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	F1 = <<"fragment1">>,
	F2 = <<"fragment2">>,
	Mask = 16#01020304,
	M1 = do_mask(F1, Mask, <<>>),
	M2 = do_mask(F2, Mask, <<>>),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, [
		<<0:1, 0:3, 1:4, 1:1, 9:7, Mask:32, M1/binary>>,
		<<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>,
		<<1:1, 0:3, 0:4, 1:1, 9:7, Mask:32, M2/binary>>
	]),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

large_text_then_close_then_ping(Config) ->
	doc("A 256KiB text frame, a text frame and a close are handled in order. The ping "
		"after the close gets no pong. (RFC6455 5.5.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Unit = <<"BAsd7&jh23">>,
	Len = 262144,
	Payload = <<(binary:copy(Unit, Len div byte_size(Unit)))/binary,
		(binary:part(Unit, 0, Len rem byte_size(Unit)))/binary>>,
	Hello = <<"Hello World!">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	HelloMasked = do_mask(Hello, Mask, <<>>),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, [
		<<1:1, 0:3, 1:4, 1:1, 127:7, 0:1, Len:63, Mask:32, Masked/binary>>,
		<<1:1, 0:3, 1:4, 1:1, 12:7, Mask:32, HelloMasked/binary>>,
		<<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>,
		<<1:1, 0:3, 9:4, 1:1, 0:7, Mask:32>>
	]),
	{closed, <<
		1:1, 0:3, 1:4, 0:1, 127:7, 0:1, Len:63, Payload:Len/binary,
		1:1, 0:3, 1:4, 0:1, 12:7, Hello:12/binary,
		1:1, 0:3, 8:4, 0:1, 2:7, 1000:16
	>>} = do_recv_until_closed(Client),
	ok.

close_payload_0(Config) ->
	doc("A close frame with an empty payload is answered by an empty close frame. "
		"(RFC6455 5.5.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	Masked = do_mask(<<>>, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 0:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 0:7>>} = do_recv_until_closed(Client),
	ok.

close_payload_1(Config) ->
	doc("A close frame whose payload is a single byte fails the connection. "
		"(RFC6455 5.5.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"a">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 1:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

close_payload_2(Config) ->
	doc("A close frame carrying status code 1000 is answered with 1000. "
		"(RFC6455 5.5.1, RFC6455 7.4.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

close_with_reason(Config) ->
	doc("A close frame with status code 1000 and a UTF-8 reason is answered with 1000. "
		"(RFC6455 5.5.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Reason = <<"Hello World!">>,
	Payload = <<1000:16, Reason/binary>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 14:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

close_reason_123(Config) ->
	doc("A close reason of 123 bytes, the maximum, is accepted and answered with 1000. "
		"(RFC6455 5.5)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Reason = binary:copy(<<"*">>, 123),
	Payload = <<1000:16, Reason/binary>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 125:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

close_reason_124(Config) ->
	doc("A close payload of 126 bytes is rejected. Control frames must be at most 125 "
		"bytes. (RFC6455 5.5)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Reason = binary:copy(<<"*">>, 124),
	Payload = <<1000:16, Reason/binary>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 126:7, 126:16, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

close_reason_invalid_utf8(Config) ->
	doc("A close reason that is not valid UTF-8 fails the connection with 1007. "
		"(RFC6455 5.5.1, RFC6455 8.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Reason = <<16#ce, 16#ba, 16#e1, 16#bd, 16#b9, 16#cf, 16#83, 16#ce, 16#bc, 16#ce, 16#b5,
		16#ed, 16#a0, 16#80, <<"cowboy">>/binary>>,
	Payload = <<1000:16, Reason/binary>>,
	Len = byte_size(Payload),
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, Len:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

close_code_1000(Config) ->
	doc("Close status code 1000 is echoed. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<1000:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

close_code_1001(Config) ->
	doc("Close status code 1001 is echoed. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<1001:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1001:16>>} = do_recv_until_closed(Client),
	ok.

close_code_1002(Config) ->
	doc("Close status code 1002 is echoed. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<1002:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

close_code_1003(Config) ->
	doc("Close status code 1003 is echoed. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<1003:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1003:16>>} = do_recv_until_closed(Client),
	ok.

close_code_1007(Config) ->
	doc("Close status code 1007 is echoed. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<1007:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1007:16>>} = do_recv_until_closed(Client),
	ok.

close_code_1008(Config) ->
	doc("Close status code 1008 is echoed. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<1008:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1008:16>>} = do_recv_until_closed(Client),
	ok.

close_code_1009(Config) ->
	doc("Close status code 1009 is echoed. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<1009:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1009:16>>} = do_recv_until_closed(Client),
	ok.

close_code_1010(Config) ->
	doc("Close status code 1010 is echoed. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<1010:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1010:16>>} = do_recv_until_closed(Client),
	ok.

close_code_1011(Config) ->
	doc("Close status code 1011 is echoed. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<1011:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1011:16>>} = do_recv_until_closed(Client),
	ok.

close_code_3000(Config) ->
	doc("Close status code 3000 is echoed. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<3000:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 3000:16>>} = do_recv_until_closed(Client),
	ok.

close_code_3999(Config) ->
	doc("Close status code 3999 is echoed. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<3999:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 3999:16>>} = do_recv_until_closed(Client),
	ok.

close_code_4000(Config) ->
	doc("Close status code 4000 is echoed. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<4000:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 4000:16>>} = do_recv_until_closed(Client),
	ok.

close_code_4999(Config) ->
	doc("Close status code 4999 is echoed. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<4999:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 4999:16>>} = do_recv_until_closed(Client),
	ok.

close_code_invalid_0(Config) ->
	doc("Close status code 0 is rejected with 1002. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<0:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

close_code_invalid_999(Config) ->
	doc("Close status code 999 is rejected with 1002. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<999:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

close_code_invalid_1004(Config) ->
	doc("Close status code 1004 is rejected with 1002. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<1004:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

close_code_invalid_1005(Config) ->
	doc("Close status code 1005 is rejected with 1002. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<1005:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

close_code_invalid_1006(Config) ->
	doc("Close status code 1006 is rejected with 1002. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<1006:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

close_code_invalid_1016(Config) ->
	doc("Close status code 1016 is rejected with 1002. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<1016:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

close_code_invalid_1100(Config) ->
	doc("Close status code 1100 is rejected with 1002. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<1100:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

close_code_invalid_2000(Config) ->
	doc("Close status code 2000 is rejected with 1002. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<2000:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

close_code_invalid_2999(Config) ->
	doc("Close status code 2999 is rejected with 1002. (RFC6455 7.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<2999:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

close_code_5000(Config) ->
	doc("Close status code 5000 is outside the allowed range and is rejected with 1002. "
		"(RFC6455 7.4.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<5000:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

close_code_65535(Config) ->
	doc("Close status code 65535 is outside the allowed range and is rejected with 1002. "
		"(RFC6455 7.4.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<65535:16>>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

text_fragment_1300(Config) ->
	doc("A 65536-byte text message delivered in 1300-byte fragments is echoed intact. "
		"(RFC6455 5.4)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = binary:copy(<<"*">>, 65536),
	Mask = 16#01020304,
	Frames = [begin
		Len = min(1300, byte_size(Payload) - Off),
		Part = binary:part(Payload, Off, Len),
		Masked = do_mask(Part, Mask, <<>>),
		Fin = case Off + Len =:= byte_size(Payload) of true -> 1; false -> 0 end,
		Opcode = case Off of 0 -> 1; _ -> 0 end,
		<<Fin:1, 0:3, Opcode:4, 1:1, 126:7, Len:16, Mask:32, Masked/binary>>
	end || Off <- lists:seq(0, 65535, 1300)],
	ok = do_send(Client, Frames),
	{ok, <<1:1, 0:3, 1:4, 0:1, 127:7, 0:1, 65536:63, Payload:65536/binary>>}
		= do_recv(Client, 65546, 10000),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok.

text_unmasked(Config) ->
	doc("A client text frame with the mask bit clear fails the connection. (RFC6455 5.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"Hello">>,
	ok = do_send(Client, <<1:1, 0:3, 1:4, 0:1, 5:7, Payload/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

binary_unmasked(Config) ->
	doc("A client binary frame with the mask bit clear fails the connection. (RFC6455 5.1)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<0, 1, 2, 3, 4>>,
	ok = do_send(Client, <<1:1, 0:3, 2:4, 0:1, 5:7, Payload/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

text_length_64_msb(Config) ->
	doc("A text frame whose 64-bit length has the high bit set fails the connection. "
		"(RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 127:7, 1:1, 0:7>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

text_length_16_non_minimal_0(Config) ->
	doc("A 0-byte text frame that uses the 16-bit length form fails the connection. "
		"(RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 126:7, 0:16, Mask:32>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

text_length_16_non_minimal_125(Config) ->
	doc("A 125-byte text frame that uses the 16-bit length form fails the connection. "
		"(RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Mask = 16#01020304,
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 126:7, 125:16, Mask:32>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

text_length_non_minimal(Config) ->
	doc("A 1-byte text frame that uses the 64-bit length form fails the connection. The "
		"minimal number of bytes must encode the length. (RFC6455 5.2)"),
	Client = do_open(Config),
	ok = do_handshake(Client, "/ws_echo"),
	Payload = <<"A">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 127:7, 0:1, 1:63, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

%% Helpers.

do_open(Config) ->
	cowboy_test:raw_open(Config).

do_handshake({raw_client, Socket, Transport}, Path) ->
	ok = Transport:send(Socket, [
		"GET ", Path, " HTTP/1.1\r\n",
		"Host: localhost\r\n",
		"Connection: Upgrade\r\n",
		"Origin: http://localhost\r\n",
		"Sec-WebSocket-Version: 13\r\n",
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n",
		"Upgrade: websocket\r\n",
		"\r\n"]),
	{ok, Handshake} = Transport:recv(Socket, 0, 10000),
	{ok, {http_response, {1, 1}, 101, _}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, Data] = do_decode_headers(erlang:decode_packet(httph, Rest, []), []),
	case Data of
		<<>> -> ok;
		_ -> gen_tcp:unrecv(Socket, Data)
	end,
	{_, "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{_, "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{_, "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	ok.

do_decode_headers({ok, http_eoh, Rest}, Acc) ->
	[Acc, Rest];
do_decode_headers({ok, {http_header, _I, Key, _R, Value}, Rest}, Acc) ->
	F = fun(S) when is_atom(S) -> S; (S) -> string:to_lower(S) end,
	do_decode_headers(erlang:decode_packet(httph, Rest, []), [{F(Key), Value}|Acc]).

do_send({raw_client, Socket, Transport}, Data) ->
	Transport:send(Socket, Data).

do_send_chop(Client, Data, Chop) when byte_size(Data) =< Chop ->
	do_send(Client, Data);
do_send_chop(Client, Data, Chop) ->
	<<Part:Chop/binary, Rest/binary>> = Data,
	ok = do_send(Client, Part),
	do_send_chop(Client, Rest, Chop).

do_recv({raw_client, Socket, Transport}, Length, Timeout) ->
	Transport:recv(Socket, Length, Timeout).

do_recv_until_closed(Client) ->
	do_recv_until_closed(Client, <<>>, 10000).

do_recv_until_closed(Client = {raw_client, Socket, Transport}, Acc, Timeout) ->
	case Transport:recv(Socket, 0, Timeout) of
		{ok, Data} ->
			do_recv_until_closed(Client, <<Acc/binary, Data/binary>>, Timeout);
		{error, closed} ->
			{closed, Acc};
		{error, timeout} ->
			{timeout, Acc};
		{error, Reason} ->
			{error, Reason, Acc}
	end.

do_mask(<<>>, _, Acc) ->
	Acc;
do_mask(<<O:32, Rest/bits>>, MaskKey, Acc) ->
	do_mask(Rest, MaskKey, <<Acc/binary, (O bxor MaskKey):32>>);
do_mask(<<O:24>>, MaskKey, Acc) ->
	<<MaskKey2:24, _:8>> = <<MaskKey:32>>,
	<<Acc/binary, (O bxor MaskKey2):24>>;
do_mask(<<O:16>>, MaskKey, Acc) ->
	<<MaskKey2:16, _:16>> = <<MaskKey:32>>,
	<<Acc/binary, (O bxor MaskKey2):16>>;
do_mask(<<O:8>>, MaskKey, Acc) ->
	<<MaskKey2:8, _:24>> = <<MaskKey:32>>,
	<<Acc/binary, (O bxor MaskKey2):8>>.
