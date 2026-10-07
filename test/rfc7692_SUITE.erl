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

%% RFC 7692 server conformance.

-module(rfc7692_SUITE).
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
	cowboy_test:init_http(rfc7692, #{
		env => #{dispatch => cowboy_router:compile(init_routes())}
	}, Config).

end_per_group(ws, _Config) ->
	ok = cowboy:stop_listener(rfc7692).

init_routes() ->
	[{"localhost", [
		{"/ws_echo", ws_echo, []}
	]}].

text_default_offer(Config) ->
	doc("Two identical compressed text messages are echoed; the client resets its context "
		"and the server does not. (RFC7692 7.1.1)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_no_context_takeover; client_max_window_bits=15"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_no_context_takeover; client_max_window_bits"),
	Plain = binary:copy(<<"The quick brown fox jumps over the lazy dog. ">>, 6),
	Mask = 16#01020304,
	ZDef = zlib:open(),
	ok = zlib:deflateInit(ZDef, default, deflated, -15, 8, default),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -15),
	C1 = do_deflate(ZDef, Plain),
	ok = zlib:deflateReset(ZDef),
	C2 = do_deflate(ZDef, Plain),
	C1 = C2,
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C1),
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C2),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E1} = do_recv_frame(Client),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E2} = do_recv_frame(Client),
	true = E1 =/= E2,
	Plain = do_inflate(ZInf, E1),
	Plain = do_inflate(ZInf, E2),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

text_server_no_context_takeover(Config) ->
	doc("Two identical compressed text messages are echoed with server context takeover "
		"disabled. (RFC7692 7.1.1)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; server_no_context_takeover; client_no_context_takeover; "
		"client_max_window_bits=15"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; server_no_context_takeover; client_no_context_takeover; "
				"client_max_window_bits"),
	Plain = binary:copy(<<"The quick brown fox jumps over the lazy dog. ">>, 6),
	Mask = 16#01020304,
	ZDef = zlib:open(),
	ok = zlib:deflateInit(ZDef, default, deflated, -15, 8, default),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -15),
	C1 = do_deflate(ZDef, Plain),
	ok = zlib:deflateReset(ZDef),
	C2 = do_deflate(ZDef, Plain),
	C1 = C2,
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C1),
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C2),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E1} = do_recv_frame(Client),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E2} = do_recv_frame(Client),
	E1 = E2,
	Plain = do_inflate(ZInf, E1),
	ok = zlib:inflateReset(ZInf),
	Plain = do_inflate(ZInf, E2),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

text_server_max_window_bits_9(Config) ->
	doc("Two identical compressed text messages are echoed with server_max_window_bits 9. "
		"(RFC7692 7.1.2)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_no_context_takeover; client_max_window_bits=15; "
		"server_max_window_bits=9"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_no_context_takeover; client_max_window_bits; "
				"server_max_window_bits=9"),
	Plain = binary:copy(<<"The quick brown fox jumps over the lazy dog. ">>, 6),
	Mask = 16#01020304,
	ZDef = zlib:open(),
	ok = zlib:deflateInit(ZDef, default, deflated, -15, 8, default),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -9),
	C1 = do_deflate(ZDef, Plain),
	ok = zlib:deflateReset(ZDef),
	C2 = do_deflate(ZDef, Plain),
	C1 = C2,
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C1),
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C2),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E1} = do_recv_frame(Client),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E2} = do_recv_frame(Client),
	true = E1 =/= E2,
	Plain = do_inflate(ZInf, E1),
	Plain = do_inflate(ZInf, E2),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

text_server_max_window_bits_15(Config) ->
	doc("Two identical compressed text messages are echoed with server_max_window_bits 15. "
		"(RFC7692 7.1.2)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_no_context_takeover; client_max_window_bits=15; "
		"server_max_window_bits=15"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_no_context_takeover; client_max_window_bits; "
				"server_max_window_bits=15"),
	Plain = binary:copy(<<"The quick brown fox jumps over the lazy dog. ">>, 6),
	Mask = 16#01020304,
	ZDef = zlib:open(),
	ok = zlib:deflateInit(ZDef, default, deflated, -15, 8, default),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -15),
	C1 = do_deflate(ZDef, Plain),
	ok = zlib:deflateReset(ZDef),
	C2 = do_deflate(ZDef, Plain),
	C1 = C2,
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C1),
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C2),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E1} = do_recv_frame(Client),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E2} = do_recv_frame(Client),
	true = E1 =/= E2,
	Plain = do_inflate(ZInf, E1),
	Plain = do_inflate(ZInf, E2),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

text_server_no_takeover_window_9(Config) ->
	doc("Two identical compressed text messages are echoed with no server context takeover "
		"and server_max_window_bits 9. (RFC7692 7.1.1, RFC7692 7.1.2)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; server_no_context_takeover; client_no_context_takeover; "
		"client_max_window_bits=15; server_max_window_bits=9"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; server_no_context_takeover; client_no_context_takeover; "
				"client_max_window_bits; server_max_window_bits=9"),
	Plain = binary:copy(<<"The quick brown fox jumps over the lazy dog. ">>, 6),
	Mask = 16#01020304,
	ZDef = zlib:open(),
	ok = zlib:deflateInit(ZDef, default, deflated, -15, 8, default),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -9),
	C1 = do_deflate(ZDef, Plain),
	ok = zlib:deflateReset(ZDef),
	C2 = do_deflate(ZDef, Plain),
	C1 = C2,
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C1),
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C2),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E1} = do_recv_frame(Client),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E2} = do_recv_frame(Client),
	E1 = E2,
	Plain = do_inflate(ZInf, E1),
	ok = zlib:inflateReset(ZInf),
	Plain = do_inflate(ZInf, E2),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

text_server_no_takeover_window_15(Config) ->
	doc("Two identical compressed text messages are echoed with no server context takeover "
		"and server_max_window_bits 15. (RFC7692 7.1.1, RFC7692 7.1.2)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; server_no_context_takeover; client_no_context_takeover; "
		"client_max_window_bits=15; server_max_window_bits=15"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; server_no_context_takeover; client_no_context_takeover; "
				"client_max_window_bits; server_max_window_bits=15"),
	Plain = binary:copy(<<"The quick brown fox jumps over the lazy dog. ">>, 6),
	Mask = 16#01020304,
	ZDef = zlib:open(),
	ok = zlib:deflateInit(ZDef, default, deflated, -15, 8, default),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -15),
	C1 = do_deflate(ZDef, Plain),
	ok = zlib:deflateReset(ZDef),
	C2 = do_deflate(ZDef, Plain),
	C1 = C2,
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C1),
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C2),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E1} = do_recv_frame(Client),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E2} = do_recv_frame(Client),
	E1 = E2,
	Plain = do_inflate(ZInf, E1),
	ok = zlib:inflateReset(ZInf),
	Plain = do_inflate(ZInf, E2),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

text_three_offers(Config) ->
	doc("The first of three permessage-deflate offers is accepted and two compressed text "
		"messages are echoed. (RFC7692 5)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; server_no_context_takeover; client_no_context_takeover; "
		"client_max_window_bits=15; server_max_window_bits=9"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; server_no_context_takeover; client_no_context_takeover; "
				"client_max_window_bits; server_max_window_bits=9, permessage-deflate; "
				"server_no_context_takeover; client_no_context_takeover; client_max_window_bits, "
				"permessage-deflate; client_no_context_takeover; client_max_window_bits"),
	Plain = binary:copy(<<"The quick brown fox jumps over the lazy dog. ">>, 6),
	Mask = 16#01020304,
	ZDef = zlib:open(),
	ok = zlib:deflateInit(ZDef, default, deflated, -15, 8, default),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -9),
	C1 = do_deflate(ZDef, Plain),
	ok = zlib:deflateReset(ZDef),
	C2 = do_deflate(ZDef, Plain),
	C1 = C2,
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C1),
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C2),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E1} = do_recv_frame(Client),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E2} = do_recv_frame(Client),
	E1 = E2,
	Plain = do_inflate(ZInf, E1),
	ok = zlib:inflateReset(ZInf),
	Plain = do_inflate(ZInf, E2),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

binary_default_offer(Config) ->
	doc("Two identical compressed binary messages are echoed for the default "
		"permessage-deflate offer. (RFC7692 6.1)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_no_context_takeover; client_max_window_bits=15"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_no_context_takeover; client_max_window_bits"),
	Plain = binary:copy(<<"The quick brown fox jumps over the lazy dog. ">>, 6),
	Mask = 16#01020304,
	ZDef = zlib:open(),
	ok = zlib:deflateInit(ZDef, default, deflated, -15, 8, default),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -15),
	C1 = do_deflate(ZDef, Plain),
	ok = zlib:deflateReset(ZDef),
	C2 = do_deflate(ZDef, Plain),
	C1 = C2,
	ok = do_send_frame(Client, 1, 2#100, 2, Mask, C1),
	ok = do_send_frame(Client, 1, 2#100, 2, Mask, C2),
	{ok, <<1:1, 1:1, 0:2, 2:4, _/bits>>, E1} = do_recv_frame(Client),
	{ok, <<1:1, 1:1, 0:2, 2:4, _/bits>>, E2} = do_recv_frame(Client),
	true = E1 =/= E2,
	Plain = do_inflate(ZInf, E1),
	Plain = do_inflate(ZInf, E2),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

text_payload_16(Config) ->
	doc("A 16-byte compressed text message is echoed with the 7-bit length. "
		"(RFC6455 5.2, RFC7692 6.1)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_no_context_takeover; client_max_window_bits=15"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_no_context_takeover; client_max_window_bits"),
	Plain = <<"0123456789abcdef">>,
	Mask = 16#01020304,
	ZDef = zlib:open(),
	ok = zlib:deflateInit(ZDef, default, deflated, -15, 8, default),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -15),
	C1 = do_deflate(ZDef, Plain),
	ok = zlib:deflateReset(ZDef),
	C2 = do_deflate(ZDef, Plain),
	C1 = C2,
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C1),
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C2),
	{ok, <<1:1, 1:1, 0:2, 1:4, 0:1, L7A:7>>, E1} = do_recv_frame(Client),
	true = L7A =< 125,
	true = L7A > 0,
	{ok, <<1:1, 1:1, 0:2, 1:4, 0:1, L7B:7>>, E2} = do_recv_frame(Client),
	true = L7B =< 125,
	true = L7B > 0,
	true = E1 =/= E2,
	Plain = do_inflate(ZInf, E1),
	Plain = do_inflate(ZInf, E2),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

binary_payload_large(Config) ->
	doc("A compressed binary message larger than 125 bytes is echoed with the 16-bit "
		"length. (RFC6455 5.2, RFC7692 6.1)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_no_context_takeover; client_max_window_bits=15"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_no_context_takeover; client_max_window_bits"),
	Plain = << <<((I * 13 + 91) rem 251):8>> || I <- lists:seq(0, 179) >>,
	Mask = 16#01020304,
	ZDef = zlib:open(),
	ok = zlib:deflateInit(ZDef, default, deflated, -15, 8, default),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -15),
	C1 = do_deflate(ZDef, Plain),
	ok = zlib:deflateReset(ZDef),
	C2 = do_deflate(ZDef, Plain),
	C1 = C2,
	ok = do_send_frame(Client, 1, 2#100, 2, Mask, C1),
	ok = do_send_frame(Client, 1, 2#100, 2, Mask, C2),
	{ok, <<1:1, 1:1, 0:2, 2:4, 0:1, 126:7, Ext1:16>>, E1}
		= do_recv_frame(Client),
	true = Ext1 > 125,
	{ok, <<1:1, 1:1, 0:2, 2:4, _/bits>>, E2} = do_recv_frame(Client),
	true = E1 =/= E2,
	Plain = do_inflate(ZInf, E1),
	Plain = do_inflate(ZInf, E2),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

binary_fragment_256(Config) ->
	doc("A compressed binary message fragmented into 256-byte frames is echoed. "
		"(RFC7692 6.1)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_no_context_takeover; client_max_window_bits=15"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_no_context_takeover; client_max_window_bits"),
	Plain = << <<((I * 37 + 11) rem 256):8>> || I <- lists:seq(0, 299) >>,
	Mask = 16#01020304,
	ZDef = zlib:open(),
	ok = zlib:deflateInit(ZDef, default, deflated, -15, 8, default),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -15),
	C1 = do_deflate(ZDef, Plain),
	true = byte_size(C1) > 256,
	<<Part1:256/binary, Part2/binary>> = C1,
	true = byte_size(Part2) > 0,
	true = byte_size(Part2) < 256,
	Masked1 = do_mask(Part1, Mask, <<>>),
	ok = do_send(Client,
		<<0:1, 1:1, 0:2, 2:4, 1:1, 126:7, 256:16, Mask:32, Masked1/binary>>),
	ok = do_send_frame(Client, 1, 0, 0, Mask, Part2),
	{ok, <<1:1, 1:1, 0:2, 2:4, _/bits>>, E1} = do_recv_frame(Client),
	Plain = do_inflate(ZInf, E1),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

text_client_context_takeover(Config) ->
	doc("Two compressed text messages keep the client context and are echoed. "
		"(RFC7692 7.1.1)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_max_window_bits=15"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_max_window_bits"),
	Plain = binary:copy(<<"The quick brown fox jumps over the lazy dog. ">>, 6),
	Mask = 16#01020304,
	ZDef = zlib:open(),
	ok = zlib:deflateInit(ZDef, default, deflated, -15, 8, default),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -15),
	C1 = do_deflate(ZDef, Plain),
	C2 = do_deflate(ZDef, Plain),
	true = C1 =/= C2,
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C1),
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C2),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E1} = do_recv_frame(Client),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E2} = do_recv_frame(Client),
	true = E1 =/= E2,
	Plain = do_inflate(ZInf, E1),
	Plain = do_inflate(ZInf, E2),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

text_server_max_window_bits_8(Config) ->
	doc("A compressed text message is echoed when server_max_window_bits is 8. "
		"(RFC7692 7.1.2)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_no_context_takeover; client_max_window_bits=15; "
		"server_max_window_bits=8"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_no_context_takeover; client_max_window_bits; "
				"server_max_window_bits=8"),
	Plain = binary:copy(<<"The quick brown fox jumps over the lazy dog. ">>, 6),
	Mask = 16#01020304,
	ZDef = zlib:open(),
	ok = zlib:deflateInit(ZDef, default, deflated, -15, 8, default),
	ZInf = zlib:open(),
	%% Cowlib deflates server_max_window_bits 8 with window 9.
	ok = zlib:inflateInit(ZInf, -15),
	C1 = do_deflate(ZDef, Plain),
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C1),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E1} = do_recv_frame(Client),
	Plain = do_inflate(ZInf, E1),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

text_client_max_window_bits_8(Config) ->
	doc("A compressed text message is echoed when client_max_window_bits is 8. "
		"(RFC7692 7.1.2)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_max_window_bits=8"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_max_window_bits=8"),
	Plain = binary:copy(<<"The quick brown fox jumps over the lazy dog. ">>, 4),
	Mask = 16#01020304,
	ZDef = zlib:open(),
	%% zlib 1.2.11+ rejects deflateInit window -8.
	ok = zlib:deflateInit(ZDef, default, deflated, -9, 8, default),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -15),
	C1 = do_deflate(ZDef, Plain),
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C1),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E1} = do_recv_frame(Client),
	Plain = do_inflate(ZInf, E1),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

text_server_max_window_bits_7(Config) ->
	doc("server_max_window_bits 7 is rejected and an RSV1 frame fails the connection. "
		"(RFC7692 7.1.2)"),
	Client = do_open(Config),
	{ok, undefined} = do_handshake(Client, "/ws_echo",
		"permessage-deflate; client_no_context_takeover; client_max_window_bits; "
			"server_max_window_bits=7"),
	Plain = <<"Hi">>,
	Mask = 16#01020304,
	Masked = do_mask(Plain, Mask, <<>>),
	ok = do_send(Client, <<1:1, 1:1, 0:2, 1:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

text_empty(Config) ->
	doc("An empty compressed text message is echoed. (RFC7692 6.1, RFC7692 7.2.1)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_no_context_takeover; client_max_window_bits=15"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_no_context_takeover; client_max_window_bits"),
	Plain = <<>>,
	Mask = 16#01020304,
	ZDef = zlib:open(),
	ok = zlib:deflateInit(ZDef, default, deflated, -15, 8, default),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -15),
	C1 = do_deflate(ZDef, Plain),
	ok = do_send_frame(Client, 1, 2#100, 1, Mask, C1),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E1} = do_recv_frame(Client),
	Plain = do_inflate(ZInf, E1),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZDef),
	ok = zlib:close(ZInf),
	ok.

text_rsv_invalid(Config) ->
	doc("A data frame with RSV1 and RSV2 set fails the connection. "
		"(RFC6455 5.2, RFC7692 6.1)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_no_context_takeover; client_max_window_bits=15"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_no_context_takeover; client_max_window_bits"),
	Plain = <<"Hi">>,
	Mask = 16#01020304,
	Masked = do_mask(Plain, Mask, <<>>),
	ok = do_send(Client, <<1:1, 6:3, 1:4, 1:1, 2:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

ping_rsv1(Config) ->
	doc("A ping with RSV1 set fails the connection when permessage-deflate is negotiated. "
		"(RFC7692 6.1)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_no_context_takeover; client_max_window_bits=15"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_no_context_takeover; client_max_window_bits"),
	Mask = 16#01020304,
	ok = do_send(Client, <<1:1, 4:3, 9:4, 1:1, 0:7, Mask:32>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

ping_rsv1_payload(Config) ->
	doc("A ping with RSV1 set and a payload fails the connection when permessage-deflate "
		"is negotiated. (RFC7692 6.1)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_no_context_takeover; client_max_window_bits=15"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_no_context_takeover; client_max_window_bits"),
	Payload = <<"ping">>,
	Mask = 16#01020304,
	Masked = do_mask(Payload, Mask, <<>>),
	ok = do_send(Client, <<1:1, 4:3, 9:4, 1:1, 4:7, Mask:32, Masked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1002:16>>} = do_recv_until_closed(Client),
	ok.

text_uncompressed(Config) ->
	doc("The echo handler compresses an uncompressed data frame on the way back. "
		"(RFC7692 6.1)"),
	Client = do_open(Config),
	{ok, "permessage-deflate; client_no_context_takeover; client_max_window_bits=15"}
		= do_handshake(Client, "/ws_echo",
			"permessage-deflate; client_no_context_takeover; client_max_window_bits"),
	Plain = <<"Hello">>,
	Mask = 16#01020304,
	Masked = do_mask(Plain, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, Masked/binary>>),
	ZInf = zlib:open(),
	ok = zlib:inflateInit(ZInf, -15),
	{ok, <<1:1, 1:1, 0:2, 1:4, _/bits>>, E1} = do_recv_frame(Client),
	Plain = do_inflate(ZInf, E1),
	CloseBin = <<1000:16>>,
	CloseMasked = do_mask(CloseBin, Mask, <<>>),
	ok = do_send(Client, <<1:1, 0:3, 8:4, 1:1, 2:7, Mask:32, CloseMasked/binary>>),
	{closed, <<1:1, 0:3, 8:4, 0:1, 2:7, 1000:16>>} = do_recv_until_closed(Client),
	ok = zlib:close(ZInf),
	ok.

%% Helpers.

do_open(Config) ->
	cowboy_test:raw_open(Config).

do_handshake({raw_client, Socket, Transport}, Path, Offer) ->
	ok = Transport:send(Socket, [
		"GET ", Path, " HTTP/1.1\r\n",
		"Host: localhost\r\n",
		"Connection: Upgrade\r\n",
		"Origin: http://localhost\r\n",
		"Sec-WebSocket-Version: 13\r\n",
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n",
		"Upgrade: websocket\r\n",
		"Sec-WebSocket-Extensions: ", Offer, "\r\n",
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
	case lists:keyfind("sec-websocket-extensions", 1, Headers) of
		{_, Extensions} -> {ok, Extensions};
		false -> {ok, undefined}
	end.

do_decode_headers({ok, http_eoh, Rest}, Acc) ->
	[Acc, Rest];
do_decode_headers({ok, {http_header, _I, Key, _R, Value}, Rest}, Acc) ->
	F = fun(S) when is_atom(S) -> S; (S) -> string:to_lower(S) end,
	do_decode_headers(erlang:decode_packet(httph, Rest, []), [{F(Key), Value}|Acc]).

do_send_frame(Client, Fin, Rsv, Opcode, Mask, Payload) ->
	Masked = do_mask(Payload, Mask, <<>>),
	Len = byte_size(Payload),
	Frame = case Len of
		N when N =< 125 ->
			<<Fin:1, Rsv:3, Opcode:4, 1:1, N:7, Mask:32, Masked/binary>>;
		N when N =< 65535 ->
			<<Fin:1, Rsv:3, Opcode:4, 1:1, 126:7, N:16, Mask:32, Masked/binary>>;
		N ->
			<<Fin:1, Rsv:3, Opcode:4, 1:1, 127:7, 0:1, N:63, Mask:32, Masked/binary>>
	end,
	do_send(Client, Frame).

do_recv_frame(Client) ->
	{ok, H2 = <<_:1, _:3, _:4, 0:1, Len7:7>>}
		= do_recv(Client, 2, 10000),
	{Header, Len} = case Len7 of
		N when N =< 125 ->
			{H2, N};
		126 ->
			{ok, Ext} = do_recv(Client, 2, 10000),
			<<Len16:16>> = Ext,
			{<<H2/binary, Ext/binary>>, Len16};
		127 ->
			{ok, Ext} = do_recv(Client, 8, 10000),
			<<0:1, Len64:63>> = Ext,
			{<<H2/binary, Ext/binary>>, Len64}
	end,
	Payload = case Len of
		0 ->
			<<>>;
		_ ->
			{ok, Body} = do_recv(Client, Len, 10000),
			Body
	end,
	{ok, Header, Payload}.

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

do_deflate(Z, Data) ->
	Deflated = iolist_to_binary(zlib:deflate(Z, Data, sync)),
	Len = byte_size(Deflated) - 4,
	<<Body:Len/binary, 0, 0, 255, 255>> = Deflated,
	Body.

do_inflate(Z, Data) ->
	iolist_to_binary(zlib:inflate(Z, <<Data/binary, 0, 0, 255, 255>>)).
