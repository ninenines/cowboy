%% Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(rfc8441_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

all() -> [{group, enabled}].

groups() ->
	Tests = ct_helper:all(?MODULE),
	[{enabled, [parallel], Tests}].

init_per_group(Name = enabled, Config) ->
	cowboy_test:init_http(Name, #{
		enable_connect_protocol => true,
		env => #{dispatch => cowboy_router:compile(init_routes(Config))}
	}, Config).

end_per_group(Name, _) ->
	ok = cowboy:stop_listener(Name).

init_routes(_) -> [
	{"localhost", [
		{"/ws", ws_echo, []}
	]}
].

%% Do a prior knowledge handshake.
do_handshake(Config) ->
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a valid preface.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, SettingsPayload:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	Settings = cow_http2:parse_settings_payload(SettingsPayload),
	%% Send the SETTINGS ack.
	ok = gen_tcp:send(Socket, cow_http2:settings_ack()),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, Socket, Settings}.

% The SETTINGS_ENABLE_CONNECT_PROTOCOL SETTINGS Parameter.

% The new parameter name is SETTINGS_ENABLE_CONNECT_PROTOCOL.  The
% value of the parameter MUST be 0 or 1.

%    Upon receipt of SETTINGS_ENABLE_CONNECT_PROTOCOL with a value of 1 a
%    client MAY use the Extended CONNECT definition of this document when
%    creating new streams.  Receipt of this parameter by a server does not
%    have any impact.
%% @todo ignore_client_enable_setting(Config) ->

% A sender MUST NOT send a SETTINGS_ENABLE_CONNECT_PROTOCOL parameter
% with the value of 0 after previously sending a value of 1.

reject_handshake_when_disabled(Config0) ->
	doc("Extended CONNECT requests MUST be rejected with a "
		"PROTOCOL_ERROR stream error when enable_connect_protocol=false. (draft-01 3)"),
	Config = cowboy_test:init_http(disabled, #{
		enable_connect_protocol => false,
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))}
	}, Config0),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 0.
	{ok, Socket, Settings} = do_handshake(Config),
	case Settings of
		#{enable_connect_protocol := false} -> ok;
		_ when map_size(Settings) =:= 0 -> ok
	end,
	%% Send a CONNECT :protocol request to upgrade the stream to Websocket.
	{ReqHeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, ReqHeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_handshake_disabled_by_default(Config0) ->
	doc("Extended CONNECT requests MUST be rejected with a "
		"PROTOCOL_ERROR stream error with default enable_connect_protocol. (draft-01 3)"),
	Config = cowboy_test:init_http(disabled_by_default, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))}
	}, Config0),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 0.
	{ok, Socket, Settings} = do_handshake(Config),
	case Settings of
		#{enable_connect_protocol := false} -> ok;
		_ when map_size(Settings) =:= 0 -> ok
	end,
	%% Send a CONNECT :protocol request to upgrade the stream to Websocket.
	{ReqHeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, ReqHeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

% The Extended CONNECT Method.

accept_uppercase_pseudo_header_protocol(Config) ->
	doc("The :protocol pseudo header is case insensitive. (draft-01 4)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	{ok, Socket, Settings} = do_handshake(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send a CONNECT :protocol request to upgrade the stream to Websocket.
	{ReqHeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"WEBSOCKET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, ReqHeadersBlock)),
	%% Receive a 200 response.
	{ok, << Len1:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, RespHeadersBlock} = gen_tcp:recv(Socket, Len1, 1000),
	{RespHeaders, _} = cow_hpack:decode(RespHeadersBlock),
	{_, <<"200">>} = lists:keyfind(<<":status">>, 1, RespHeaders),
	ok.

reject_many_pseudo_header_protocol(Config) ->
	doc("An extended CONNECT request containing more than one protocol component "
		"must be rejected with a PROTOCOL_ERROR stream error. (draft-01 4, RFC7540 8.1.2.6)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	{ok, Socket, Settings} = do_handshake(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request with more than one :protocol pseudo-header.
	{ReqHeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":protocol">>, <<"mqtt">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, ReqHeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_unknown_pseudo_header_protocol(Config) ->
	doc("An extended CONNECT request with an unknown protocol must be rejected "
		"with a 400 error. (draft-01 4)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	{ok, Socket, Settings} = do_handshake(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request with an unknown :protocol pseudo-header.
	{ReqHeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"mqtt">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, ReqHeadersBlock)),
	%% Receive a 400 response.
	{ok, << Len1:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, RespHeadersBlock} = gen_tcp:recv(Socket, Len1, 1000),
	{RespHeaders, _} = cow_hpack:decode(RespHeadersBlock),
	{_, <<"400">>} = lists:keyfind(<<":status">>, 1, RespHeaders),
	ok.

reject_invalid_pseudo_header_protocol(Config) ->
	doc("An extended CONNECT request with an invalid protocol must be rejected "
		"with a 400 error. (draft-01 4)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	{ok, Socket, Settings} = do_handshake(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request with an invalid :protocol pseudo-header.
	{ReqHeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket mqtt">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, ReqHeadersBlock)),
	%% Receive a 400 response.
	{ok, << Len1:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, RespHeadersBlock} = gen_tcp:recv(Socket, Len1, 1000),
	{RespHeaders, _} = cow_hpack:decode(RespHeadersBlock),
	{_, <<"400">>} = lists:keyfind(<<":status">>, 1, RespHeaders),
	ok.

reject_missing_pseudo_header_scheme(Config) ->
	doc("An extended CONNECT request without a scheme component must be rejected "
		"with a PROTOCOL_ERROR stream error. (draft-01 4, RFC7540 8.1.2.6)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	{ok, Socket, Settings} = do_handshake(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request without a :scheme pseudo-header.
	{ReqHeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, ReqHeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_missing_pseudo_header_path(Config) ->
	doc("An extended CONNECT request without a path component must be rejected "
		"with a PROTOCOL_ERROR stream error. (draft-01 4, RFC7540 8.1.2.6)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	{ok, Socket, Settings} = do_handshake(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request without a :path pseudo-header.
	{ReqHeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, ReqHeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

% On requests bearing the :protocol pseudo-header, the :authority
% pseudo-header field is interpreted according to Section 8.1.2.3 of
% [RFC7540] instead of Section 8.3 of [RFC7540].  In particular the
% server MUST not make a new TCP connection to the host and port
% indicated by the :authority.

reject_missing_pseudo_header_authority(Config) ->
	doc("An extended CONNECT request without an authority component must be rejected "
		"with a PROTOCOL_ERROR stream error. (draft-01 4, draft-01 5)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	{ok, Socket, Settings} = do_handshake(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request without an :authority pseudo-header.
	{ReqHeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/ws">>},
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, ReqHeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

% Using Extended CONNECT To Bootstrap The WebSocket Protocol.

reject_missing_pseudo_header_protocol(Config) ->
	doc("An extended CONNECT request without a protocol component must be rejected "
		"with a PROTOCOL_ERROR stream error. (draft-01 4, RFC7540 8.1.2.6)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	{ok, Socket, Settings} = do_handshake(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request without a :scheme pseudo-header.
	{ReqHeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"CONNECT">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, ReqHeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

% The scheme of the Target URI [RFC7230] MUST be https for wss schemed
% WebSockets and http for ws schemed WebSockets.  The websocket URI is
% still used for proxy autoconfiguration.

reject_connection_header(Config) ->
	doc("An extended CONNECT request with a connection header must be rejected "
		"with a PROTOCOL_ERROR stream error. (draft-01 5, RFC7540 8.1.2.6)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	{ok, Socket, Settings} = do_handshake(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request without a :scheme pseudo-header.
	{ReqHeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"connection">>, <<"upgrade">>},
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, ReqHeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

reject_upgrade_header(Config) ->
	doc("An extended CONNECT request with a upgrade header must be rejected "
		"with a PROTOCOL_ERROR stream error. (draft-01 5, RFC7540 8.1.2.6)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	{ok, Socket, Settings} = do_handshake(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request without a :scheme pseudo-header.
	{ReqHeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"upgrade">>, <<"websocket">>},
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, ReqHeadersBlock)),
	%% Receive a PROTOCOL_ERROR stream error.
	{ok, << _:24, 3:8, _:8, 1:32, 1:32 >>} = gen_tcp:recv(Socket, 13, 6000),
	ok.

%    After successfully processing the opening handshake the peers should
%    proceed with The WebSocket Protocol [RFC6455] using the HTTP/2 stream
%    from the CONNECT transaction as if it were the TCP connection
%    referred to in [RFC6455].  The state of the WebSocket connection at
%    this point is OPEN as defined by [RFC6455], Section 4.1.
%% @todo I'm guessing we should test for things like RST_STREAM,
%% closing the connection and others?

% Examples.

%% @todo Probably worth testing that we get the correct option
%% over all different connection types (alpn, prior, upgrade).
accept_handshake_when_enabled(Config) ->
	doc("Confirm the example for Websocket over HTTP/2 works. (draft-01 5.1)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	{ok, Socket, Settings} = do_handshake(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send a CONNECT :protocol request to upgrade the stream to Websocket.
	{ReqHeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	]),
	ok = gen_tcp:send(Socket, cow_http2:headers(1, nofin, ReqHeadersBlock)),
	%% Receive a 200 response.
	{ok, << Len1:24, 1:8, _:8, 1:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, RespHeadersBlock} = gen_tcp:recv(Socket, Len1, 1000),
	{RespHeaders, _} = cow_hpack:decode(RespHeadersBlock),
	{_, <<"200">>} = lists:keyfind(<<":status">>, 1, RespHeaders),
	%% Masked text hello echoed back clear by the server.
	%%
	%% We receive WINDOW_UPDATE frames before the actual data
	%% due to flow control updates every time a data frame is received.
	Mask = 16#37fa213d,
	MaskedHello = ws_SUITE:do_mask(<<"Hello">>, Mask, <<>>),
	ok = gen_tcp:send(Socket, cow_http2:data(1, nofin,
		<<1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, MaskedHello/binary>>)),
	{ok, <<4:24, 8:8, _:72>>} = gen_tcp:recv(Socket, 13, 1000),
	{ok, <<4:24, 8:8, _:72>>} = gen_tcp:recv(Socket, 13, 1000),
	{ok, <<Len2:24, _:8, _:8, _:32>>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, <<1:1, 0:3, 1:4, 0:1, 5:7, "Hello">>} = gen_tcp:recv(Socket, Len2, 1000),
	ok.

%% Closing a Websocket stream.

%    The HTTP/2 stream closure is also analagous to the TCP connection closure of
% 	 [RFC6455].  Orderly TCP level closures are represented as END_STREAM
% 	 ([RFC7540] Section 6.1) flags and RST exceptions are represented with
% 	 the RST_STREAM ([RFC7540] Section 6.4) frame with the CANCEL
% 	 ([RFC7540] Secion 7) error code.

%% @todo client close frame with END_STREAM
%% @todo server close frame with END_STREAM
%% @todo client other frame with END_STREAM
%% @todo server other frame with END_STREAM
%% @todo client close connection
