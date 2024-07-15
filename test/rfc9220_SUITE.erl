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

-module(rfc9220_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

all() ->
	[{group, enabled}].

groups() ->
	Tests = ct_helper:all(?MODULE),
	[{enabled, [], Tests}]. %% @todo Enable parallel when all is better.

init_per_group(Name = enabled, Config) ->
	cowboy_test:init_http3(Name, #{
		enable_connect_protocol => true,
		env => #{dispatch => cowboy_router:compile(init_routes(Config))}
	}, Config).

end_per_group(Name, _) ->
	cowboy_test:stop_group(Name).

init_routes(_) -> [
	{"localhost", [
		{"/ws", ws_echo, []}
	]}
].

% The SETTINGS_ENABLE_CONNECT_PROTOCOL SETTINGS Parameter.

% The new parameter name is SETTINGS_ENABLE_CONNECT_PROTOCOL.  The
% value of the parameter MUST be 0 or 1.

%    Upon receipt of SETTINGS_ENABLE_CONNECT_PROTOCOL with a value of 1 a
%    client MAY use the Extended CONNECT definition of this document when
%    creating new streams.  Receipt of this parameter by a server does not
%    have any impact.
%% @todo ignore_client_enable_setting(Config) ->

reject_handshake_when_disabled(Config0) ->
	doc("Extended CONNECT requests MUST be rejected with a "
		"H3_MESSAGE_ERROR stream error when enable_connect_protocol=false. "
		"(RFC9220, RFC8441 4)"),
	Config = cowboy_test:init_http3(disabled, #{
		enable_connect_protocol => false,
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))}
	}, Config0),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 0.
	#{
		conn := Conn,
		settings := Settings
	} = rfc9114_SUITE:do_connect(Config),
	case Settings of
		#{enable_connect_protocol := false} -> ok;
		_ when map_size(Settings) =:= 0 -> ok
	end,
	%% Send a CONNECT :protocol request to upgrade the stream to Websocket.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	]),
	%% The stream should have been aborted.
	#{reason := h3_message_error} = rfc9114_SUITE:do_wait_stream_aborted(StreamRef),
	ok.

reject_handshake_disabled_by_default(Config0) ->
	doc("Extended CONNECT requests MUST be rejected with a "
		"H3_MESSAGE_ERROR stream error when enable_connect_protocol=false. "
		"(RFC9220, RFC8441 4)"),
	Config = cowboy_test:init_http3(disabled, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))}
	}, Config0),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 0.
	#{
		conn := Conn,
		settings := Settings
	} = rfc9114_SUITE:do_connect(Config),
	case Settings of
		#{enable_connect_protocol := false} -> ok;
		_ when map_size(Settings) =:= 0 -> ok
	end,
	%% Send a CONNECT :protocol request to upgrade the stream to Websocket.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	]),
	%% The stream should have been aborted.
	#{reason := h3_message_error} = rfc9114_SUITE:do_wait_stream_aborted(StreamRef),
	ok.

% The Extended CONNECT Method.

accept_uppercase_pseudo_header_protocol(Config) ->
	doc("The :protocol pseudo header is case insensitive. (RFC9220, RFC8441 4, RFC9110 7.8)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	#{conn := Conn, settings := Settings} = rfc9114_SUITE:do_connect(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send a CONNECT :protocol request to upgrade the stream to Websocket.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"WEBSOCKET">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	]),
	%% Receive a 200 response.
	{ok, Data} = rfc9114_SUITE:do_receive_data(StreamRef),
	{HLenEnc, HLenBits} = rfc9114_SUITE:do_guess_int_encoding(Data),
	<<
		1, %% HEADERS frame.
		HLenEnc:2, HLen:HLenBits,
		EncodedResponse:HLen/bytes
	>> = Data,
	{ok, DecodedResponse, _DecData, _DecSt}
		= cow_qpack:decode_field_section(EncodedResponse, 0, cow_qpack:init(decoder)),
	#{<<":status">> := <<"200">>} = maps:from_list(DecodedResponse),
	ok.

reject_many_pseudo_header_protocol(Config) ->
	doc("An extended CONNECT request containing more than one "
		"protocol component must be rejected with a H3_MESSAGE_ERROR "
		"stream error. (RFC9220, RFC9114 4.3.1, RFC9114 4.1.2)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	#{conn := Conn, settings := Settings} = rfc9114_SUITE:do_connect(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request with more than one :protocol pseudo-header.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":protocol">>, <<"mqtt">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	]),
	%% The stream should have been aborted.
	#{reason := h3_message_error} = rfc9114_SUITE:do_wait_stream_aborted(StreamRef),
	ok.

reject_unknown_pseudo_header_protocol(Config) ->
	doc("An extended CONNECT request containing more than one "
		"protocol component must be rejected with a 501 Not Implemented "
		"response. (RFC9220, RFC8441 4)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	#{conn := Conn, settings := Settings} = rfc9114_SUITE:do_connect(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request with an unknown protocol.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"mqtt">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	]),
	%% The stream should have been rejected with a 501 Not Implemented.
	#{headers := #{<<":status">> := <<"501">>}} = rfc9114_SUITE:do_receive_response(StreamRef),
	ok.

reject_invalid_pseudo_header_protocol(Config) ->
	doc("An extended CONNECT request with an invalid protocol "
		"component must be rejected with a 501 Not Implemented "
		"response. (RFC9220, RFC8441 4)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	#{conn := Conn, settings := Settings} = rfc9114_SUITE:do_connect(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request with an invalid protocol.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket mqtt">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	]),
	%% The stream should have been rejected with a 501 Not Implemented.
	#{headers := #{<<":status">> := <<"501">>}} = rfc9114_SUITE:do_receive_response(StreamRef),
	ok.

reject_missing_pseudo_header_scheme(Config) ->
	doc("An extended CONNECT request whtout a scheme component "
		"must be rejected with a H3_MESSAGE_ERROR stream error. "
		"(RFC9220, RFC9114 4.3.1, RFC9114 4.1.2)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	#{conn := Conn, settings := Settings} = rfc9114_SUITE:do_connect(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request without a :scheme pseudo-header.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	]),
	%% The stream should have been aborted.
	#{reason := h3_message_error} = rfc9114_SUITE:do_wait_stream_aborted(StreamRef),
	ok.

reject_missing_pseudo_header_path(Config) ->
	doc("An extended CONNECT request whtout a path component "
		"must be rejected with a H3_MESSAGE_ERROR stream error. "
		"(RFC9220, RFC9114 4.3.1, RFC9114 4.1.2)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	#{conn := Conn, settings := Settings} = rfc9114_SUITE:do_connect(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request without a :path pseudo-header.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	]),
	%% The stream should have been aborted.
	#{reason := h3_message_error} = rfc9114_SUITE:do_wait_stream_aborted(StreamRef),
	ok.

% On requests bearing the :protocol pseudo-header, the :authority
% pseudo-header field is interpreted according to Section 8.1.2.3 of
% [RFC7540] instead of Section 8.3 of [RFC7540].  In particular the
% server MUST not make a new TCP connection to the host and port
% indicated by the :authority.

reject_missing_pseudo_header_authority(Config) ->
	doc("An extended CONNECT request whtout an authority component "
		"must be rejected with a H3_MESSAGE_ERROR stream error. "
		"(RFC9220, RFC9114 4.3.1, RFC9114 4.1.2)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	#{conn := Conn, settings := Settings} = rfc9114_SUITE:do_connect(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request without an :authority pseudo-header.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/ws">>},
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	]),
	%% The stream should have been aborted.
	#{reason := h3_message_error} = rfc9114_SUITE:do_wait_stream_aborted(StreamRef),
	ok.

% Using Extended CONNECT To Bootstrap The WebSocket Protocol.

reject_missing_pseudo_header_protocol(Config) ->
	doc("An extended CONNECT request whtout a protocol component "
		"must be rejected with a H3_MESSAGE_ERROR stream error. "
		"(RFC9220, RFC9114 4.3.1, RFC9114 4.1.2)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	#{conn := Conn, settings := Settings} = rfc9114_SUITE:do_connect(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request without a :protocol pseudo-header.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	]),
	%% The stream should have been aborted.
	#{reason := h3_message_error} = rfc9114_SUITE:do_wait_stream_aborted(StreamRef),
	ok.

% The scheme of the Target URI [RFC7230] MUST be https for wss schemed
% WebSockets. HTTP/3 does not provide support for ws schemed WebSockets.
% The websocket URI is still used for proxy autoconfiguration.

reject_connection_header(Config) ->
	doc("An extended CONNECT request with a connection header "
		"must be rejected with a H3_MESSAGE_ERROR stream error. "
		"(RFC9220, RFC8441 4, RFC9114 4.2, RFC9114 4.5, RFC9114 4.1.2)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	#{conn := Conn, settings := Settings} = rfc9114_SUITE:do_connect(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request with a connection header.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"connection">>, <<"upgrade">>},
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	]),
	%% The stream should have been aborted.
	#{reason := h3_message_error} = rfc9114_SUITE:do_wait_stream_aborted(StreamRef),
	ok.

reject_upgrade_header(Config) ->
	doc("An extended CONNECT request with a upgrade header "
		"must be rejected with a H3_MESSAGE_ERROR stream error. "
		"(RFC9220, RFC8441 4, RFC9114 4.2, RFC9114 4.5, RFC9114 4.1.2)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	#{conn := Conn, settings := Settings} = rfc9114_SUITE:do_connect(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send an extended CONNECT request with a upgrade header.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"upgrade">>, <<"websocket">>},
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	]),
	%% The stream should have been aborted.
	#{reason := h3_message_error} = rfc9114_SUITE:do_wait_stream_aborted(StreamRef),
	ok.

%    After successfully processing the opening handshake the peers should
%    proceed with The WebSocket Protocol [RFC6455] using the HTTP/2 stream
%    from the CONNECT transaction as if it were the TCP connection
%    referred to in [RFC6455].  The state of the WebSocket connection at
%    this point is OPEN as defined by [RFC6455], Section 4.1.
%% @todo I'm guessing we should test for things like RST_STREAM,
%% closing the connection and others?

% Examples.

accept_handshake_when_enabled(Config) ->
	doc("Confirm the example for Websocket over HTTP/2 works. (RFC9220, RFC8441 5.1)"),
	%% Connect to server and confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	#{conn := Conn, settings := Settings} = rfc9114_SUITE:do_connect(Config),
	#{enable_connect_protocol := true} = Settings,
	%% Send a CONNECT :protocol request to upgrade the stream to Websocket.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	]),
	%% Receive a 200 response.
	{ok, Data} = rfc9114_SUITE:do_receive_data(StreamRef),
	{HLenEnc, HLenBits} = rfc9114_SUITE:do_guess_int_encoding(Data),
	<<
		1, %% HEADERS frame.
		HLenEnc:2, HLen:HLenBits,
		EncodedResponse:HLen/bytes
	>> = Data,
	{ok, DecodedResponse, _DecData, _DecSt}
		= cow_qpack:decode_field_section(EncodedResponse, 0, cow_qpack:init(decoder)),
	#{<<":status">> := <<"200">>} = maps:from_list(DecodedResponse),
	%% Masked text hello echoed back clear by the server.
	Mask = 16#37fa213d,
	MaskedHello = ws_SUITE:do_mask(<<"Hello">>, Mask, <<>>),
	{ok, _} = quicer:send(StreamRef, cow_http3:data(
		<<1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, MaskedHello/binary>>)),
	{ok, WsData} = rfc9114_SUITE:do_receive_data(StreamRef),
	<<
		0, %% DATA frame.
		0:2, 7:6, %% Length (2 bytes header + "Hello").
		1:1, 0:3, 1:4, 0:1, 5:7, "Hello" %% Websocket frame.
	>> = WsData,
	ok.

%% Closing a Websocket stream.

%    The HTTP/3 stream closure is also analogous to the TCP connection
%    closure of [RFC6455]. Orderly TCP-level closures are represented
%    as a FIN bit on the stream (Section 4.4 of [HTTP/3]). RST exceptions
%    are represented with a stream error (Section 8 of [HTTP/3]) of type
%    H3_REQUEST_CANCELLED (Section 8.1 of [HTTP/3]).

%% @todo client close frame with FIN
%% @todo server close frame with FIN
%% @todo client other frame with FIN
%% @todo server other frame with FIN
%% @todo client close connection
