%% Copyright (c) Loic Hoguin <essen@ninenines.eu>
%% Copyright (c) Benoit Chesneau <bchesneau@gmail.com>
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

-module(rfc9220_quic_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

all() ->
	[{group, enabled}].

groups() ->
	Tests = ct_helper:all(?MODULE),
	[{enabled, [], Tests}].

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

do_connect(Config) ->
	Port = config(port, Config),
	{ok, Conn} = quic:connect("localhost", Port,
		#{alpn => [<<"h3">>], verify => false}, self()),
	receive
		{quic, Conn, {connected, _}} -> ok
	after 5000 ->
		error(timeout_connect)
	end,
	{ok, SettingsBin, _} = cow_http3_machine:init(client, #{
		enable_connect_protocol => true
	}),
	{ok, ControlID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, ControlID, [<<0>>, SettingsBin], false),
	{ok, EncoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, EncoderID, <<2>>, false),
	{ok, DecoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, DecoderID, <<3>>, false),
	timer:sleep(100),
	Settings = receive_server_settings(Conn, #{}),
	#{conn => Conn, settings => Settings}.

receive_server_settings(Conn, Acc) ->
	receive
		{quic, Conn, {stream_opened, _StreamID, unidirectional}} ->
			receive_server_settings(Conn, Acc);
		{quic, Conn, {stream_data, _StreamID, Data, _Fin}} ->
			parse_settings(Data, Acc);
		{quic, Conn, _Other} ->
			receive_server_settings(Conn, Acc)
	after 500 ->
		Acc
	end.

parse_settings(<<0, Rest/binary>>, Acc) ->
	parse_settings_frame(Rest, Acc);
parse_settings(<<_, _/binary>>, Acc) ->
	Acc;
parse_settings(<<>>, Acc) ->
	Acc.

parse_settings_frame(<<4, Rest/binary>>, Acc) ->
	{Len, Rest2} = cow_http3:parse_int(Rest),
	<<SettingsPayload:Len/binary, _/binary>> = Rest2,
	decode_settings(SettingsPayload, Acc);
parse_settings_frame(_, Acc) ->
	Acc.

decode_settings(<<>>, Acc) ->
	Acc;
decode_settings(Bin, Acc) ->
	{Id, Rest} = cow_http3:parse_int(Bin),
	{Value, Rest2} = cow_http3:parse_int(Rest),
	Acc2 = case Id of
		8 -> Acc#{enable_connect_protocol => Value =:= 1};
		_ -> Acc
	end,
	decode_settings(Rest2, Acc2).

do_receive_data(Conn, StreamID) ->
	do_receive_data(Conn, StreamID, <<>>, 5000).

do_receive_data(Conn, StreamID, Acc, Timeout) ->
	receive
		{quic, Conn, {stream_data, StreamID, Data, true}} ->
			{ok, <<Acc/binary, Data/binary>>};
		{quic, Conn, {stream_data, StreamID, Data, false}} ->
			{ok, <<Acc/binary, Data/binary>>};
		{quic, Conn, {stream_closed, StreamID, _ErrorCode}} ->
			{ok, Acc};
		{quic, Conn, _Other} ->
			do_receive_data(Conn, StreamID, Acc, Timeout)
	after Timeout ->
		{error, timeout}
	end.

do_wait_stream_aborted(Conn, StreamID) ->
	receive
		{quic, Conn, {stream_closed, StreamID, ErrorCode}} ->
			#{reason => do_error_code_to_reason(ErrorCode)};
		{quic, Conn, {stream_data, StreamID, _, _}} ->
			do_wait_stream_aborted(Conn, StreamID);
		{quic, Conn, _Other} ->
			do_wait_stream_aborted(Conn, StreamID)
	after 5000 ->
		{error, timeout}
	end.

do_error_code_to_reason(16#0106) -> h3_message_error;
do_error_code_to_reason(Code) -> {unknown, Code}.

accept_handshake_when_enabled(Config) ->
	doc("Confirm the example for Websocket over HTTP/3 works. (RFC9220, RFC8441 5.1)"),
	#{conn := Conn, settings := Settings} = do_connect(Config),
	#{enable_connect_protocol := true} = Settings,
	{ok, StreamID} = quic:open_stream(Conn),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>},
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	ok = quic:send_data(Conn, StreamID, [
		<<1>>,
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	], false),
	{ok, Data} = do_receive_data(Conn, StreamID),
	{HLenEnc, HLenBits} = do_guess_int_encoding(Data),
	<<
		1,
		HLenEnc:2, HLen:HLenBits,
		EncodedResponse:HLen/bytes,
		_Rest/binary
	>> = Data,
	{ok, DecodedResponse, _DecData, _DecSt}
		= cow_qpack:decode_field_section(EncodedResponse, 0, cow_qpack:init(decoder)),
	#{<<":status">> := <<"200">>} = maps:from_list(DecodedResponse),
	Mask = 16#37fa213d,
	MaskedHello = ws_SUITE:do_mask(<<"Hello">>, Mask, <<>>),
	ok = quic:send_data(Conn, StreamID, cow_http3:data(
		<<1:1, 0:3, 1:4, 1:1, 5:7, Mask:32, MaskedHello/binary>>), false),
	{ok, WsData} = do_receive_data(Conn, StreamID),
	<<
		0,
		0:2, 7:6,
		1:1, 0:3, 1:4, 0:1, 5:7, "Hello"
	>> = WsData,
	quic:close(Conn, 0),
	ok.

accept_uppercase_pseudo_header_protocol(Config) ->
	doc("The :protocol pseudo header is case insensitive. (RFC9220, RFC8441 4, RFC9110 7.8)"),
	#{conn := Conn, settings := Settings} = do_connect(Config),
	#{enable_connect_protocol := true} = Settings,
	{ok, StreamID} = quic:open_stream(Conn),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"WEBSOCKET">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>},
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	ok = quic:send_data(Conn, StreamID, [
		<<1>>,
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	], false),
	{ok, Data} = do_receive_data(Conn, StreamID),
	{HLenEnc, HLenBits} = do_guess_int_encoding(Data),
	<<
		1,
		HLenEnc:2, HLen:HLenBits,
		EncodedResponse:HLen/bytes,
		_/binary
	>> = Data,
	{ok, DecodedResponse, _, _} = cow_qpack:decode_field_section(EncodedResponse, 0, cow_qpack:init(decoder)),
	#{<<":status">> := <<"200">>} = maps:from_list(DecodedResponse),
	quic:close(Conn, 0),
	ok.

reject_unknown_pseudo_header_protocol(Config) ->
	doc("An extended CONNECT request with an unknown protocol "
		"must be rejected with a 501 Not Implemented response. (RFC9220, RFC8441 4)"),
	#{conn := Conn, settings := Settings} = do_connect(Config),
	#{enable_connect_protocol := true} = Settings,
	{ok, StreamID} = quic:open_stream(Conn),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"mqtt">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/ws">>},
		{<<":authority">>, <<"localhost">>},
		{<<"sec-websocket-version">>, <<"13">>},
		{<<"origin">>, <<"http://localhost">>}
	], 0, cow_qpack:init(encoder)),
	ok = quic:send_data(Conn, StreamID, [
		<<1>>,
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	], true),
	{ok, Data} = do_receive_data(Conn, StreamID),
	{HLenEnc, HLenBits} = do_guess_int_encoding(Data),
	<<
		1,
		HLenEnc:2, HLen:HLenBits,
		EncodedResponse:HLen/bytes,
		_/binary
	>> = Data,
	{ok, DecodedResponse, _, _} = cow_qpack:decode_field_section(EncodedResponse, 0, cow_qpack:init(decoder)),
	#{<<":status">> := <<"501">>} = maps:from_list(DecodedResponse),
	quic:close(Conn, 0),
	ok.

do_guess_int_encoding(<<0:2, _:6, _/bits>>) -> {0, 6};
do_guess_int_encoding(<<1:2, _:14, _/bits>>) -> {1, 14};
do_guess_int_encoding(<<2:2, _:30, _/bits>>) -> {2, 30};
do_guess_int_encoding(<<3:2, _:62, _/bits>>) -> {3, 62}.
