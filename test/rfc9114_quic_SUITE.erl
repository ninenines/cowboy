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

-module(rfc9114_quic_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

all() ->
	[{group, h3}].

groups() ->
	[{h3, [], ct_helper:all(?MODULE)}].

init_per_group(Name = h3, Config) ->
	cowboy_test:init_http3(Name, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))}
	}, Config).

end_per_group(Name, _) ->
	cowboy_test:stop_group(Name).

init_routes(_) -> [
	{"localhost", [
		{"/", hello_h, []},
		{"/echo/:key", echo_h, []}
	]}
].

alpn(Config) ->
	doc("Successful ALPN negotiation. (RFC9114 3.1)"),
	{ok, Conn} = quic:connect("localhost", config(port, Config),
		#{alpn => [<<"h3">>], verify => false}, self()),
	receive
		{quic, Conn, {connected, _}} ->
			ok
	after 5000 ->
		error(timeout)
	end,
	quic:close(Conn, 0),
	ok.

req_stream(Config) ->
	doc("Complete lifecycle of a request stream. (RFC9114 4.1)"),
	Port = config(port, Config),
	{ok, Conn} = quic:connect("localhost", Port,
		#{alpn => [<<"h3">>], verify => false}, self()),
	receive
		{quic, Conn, {connected, _}} -> ok
	after 5000 ->
		error(timeout_connect)
	end,
	{ok, SettingsBin, _} = cow_http3_machine:init(client, #{}),
	{ok, ControlID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, ControlID, [<<0>>, SettingsBin], false),
	{ok, EncoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, EncoderID, <<2>>, false),
	{ok, DecoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, DecoderID, <<3>>, false),
	{ok, StreamID} = quic:open_stream(Conn),
	{ok, HeaderBlock, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>}
	], 0, cow_qpack:init(encoder)),
	ok = quic:send_data(Conn, StreamID, cow_http3:headers(HeaderBlock), true),
	_Response = do_receive_data(Conn, StreamID),
	quic:close(Conn, 0),
	ok.

connection_establishment(Config) ->
	doc("Verify connection can be established and closed cleanly."),
	Port = config(port, Config),
	{ok, Conn} = quic:connect("localhost", Port,
		#{alpn => [<<"h3">>], verify => false}, self()),
	receive
		{quic, Conn, {connected, _}} -> ok
	after 5000 ->
		error(timeout_connect)
	end,
	timer:sleep(100),
	quic:close(Conn, 0),
	ok.

multiple_requests(Config) ->
	doc("Send multiple requests on separate streams."),
	Port = config(port, Config),
	{ok, Conn} = quic:connect("localhost", Port,
		#{alpn => [<<"h3">>], verify => false}, self()),
	receive
		{quic, Conn, {connected, _}} -> ok
	after 5000 ->
		error(timeout_connect)
	end,
	{ok, SettingsBin, _} = cow_http3_machine:init(client, #{}),
	{ok, ControlID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, ControlID, [<<0>>, SettingsBin], false),
	{ok, EncoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, EncoderID, <<2>>, false),
	{ok, DecoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, DecoderID, <<3>>, false),
	{ok, StreamID1} = quic:open_stream(Conn),
	{ok, HeaderBlock1, _, _} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>}
	], 0, cow_qpack:init(encoder)),
	ok = quic:send_data(Conn, StreamID1, cow_http3:headers(HeaderBlock1), true),
	{ok, StreamID2} = quic:open_stream(Conn),
	{ok, HeaderBlock2, _, _} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>}
	], 0, cow_qpack:init(encoder)),
	ok = quic:send_data(Conn, StreamID2, cow_http3:headers(HeaderBlock2), true),
	_Response1 = do_receive_data(Conn, StreamID1),
	_Response2 = do_receive_data(Conn, StreamID2),
	quic:close(Conn, 0),
	ok.

post_with_body(Config) ->
	doc("POST request with body that gets echoed back. (RFC9114 4.1)"),
	Port = config(port, Config),
	{ok, Conn} = quic:connect("localhost", Port,
		#{alpn => [<<"h3">>], verify => false}, self()),
	receive
		{quic, Conn, {connected, _}} -> ok
	after 5000 ->
		error(timeout_connect)
	end,
	{ok, SettingsBin, _} = cow_http3_machine:init(client, #{}),
	{ok, ControlID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, ControlID, [<<0>>, SettingsBin], false),
	{ok, EncoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, EncoderID, <<2>>, false),
	{ok, DecoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, DecoderID, <<3>>, false),
	{ok, StreamID} = quic:open_stream(Conn),
	Body = <<"Hello HTTP/3 World!">>,
	{ok, HeaderBlock, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/echo/read_body">>},
		{<<"content-length">>, integer_to_binary(byte_size(Body))}
	], 0, cow_qpack:init(encoder)),
	ok = quic:send_data(Conn, StreamID, cow_http3:headers(HeaderBlock), false),
	ok = quic:send_data(Conn, StreamID, cow_http3:data(Body), true),
	_Response = do_receive_data(Conn, StreamID),
	quic:close(Conn, 0),
	ok.

headers_then_trailers(Config) ->
	doc("Receipt of HEADERS followed by trailer HEADERS must be accepted. (RFC9114 4.1)"),
	Port = config(port, Config),
	{ok, Conn} = quic:connect("localhost", Port,
		#{alpn => [<<"h3">>], verify => false}, self()),
	receive
		{quic, Conn, {connected, _}} -> ok
	after 5000 ->
		error(timeout_connect)
	end,
	{ok, SettingsBin, _} = cow_http3_machine:init(client, #{}),
	{ok, ControlID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, ControlID, [<<0>>, SettingsBin], false),
	{ok, EncoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, EncoderID, <<2>>, false),
	{ok, DecoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, DecoderID, <<3>>, false),
	{ok, StreamID} = quic:open_stream(Conn),
	EncSt0 = cow_qpack:init(encoder),
	{ok, HeaderBlock, _, EncSt1} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>}
	], 0, EncSt0),
	{ok, TrailerBlock, _, _} = cow_qpack:encode_field_section([
		{<<"x-trailer">>, <<"value">>}
	], 0, EncSt1),
	ok = quic:send_data(Conn, StreamID, cow_http3:headers(HeaderBlock), false),
	ok = quic:send_data(Conn, StreamID, cow_http3:headers(TrailerBlock), true),
	_Response = do_receive_data(Conn, StreamID),
	quic:close(Conn, 0),
	ok.

large_body(Config) ->
	doc("Send a request with a moderately large body (8KB)."),
	Port = config(port, Config),
	{ok, Conn} = quic:connect("localhost", Port,
		#{alpn => [<<"h3">>], verify => false}, self()),
	receive
		{quic, Conn, {connected, _}} -> ok
	after 5000 ->
		error(timeout_connect)
	end,
	{ok, SettingsBin, _} = cow_http3_machine:init(client, #{}),
	{ok, ControlID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, ControlID, [<<0>>, SettingsBin], false),
	{ok, EncoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, EncoderID, <<2>>, false),
	{ok, DecoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, DecoderID, <<3>>, false),
	{ok, StreamID} = quic:open_stream(Conn),
	Body = binary:copy(<<"x">>, 8192),
	{ok, HeaderBlock, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/echo/read_body">>},
		{<<"content-length">>, integer_to_binary(byte_size(Body))}
	], 0, cow_qpack:init(encoder)),
	ok = quic:send_data(Conn, StreamID, cow_http3:headers(HeaderBlock), false),
	ok = quic:send_data(Conn, StreamID, cow_http3:data(Body), true),
	_Response = do_receive_data(Conn, StreamID, <<>>, 15000),
	quic:close(Conn, 0),
	ok.

concurrent_streams(Config) ->
	doc("Send many concurrent requests to test stream handling."),
	Port = config(port, Config),
	{ok, Conn} = quic:connect("localhost", Port,
		#{alpn => [<<"h3">>], verify => false}, self()),
	receive
		{quic, Conn, {connected, _}} -> ok
	after 5000 ->
		error(timeout_connect)
	end,
	{ok, SettingsBin, _} = cow_http3_machine:init(client, #{}),
	{ok, ControlID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, ControlID, [<<0>>, SettingsBin], false),
	{ok, EncoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, EncoderID, <<2>>, false),
	{ok, DecoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, DecoderID, <<3>>, false),
	StreamIDs = lists:map(fun(_) ->
		{ok, StreamID} = quic:open_stream(Conn),
		{ok, HeaderBlock, _, _} = cow_qpack:encode_field_section([
			{<<":method">>, <<"GET">>},
			{<<":scheme">>, <<"https">>},
			{<<":authority">>, <<"localhost">>},
			{<<":path">>, <<"/">>}
		], 0, cow_qpack:init(encoder)),
		ok = quic:send_data(Conn, StreamID, cow_http3:headers(HeaderBlock), true),
		StreamID
	end, lists:seq(1, 10)),
	lists:foreach(fun(StreamID) ->
		_Response = do_receive_data(Conn, StreamID)
	end, StreamIDs),
	quic:close(Conn, 0),
	ok.

do_receive_data(Conn, StreamID) ->
	do_receive_data(Conn, StreamID, <<>>, 10000).

do_receive_data(Conn, StreamID, Acc, Timeout) ->
	receive
		{quic, Conn, {stream_data, StreamID, Data, true}} ->
			<<Acc/binary, Data/binary>>;
		{quic, Conn, {stream_data, StreamID, Data, false}} ->
			do_receive_data(Conn, StreamID, <<Acc/binary, Data/binary>>, Timeout);
		{quic, Conn, _Msg} ->
			do_receive_data(Conn, StreamID, Acc, Timeout)
	after Timeout ->
		Acc
	end.
