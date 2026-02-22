%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
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

%% HTTP/3 test suite using erlang_quic client.

-module(h3_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

all() ->
	[{group, h3}].

groups() ->
	[{h3, [], [
		connect_h3,
		request_response,
		request_with_body,
		multiple_requests
	]}].

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

%% Tests.

connect_h3(Config) ->
	doc("Establish an HTTP/3 connection."),
	Port = config(port, Config),
	Opts = #{
		alpn => [<<"h3">>],
		verify => false
	},
	{ok, Conn} = quic:connect("localhost", Port, Opts, self()),
	receive
		{quic, Conn, {connected, _Info}} ->
			ok
	after 5000 ->
		error(timeout)
	end,
	quic:close(Conn, 0),
	ok.

request_response(Config) ->
	doc("Send a simple GET request and receive response."),
	Port = config(port, Config),
	Opts = #{
		alpn => [<<"h3">>],
		verify => false
	},
	{ok, Conn} = quic:connect("localhost", Port, Opts, self()),
	receive
		{quic, Conn, {connected, _}} -> ok
	after 5000 ->
		error(timeout_connect)
	end,
	%% Open control, encoder, decoder streams (required for HTTP/3).
	{ok, SettingsBin, _} = cow_http3_machine:init(client, #{}),
	{ok, ControlID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, ControlID, [<<0>>, SettingsBin], false),
	{ok, EncoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, EncoderID, <<2>>, false),
	{ok, DecoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, DecoderID, <<3>>, false),
	%% Open request stream and send GET request.
	{ok, StreamID} = quic:open_stream(Conn),
	{ok, HeaderBlock, _EncData, _} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>}
	], 0, cow_qpack:init(encoder)),
	ok = quic:send_data(Conn, StreamID, cow_http3:headers(HeaderBlock), true),
	%% Wait for response.
	_Response = receive_response(Conn, StreamID, <<>>, 5000),
	quic:close(Conn, 0),
	ok.

request_with_body(Config) ->
	doc("Send a POST request with body."),
	Port = config(port, Config),
	Opts = #{
		alpn => [<<"h3">>],
		verify => false
	},
	{ok, Conn} = quic:connect("localhost", Port, Opts, self()),
	receive
		{quic, Conn, {connected, _}} -> ok
	after 5000 ->
		error(timeout_connect)
	end,
	%% Open control, encoder, decoder streams.
	{ok, SettingsBin, _} = cow_http3_machine:init(client, #{}),
	{ok, ControlID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, ControlID, [<<0>>, SettingsBin], false),
	{ok, EncoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, EncoderID, <<2>>, false),
	{ok, DecoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, DecoderID, <<3>>, false),
	%% Open request stream and send POST request.
	{ok, StreamID} = quic:open_stream(Conn),
	Body = <<"Hello, HTTP/3!">>,
	{ok, HeaderBlock, _EncData, _} = cow_qpack:encode_field_section([
		{<<":method">>, <<"POST">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/echo/body">>},
		{<<"content-length">>, integer_to_binary(byte_size(Body))}
	], 0, cow_qpack:init(encoder)),
	ok = quic:send_data(Conn, StreamID, cow_http3:headers(HeaderBlock), false),
	ok = quic:send_data(Conn, StreamID, cow_http3:data(Body), true),
	%% Wait for response.
	_Response = receive_response(Conn, StreamID, <<>>, 5000),
	quic:close(Conn, 0),
	ok.

multiple_requests(Config) ->
	doc("Send multiple requests on the same connection."),
	Port = config(port, Config),
	Opts = #{
		alpn => [<<"h3">>],
		verify => false
	},
	{ok, Conn} = quic:connect("localhost", Port, Opts, self()),
	receive
		{quic, Conn, {connected, _}} -> ok
	after 5000 ->
		error(timeout_connect)
	end,
	%% Open control, encoder, decoder streams.
	{ok, SettingsBin, _} = cow_http3_machine:init(client, #{}),
	{ok, ControlID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, ControlID, [<<0>>, SettingsBin], false),
	{ok, EncoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, EncoderID, <<2>>, false),
	{ok, DecoderID} = quic:open_unidirectional_stream(Conn),
	ok = quic:send_data(Conn, DecoderID, <<3>>, false),
	%% Send first request.
	{ok, StreamID1} = quic:open_stream(Conn),
	{ok, HeaderBlock1, _, _} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>}
	], 0, cow_qpack:init(encoder)),
	ok = quic:send_data(Conn, StreamID1, cow_http3:headers(HeaderBlock1), true),
	%% Send second request before waiting for first response.
	{ok, StreamID2} = quic:open_stream(Conn),
	{ok, HeaderBlock2, _, _} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>}
	], 0, cow_qpack:init(encoder)),
	ok = quic:send_data(Conn, StreamID2, cow_http3:headers(HeaderBlock2), true),
	%% Wait for both responses.
	_Response1 = receive_response(Conn, StreamID1, <<>>, 5000),
	_Response2 = receive_response(Conn, StreamID2, <<>>, 5000),
	quic:close(Conn, 0),
	ok.

%% Helpers.

receive_response(Conn, StreamID, Acc, Timeout) ->
	receive
		{quic, Conn, {stream_data, StreamID, Data, true}} ->
			<<Acc/binary, Data/binary>>;
		{quic, Conn, {stream_data, StreamID, Data, false}} ->
			receive_response(Conn, StreamID, <<Acc/binary, Data/binary>>, Timeout);
		{quic, Conn, _Msg} ->
			%% Ignore other messages (stream_opened, etc.)
			receive_response(Conn, StreamID, Acc, Timeout)
	after Timeout ->
		{partial, Acc}
	end.
