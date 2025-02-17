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

-module(rfc9114_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

-ifdef(COWBOY_QUICER).

-include_lib("quicer/include/quicer.hrl").

all() ->
	[{group, h3}].

groups() ->
	%% @todo Enable parallel tests but for this issues in the
	%% QUIC accept loop need to be figured out (can't connect
	%% concurrently somehow, no backlog?).
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

%% Starting HTTP/3 for "https" URIs.

alpn(Config) ->
	doc("Successful ALPN negotiation. (RFC9114 3.1)"),
	{ok, Conn} = quicer:connect("localhost", config(port, Config),
		#{alpn => ["h3"], verify => none}, 5000),
	{ok, <<"h3">>} = quicer:negotiated_protocol(Conn),
	%% To make sure the connection is fully established we wait
	%% to receive the SETTINGS frame on the control stream.
	{ok, _ControlRef, _Settings} = do_wait_settings(Conn),
	ok.

alpn_error(Config) ->
	doc("Failed ALPN negotiation using the 'h2' token. (RFC9114 3.1)"),
	{error, transport_down, #{status := alpn_neg_failure}}
		= quicer:connect("localhost", config(port, Config),
			#{alpn => ["h2"], verify => none}, 5000),
	ok.

%% @todo 3.2. Connection Establishment
%% After the QUIC connection is established, a SETTINGS frame MUST be sent by each endpoint as the initial frame of their respective HTTP control stream.

%% @todo 3.3. Connection Reuse
%% Servers are encouraged to maintain open HTTP/3 connections for as long as
%possible but are permitted to terminate idle connections if necessary. When
%either endpoint chooses to close the HTTP/3 connection, the terminating
%endpoint SHOULD first send a GOAWAY frame (Section 5.2) so that both endpoints
%can reliably determine whether previously sent frames have been processed and
%gracefully complete or terminate any necessary remaining tasks.

%% Frame format.

req_stream(Config) ->
	doc("Complete lifecycle of a request stream. (RFC9114 4.1)"),
	{ok, Conn} = quicer:connect("localhost", config(port, Config),
		#{alpn => ["h3"], verify => none}, 5000),
	%% To make sure the connection is fully established we wait
	%% to receive the SETTINGS frame on the control stream.
	{ok, ControlRef, _Settings} = do_wait_settings(Conn),
	%% Send a request on a request stream.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	], ?QUIC_SEND_FLAG_FIN),
	%% Receive the response.
	{ok, Data} = do_receive_data(StreamRef),
	{HLenEnc, HLenBits} = do_guess_int_encoding(Data),
	<<
		1, %% HEADERS frame.
		HLenEnc:2, HLen:HLenBits,
		EncodedResponse:HLen/bytes,
		Rest/bits
	>> = Data,
	{ok, DecodedResponse, _DecData, _DecSt}
		= cow_qpack:decode_field_section(EncodedResponse, 0, cow_qpack:init(decoder)),
	#{
		<<":status">> := <<"200">>,
		<<"content-length">> := BodyLen
	} = maps:from_list(DecodedResponse),
	{DLenEnc, DLenBits} = do_guess_int_encoding(Rest),
	<<
		0, %% DATA frame.
		DLenEnc:2, DLen:DLenBits,
		Body:DLen/bytes
	>> = Rest,
	<<"Hello world!">> = Body,
	BodyLen = integer_to_binary(byte_size(Body)),
	ok = do_wait_peer_send_shutdown(StreamRef),
	ok = do_wait_stream_closed(StreamRef).

%% @todo Same test as above but with content-length unset?

req_stream_two_requests(Config) ->
	doc("Receipt of multiple requests on a single stream must "
		"be rejected with an H3_MESSAGE_ERROR stream error. "
		"(RFC9114 4.1, RFC9114 4.1.2)"),
	{ok, Conn} = quicer:connect("localhost", config(port, Config),
		#{alpn => ["h3"], verify => none}, 5000),
	%% To make sure the connection is fully established we wait
	%% to receive the SETTINGS frame on the control stream.
	{ok, ControlRef, _Settings} = do_wait_settings(Conn),
	%% Send two requests on a request stream.
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest1, _EncData1, EncSt0} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>}
	], 0, cow_qpack:init(encoder)),
	{ok, EncodedRequest2, _EncData2, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>}
	], 0, EncSt0),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest1)),
		EncodedRequest1,
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest2)),
		EncodedRequest2
	]),
	%% The stream should have been aborted.
	#{reason := h3_message_error} = do_wait_stream_aborted(StreamRef),
	ok.

headers_then_trailers(Config) ->
	doc("Receipt of HEADERS followed by trailer HEADERS must be accepted. (RFC9114 4.1)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData1, EncSt0} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>}
	], 0, cow_qpack:init(encoder)),
	{ok, EncodedTrailers, _EncData2, _EncSt} = cow_qpack:encode_field_section([
		{<<"content-type">>, <<"text/plain">>}
	], 0, EncSt0),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<1>>, %% HEADERS frame for trailers.
		cow_http3:encode_int(iolist_size(EncodedTrailers)),
		EncodedTrailers
	], ?QUIC_SEND_FLAG_FIN),
	#{
		headers := #{<<":status">> := <<"200">>},
		body := <<"Hello world!">>
	} = do_receive_response(StreamRef),
	ok.

headers_then_data_then_trailers(Config) ->
	doc("Receipt of HEADERS followed by DATA followed by trailer HEADERS "
		"must be accepted. (RFC9114 4.1)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData1, EncSt0} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"13">>}
	], 0, cow_qpack:init(encoder)),
	{ok, EncodedTrailers, _EncData2, _EncSt} = cow_qpack:encode_field_section([
		{<<"content-type">>, <<"text/plain">>}
	], 0, EncSt0),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(13),
		<<"Hello server!">>,
		<<1>>, %% HEADERS frame for trailers.
		cow_http3:encode_int(iolist_size(EncodedTrailers)),
		EncodedTrailers
	], ?QUIC_SEND_FLAG_FIN),
	#{
		headers := #{<<":status">> := <<"200">>},
		body := <<"Hello world!">>
	} = do_receive_response(StreamRef),
	ok.

data_then_headers(Config) ->
	doc("Receipt of DATA before HEADERS must be rejected "
		"with an H3_FRAME_UNEXPECTED connection error. (RFC9114 4.1)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData1, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"13">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<0>>, %% DATA frame.
		cow_http3:encode_int(13),
		<<"Hello server!">>,
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders
	], ?QUIC_SEND_FLAG_FIN),
	%% The connection should have been closed.
	#{reason := h3_frame_unexpected} = do_wait_connection_closed(Conn),
	ok.

headers_then_trailers_then_data(Config) ->
	doc("Receipt of DATA after trailer HEADERS must be rejected "
		"with an H3_FRAME_UNEXPECTED connection error. (RFC9114 4.1)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData1, EncSt0} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>}
	], 0, cow_qpack:init(encoder)),
	{ok, EncodedTrailers, _EncData2, _EncSt} = cow_qpack:encode_field_section([
		{<<"content-type">>, <<"text/plain">>}
	], 0, EncSt0),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<1>>, %% HEADERS frame for trailers.
		cow_http3:encode_int(iolist_size(EncodedTrailers)),
		EncodedTrailers,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(13),
		<<"Hello server!">>
	], ?QUIC_SEND_FLAG_FIN),
	%% The connection should have been closed.
	#{reason := h3_frame_unexpected} = do_wait_connection_closed(Conn),
	ok.

headers_then_data_then_trailers_then_data(Config) ->
	doc("Receipt of DATA after trailer HEADERS must be rejected "
		"with an H3_FRAME_UNEXPECTED connection error. (RFC9114 4.1)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData1, EncSt0} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"13">>}
	], 0, cow_qpack:init(encoder)),
	{ok, EncodedTrailers, _EncData2, _EncSt} = cow_qpack:encode_field_section([
		{<<"content-type">>, <<"text/plain">>}
	], 0, EncSt0),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(13),
		<<"Hello server!">>,
		<<1>>, %% HEADERS frame for trailers.
		cow_http3:encode_int(iolist_size(EncodedTrailers)),
		EncodedTrailers,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(13),
		<<"Hello server!">>
	], ?QUIC_SEND_FLAG_FIN),
	%% The connection should have been closed.
	#{reason := h3_frame_unexpected} = do_wait_connection_closed(Conn),
	ok.

headers_then_data_then_trailers_then_trailers(Config) ->
	doc("Receipt of DATA after trailer HEADERS must be rejected "
		"with an H3_FRAME_UNEXPECTED connection error. (RFC9114 4.1)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData1, EncSt0} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"13">>}
	], 0, cow_qpack:init(encoder)),
	{ok, EncodedTrailers1, _EncData2, EncSt1} = cow_qpack:encode_field_section([
		{<<"content-type">>, <<"text/plain">>}
	], 0, EncSt0),
	{ok, EncodedTrailers2, _EncData3, _EncSt} = cow_qpack:encode_field_section([
		{<<"content-type">>, <<"text/plain">>}
	], 0, EncSt1),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(13),
		<<"Hello server!">>,
		<<1>>, %% HEADERS frame for trailers.
		cow_http3:encode_int(iolist_size(EncodedTrailers1)),
		EncodedTrailers1,
		<<1>>, %% HEADERS frame for trailers.
		cow_http3:encode_int(iolist_size(EncodedTrailers2)),
		EncodedTrailers2
	], ?QUIC_SEND_FLAG_FIN),
	%% The connection should have been closed.
	#{reason := h3_frame_unexpected} = do_wait_connection_closed(Conn),
	ok.

unknown_then_headers(Config) ->
	doc("Receipt of unknown frame followed by HEADERS "
		"must be accepted. (RFC9114 4.1, RFC9114 9)"),
	unknown_then_headers(Config, do_unknown_frame_type(),
		rand:bytes(rand:uniform(4096))).

unknown_then_headers(Config, Type, Bytes) ->
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		cow_http3:encode_int(Type), %% Unknown frame.
		cow_http3:encode_int(iolist_size(Bytes)),
		Bytes,
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders
	], ?QUIC_SEND_FLAG_FIN),
	#{
		headers := #{<<":status">> := <<"200">>},
		body := <<"Hello world!">>
	} = do_receive_response(StreamRef),
	ok.

headers_then_unknown(Config) ->
	doc("Receipt of HEADERS followed by unknown frame "
		"must be accepted. (RFC9114 4.1, RFC9114 9)"),
	headers_then_unknown(Config, do_unknown_frame_type(),
		rand:bytes(rand:uniform(4096))).

headers_then_unknown(Config, Type, Bytes) ->
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		cow_http3:encode_int(Type), %% Unknown frame.
		cow_http3:encode_int(iolist_size(Bytes)),
		Bytes
	], ?QUIC_SEND_FLAG_FIN),
	#{
		headers := #{<<":status">> := <<"200">>},
		body := <<"Hello world!">>
	} = do_receive_response(StreamRef),
	ok.

headers_then_data_then_unknown(Config) ->
	doc("Receipt of HEADERS followed by DATA followed by unknown frame "
		"must be accepted. (RFC9114 4.1, RFC9114 9)"),
	headers_then_data_then_unknown(Config, do_unknown_frame_type(),
		rand:bytes(rand:uniform(4096))).

headers_then_data_then_unknown(Config, Type, Bytes) ->
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"13">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(13),
		<<"Hello server!">>,
		cow_http3:encode_int(Type), %% Unknown frame.
		cow_http3:encode_int(iolist_size(Bytes)),
		Bytes
	], ?QUIC_SEND_FLAG_FIN),
	#{
		headers := #{<<":status">> := <<"200">>},
		body := <<"Hello world!">>
	} = do_receive_response(StreamRef),
	ok.

headers_then_trailers_then_unknown(Config) ->
	doc("Receipt of HEADERS followed by trailer HEADERS followed by unknown frame "
		"must be accepted. (RFC9114 4.1, RFC9114 9)"),
	headers_then_data_then_unknown(Config, do_unknown_frame_type(),
		rand:bytes(rand:uniform(4096))).

headers_then_trailers_then_unknown(Config, Type, Bytes) ->
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData, EncSt0} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>}
	], 0, cow_qpack:init(encoder)),
	{ok, EncodedTrailers, _EncData2, _EncSt} = cow_qpack:encode_field_section([
		{<<"content-type">>, <<"text/plain">>}
	], 0, EncSt0),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<1>>, %% HEADERS frame for trailers.
		cow_http3:encode_int(iolist_size(EncodedTrailers)),
		EncodedTrailers,
		cow_http3:encode_int(Type), %% Unknown frame.
		cow_http3:encode_int(iolist_size(Bytes)),
		Bytes
	], ?QUIC_SEND_FLAG_FIN),
	#{
		headers := #{<<":status">> := <<"200">>},
		body := <<"Hello world!">>
	} = do_receive_response(StreamRef),
	ok.

headers_then_data_then_unknown_then_trailers(Config) ->
	doc("Receipt of HEADERS followed by DATA followed by "
		"unknown frame followed by trailer HEADERS "
		"must be accepted. (RFC9114 4.1, RFC9114 9)"),
	headers_then_data_then_unknown_then_trailers(Config,
		do_unknown_frame_type(), rand:bytes(rand:uniform(4096))).

headers_then_data_then_unknown_then_trailers(Config, Type, Bytes) ->
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData, EncSt0} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"13">>}
	], 0, cow_qpack:init(encoder)),
	{ok, EncodedTrailers, _EncData2, _EncSt} = cow_qpack:encode_field_section([
		{<<"content-type">>, <<"text/plain">>}
	], 0, EncSt0),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(13),
		<<"Hello server!">>,
		cow_http3:encode_int(Type), %% Unknown frame.
		cow_http3:encode_int(iolist_size(Bytes)),
		Bytes,
		<<1>>, %% HEADERS frame for trailers.
		cow_http3:encode_int(iolist_size(EncodedTrailers)),
		EncodedTrailers
	], ?QUIC_SEND_FLAG_FIN),
	#{
		headers := #{<<":status">> := <<"200">>},
		body := <<"Hello world!">>
	} = do_receive_response(StreamRef),
	ok.

headers_then_data_then_unknown_then_data(Config) ->
	doc("Receipt of HEADERS followed by DATA followed by "
		"unknown frame followed by DATA "
		"must be accepted. (RFC9114 4.1, RFC9114 9)"),
	headers_then_data_then_unknown_then_data(Config,
		do_unknown_frame_type(), rand:bytes(rand:uniform(4096))).

headers_then_data_then_unknown_then_data(Config, Type, Bytes) ->
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"13">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(6),
		<<"Hello ">>,
		cow_http3:encode_int(Type), %% Unknown frame.
		cow_http3:encode_int(iolist_size(Bytes)),
		Bytes,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(7),
		<<"server!">>
	], ?QUIC_SEND_FLAG_FIN),
	#{
		headers := #{<<":status">> := <<"200">>},
		body := <<"Hello world!">>
	} = do_receive_response(StreamRef),
	ok.

headers_then_data_then_trailers_then_unknown(Config) ->
	doc("Receipt of HEADERS followed by DATA followed by "
		"trailer HEADERS followed by unknown frame "
		"must be accepted. (RFC9114 4.1, RFC9114 9)"),
	headers_then_data_then_trailers_then_unknown(Config,
		do_unknown_frame_type(), rand:bytes(rand:uniform(4096))).

headers_then_data_then_trailers_then_unknown(Config, Type, Bytes) ->
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData, EncSt0} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"13">>}
	], 0, cow_qpack:init(encoder)),
	{ok, EncodedTrailers, _EncData2, _EncSt} = cow_qpack:encode_field_section([
		{<<"content-type">>, <<"text/plain">>}
	], 0, EncSt0),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(13),
		<<"Hello server!">>,
		<<1>>, %% HEADERS frame for trailers.
		cow_http3:encode_int(iolist_size(EncodedTrailers)),
		EncodedTrailers,
		cow_http3:encode_int(Type), %% Unknown frame.
		cow_http3:encode_int(iolist_size(Bytes)),
		Bytes
	], ?QUIC_SEND_FLAG_FIN),
	#{
		headers := #{<<":status">> := <<"200">>},
		body := <<"Hello world!">>
	} = do_receive_response(StreamRef),
	ok.

do_unknown_frame_type() ->
	Type = rand:uniform(4611686018427387904) - 1,
	%% Retry if we get a value that's specified.
	case lists:member(Type, [
		16#0, 16#1, 16#3, 16#4, 16#5, 16#7, 16#d, %% HTTP/3 core frame types.
		16#2, 16#6, 16#8, 16#9 %% HTTP/3 reserved frame types that must be rejected.
	]) of
		true -> do_unknown_frame_type();
		false -> Type
	end.

reserved_then_headers(Config) ->
	doc("Receipt of reserved frame followed by HEADERS "
		"must be accepted when the reserved frame type is "
		"of the format 0x1f * N + 0x21. (RFC9114 4.1, RFC9114 7.2.8)"),
	unknown_then_headers(Config, do_reserved_type(),
		rand:bytes(rand:uniform(4096))).

headers_then_reserved(Config) ->
	doc("Receipt of HEADERS followed by reserved frame "
		"must be accepted when the reserved frame type is "
		"of the format 0x1f * N + 0x21. (RFC9114 4.1, RFC9114 7.2.8)"),
	headers_then_unknown(Config, do_reserved_type(),
		rand:bytes(rand:uniform(4096))).

headers_then_data_then_reserved(Config) ->
	doc("Receipt of HEADERS followed by DATA followed by reserved frame "
		"must be accepted when the reserved frame type is "
		"of the format 0x1f * N + 0x21. (RFC9114 4.1, RFC9114 7.2.8)"),
	headers_then_data_then_unknown(Config, do_reserved_type(),
		rand:bytes(rand:uniform(4096))).

headers_then_trailers_then_reserved(Config) ->
	doc("Receipt of HEADERS followed by trailer HEADERS followed by reserved frame "
		"must be accepted when the reserved frame type is "
		"of the format 0x1f * N + 0x21. (RFC9114 4.1, RFC9114 7.2.8)"),
	headers_then_trailers_then_unknown(Config, do_reserved_type(),
		rand:bytes(rand:uniform(4096))).

headers_then_data_then_reserved_then_trailers(Config) ->
	doc("Receipt of HEADERS followed by DATA followed by "
		"reserved frame followed by trailer HEADERS "
		"must be accepted when the reserved frame type is "
		"of the format 0x1f * N + 0x21. (RFC9114 4.1, RFC9114 7.2.8)"),
	headers_then_data_then_unknown_then_trailers(Config,
		do_reserved_type(), rand:bytes(rand:uniform(4096))).

headers_then_data_then_reserved_then_data(Config) ->
	doc("Receipt of HEADERS followed by DATA followed by "
		"reserved frame followed by DATA "
		"must be accepted when the reserved frame type is "
		"of the format 0x1f * N + 0x21. (RFC9114 4.1, RFC9114 7.2.8)"),
	headers_then_data_then_unknown_then_data(Config,
		do_reserved_type(), rand:bytes(rand:uniform(4096))).

headers_then_data_then_trailers_then_reserved(Config) ->
	doc("Receipt of HEADERS followed by DATA followed by "
		"trailer HEADERS followed by reserved frame "
		"must be accepted when the reserved frame type is "
		"of the format 0x1f * N + 0x21. (RFC9114 4.1, RFC9114 7.2.8)"),
	headers_then_data_then_trailers_then_unknown(Config,
		do_reserved_type(), rand:bytes(rand:uniform(4096))).

reject_transfer_encoding_header_with_body(Config) ->
	doc("Requests containing a transfer-encoding header must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.1, RFC9114 4.1.2, RFC9114 4.2)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData1, _EncSt0} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"transfer-encoding">>, <<"chunked">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(24),
		<<"13\r\nHello server!\r\n0\r\n\r\n">>
	]),
	%% The stream should have been aborted.
	#{reason := h3_message_error} = do_wait_stream_aborted(StreamRef),
	ok.

%% 4. Expressing HTTP Semantics in HTTP/3
%% 4.1. HTTP Message Framing

%% An HTTP request/response exchange fully consumes a client-initiated
%bidirectional QUIC stream. After sending a request, a client MUST close the
%stream for sending. Unless using the CONNECT method (see Section 4.4), clients
%MUST NOT make stream closure dependent on receiving a response to their
%request. After sending a final response, the server MUST close the stream for
%sending. At this point, the QUIC stream is fully closed.
%% @todo What to do with clients that DON'T close the stream
%%       for sending after the request is sent?

%% If a client-initiated stream terminates without enough of the HTTP message
%to provide a complete response, the server SHOULD abort its response stream
%with the error code H3_REQUEST_INCOMPLETE.
%% @todo difficult!!

%% When the server does not need to receive the remainder of the request, it
%MAY abort reading the request stream, send a complete response, and cleanly
%close the sending part of the stream. The error code H3_NO_ERROR SHOULD be
%used when requesting that the client stop sending on the request stream.
%% @todo read_body related; h2 has this behavior but there is no corresponding test

%% 4.1.1. Request Cancellation and Rejection

%% When possible, it is RECOMMENDED that servers send an HTTP response with an
%appropriate status code rather than cancelling a request it has already begun
%processing.

%% Implementations SHOULD cancel requests by abruptly terminating any
%directions of a stream that are still open. To do so, an implementation resets
%the sending parts of streams and aborts reading on the receiving parts of
%streams; see Section 2.4 of [QUIC-TRANSPORT].

%% When the server cancels a request without performing any application
%processing, the request is considered "rejected". The server SHOULD abort its
%response stream with the error code H3_REQUEST_REJECTED. In this context,
%"processed" means that some data from the stream was passed to some higher
%layer of software that might have taken some action as a result. The client
%can treat requests rejected by the server as though they had never been sent
%at all, thereby allowing them to be retried later.

%% Servers MUST NOT use the H3_REQUEST_REJECTED error code for requests that
%were partially or fully processed. When a server abandons a response after
%partial processing, it SHOULD abort its response stream with the error code
%H3_REQUEST_CANCELLED.
%% @todo

%% Client SHOULD use the error code H3_REQUEST_CANCELLED to cancel requests.
%Upon receipt of this error code, a server MAY abruptly terminate the response
%using the error code H3_REQUEST_REJECTED if no processing was performed.
%Clients MUST NOT use the H3_REQUEST_REJECTED error code, except when a server
%has requested closure of the request stream with this error code.
%% @todo

%4.1.2. Malformed Requests and Responses
%A malformed request or response is one that is an otherwise valid sequence of
%frames but is invalid due to:
%
%the presence of prohibited fields or pseudo-header fields,
%% @todo reject_response_pseudo_headers
%% @todo reject_unknown_pseudo_headers
%% @todo reject_pseudo_headers_in_trailers

%the absence of mandatory pseudo-header fields,
%invalid values for pseudo-header fields,
%pseudo-header fields after fields,
%% @todo reject_pseudo_headers_after_regular_headers

%an invalid sequence of HTTP messages,
%the inclusion of invalid characters in field names or values.
%
%A request or response that is defined as having content when it contains a
%Content-Length header field (Section 8.6 of [HTTP]) is malformed if the value
%of the Content-Length header field does not equal the sum of the DATA frame
%lengths received. A response that is defined as never having content, even
%when a Content-Length is present, can have a non-zero Content-Length header
%field even though no content is included in DATA frames.
%
%For malformed requests, a server MAY send an HTTP response indicating the
%error prior to closing or resetting the stream.
%% @todo All the malformed tests

headers_reject_uppercase_header_name(Config) ->
	doc("Requests containing uppercase header names must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.2, RFC9114 4.1.2)"),
	do_reject_malformed_header(Config,
		{<<"I-AM-GIGANTIC">>, <<"How's the weather up there?">>}
	).

%% 4.2. HTTP Fields
%% An endpoint MUST NOT generate an HTTP/3 field section containing
%connection-specific fields; any message containing connection-specific fields
%MUST be treated as malformed.

reject_connection_header(Config) ->
	doc("Requests containing a connection header must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.2, RFC9114 4.1.2)"),
	do_reject_malformed_header(Config,
		{<<"connection">>, <<"close">>}
	).

reject_keep_alive_header(Config) ->
	doc("Requests containing a keep-alive header must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.2, RFC9114 4.1.2)"),
	do_reject_malformed_header(Config,
		{<<"keep-alive">>, <<"timeout=5, max=1000">>}
	).

reject_proxy_authenticate_header(Config) ->
	doc("Requests containing a proxy-authenticate header must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.2, RFC9114 4.1.2)"),
	do_reject_malformed_header(Config,
		{<<"proxy-authenticate">>, <<"Basic">>}
	).

reject_proxy_authorization_header(Config) ->
	doc("Requests containing a proxy-authorization header must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.2, RFC9114 4.1.2)"),
	do_reject_malformed_header(Config,
		{<<"proxy-authorization">>, <<"Basic YWxhZGRpbjpvcGVuc2VzYW1l">>}
	).

reject_transfer_encoding_header(Config) ->
	doc("Requests containing a transfer-encoding header must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.2, RFC9114 4.1.2)"),
	do_reject_malformed_header(Config,
		{<<"transfer-encoding">>, <<"chunked">>}
	).

reject_upgrade_header(Config) ->
	doc("Requests containing an upgrade header must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.2, RFC9114 4.5, RFC9114 4.1.2)"),
	do_reject_malformed_header(Config,
		{<<"upgrade">>, <<"websocket">>}
	).

accept_te_header_value_trailers(Config) ->
	doc("Requests containing a TE header with a value of \"trailers\" "
		"must be accepted. (RFC9114 4.2)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData1, EncSt0} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>},
		{<<"te">>, <<"trailers">>}
	], 0, cow_qpack:init(encoder)),
	{ok, EncodedTrailers, _EncData2, _EncSt} = cow_qpack:encode_field_section([
		{<<"content-type">>, <<"text/plain">>}
	], 0, EncSt0),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<1>>, %% HEADERS frame for trailers.
		cow_http3:encode_int(iolist_size(EncodedTrailers)),
		EncodedTrailers
	], ?QUIC_SEND_FLAG_FIN),
	#{
		headers := #{<<":status">> := <<"200">>},
		body := <<"Hello world!">>
	} = do_receive_response(StreamRef),
	ok.

reject_te_header_other_values(Config) ->
	doc("Requests containing a TE header with a value other than \"trailers\" must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.2, RFC9114 4.1.2)"),
	do_reject_malformed_header(Config,
		{<<"te">>, <<"trailers, deflate;q=0.5">>}
	).

%% @todo response_dont_send_header_in_connection
%% @todo response_dont_send_connection_header
%% @todo response_dont_send_keep_alive_header
%% @todo response_dont_send_proxy_connection_header
%% @todo response_dont_send_transfer_encoding_header
%% @todo response_dont_send_upgrade_header

%% 4.2.1. Field Compression
%% To allow for better compression efficiency, the Cookie header field
%([COOKIES]) MAY be split into separate field lines, each with one or more
%cookie-pairs, before compression. If a decompressed field section contains
%multiple cookie field lines, these MUST be concatenated into a single byte
%string using the two-byte delimiter of "; " (ASCII 0x3b, 0x20) before being
%passed into a context other than HTTP/2 or HTTP/3, such as an HTTP/1.1
%connection, or a generic HTTP server application.

%% 4.2.2. Header Size Constraints
%% An HTTP/3 implementation MAY impose a limit on the maximum size of the
%message header it will accept on an individual HTTP message. A server that
%receives a larger header section than it is willing to handle can send an HTTP
%431 (Request Header Fields Too Large) status code ([RFC6585]). The size of a
%field list is calculated based on the uncompressed size of fields, including
%the length of the name and value in bytes plus an overhead of 32 bytes for
%each field.
%% If an implementation wishes to advise its peer of this limit, it can be
%conveyed as a number of bytes in the SETTINGS_MAX_FIELD_SECTION_SIZE
%parameter. 

reject_unknown_pseudo_headers(Config) ->
	doc("Requests containing unknown pseudo-headers must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.3, RFC9114 4.1.2)"),
	do_reject_malformed_header(Config,
		{<<":upgrade">>, <<"websocket">>}
	).

reject_response_pseudo_headers(Config) ->
	doc("Requests containing response pseudo-headers must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.3, RFC9114 4.1.2)"),
	do_reject_malformed_header(Config,
		{<<":status">>, <<"200">>}
	).

reject_pseudo_headers_in_trailers(Config) ->
	doc("Requests containing pseudo-headers in trailers must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.3, RFC9114 4.1.2)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData1, EncSt0} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"trailer">>, <<"x-checksum">>}
	], 0, cow_qpack:init(encoder)),
	{ok, EncodedTrailers, _EncData2, _EncSt} = cow_qpack:encode_field_section([
		{<<"x-checksum">>, <<"md5:4cc909a007407f3706399b6496babec3">>},
		{<<":path">>, <<"/">>}
	], 0, EncSt0),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(10000),
		<<0:10000/unit:8>>,
		<<1>>, %% HEADERS frame for trailers.
		cow_http3:encode_int(iolist_size(EncodedTrailers)),
		EncodedTrailers
	]),
	%% The stream should have been aborted.
	#{reason := h3_message_error} = do_wait_stream_aborted(StreamRef),
	ok.

reject_pseudo_headers_after_regular_headers(Config) ->
	doc("Requests containing pseudo-headers after regular headers must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.3, RFC9114 4.1.2)"),
	do_reject_malformed_headers(Config, [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<"content-length">>, <<"0">>},
		{<<":path">>, <<"/">>}
	]).

reject_userinfo(Config) ->
	doc("An authority containing a userinfo component must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.3.1, RFC9114 4.1.2)"),
	do_reject_malformed_headers(Config, [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"user@localhost">>},
		{<<":path">>, <<"/">>}
	]).

%% To ensure that the HTTP/1.1 request line can be reproduced accurately, this
%% pseudo-header field (:authority) MUST be omitted when translating from an
%% HTTP/1.1 request that has a request target in a method-specific form;
%% see Section 7.1 of [HTTP]. 

reject_empty_path(Config) ->
	doc("A request containing an empty path component must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.3.1, RFC9114 4.1.2)"),
	do_reject_malformed_headers(Config, [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<>>}
	]).

reject_missing_pseudo_header_method(Config) ->
	doc("A request without a method component must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.3.1, RFC9114 4.1.2)"),
	do_reject_malformed_headers(Config, [
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>}
	]).

reject_many_pseudo_header_method(Config) ->
	doc("A request containing more than one method component must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.3.1, RFC9114 4.1.2)"),
	do_reject_malformed_headers(Config, [
		{<<":method">>, <<"GET">>},
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>}
	]).

reject_missing_pseudo_header_scheme(Config) ->
	doc("A request without a scheme component must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.3.1, RFC9114 4.1.2)"),
	do_reject_malformed_headers(Config, [
		{<<":method">>, <<"GET">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>}
	]).

reject_many_pseudo_header_scheme(Config) ->
	doc("A request containing more than one scheme component must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.3.1, RFC9114 4.1.2)"),
	do_reject_malformed_headers(Config, [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>}
	]).

reject_missing_pseudo_header_authority(Config) ->
	doc("A request without an authority or host component must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.3.1, RFC9114 4.1.2)"),
	do_reject_malformed_headers(Config, [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":path">>, <<"/">>}
	]).

accept_host_header_on_missing_pseudo_header_authority(Config) ->
	doc("A request without an authority but with a host header must be accepted. "
		"(RFC9114 4.3.1)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData1, _EncSt0} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/">>},
		{<<"host">>, <<"localhost">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders
	], ?QUIC_SEND_FLAG_FIN),
	#{
		headers := #{<<":status">> := <<"200">>},
		body := <<"Hello world!">>
	} = do_receive_response(StreamRef),
	ok.

%% @todo
%% If the :scheme pseudo-header field identifies a scheme that has a mandatory
%% authority component (including "http" and "https"), the request MUST contain
%% either an :authority pseudo-header field or a Host header field.
%%  - If both fields are present, they MUST NOT be empty.
%%  - If both fields are present, they MUST contain the same value. 

reject_many_pseudo_header_authority(Config) ->
	doc("A request containing more than one authority component must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.3.1, RFC9114 4.1.2)"),
	do_reject_malformed_headers(Config, [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>}
	]).

reject_missing_pseudo_header_path(Config) ->
	doc("A request without a path component must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.3.1, RFC9114 4.1.2)"),
	do_reject_malformed_headers(Config, [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}
	]).

reject_many_pseudo_header_path(Config) ->
	doc("A request containing more than one path component must be rejected "
		"with an H3_MESSAGE_ERROR stream error. (RFC9114 4.3.1, RFC9114 4.1.2)"),
	do_reject_malformed_headers(Config, [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<":path">>, <<"/">>}
	]).

do_reject_malformed_header(Config, Header) ->
	do_reject_malformed_headers(Config, [
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		Header
	]).

do_reject_malformed_headers(Config, Headers) ->
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData1, _EncSt0}
		= cow_qpack:encode_field_section(Headers, 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders
	]),
	%% The stream should have been aborted.
	#{reason := h3_message_error} = do_wait_stream_aborted(StreamRef),
	ok.

%% For responses, a single ":status" pseudo-header field is defined that
%% carries the HTTP status code; see Section 15 of [HTTP]. This pseudo-header
%% field MUST be included in all responses; otherwise, the response is malformed
%% (see Section 4.1.2).

%% @todo Implement CONNECT. (RFC9114 4.4. The CONNECT Method)

%% @todo Maybe block the sending of 101 responses? (RFC9114 4.5. HTTP Upgrade) - also HTTP/2.

%% @todo Implement server push (RFC9114 4.6. Server Push)

%% @todo - need a way to list connections
%% 5.2. Connection Shutdown
%% Endpoints initiate the graceful shutdown of an HTTP/3 connection by sending
%% a GOAWAY frame. The GOAWAY frame contains an identifier that indicates to the
%% receiver the range of requests or pushes that were or might be processed in
%% this connection. The server sends a client-initiated bidirectional stream ID;
%% the client sends a push ID. Requests or pushes with the indicated identifier
%% or greater are rejected (Section 4.1.1) by the sender of the GOAWAY. This
%% identifier MAY be zero if no requests or pushes were processed.

%% @todo
%% Upon sending a GOAWAY frame, the endpoint SHOULD explicitly cancel (see
%% Sections 4.1.1 and 7.2.3) any requests or pushes that have identifiers greater
%% than or equal to the one indicated, in order to clean up transport state for
%% the affected streams. The endpoint SHOULD continue to do so as more requests
%% or pushes arrive.

%% @todo
%% Endpoints MUST NOT initiate new requests or promise new pushes on the
%% connection after receipt of a GOAWAY frame from the peer.

%% @todo
%% Requests on stream IDs less than the stream ID in a GOAWAY frame from the
%% server might have been processed; their status cannot be known until a
%% response is received, the stream is reset individually, another GOAWAY is
%% received with a lower stream ID than that of the request in question, or the
%% connection terminates.

%% @todo
%% Servers MAY reject individual requests on streams below the indicated ID if
%% these requests were not processed.

%% @todo
%% If a server receives a GOAWAY frame after having promised pushes with a push
%% ID greater than or equal to the identifier contained in the GOAWAY frame,
%% those pushes will not be accepted.

%% @todo
%% Servers SHOULD send a GOAWAY frame when the closing of a connection is known
%% in advance, even if the advance notice is small, so that the remote peer can
%% know whether or not a request has been partially processed.

%% @todo
%% An endpoint MAY send multiple GOAWAY frames indicating different
%% identifiers, but the identifier in each frame MUST NOT be greater than the
%% identifier in any previous frame, since clients might already have retried
%% unprocessed requests on another HTTP connection. Receiving a GOAWAY containing
%% a larger identifier than previously received MUST be treated as a connection
%% error of type H3_ID_ERROR.

%% @todo
%% An endpoint that is attempting to gracefully shut down a connection can send
%% a GOAWAY frame with a value set to the maximum possible value (2^62-4 for
%% servers, 2^62-1 for clients).

%% @todo
%% Even when a GOAWAY indicates that a given request or push will not be
%% processed or accepted upon receipt, the underlying transport resources still
%% exist. The endpoint that initiated these requests can cancel them to clean up
%% transport state.

%% @todo
%% Once all accepted requests and pushes have been processed, the endpoint can
%% permit the connection to become idle, or it MAY initiate an immediate closure
%% of the connection. An endpoint that completes a graceful shutdown SHOULD use
%% the H3_NO_ERROR error code when closing the connection.

%% @todo
%% If a client has consumed all available bidirectional stream IDs with
%% requests, the server need not send a GOAWAY frame, since the client is unable
%% to make further requests. @todo OK that one's some weird stuff lol

%% @todo
%% 5.3. Immediate Application Closure
%% Before closing the connection, a GOAWAY frame MAY be sent to allow the
%% client to retry some requests. Including the GOAWAY frame in the same packet
%% as the QUIC CONNECTION_CLOSE frame improves the chances of the frame being
%% received by clients.

bidi_allow_at_least_a_hundred(Config) ->
	doc("Endpoints must allow the peer to create at least "
		"one hundred bidirectional streams. (RFC9114 6.1"),
	#{conn := Conn} = do_connect(Config),
	receive
		{quic, streams_available, Conn, #{bidi_streams := NumStreams}} ->
			true = NumStreams >= 100,
			ok
	after 5000 ->
		error(timeout)
	end.

unidi_allow_at_least_three(Config) ->
	doc("Endpoints must allow the peer to create at least "
		"three unidirectional streams. (RFC9114 6.2"),
	#{conn := Conn} = do_connect(Config),
	%% Confirm that the server advertised support for at least 3 unidi streams.
	receive
		{quic, streams_available, Conn, #{unidi_streams := NumStreams}} ->
			true = NumStreams >= 3,
			ok
	after 5000 ->
		error(timeout)
	end,
	%% Confirm that we can create the unidi streams.
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(ControlRef, [<<0>>, SettingsBin]),
	{ok, EncoderRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(EncoderRef, <<2>>),
	{ok, DecoderRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(DecoderRef, <<3>>),
	%% Streams shouldn't get closed.
	fun Loop() ->
		receive
			%% We don't care about these messages.
			{quic, dgram_state_changed, Conn, _} ->
				Loop();
			{quic, peer_needs_streams, Conn, _} ->
				Loop();
			%% Any other we do care.
			Msg ->
				error(Msg)
		after 1000 ->
			ok
		end
	end().

unidi_create_critical_first(Config) ->
	doc("Endpoints should create the HTTP control stream as well as "
		"the QPACK encoder and decoder streams first. (RFC9114 6.2"),
	%% The control stream is accepted in the do_connect/1 function.
	#{conn := Conn} = do_connect(Config, #{peer_unidi_stream_count => 3}),
	Unidi1 = do_accept_qpack_stream(Conn),
	Unidi2 = do_accept_qpack_stream(Conn),
	case {Unidi1, Unidi2} of
		{{encoder, _}, {decoder, _}} ->
			ok;
		{{decoder, _}, {encoder, _}} ->
			ok
	end.

do_accept_qpack_stream(Conn) ->
	receive
		{quic, new_stream, StreamRef, #{flags := Flags}} ->
			ok = quicer:setopt(StreamRef, active, true),
			true = quicer:is_unidirectional(Flags),
			receive {quic, <<Type>>, StreamRef, _} ->
				{case Type of
					2 -> encoder;
					3 -> decoder
				end, StreamRef}
			after 5000 ->
				error(timeout)
			end
	after 5000 ->
		error(timeout)
	end.

%% @todo We should also confirm that there's at least 1,024 bytes of
%%       flow-control credit for each unidi stream the server creates. (How?)
%%       It can be set via stream_recv_window_default in quicer.

unidi_abort_unknown_type(Config) ->
	doc("Receipt of an unknown stream type must be aborted "
		"with an H3_STREAM_CREATION_ERROR stream error. (RFC9114 6.2, RFC9114 9)"),
	#{conn := Conn} = do_connect(Config),
	%% Create an unknown unidirectional stream.
	{ok, StreamRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(StreamRef, [
		cow_http3:encode_int(1 + do_reserved_type()),
		rand:bytes(rand:uniform(4096))
	]),
	%% The stream should have been aborted.
	#{reason := h3_stream_creation_error} = do_wait_stream_aborted(StreamRef),
	ok.

unidi_abort_reserved_type(Config) ->
	doc("Receipt of a reserved stream type must be aborted "
		"with an H3_STREAM_CREATION_ERROR stream error. "
		"(RFC9114 6.2, RFC9114 6.2.3, RFC9114 9)"),
	#{conn := Conn} = do_connect(Config),
	%% Create a reserved unidirectional stream.
	{ok, StreamRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(StreamRef, [
		cow_http3:encode_int(do_reserved_type()),
		rand:bytes(rand:uniform(4096))
	]),
	%% The stream should have been aborted.
	#{reason := h3_stream_creation_error} = do_wait_stream_aborted(StreamRef),
	ok.

%% As certain stream types can affect connection state, a recipient SHOULD NOT
%% discard data from incoming unidirectional streams prior to reading the stream type.

%% Implementations MAY send stream types before knowing whether the peer
%supports them. However, stream types that could modify the state or semantics
%of existing protocol components, including QPACK or other extensions, MUST NOT
%be sent until the peer is known to support them.
%% @todo It may make sense for Cowboy to delay the creation of unidi streams
%%       a little in order to save resources. We could create them when the
%%       client does as well, or something similar.

%% A receiver MUST tolerate unidirectional streams being closed or reset prior
%% to the reception of the unidirectional stream header.

%% Each side MUST initiate a single control stream at the beginning of the
%% connection and send its SETTINGS frame as the first frame on this stream.
%% @todo What to do when the client never opens a control stream?
%% @todo Similarly, a stream could be opened but with no data being sent.
%% @todo Similarly, a control stream could be opened with no SETTINGS frame sent.

control_reject_first_frame_data(Config) ->
	doc("The first frame on a control stream must be a SETTINGS frame "
		"or the connection must be closed with an H3_MISSING_SETTINGS "
		"connection error. (RFC9114 6.2.1, RFC9114 9)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		<<0>>, %% DATA frame.
		cow_http3:encode_int(12),
		<<"Hello world!">>
	]),
	%% The connection should have been closed.
	#{reason := h3_missing_settings} = do_wait_connection_closed(Conn),
	ok.

control_reject_first_frame_headers(Config) ->
	doc("The first frame on a control stream must be a SETTINGS frame "
		"or the connection must be closed with an H3_MISSING_SETTINGS "
		"connection error. (RFC9114 6.2.1, RFC9114 9)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, EncodedHeaders, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders
	]),
	%% The connection should have been closed.
	#{reason := h3_missing_settings} = do_wait_connection_closed(Conn),
	ok.

control_reject_first_frame_cancel_push(Config) ->
	doc("The first frame on a control stream must be a SETTINGS frame "
		"or the connection must be closed with an H3_MISSING_SETTINGS "
		"connection error. (RFC9114 6.2.1, RFC9114 9)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		<<3>>, %% CANCEL_PUSH frame.
		cow_http3:encode_int(1),
		cow_http3:encode_int(0)
	]),
	%% The connection should have been closed.
	#{reason := h3_missing_settings} = do_wait_connection_closed(Conn),
	ok.

control_accept_first_frame_settings(Config) ->
	doc("The first frame on a control stream "
		"must be a SETTINGS frame. (RFC9114 6.2.1, RFC9114 9)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		SettingsBin
	]),
	%% The connection should remain up.
	receive
		{quic, shutdown, Conn, {unknown_quic_status, Code}} ->
			Reason = cow_http3:code_to_error(Code),
			error(Reason)
	after 1000 ->
		ok
	end.

control_reject_first_frame_push_promise(Config) ->
	doc("The first frame on a control stream must be a SETTINGS frame "
		"or the connection must be closed with an H3_MISSING_SETTINGS "
		"connection error. (RFC9114 6.2.1, RFC9114 9)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, EncodedHeaders, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>}
	], 0, cow_qpack:init(encoder)),

	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		<<5>>, %% PUSH_PROMISE frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders) + 1),
		cow_http3:encode_int(0),
		EncodedHeaders
	]),
	%% The connection should have been closed.
	#{reason := h3_missing_settings} = do_wait_connection_closed(Conn),
	ok.

control_reject_first_frame_goaway(Config) ->
	doc("The first frame on a control stream must be a SETTINGS frame "
		"or the connection must be closed with an H3_MISSING_SETTINGS "
		"connection error. (RFC9114 6.2.1, RFC9114 9)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		<<7>>, %% GOAWAY frame.
		cow_http3:encode_int(1),
		cow_http3:encode_int(0)
	]),
	%% The connection should have been closed.
	#{reason := h3_missing_settings} = do_wait_connection_closed(Conn),
	ok.

control_reject_first_frame_max_push_id(Config) ->
	doc("The first frame on a control stream must be a SETTINGS frame "
		"or the connection must be closed with an H3_MISSING_SETTINGS "
		"connection error. (RFC9114 6.2.1, RFC9114 9)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		<<13>>, %% MAX_PUSH_ID frame.
		cow_http3:encode_int(1),
		cow_http3:encode_int(0)
	]),
	%% The connection should have been closed.
	#{reason := h3_missing_settings} = do_wait_connection_closed(Conn),
	ok.

control_reject_first_frame_reserved(Config) ->
	doc("The first frame on a control stream must be a SETTINGS frame "
		"or the connection must be closed with an H3_MISSING_SETTINGS "
		"connection error. (RFC9114 6.2.1, RFC9114 9)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	Len = rand:uniform(512),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		cow_http3:encode_int(do_reserved_type()),
		cow_http3:encode_int(Len),
		rand:bytes(Len)
	]),
	%% The connection should have been closed.
	#{reason := h3_missing_settings} = do_wait_connection_closed(Conn),
	ok.

control_reject_multiple(Config) ->
	doc("Endpoints must not create multiple control streams. (RFC9114 6.2.1)"),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	do_critical_reject_multiple(Config, [<<0>>, SettingsBin]).

do_critical_reject_multiple(Config, HeaderData) ->
	#{conn := Conn} = do_connect(Config),
	%% Create two critical streams.
	{ok, StreamRef1} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(StreamRef1, HeaderData),
	{ok, StreamRef2} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(StreamRef2, HeaderData),
	%% The connection should have been closed.
	#{reason := h3_stream_creation_error} = do_wait_connection_closed(Conn),
	ok.

control_local_closed_abort(Config) ->
	doc("Endpoints must not close the control stream. (RFC9114 6.2.1)"),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	do_critical_local_closed_abort(Config, [<<0>>, SettingsBin]).

do_critical_local_closed_abort(Config, HeaderData) ->
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(StreamRef, HeaderData),
	%% Wait a little to make sure the stream data was received before we abort.
	timer:sleep(100),
	%% Close the critical stream.
	quicer:async_shutdown_stream(StreamRef, ?QUIC_STREAM_SHUTDOWN_FLAG_ABORT, 0),
	%% The connection should have been closed.
	timer:sleep(1000),
	#{reason := h3_closed_critical_stream} = do_wait_connection_closed(Conn),
	ok.

control_local_closed_graceful(Config) ->
	doc("Endpoints must not close the control stream. (RFC9114 6.2.1)"),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	do_critical_local_closed_graceful(Config, [<<0>>, SettingsBin]).

do_critical_local_closed_graceful(Config, HeaderData) ->
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(StreamRef, HeaderData),
	%% Close the critical stream.
	quicer:async_shutdown_stream(StreamRef, ?QUIC_STREAM_SHUTDOWN_FLAG_GRACEFUL, 0),
	%% The connection should have been closed.
	#{reason := h3_closed_critical_stream} = do_wait_connection_closed(Conn),
	ok.

control_remote_closed_abort(Config) ->
	doc("Endpoints must not close the control stream. (RFC9114 6.2.1)"),
	#{conn := Conn, control := ControlRef} = do_connect(Config),
	%% Close the control stream.
	quicer:async_shutdown_stream(ControlRef, ?QUIC_STREAM_SHUTDOWN_FLAG_ABORT, 0),
	%% The connection should have been closed.
	#{reason := h3_closed_critical_stream} = do_wait_connection_closed(Conn),
	ok.

%% We cannot gracefully shutdown a remote unidi stream; only abort reading.

%% Because the contents of the control stream are used to manage the behavior
%% of other streams, endpoints SHOULD provide enough flow-control credit to keep
%% the peer's control stream from becoming blocked.

%% @todo Implement server push (RFC9114 6.2.2 Push Streams)

data_frame_can_span_multiple_packets(Config) ->
	doc("HTTP/3 frames can span multiple packets. (RFC9114 7)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/echo/read_body">>},
		{<<"content-length">>, <<"13">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(13),
		<<"Hello ">>
	]),
	timer:sleep(100),
	{ok, _} = quicer:send(StreamRef, [
		<<"server!">>
	], ?QUIC_SEND_FLAG_FIN),
	#{
		headers := #{<<":status">> := <<"200">>},
		body := <<"Hello server!">>
	} = do_receive_response(StreamRef),
	ok.

headers_frame_can_span_multiple_packets(Config) ->
	doc("HTTP/3 frames can span multiple packets. (RFC9114 7)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>}
	], 0, cow_qpack:init(encoder)),
	Half = iolist_size(EncodedHeaders) div 2,
	<<EncodedHeadersPart1:Half/binary, EncodedHeadersPart2/bits>>
		= iolist_to_binary(EncodedHeaders),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeadersPart1
	]),
	timer:sleep(100),
	{ok, _} = quicer:send(StreamRef, [
		EncodedHeadersPart2
	]),
	#{
		headers := #{<<":status">> := <<"200">>},
		body := <<"Hello world!">>
	} = do_receive_response(StreamRef),
	ok.

%% @todo Implement server push. cancel_push_frame_can_span_multiple_packets(Config) ->

settings_frame_can_span_multiple_packets(Config) ->
	doc("HTTP/3 frames can span multiple packets. (RFC9114 7)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	<<SettingsPart1:1/binary, SettingsPart2/bits>> = SettingsBin,
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		SettingsPart1
	]),
	timer:sleep(100),
	{ok, _} = quicer:send(ControlRef, [
		SettingsPart2
	]),
	%% The connection should remain up.
	receive
		{quic, shutdown, Conn, {unknown_quic_status, Code}} ->
			Reason = cow_http3:code_to_error(Code),
			error(Reason)
	after 1000 ->
		ok
	end.

goaway_frame_can_span_multiple_packets(Config) ->
	doc("HTTP/3 frames can span multiple packets. (RFC9114 7)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		SettingsBin,
		<<7>>, cow_http3:encode_int(1) %% GOAWAY part 1.
	]),
	timer:sleep(100),
	{ok, _} = quicer:send(ControlRef, [
		cow_http3:encode_int(0) %% GOAWAY part 2.
	]),
	%% The connection should be closed gracefully.
	receive
		{quic, shutdown, Conn, {unknown_quic_status, Code}} ->
			h3_no_error = cow_http3:code_to_error(Code),
			ok;
		%% @todo Temporarily also accept this message. I am
		%%       not sure why it happens but it isn't wrong per se.
		{quic, shutdown, Conn, success} ->
			ok
	after 1000 ->
		error(timeout)
	end.

max_push_id_frame_can_span_multiple_packets(Config) ->
	doc("HTTP/3 frames can span multiple packets. (RFC9114 7)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		SettingsBin,
		<<13>>, cow_http3:encode_int(1) %% MAX_PUSH_ID part 1.
	]),
	timer:sleep(100),
	{ok, _} = quicer:send(ControlRef, [
		cow_http3:encode_int(0) %% MAX_PUSH_ID part 2.
	]),
	%% The connection should remain up.
	receive
		{quic, shutdown, Conn, {unknown_quic_status, Code}} ->
			Reason = cow_http3:code_to_error(Code),
			error(Reason)
	after 1000 ->
		ok
	end.

unknown_frame_can_span_multiple_packets(Config) ->
	doc("HTTP/3 frames can span multiple packets. (RFC9114 7)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(StreamRef, [
		cow_http3:encode_int(do_unknown_frame_type()),
		cow_http3:encode_int(16383)
	]),
	timer:sleep(100),
	{ok, _} = quicer:send(StreamRef, rand:bytes(4096)),
	timer:sleep(100),
	{ok, _} = quicer:send(StreamRef, rand:bytes(4096)),
	timer:sleep(100),
	{ok, _} = quicer:send(StreamRef, rand:bytes(4096)),
	timer:sleep(100),
	{ok, _} = quicer:send(StreamRef, rand:bytes(4095)),
	{ok, EncodedHeaders, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders
	], ?QUIC_SEND_FLAG_FIN),
	#{
		headers := #{<<":status">> := <<"200">>},
		body := <<"Hello world!">>
	} = do_receive_response(StreamRef),
	ok.

%% The DATA and SETTINGS frames can be zero-length therefore
%% they cannot be too short.

headers_frame_too_short(Config) ->
	doc("Frames that terminate before the end of identified fields "
		"must be rejected with an H3_FRAME_ERROR connection error. "
		"(RFC9114 7.1, RFC9114 10.8)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(0)
	]),
	%% The connection should have been closed.
	#{reason := h3_frame_error} = do_wait_connection_closed(Conn),
	ok.

%% @todo Implement server push. cancel_push_frame_too_short(Config) ->

goaway_frame_too_short(Config) ->
	doc("Frames that terminate before the end of identified fields "
		"must be rejected with an H3_FRAME_ERROR connection error. "
		"(RFC9114 7.1, RFC9114 10.8)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		SettingsBin,
		<<7>>, cow_http3:encode_int(0) %% GOAWAY.
	]),
	%% The connection should have been closed.
	#{reason := h3_frame_error} = do_wait_connection_closed(Conn),
	ok.

max_push_id_frame_too_short(Config) ->
	doc("Frames that terminate before the end of identified fields "
		"must be rejected with an H3_FRAME_ERROR connection error. "
		"(RFC9114 7.1, RFC9114 10.8)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		SettingsBin,
		<<13>>, cow_http3:encode_int(0) %% MAX_PUSH_ID.
	]),
	%% The connection should have been closed.
	#{reason := h3_frame_error} = do_wait_connection_closed(Conn),
	ok.

data_frame_truncated(Config) ->
	doc("Truncated frames must be rejected with an "
		"H3_FRAME_ERROR connection error. (RFC9114 7.1, RFC9114 10.8)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/echo/read_body">>},
		{<<"content-length">>, <<"13">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(13),
		<<"Hello ">>
	], ?QUIC_SEND_FLAG_FIN),
	%% The connection should have been closed.
	#{reason := h3_frame_error} = do_wait_connection_closed(Conn),
	ok.

headers_frame_truncated(Config) ->
	doc("Truncated frames must be rejected with an "
		"H3_FRAME_ERROR connection error. (RFC9114 7.1, RFC9114 10.8)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders))
	], ?QUIC_SEND_FLAG_FIN),
	%% The connection should have been closed.
	#{reason := h3_frame_error} = do_wait_connection_closed(Conn),
	ok.

%% I am not sure how to test truncated CANCEL_PUSH, SETTINGS, GOAWAY
%% or MAX_PUSH_ID frames, as those are sent on the control stream,
%% which we cannot terminate.

%% The DATA, HEADERS and SETTINGS frames can be of any length
%% therefore they cannot be too long per se, even if unwanted
%% data can be included at the end of the frame's payload.

%% @todo Implement server push. cancel_push_frame_too_long(Config) ->

goaway_frame_too_long(Config) ->
	doc("Frames that contain additional bytes after the end of identified fields "
		"must be rejected with an H3_FRAME_ERROR connection error. "
		"(RFC9114 7.1, RFC9114 10.8)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		SettingsBin,
		<<7>>, cow_http3:encode_int(3), %% GOAWAY.
		<<0, 1, 2>>
	]),
	%% The connection should have been closed.
	#{reason := h3_frame_error} = do_wait_connection_closed(Conn),
	ok.

max_push_id_frame_too_long(Config) ->
	doc("Frames that contain additional bytes after the end of identified fields "
		"must be rejected with an H3_FRAME_ERROR connection error. "
		"(RFC9114 7.1, RFC9114 10.8)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		SettingsBin,
		<<13>>, cow_http3:encode_int(9), %% MAX_PUSH_ID.
		<<0, 1, 2, 3, 4, 5, 6, 7, 8>>
	]),
	%% The connection should have been closed.
	#{reason := h3_frame_error} = do_wait_connection_closed(Conn),
	ok.

%% Streams may terminate abruptly in the middle of frames.

data_frame_rejected_on_control_stream(Config) ->
	doc("DATA frames received on the control stream must be rejected "
		"with an H3_FRAME_UNEXPECTED connection error. (RFC9114 7.2.1)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		SettingsBin,
		<<0>>, %% DATA frame.
		cow_http3:encode_int(12),
		<<"Hello world!">>
	]),
	%% The connection should have been closed.
	#{reason := h3_frame_unexpected} = do_wait_connection_closed(Conn),
	ok.

headers_frame_rejected_on_control_stream(Config) ->
	doc("HEADERS frames received on the control stream must be rejected "
		"with an H3_FRAME_UNEXPECTED connection error. (RFC9114 7.2.2)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	{ok, EncodedHeaders, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		SettingsBin,
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders
	]),
	%% The connection should have been closed.
	#{reason := h3_frame_unexpected} = do_wait_connection_closed(Conn),
	ok.

%% @todo Implement server push. (RFC9114 7.2.3. CANCEL_PUSH)

settings_twice(Config) ->
	doc("Receipt of a second SETTINGS frame on the control stream "
		"must be rejected with an H3_FRAME_UNEXPECTED connection error. (RFC9114 7.2.4)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		SettingsBin,
		SettingsBin
	]),
	%% The connection should have been closed.
	#{reason := h3_frame_unexpected} = do_wait_connection_closed(Conn),
	ok.

settings_on_bidi_stream(Config) ->
	doc("Receipt of a SETTINGS frame on a bidirectional stream "
		"must be rejected with an H3_FRAME_UNEXPECTED connection error. (RFC9114 7.2.4)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>}
	], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(StreamRef, [
		SettingsBin,
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	], ?QUIC_SEND_FLAG_FIN),
	%% The connection should have been closed.
	#{reason := h3_frame_unexpected} = do_wait_connection_closed(Conn),
	ok.

settings_identifier_twice(Config) ->
	doc("Receipt of a duplicate SETTINGS identifier must be rejected "
		"with an H3_SETTINGS_ERROR connection error. (RFC9114 7.2.4)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	SettingsPayload = [
		cow_http3:encode_int(6), cow_http3:encode_int(4096),
		cow_http3:encode_int(6), cow_http3:encode_int(8192)
	],
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		<<4>>, %% SETTINGS frame.
		cow_http3:encode_int(iolist_size(SettingsPayload)),
		SettingsPayload
	]),
	%% The connection should have been closed.
	#{reason := h3_settings_error} = do_wait_connection_closed(Conn),
	ok.

settings_ignore_unknown_identifier(Config) ->
	doc("Unknown SETTINGS identifiers must be ignored (RFC9114 7.2.4, RFC9114 9)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	SettingsPayload = [
		cow_http3:encode_int(999), cow_http3:encode_int(4096)
	],
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		<<4>>, %% SETTINGS frame.
		cow_http3:encode_int(iolist_size(SettingsPayload)),
		SettingsPayload
	]),
	%% The connection should remain up.
	receive
		{quic, shutdown, Conn, {unknown_quic_status, Code}} ->
			Reason = cow_http3:code_to_error(Code),
			error(Reason)
	after 1000 ->
		ok
	end.

settings_ignore_reserved_identifier(Config) ->
	doc("Reserved SETTINGS identifiers must be ignored (RFC9114 7.2.4.1)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	SettingsPayload = [
		cow_http3:encode_int(do_reserved_type()), cow_http3:encode_int(4096)
	],
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		<<4>>, %% SETTINGS frame.
		cow_http3:encode_int(iolist_size(SettingsPayload)),
		SettingsPayload
	]),
	%% The connection should remain up.
	receive
		{quic, shutdown, Conn, {unknown_quic_status, Code}} ->
			Reason = cow_http3:code_to_error(Code),
			error(Reason)
	after 1000 ->
		ok
	end.

%% @todo Check that we send a reserved SETTINGS identifier when sending a
%%       non-empty SETTINGS frame. (7.2.4.1. Defined SETTINGS Parameters)

%% @todo Check that setting SETTINGS_MAX_FIELD_SECTION_SIZE works.

%% It is unclear whether the SETTINGS identifier 0x00 must be rejected or ignored.

settings_reject_http2_0x02(Config) ->
	do_settings_reject_http2(Config, 2, 1).

settings_reject_http2_0x03(Config) ->
	do_settings_reject_http2(Config, 3, 100).

settings_reject_http2_0x04(Config) ->
	do_settings_reject_http2(Config, 4, 128000).

settings_reject_http2_0x05(Config) ->
	do_settings_reject_http2(Config, 5, 1000000).

do_settings_reject_http2(Config, Identifier, Value) ->
	doc("Receipt of an unused HTTP/2 SETTINGS identifier must be rejected "
		"with an H3_SETTINGS_ERROR connection error. (RFC9114 7.2.4, RFC9114 11.2.2)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	SettingsPayload = [
		cow_http3:encode_int(Identifier), cow_http3:encode_int(Value)
	],
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		<<4>>, %% SETTINGS frame.
		cow_http3:encode_int(iolist_size(SettingsPayload)),
		SettingsPayload
	]),
	%% The connection should have been closed.
	#{reason := h3_settings_error} = do_wait_connection_closed(Conn),
	ok.

%% 7.2.4.2. Initialization
%% An HTTP implementation MUST NOT send frames or requests that would be
%% invalid based on its current understanding of the peer's settings.
%% @todo In the case of SETTINGS_MAX_FIELD_SECTION_SIZE I don't think we have a choice.

%% All settings begin at an initial value. Each endpoint SHOULD use these
%% initial values to send messages before the peer's SETTINGS frame has arrived,
%% as packets carrying the settings can be lost or delayed. When the SETTINGS
%% frame arrives, any settings are changed to their new values.

%% Endpoints MUST NOT require any data to be received from the peer prior to
%% sending the SETTINGS frame; settings MUST be sent as soon as the transport is
%% ready to send data.

%% @todo Implement 0-RTT. (7.2.4.2. Initialization)

%% @todo Implement server push. (7.2.5. PUSH_PROMISE)

goaway_on_bidi_stream(Config) ->
	doc("Receipt of a GOAWAY frame on a bidirectional stream "
		"must be rejected with an H3_FRAME_UNEXPECTED connection error. (RFC9114 7.2.6)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(StreamRef, [
		<<7>>, cow_http3:encode_int(1), cow_http3:encode_int(0) %% GOAWAY.
	], ?QUIC_SEND_FLAG_FIN),
	%% The connection should have been closed.
	#{reason := h3_frame_unexpected} = do_wait_connection_closed(Conn),
	ok.

%% @todo Implement server push. (7.2.6 GOAWAY - will have to reject too large push IDs)

max_push_id_on_bidi_stream(Config) ->
	doc("Receipt of a MAX_PUSH_ID frame on a bidirectional stream "
		"must be rejected with an H3_FRAME_UNEXPECTED connection error. (RFC9114 7.2.7)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(StreamRef, [
		<<13>>, cow_http3:encode_int(1), cow_http3:encode_int(0) %% MAX_PUSH_ID.
	], ?QUIC_SEND_FLAG_FIN),
	%% The connection should have been closed.
	#{reason := h3_frame_unexpected} = do_wait_connection_closed(Conn),
	ok.

%% @todo Implement server push. (7.2.7 MAX_PUSH_ID)

max_push_id_reject_lower(Config) ->
	doc("Receipt of a MAX_PUSH_ID value lower than previously received "
		"must be rejected with an H3_ID_ERROR connection error. (RFC9114 7.2.7)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		SettingsBin,
		<<13>>, cow_http3:encode_int(1), cow_http3:encode_int(20), %% MAX_PUSH_ID.
		<<13>>, cow_http3:encode_int(1), cow_http3:encode_int(10) %% MAX_PUSH_ID.
	]),
	%% The connection should have been closed.
	#{reason := h3_id_error} = do_wait_connection_closed(Conn),
	ok.

reserved_on_control_stream(Config) ->
	doc("Receipt of a reserved frame type on a control stream "
		"must be ignored. (RFC9114 7.2.8)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	Len = rand:uniform(512),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		SettingsBin,
		cow_http3:encode_int(do_reserved_type()),
		cow_http3:encode_int(Len),
		rand:bytes(Len)
	]),
	%% The connection should remain up.
	receive
		{quic, shutdown, Conn, {unknown_quic_status, Code}} ->
			Reason = cow_http3:code_to_error(Code),
			error(Reason)
	after 1000 ->
		ok
	end.

reserved_reject_http2_0x02_control(Config) ->
	do_reserved_reject_http2_control(Config, 2).

reserved_reject_http2_0x06_control(Config) ->
	do_reserved_reject_http2_control(Config, 6).

reserved_reject_http2_0x08_control(Config) ->
	do_reserved_reject_http2_control(Config, 8).

reserved_reject_http2_0x09_control(Config) ->
	do_reserved_reject_http2_control(Config, 9).

do_reserved_reject_http2_control(Config, Type) ->
	doc("Receipt of an unused HTTP/2 frame type must be rejected "
		"with an H3_FRAME_UNEXPECTED connection error. (RFC9114 7.2.8, RFC9114 11.2.1)"),
	#{conn := Conn} = do_connect(Config),
	{ok, ControlRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
	Len = rand:uniform(512),
	{ok, _} = quicer:send(ControlRef, [
		<<0>>, %% CONTROL stream.
		SettingsBin,
		cow_http3:encode_int(Type),
		cow_http3:encode_int(Len),
		rand:bytes(Len)
	]),
	%% The connection should have been closed.
	#{reason := h3_frame_unexpected} = do_wait_connection_closed(Conn),
	ok.

reserved_reject_http2_0x02_bidi(Config) ->
	do_reserved_reject_http2_bidi(Config, 2).

reserved_reject_http2_0x06_bidi(Config) ->
	do_reserved_reject_http2_bidi(Config, 6).

reserved_reject_http2_0x08_bidi(Config) ->
	do_reserved_reject_http2_bidi(Config, 8).

reserved_reject_http2_0x09_bidi(Config) ->
	do_reserved_reject_http2_bidi(Config, 9).

do_reserved_reject_http2_bidi(Config, Type) ->
	doc("Receipt of an unused HTTP/2 frame type must be rejected "
		"with an H3_FRAME_UNEXPECTED connection error. (RFC9114 7.2.8, RFC9114 11.2.1)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedHeaders, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"https">>},
		{<<":authority">>, <<"localhost">>},
		{<<":path">>, <<"/">>},
		{<<"content-length">>, <<"0">>}
	], 0, cow_qpack:init(encoder)),
	Len = rand:uniform(512),
	{ok, _} = quicer:send(StreamRef, [
		cow_http3:encode_int(Type),
		cow_http3:encode_int(Len),
		rand:bytes(Len),
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedHeaders)),
		EncodedHeaders
	], ?QUIC_SEND_FLAG_FIN),
	%% The connection should have been closed.
	#{reason := h3_frame_unexpected} = do_wait_connection_closed(Conn),
	ok.

%% An endpoint MAY choose to treat a stream error as a connection error under
%% certain circumstances, closing the entire connection in response to a
%% condition on a single stream.

%% Because new error codes can be defined without negotiation (see Section 9),
%% use of an error code in an unexpected context or receipt of an unknown error
%% code MUST be treated as equivalent to H3_NO_ERROR.

%% 8.1. HTTP/3 Error Codes
%% H3_INTERNAL_ERROR (0x0102): An internal error has occurred in the HTTP stack.
%% H3_EXCESSIVE_LOAD (0x0107): The endpoint detected that its peer is
%% exhibiting a behavior that might be generating excessive load.
%% H3_MISSING_SETTINGS (0x010a): No SETTINGS frame was received
%% at the beginning of the control stream.
%% H3_REQUEST_REJECTED (0x010b): A server rejected a request without
%% performing any application processing.
%% H3_REQUEST_CANCELLED (0x010c): The request or its response
%% (including pushed response) is cancelled.
%% H3_REQUEST_INCOMPLETE (0x010d): The client's stream terminated
%% without containing a fully formed request.
%% H3_CONNECT_ERROR (0x010f): The TCP connection established in
%% response to a CONNECT request was reset or abnormally closed.
%% H3_VERSION_FALLBACK (0x0110): The requested operation cannot
%% be served over HTTP/3. The peer should retry over HTTP/1.1.

%% 9. Extensions to HTTP/3
%% If a setting is used for extension negotiation, the default value MUST be
%% defined in such a fashion that the extension is disabled if the setting is
%% omitted.

%% 10. Security Considerations
%% 10.3. Intermediary-Encapsulation Attacks
%% Requests or responses containing invalid field names MUST be treated as malformed.
%% Any request or response that contains a character not permitted in a field
%% value MUST be treated as malformed.

%% 10.5. Denial-of-Service Considerations
%% Implementations SHOULD track the use of these features and set limits on
%% their use. An endpoint MAY treat activity that is suspicious as a connection
%% error of type H3_EXCESSIVE_LOAD, but false positives will result in disrupting
%% valid connections and requests.

reject_large_unknown_frame(Config) ->
	doc("Large unknown frames may risk denial-of-service "
		"and should be rejected. (RFC9114 10.5)"),
	#{conn := Conn} = do_connect(Config),
	{ok, StreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(StreamRef, [
		cow_http3:encode_int(do_unknown_frame_type()),
		cow_http3:encode_int(16385)
	]),
	#{reason := h3_excessive_load} = do_wait_connection_closed(Conn),
	ok.

%% 10.5.1. Limits on Field Section Size
%% An endpoint can use the SETTINGS_MAX_FIELD_SECTION_SIZE (Section 4.2.2)
%% setting to advise peers of limits that might apply on the size of field
%% sections.
%%
%% A server that receives a larger field section than it is willing to handle
%% can send an HTTP 431 (Request Header Fields Too Large) status code
%% ([RFC6585]).

%% 10.6. Use of Compression
%% Implementations communicating on a secure channel MUST NOT compress content
%% that includes both confidential and attacker-controlled data unless separate
%% compression contexts are used for each source of data. Compression MUST NOT be
%% used if the source of data cannot be reliably determined.

%% 10.9. Early Data
%% The anti-replay mitigations in [HTTP-REPLAY] MUST be applied when using HTTP/3 with 0-RTT.

%% 10.10. Migration
%% Certain HTTP implementations use the client address for logging or
%% access-control purposes. Since a QUIC client's address might change during a
%% connection (and future versions might support simultaneous use of multiple
%% addresses), such implementations will need to either actively retrieve the
%% client's current address or addresses when they are relevant or explicitly
%% accept that the original address might change. @todo Document this behavior.

%% Appendix A. Considerations for Transitioning from HTTP/2
%% A.1. Streams
%% QUIC considers a stream closed when all data has been received and sent data
%% has been acknowledged by the peer. HTTP/2 considers a stream closed when the
%% frame containing the END_STREAM bit has been committed to the transport. As a
%% result, the stream for an equivalent exchange could remain "active" for a
%% longer period of time. HTTP/3 servers might choose to permit a larger number
%% of concurrent client-initiated bidirectional streams to achieve equivalent
%% concurrency to HTTP/2, depending on the expected usage patterns. @todo Document this.

%% Helper functions.

%% @todo Maybe have a function in cow_http3.
do_reserved_type() ->
	16#1f * (rand:uniform(148764065110560900) - 1) + 16#21.

do_connect(Config) ->
	do_connect(Config, #{}).

do_connect(Config, Opts) ->
	{ok, Conn} = quicer:connect("localhost", config(port, Config),
		Opts#{alpn => ["h3"], verify => none}, 5000),
	%% To make sure the connection is fully established we wait
	%% to receive the SETTINGS frame on the control stream.
	{ok, ControlRef, Settings} = do_wait_settings(Conn),
	#{
		conn => Conn,
		control => ControlRef, %% This is the peer control stream.
		settings => Settings
	}.

do_wait_settings(Conn) ->
	receive
		{quic, new_stream, StreamRef, #{flags := Flags}} ->
			ok = quicer:setopt(StreamRef, active, true),
			true = quicer:is_unidirectional(Flags),
			receive {quic, <<
				0, %% Control stream.
				SettingsFrame/bits
			>>, StreamRef, _} ->
				{ok, {settings, Settings}, <<>>} = cow_http3:parse(SettingsFrame),
				{ok, StreamRef, Settings}
			after 5000 ->
				{error, timeout}
			end
	after 5000 ->
		{error, timeout}
	end.

do_receive_data(StreamRef) ->
	receive
		{quic, Data, StreamRef, _Flags} when is_binary(Data) ->
			{ok, Data}
	after 5000 ->
		{error, timeout}
	end.

do_guess_int_encoding(Data) ->
	SizeWithLen = byte_size(Data) - 1,
	if
		SizeWithLen < 64 + 1 ->
			{0, 6};
		SizeWithLen < 16384 + 2 ->
			{1, 14};
		SizeWithLen < 1073741824 + 4 ->
			{2, 30};
		SizeWithLen < 4611686018427387904 + 8 ->
			{3, 62}
	end.

do_wait_peer_send_shutdown(StreamRef) ->
	receive
		{quic, peer_send_shutdown, StreamRef, undefined} ->
			ok
	after 5000 ->
		{error, timeout}
	end.

do_wait_stream_aborted(StreamRef) ->
	receive
		{quic, peer_send_aborted, StreamRef, Code} ->
			Reason = cow_http3:code_to_error(Code),
			#{reason => Reason};
		{quic, peer_receive_aborted, StreamRef, Code} ->
			Reason = cow_http3:code_to_error(Code),
			#{reason => Reason}
	after 5000 ->
		{error, timeout}
	end.

do_wait_stream_closed(StreamRef) ->
	receive
		{quic, stream_closed, StreamRef, #{error := Error, is_conn_shutdown := false}} ->
			0 = Error,
			ok
	after 5000 ->
		{error, timeout}
	end.

do_receive_response(StreamRef) ->
	{ok, Data} = do_receive_data(StreamRef),
	{HLenEnc, HLenBits} = do_guess_int_encoding(Data),
	<<
		1, %% HEADERS frame.
		HLenEnc:2, HLen:HLenBits,
		EncodedResponse:HLen/bytes,
		Rest/bits
	>> = Data,
	{ok, DecodedResponse, _DecData, _DecSt}
		= cow_qpack:decode_field_section(EncodedResponse, 0, cow_qpack:init(decoder)),
	Headers = maps:from_list(DecodedResponse),
	#{<<"content-length">> := BodyLen} = Headers,
	{DLenEnc, DLenBits} = do_guess_int_encoding(Rest),
	Body = case Rest of
		<<>> ->
			<<>>;
		<<
			0, %% DATA frame.
			DLenEnc:2, DLen:DLenBits,
			Body0:DLen/bytes
		>> ->
			BodyLen = integer_to_binary(byte_size(Body0)),
			Body0
	end,
	ok = do_wait_peer_send_shutdown(StreamRef),
	#{
		headers => Headers,
		body => Body
	}.

do_wait_connection_closed(Conn) ->
	receive
		{quic, shutdown, Conn, {unknown_quic_status, Code}} ->
			Reason = cow_http3:code_to_error(Code),
			#{reason => Reason}
	after 5000 ->
		{error, timeout}
	end.

-endif.
