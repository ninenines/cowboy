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

-module(draft_h3_webtransport_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(rfc9114_SUITE, [do_wait_stream_aborted/1]).

-ifdef(COWBOY_QUICER).

-include_lib("quicer/include/quicer.hrl").

all() ->
	[{group, enabled}].

groups() ->
	Tests = ct_helper:all(?MODULE),
	[{enabled, [], Tests}]. %% @todo Enable parallel when all is better.

init_per_group(Name = enabled, Config) ->
	cowboy_test:init_http3(Name, #{
		enable_connect_protocol => true,
		h3_datagram => true,
		enable_webtransport => true, %% For compatibility with draft-02.
		wt_max_sessions => 10,
		env => #{dispatch => cowboy_router:compile(init_routes(Config))}
	}, Config).

end_per_group(Name, _) ->
	cowboy_test:stop_group(Name).

init_routes(_) -> [
	{"localhost", [
		{"/wt", wt_echo_h, []}
	]}
].

%% Temporary.

%% To start Chromium the command line is roughly:
%% chromium --ignore-certificate-errors-spki-list=LeLykt63i2FRAm+XO91yBoSjKfrXnAFygqe5xt0zgDA= --ignore-certificate-errors --user-data-dir=/tmp/chromium-wt --allow-insecure-localhost --webtransport-developer-mode --enable-quic https://googlechrome.github.io/samples/webtransport/client.html
%%
%% To find the SPKI the command is roughly:
%% openssl x509 -in ~/ninenines/cowboy/test/rfc9114_SUITE_data/server.pem -pubkey -noout | \ 
%%  openssl pkey -pubin -outform der | \
%%  openssl dgst -sha256 -binary | \
%%  openssl enc -base64

%run(Config) ->
%	ct:pal("port ~p", [config(port, Config)]),
%	timer:sleep(infinity).

%% 3. Session Establishment

%% 3.1. Establishing a WebTransport-Capable HTTP/3 Connection

%% In order to indicate support for WebTransport, the server MUST send a SETTINGS_WT_MAX_SESSIONS value greater than "0" in its SETTINGS frame. (3.1)
%% @todo reject_session_disabled
%% @todo accept_session_below
%% @todo accept_session_equal
%% @todo reject_session_above

%% The client MUST NOT send a WebTransport request until it has received the setting indicating WebTransport support from the server. (3.1)

%% For draft verisons of WebTransport only, the server MUST NOT process any incoming WebTransport requests until the client settings have been received, as the client may be using a version of the WebTransport extension that is different from the one used by the server. (3.1)

%% Because WebTransport over HTTP/3 requires support for HTTP/3 datagrams and the Capsule Protocol, both the client and the server MUST indicate support for HTTP/3 datagrams by sending a SETTINGS_H3_DATAGRAM value set to 1 in their SETTINGS frame (see Section 2.1.1 of [HTTP-DATAGRAM]). (3.1)
%% @todo settings_h3_datagram_enabled

%% WebTransport over HTTP/3 also requires support for QUIC datagrams. To indicate support, both the client and the server MUST send a max_datagram_frame_size transport parameter with a value greater than 0 (see Section 3 of [QUIC-DATAGRAM]). (3.1)
%% @todo quic_datagram_enabled (if size is too low the CONNECT stream can be used for capsules)

%% Any WebTransport requests sent by the client without enabling QUIC and HTTP datagrams MUST be treated as malformed by the server, as described in Section 4.1.2 of [HTTP3]. (3.1)
%% @todo reject_h3_datagram_disabled
%% @todo reject_quic_datagram_disabled

%% WebTransport over HTTP/3 relies on the RESET_STREAM_AT frame defined in [RESET-STREAM-AT]. To indicate support, both the client and the server MUST enable the extension as described in Section 3 of [RESET-STREAM-AT]. (3.1)
%% @todo reset_stream_at_enabled

%% 3.2. Extended CONNECT in HTTP/3

%% [RFC8441] defines an extended CONNECT method in Section 4, enabled by the SETTINGS_ENABLE_CONNECT_PROTOCOL setting. That setting is defined for HTTP/3 by [RFC9220]. A server supporting WebTransport over HTTP/3 MUST send both the SETTINGS_WT_MAX_SESSIONS setting with a value greater than "0" and the SETTINGS_ENABLE_CONNECT_PROTOCOL setting with a value of "1". (3.2)
%% @todo settings_enable_connect_protocol_enabled
%% @todo reject_settings_enable_connect_protocol_disabled

%% 3.3. Creating a New Session

%% As WebTransport sessions are established over HTTP/3, they are identified using the https URI scheme ([HTTP], Section 4.2.2). (3.3)

%% In order to create a new WebTransport session, a client can send an HTTP CONNECT request. The :protocol pseudo-header field ([RFC8441]) MUST be set to webtransport. The :scheme field MUST be https. Both the :authority and the :path value MUST be set; those fields indicate the desired WebTransport server. If the WebTransport session is coming from a browser client, an Origin header [RFC6454] MUST be provided within the request; otherwise, the header is OPTIONAL. (3.3)

%% If it does not (have a WT server), it SHOULD reply with status code 404 (Section 15.5.5 of [HTTP]). (3.3)

%% When the request contains the Origin header, the WebTransport server MUST verify the Origin header to ensure that the specified origin is allowed to access the server in question. If the verification fails, the WebTransport server SHOULD reply with status code 403 (Section 15.5.4 of [HTTP]). (3.3)

accept_session_when_enabled(Config) ->
	doc("Confirm that a WebTransport session can be established over HTTP/3. "
		"(draft_webtrans_http3 3.3, RFC9220)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a bidi stream, send Hello, get Hello back.
	{ok, BidiStreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(BidiStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6, "Hello">>),
	{nofin, <<"Hello">>} = do_receive_data(BidiStreamRef),
	ok.

%% If the server accepts 0-RTT, the server MUST NOT reduce the limit of maximum open WebTransport sessions from the one negotiated during the previous session; such change would be deemed incompatible, and MUST result in a H3_SETTINGS_ERROR connection error. (3.3)

%% The capsule-protocol header field Section 3.4 of [HTTP-DATAGRAM] is not required by WebTransport and can safely be ignored by WebTransport endpoints. (3.3)

%% 3.4. Application Protocol Negotiation

application_protocol_negotiation(Config) ->
	doc("Applications can negotiate a protocol to use via WebTransport. "
		"(draft_webtrans_http3 3.4)"),
	%% Connect to the WebTransport server.
	WTAvailableProtocols = cow_http_hd:wt_available_protocols([<<"foo">>, <<"bar">>]),
	#{
		resp_headers := RespHeaders
	} = do_webtransport_connect(Config, [{<<"wt-available-protocols">>, WTAvailableProtocols}]),
	{<<"wt-protocol">>, WTProtocol} = lists:keyfind(<<"wt-protocol">>, 1, RespHeaders),
	<<"foo">> = iolist_to_binary(cow_http_hd:parse_wt_protocol(WTProtocol)),
	ok.

%% Both WT-Available-Protocols and WT-Protocol are Structured Fields [RFC8941]. WT-Available-Protocols is a List of Tokens, and WT-Protocol is a Token. The token in the WT-Protocol response header field MUST be one of the tokens listed in WT-Available-Protocols of the request. (3.4)

%% @todo 3.5 Prioritization

%% 4. WebTransport Features

%% The client MAY optimistically open unidirectional and bidirectional streams, as well as send datagrams, for a session that it has sent the CONNECT request for, even if it has not yet received the server's response to the request. (4)

%% If at any point a session ID is received that cannot be a valid ID for a client-initiated bidirectional stream, the recipient MUST close the connection with an H3_ID_ERROR error code. (4)
%% @todo Open bidi with Session ID 0, then do the CONNECT request.

%% 4.1. Unidirectional streams

unidirectional_streams(Config) ->
	doc("Both endpoints can open and use unidirectional streams. "
		"(draft_webtrans_http3 4.1)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a unidi stream, send Hello with a Fin flag.
	{ok, LocalStreamRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(LocalStreamRef,
		<<1:2, 16#54:14, 0:2, SessionID:6, "Hello">>,
		?QUIC_SEND_FLAG_FIN),
	%% Accept an identical unidi stream.
	{unidi, RemoteStreamRef} = do_receive_new_stream(),
	{nofin, <<1:2, 16#54:14, 0:2, SessionID:6>>} = do_receive_data(RemoteStreamRef),
	{fin, <<"Hello">>} = do_receive_data(RemoteStreamRef),
	ok.

%% 4.2. Bidirectional Streams

bidirectional_streams_client(Config) ->
	doc("The WT client can open and use bidirectional streams. "
		"(draft_webtrans_http3 4.2)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a bidi stream, send Hello, get Hello back.
	{ok, LocalStreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(LocalStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6, "Hello">>),
	{nofin, <<"Hello">>} = do_receive_data(LocalStreamRef),
	ok.

bidirectional_streams_server(Config) ->
	doc("The WT server can open and use bidirectional streams. "
		"(draft_webtrans_http3 4.2)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a bidi stream, send a special instruction
	%% to make the server create another bidi stream.
	{ok, LocalStreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(LocalStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6, "TEST:open_bidi">>),
	%% Accept the bidi stream and receive the data.
	{bidi, RemoteStreamRef} = do_receive_new_stream(),
	{nofin, <<1:2, 16#41:14, 0:2, SessionID:6>>} = do_receive_data(RemoteStreamRef),
	{ok, _} = quicer:send(RemoteStreamRef, <<"Hello">>,
		?QUIC_SEND_FLAG_FIN),
	{fin, <<"Hello">>} = do_receive_data(RemoteStreamRef),
	ok.

%% Endpoints MUST NOT send WT_STREAM as a frame type on HTTP/3 streams other than the very first bytes of a request stream. Receiving this frame type in any other circumstances MUST be treated as a connection error of type H3_FRAME_ERROR. (4.2)

%% 4.3. Resetting Data Streams

%% A WebTransport endpoint may send a RESET_STREAM or a STOP_SENDING frame for a WebTransport data stream. Those signals are propagated by the WebTransport implementation to the application. (4.3)

%% A WebTransport application SHALL provide an error code for those operations. (4.3)

%% WebTransport implementations MUST use the RESET_STREAM_AT frame [RESET-STREAM-AT] with a Reliable Size set to at least the size of the WebTransport header when resetting a WebTransport data stream. This ensures that the ID field associating the data stream with a WebTransport session is always delivered. (4.3)

%% WebTransport implementations SHALL forward the error code for a stream associated with a known session to the application that owns that session (4.3)

%% 4.4. Datagrams

datagrams(Config) ->
	doc("Both endpoints can send and receive datagrams. (draft_webtrans_http3 4.4)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	QuarterID = SessionID div 4,
	%% Send a Hello datagram.
	{ok, _} = quicer:send_dgram(Conn, <<0:2, QuarterID:6, "Hello">>),
	%% Receive a Hello datagram back.
	{datagram, SessionID, <<"Hello">>} = do_receive_datagram(Conn),
	ok.

%% @todo datagrams_via_capsule?

%% 4.5. Buffering Incoming Streams and Datagrams

%% To handle this case (out of order stream_open/CONNECT), WebTransport endpoints SHOULD buffer streams and datagrams until those can be associated with an established session. (4.5)

%% To avoid resource exhaustion, the endpoints MUST limit the number of buffered streams and datagrams. When the number of buffered streams is exceeded, a stream SHALL be closed by sending a RESET_STREAM and/or STOP_SENDING with the WT_BUFFERED_STREAM_REJECTED error code. When the number of buffered datagrams is exceeded, a datagram SHALL be dropped. It is up to an implementation to choose what stream or datagram to discard. (4.5)

%% 4.6. Interaction with HTTP/3 GOAWAY frame

%% A client receiving GOAWAY cannot initiate CONNECT requests for new WebTransport sessions on that HTTP/3 connection; it must open a new HTTP/3 connection to initiate new WebTransport sessions with the same peer. (4.6)

%% An HTTP/3 GOAWAY frame is also a signal to applications to initiate shutdown for all WebTransport sessions. (4.6)

%% @todo Currently receipt of a GOAWAY frame immediately ends the connection.
%%       We want to allow WT sessions to gracefully shut down before that.
%goaway_client(Config) ->
%	doc("The HTTP/3 client can initiate the close of all WT sessions "
%		"by sending a GOAWAY frame. (draft_webtrans_http3 4.6)"),
%	%% Connect to the WebTransport server.
%	#{
%		conn := Conn,
%		connect_stream_ref := ConnectStreamRef,
%		session_id := SessionID
%	} = do_webtransport_connect(Config),
%	%% Open a control stream and send a GOAWAY frame.
%	{ok, ControlRef} = quicer:start_stream(Conn,
%		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
%	{ok, SettingsBin, _HTTP3Machine0} = cow_http3_machine:init(client, #{}),
%	{ok, _} = quicer:send(ControlRef, [
%		<<0>>, %% CONTROL stream.
%		SettingsBin,
%		<<7>>, %% GOAWAY frame.
%		cow_http3:encode_int(1),
%		cow_http3:encode_int(0)
%	]),
%	%% Receive a datagram indicating processing by the WT handler.
%	{datagram, SessionID, <<"TEST:close_initiated">>} = do_receive_datagram(Conn),
%	ok.

wt_drain_session_client(Config) ->
	doc("The WT client can initiate the close of a single session. "
		"(draft_webtrans_http3 4.6)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		connect_stream_ref := ConnectStreamRef,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Send the WT_DRAIN_SESSION capsule on the CONNECT stream.
	{ok, _} = quicer:send(ConnectStreamRef, cow_capsule:wt_drain_session()),
	%% Receive a datagram indicating processing by the WT handler.
	{datagram, SessionID, <<"TEST:close_initiated">>} = do_receive_datagram(Conn),
	ok.

wt_drain_session_server(Config) ->
	doc("The WT server can initiate the close of a single session. "
		"(draft_webtrans_http3 4.6)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		connect_stream_ref := ConnectStreamRef,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a bidi stream, send a special instruction to make it initiate the close.
	{ok, LocalStreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(LocalStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6, "TEST:initiate_close">>),
	%% Receive the WT_DRAIN_SESSION capsule on the CONNECT stream.
	DrainWTSessionCapsule = cow_capsule:wt_drain_session(),
	{nofin, DrainWTSessionCapsule} = do_receive_data(ConnectStreamRef),
	ok.

wt_drain_session_continue_client(Config) ->
	doc("After the WT client has initiated the close of the session, "
		"both client and server can continue using the session and "
		"open new streams. (draft_webtrans_http3 4.6)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		connect_stream_ref := ConnectStreamRef,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Send the WT_DRAIN_SESSION capsule on the CONNECT stream.
	{ok, _} = quicer:send(ConnectStreamRef, cow_capsule:wt_drain_session()),
	%% Receive a datagram indicating processing by the WT handler.
	{datagram, SessionID, <<"TEST:close_initiated">>} = do_receive_datagram(Conn),
	%% Create a new bidi stream, send Hello, get Hello back.
	{ok, ContinueStreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(ContinueStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6, "Hello">>),
	{nofin, <<"Hello">>} = do_receive_data(ContinueStreamRef),
	ok.

wt_drain_session_continue_server(Config) ->
	doc("After the WT server has initiated the close of the session, "
		"both client and server can continue using the session and "
		"open new streams. (draft_webtrans_http3 4.6)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		connect_stream_ref := ConnectStreamRef,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a bidi stream, send a special instruction to make it initiate the close.
	{ok, LocalStreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(LocalStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6, "TEST:initiate_close">>),
	%% Receive the WT_DRAIN_SESSION capsule on the CONNECT stream.
	DrainWTSessionCapsule = cow_capsule:wt_drain_session(),
	{nofin, DrainWTSessionCapsule} = do_receive_data(ConnectStreamRef),
	%% Create a new bidi stream, send Hello, get Hello back.
	{ok, ContinueStreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(ContinueStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6, "Hello">>),
	{nofin, <<"Hello">>} = do_receive_data(ContinueStreamRef),
	ok.

%% @todo 4.7. Use of Keying Material Exporters

%% 5. Flow Control

%% 5.1. Limiting the Number of Simultaneous Sessions

%% This document defines a SETTINGS_WT_MAX_SESSIONS parameter that allows the server to limit the maximum number of concurrent WebTransport sessions on a single HTTP/3 connection. The client MUST NOT open more simultaneous sessions than indicated in the server SETTINGS parameter. The server MUST NOT close the connection if the client opens sessions exceeding this limit, as the client and the server do not have a consistent view of how many sessions are open due to the asynchronous nature of the protocol; instead, it MUST reset all of the CONNECT streams it is not willing to process with the H3_REQUEST_REJECTED status defined in [HTTP3]. (5.1)

%% 5.2. Limiting the Number of Streams Within a Session

%% The WT_MAX_STREAMS capsule (Section 5.6.1) establishes a limit on the number of streams within a WebTransport session. (5.2)

%% Note that the CONNECT stream for the session is not included in either the bidirectional or the unidirectional stream limits (5.2)

%% The session-level stream limit applies in addition to the QUIC MAX_STREAMS frame, which provides a connection-level stream limit. New streams can only be created within the session if both the stream- and the connection-level limit permit (5.2)

%% The WT_STREAMS_BLOCKED capsule (Section 5.7) can be sent to indicate that an endpoint was unable to create a stream due to the session-level stream limit. (5.2)

%% Note that enforcing this limit requires reliable resets for stream headers so that both endpoints can agree on the number of streams that are open. (5.2)

%% 5.3. Data Limits

%% The WT_MAX_DATA capsule (Section 5.8) establishes a limit on the amount of data that can be sent within a WebTransport session. This limit counts all data that is sent on streams of the corresponding type, excluding the stream header (see Section 4.1 and Section 4.2). (5.3)

%% Implementing WT_MAX_DATA requires that the QUIC stack provide the WebTransport implementation with information about the final size of streams; see { {Section 4.5 of !RFC9000}}. This allows both endpoints to agree on how much data was consumed by that stream, although the stream header exclusion above applies. (5.3)

%% The WT_DATA_BLOCKED capsule (Section 5.9) can be sent to indicate that an endpoint was unable to send data due to a limit set by the WT_MAX_DATA capsule. (5.3)

%% The WT_MAX_STREAM_DATA and WT_STREAM_DATA_BLOCKED capsules (Part XX of [I-D.ietf-webtrans-http2]) are not used and so are prohibited. Endpoints MUST treat receipt of a WT_MAX_STREAM_DATA or a WT_STREAM_DATA_BLOCKED capsule as a session error. (5.3)

%% 5.4. Flow Control and Intermediaries

%% In practice, an intermediary that translates flow control signals between similar WebTransport protocols, such as between two HTTP/3 connections, can often simply reexpress the same limits received on one connection directly on the other connection. (5.4)

%% 5.5. Flow Control SETTINGS

%% WT_MAX_STREAMS via SETTINGS_WT_INITIAL_MAX_STREAMS_UNI and SETTINGS_WT_INITIAL_MAX_STREAMS_BIDI (5.5)

%% WT_MAX_DATA via SETTINGS_WT_INITIAL_MAX_DATA (5.5)

%% 5.6. Flow Control Capsules

%% 5.6.1. WT_MAX_STREAMS Capsule

%% An HTTP capsule [HTTP-DATAGRAM] called WT_MAX_STREAMS is introduced to inform the peer of the cumulative number of streams of a given type it is permitted to open. A WT_MAX_STREAMS capsule with a type of 0x190B4D3F applies to bidirectional streams, and a WT_MAX_STREAMS capsule with a type of 0x190B4D40 applies to unidirectional streams. (5.6.1)

%% Note that, because Maximum Streams is a cumulative value representing the total allowed number of streams, including previously closed streams, endpoints repeatedly send new WT_MAX_STREAMS capsules with increasing Maximum Streams values as streams are opened. (5.6.1)

%% Maximum Streams: A count of the cumulative number of streams of the corresponding type that can be opened over the lifetime of the session. This value cannot exceed 260, as it is not possible to encode stream IDs larger than 262-1. (5.6.1)

%% An endpoint MUST NOT open more streams than permitted by the current stream limit set by its peer. (5.6.1)

%% Note that this limit includes streams that have been closed as well as those that are open. (5.6.1)

%% Initial values for these limits MAY be communicated by sending non-zero values for SETTINGS_WT_INITIAL_MAX_STREAMS_UNI and SETTINGS_WT_INITIAL_MAX_STREAMS_BIDI. (5.6.1)

%% 5.7. WT_STREAMS_BLOCKED Capsule

%% A sender SHOULD send a WT_STREAMS_BLOCKED capsule (type=0x190B4D43 for bidi or 0x190B4D44 for unidi) when it wishes to open a stream but is unable to do so due to the maximum stream limit set by its peer. (5.7)

%% 5.8. WT_MAX_DATA Capsule

%% An HTTP capsule [HTTP-DATAGRAM] called WT_MAX_DATA (type=0x190B4D3D) is introduced to inform the peer of the maximum amount of data that can be sent on the WebTransport session as a whole. (5.8)

%% This limit counts all data that is sent on streams of the corresponding type, excluding the stream header (see Section 4.1 and Section 4.2). Implementing WT_MAX_DATA requires that the QUIC stack provide the WebTransport implementation with information about the final size of streams; see Section 4.5 of [RFC9000]. (5.8)

%% All data sent in WT_STREAM capsules counts toward this limit. The sum of the lengths of Stream Data fields in WT_STREAM capsules MUST NOT exceed the value advertised by a receiver. (5.8)

%% The initial value for this limit MAY be communicated by sending a non-zero value for SETTINGS_WT_INITIAL_MAX_DATA. (5.8)

%% 5.9. WT_DATA_BLOCKED Capsule

%% A sender SHOULD send a WT_DATA_BLOCKED capsule (type=0x190B4D41) when it wishes to send data but is unable to do so due to WebTransport session-level flow control. (5.9)

%% WT_DATA_BLOCKED capsules can be used as input to tuning of flow control algorithms. (5.9)

%% 6. Session Termination

%% A WebTransport session over HTTP/3 is considered terminated when either of the following conditions is met:
%% * the CONNECT stream is closed, either cleanly or abruptly, on either side; or
%% * a WT_CLOSE_SESSION capsule is either sent or received.
%% (6)

wt_close_session_client(Config) ->
	doc("The WT client can close a single session. (draft_webtrans_http3 4.6)"),
	%% Connect to the WebTransport server.
	#{
		connect_stream_ref := ConnectStreamRef
	} = do_webtransport_connect(Config),
	%% Send the WT_CLOSE_SESSION capsule on the CONNECT stream.
	{ok, _} = quicer:send(ConnectStreamRef,
		cow_capsule:wt_close_session(0, <<>>),
		?QUIC_SEND_FLAG_FIN),
	%% Normally we should also stop reading but in order to detect
	%% that the server stops the stream we must not otherwise the
	%% stream will be de facto closed on our end.
	%%
	%% The recipient must close or reset the stream in response.
	receive
		{quic, stream_closed, ConnectStreamRef, _} ->
			ok
	after 1000 ->
		error({timeout, waiting_for_stream_closed})
	end.

wt_close_session_server(Config) ->
	doc("The WT server can close a single session. (draft_webtrans_http3 4.6)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		connect_stream_ref := ConnectStreamRef,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a bidi stream, send a special instruction to make it initiate the close.
	{ok, LocalStreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(LocalStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6, "TEST:close">>),
	%% Receive the WT_CLOSE_SESSION capsule on the CONNECT stream.
	CloseWTSessionCapsule = cow_capsule:wt_close_session(0, <<>>),
	{fin, CloseWTSessionCapsule} = do_receive_data(ConnectStreamRef),
	ok.

wt_session_gone_client(Config) ->
	doc("Upon learning that the session has been terminated, "
		"the WT server must reset associated streams with the "
		"WEBTRANSPORT_SESSION_GONE error code. (draft_webtrans_http3 4.6)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		connect_stream_ref := ConnectStreamRef,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a unidi stream.
	{ok, LocalUnidiStreamRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(LocalUnidiStreamRef,
		<<1:2, 16#54:14, 0:2, SessionID:6, "Hello">>),
	%% Accept an identical unidi stream.
	{unidi, RemoteUnidiStreamRef} = do_receive_new_stream(),
	{nofin, <<1:2, 16#54:14, 0:2, SessionID:6>>} = do_receive_data(RemoteUnidiStreamRef),
	{nofin, <<"Hello">>} = do_receive_data(RemoteUnidiStreamRef),
	%% Create a bidi stream, send a special instruction
	%% to make the server create another bidi stream.
	{ok, LocalBidiStreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(LocalBidiStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6, "TEST:open_bidi">>),
	%% Accept the bidi stream and receive the data.
	{bidi, RemoteBidiStreamRef} = do_receive_new_stream(),
	{nofin, <<1:2, 16#41:14, 0:2, SessionID:6>>} = do_receive_data(RemoteBidiStreamRef),
	{ok, _} = quicer:send(RemoteBidiStreamRef, <<"Hello">>),
	{nofin, <<"Hello">>} = do_receive_data(RemoteBidiStreamRef),
	%% Send the WT_CLOSE_SESSION capsule on the CONNECT stream.
	{ok, _} = quicer:send(ConnectStreamRef,
		cow_capsule:wt_close_session(0, <<>>),
		?QUIC_SEND_FLAG_FIN),
	%% All streams from that WT session have been aborted.
	#{reason := wt_session_gone} = do_wait_stream_aborted(LocalUnidiStreamRef),
	#{reason := wt_session_gone} = do_wait_stream_aborted(RemoteUnidiStreamRef),
	#{reason := wt_session_gone} = do_wait_stream_aborted(LocalBidiStreamRef),
	#{reason := wt_session_gone} = do_wait_stream_aborted(RemoteBidiStreamRef),
	ok.

wt_session_gone_server(Config) ->
	doc("After the session has been terminated by the WT server, "
		"the WT server must reset associated streams with the "
		"WT_SESSION_GONE error code. (draft_webtrans_http3 4.6)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		connect_stream_ref := ConnectStreamRef,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a unidi stream.
	{ok, LocalUnidiStreamRef} = quicer:start_stream(Conn,
		#{open_flag => ?QUIC_STREAM_OPEN_FLAG_UNIDIRECTIONAL}),
	{ok, _} = quicer:send(LocalUnidiStreamRef,
		<<1:2, 16#54:14, 0:2, SessionID:6, "Hello">>),
	%% Accept an identical unidi stream.
	{unidi, RemoteUnidiStreamRef} = do_receive_new_stream(),
	{nofin, <<1:2, 16#54:14, 0:2, SessionID:6>>} = do_receive_data(RemoteUnidiStreamRef),
	{nofin, <<"Hello">>} = do_receive_data(RemoteUnidiStreamRef),
	%% Create a bidi stream, send a special instruction
	%% to make the server create another bidi stream.
	{ok, LocalBidiStreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(LocalBidiStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6, "TEST:open_bidi">>),
	%% Accept the bidi stream and receive the data.
	{bidi, RemoteBidiStreamRef} = do_receive_new_stream(),
	{nofin, <<1:2, 16#41:14, 0:2, SessionID:6>>} = do_receive_data(RemoteBidiStreamRef),
	{ok, _} = quicer:send(RemoteBidiStreamRef, <<"Hello">>),
	{nofin, <<"Hello">>} = do_receive_data(RemoteBidiStreamRef),

	%% Send a special instruction to make the server initiate the close.
	{ok, _} = quicer:send(LocalBidiStreamRef, <<"TEST:close">>),
	%% Receive the WT_CLOSE_SESSION capsule on the CONNECT stream.
	CloseWTSessionCapsule = cow_capsule:wt_close_session(0, <<>>),
	{fin, CloseWTSessionCapsule} = do_receive_data(ConnectStreamRef),
	%% All streams from that WT session have been aborted.
	#{reason := wt_session_gone} = do_wait_stream_aborted(LocalUnidiStreamRef),
	#{reason := wt_session_gone} = do_wait_stream_aborted(RemoteUnidiStreamRef),
	#{reason := wt_session_gone} = do_wait_stream_aborted(LocalBidiStreamRef),
	#{reason := wt_session_gone} = do_wait_stream_aborted(RemoteBidiStreamRef),
	ok.

%% Application Error Message: A UTF-8 encoded error message string provided by the application closing the session. The message takes up the remainder of the capsule, and its length MUST NOT exceed 1024 bytes. (6)
%% @todo What if it's larger?

wt_close_session_app_code_msg_client(Config) ->
	doc("The WT client can close a single session with an application error code "
		"and an application error message. (draft_webtrans_http3 4.6)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		connect_stream_ref := ConnectStreamRef,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a bidi stream, send a special instruction to make it propagate events.
	{ok, LocalStreamRef} = quicer:start_stream(Conn, #{}),
	EventPidBin = term_to_binary(self()),
	{ok, _} = quicer:send(LocalStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6,
		"TEST:event_pid:", EventPidBin/binary>>),
	%% Send the WT_CLOSE_SESSION capsule on the CONNECT stream.
	{ok, _} = quicer:send(ConnectStreamRef,
		cow_capsule:wt_close_session(17, <<"seventeen">>),
		?QUIC_SEND_FLAG_FIN),
	%% @todo Stop reading from the CONNECt stream too. (STOP_SENDING)
	%% Receive the terminate event from the WT handler.
	receive
		{'$wt_echo_h', terminate, {closed, 17, <<"seventeen">>}, _, _} ->
			ok
	after 1000 ->
		error({timeout, waiting_for_terminate_event})
	end.

wt_close_session_app_code_server(Config) ->
	doc("The WT server can close a single session with an application error code. "
		"(draft_webtrans_http3 4.6)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		connect_stream_ref := ConnectStreamRef,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a bidi stream, send a special instruction to make it initiate the close.
	{ok, LocalStreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(LocalStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6,
		"TEST:close_app_code">>),
	%% Receive the WT_CLOSE_SESSION capsule on the CONNECT stream.
	CloseWTSessionCapsule = cow_capsule:wt_close_session(1234567890, <<>>),
	{fin, CloseWTSessionCapsule} = do_receive_data(ConnectStreamRef),
	ok.

wt_close_session_app_code_msg_server(Config) ->
	doc("The WT server can close a single session with an application error code "
		"and an application error message. (draft_webtrans_http3 4.6)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		connect_stream_ref := ConnectStreamRef,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a bidi stream, send a special instruction to make it initiate the close.
	{ok, LocalStreamRef} = quicer:start_stream(Conn, #{}),
	{ok, _} = quicer:send(LocalStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6,
		"TEST:close_app_code_msg">>),
	%% Receive the WT_CLOSE_SESSION capsule on the CONNECT stream.
	CloseWTSessionCapsule = iolist_to_binary(cow_capsule:wt_close_session(1234567890,
		<<"onetwothreefourfivesixseveneightnineten">>)),
	{fin, CloseWTSessionCapsule} = do_receive_data(ConnectStreamRef),
	ok.

%% An endpoint that sends a WT_CLOSE_SESSION capsule MUST immediately send a FIN. The endpoint MAY send a STOP_SENDING to indicate it is no longer reading from the CONNECT stream. The recipient MUST either close or reset the stream in response. (6)
%% @todo wt_close_session_server_fin
%% @todo The part about close/reset should be tested in wt_close_session_client.

%% If any additional stream data is received on the CONNECT stream after receiving a WT_CLOSE_SESSION capsule, the stream MUST be reset with code H3_MESSAGE_ERROR. (6)
%% @todo wt_close_session_followed_by_data

connect_stream_closed_cleanly_fin(Config) ->
	doc("The WT client closing the CONNECT stream cleanly "
		"is equivalent to a capsule with an application error code of 0 "
		"and an empty error string. (draft_webtrans_http3 4.6)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		connect_stream_ref := ConnectStreamRef,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a bidi stream, send a special instruction to make it propagate events.
	{ok, LocalStreamRef} = quicer:start_stream(Conn, #{}),
	EventPidBin = term_to_binary(self()),
	{ok, _} = quicer:send(LocalStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6,
		"TEST:event_pid:", EventPidBin/binary>>),
	{nofin, <<"event_pid_received">>} = do_receive_data(LocalStreamRef),
	%% Cleanly terminate the CONNECT stream.
	{ok, _} = quicer:send(ConnectStreamRef, <<>>, ?QUIC_SEND_FLAG_FIN),
	%% Receive the terminate event from the WT handler.
	receive
		{'$wt_echo_h', terminate, {closed, 0, <<>>}, _, _} ->
			ok
	after 1000 ->
		error({timeout, waiting_for_terminate_event})
	end.

connect_stream_closed_cleanly_shutdown(Config) ->
	doc("The WT client closing the CONNECT stream cleanly "
		"is equivalent to a capsule with an application error code of 0 "
		"and an empty error string. (draft_webtrans_http3 4.6)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		connect_stream_ref := ConnectStreamRef,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a bidi stream, send a special instruction to make it propagate events.
	{ok, LocalStreamRef} = quicer:start_stream(Conn, #{}),
	EventPidBin = term_to_binary(self()),
	{ok, _} = quicer:send(LocalStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6,
		"TEST:event_pid:", EventPidBin/binary>>),
	{nofin, <<"event_pid_received">>} = do_receive_data(LocalStreamRef),
	%% Cleanly terminate the CONNECT stream.
	_ = quicer:shutdown_stream(ConnectStreamRef),
	%% Receive the terminate event from the WT handler.
	receive
		{'$wt_echo_h', terminate, {closed, 0, <<>>}, _, _} ->
			ok
	after 1000 ->
		error({timeout, waiting_for_terminate_event})
	end.

connect_stream_closed_abruptly(Config) ->
	doc("The WT client may close the CONNECT stream abruptly. "
		"(draft_webtrans_http3 4.6)"),
	%% Connect to the WebTransport server.
	#{
		conn := Conn,
		connect_stream_ref := ConnectStreamRef,
		session_id := SessionID
	} = do_webtransport_connect(Config),
	%% Create a bidi stream, send a special instruction to make it propagate events.
	{ok, LocalStreamRef} = quicer:start_stream(Conn, #{}),
	EventPidBin = term_to_binary(self()),
	{ok, _} = quicer:send(LocalStreamRef, <<1:2, 16#41:14, 0:2, SessionID:6,
		"TEST:event_pid:", EventPidBin/binary>>),
	{nofin, <<"event_pid_received">>} = do_receive_data(LocalStreamRef),
	%% Abruptly terminate the CONNECT stream.
	_ = quicer:shutdown_stream(ConnectStreamRef, ?QUIC_STREAM_SHUTDOWN_FLAG_ABORT,
		0, infinity),
	%% Receive the terminate event from the WT handler.
	receive
		%% @todo It would be good to forward a stream error as well
		%%       so that a WT error can be sent, but I have been unsuccessful.
		{'$wt_echo_h', terminate, closed_abruptly, _, _} ->
			ok
	after 1000 ->
		error({timeout, waiting_for_terminate_event})
	end.

%% @todo This one is about gracefully closing HTTP/3 connection with WT sessions.
%% the endpoint SHOULD wait until all CONNECT streams have been closed by the peer before sending the CONNECTION_CLOSE (6)

%% Helpers.

do_webtransport_connect(Config) ->
	do_webtransport_connect(Config, []).

do_webtransport_connect(Config, ExtraHeaders) ->
	%% Connect to server.
	#{conn := Conn, settings := Settings} = rfc9114_SUITE:do_connect(Config, #{
		peer_unidi_stream_count => 100,
		datagram_send_enabled => 1,
		datagram_receive_enabled => 1
	}),
	%% Confirm that SETTINGS_ENABLE_CONNECT_PROTOCOL = 1.
	#{enable_connect_protocol := true} = Settings,
	%% Confirm that SETTINGS_WT_MAX_SESSIONS >= 1.
	#{wt_max_sessions := WTMaxSessions} = Settings,
	true = WTMaxSessions >= 1,
	%% Confirm that SETTINGS_H3_DATAGRAM = 1.
	#{h3_datagram := true} = Settings,
	%% Confirm that QUIC's max_datagram_size > 0.
	receive {quic, dgram_state_changed, Conn, DatagramState} ->
		#{
			dgram_max_len := DatagramMaxLen,
			dgram_send_enabled := DatagramSendEnabled
		} = DatagramState,
		true = DatagramMaxLen > 0,
		true = DatagramSendEnabled,
		ok
	after 5000 ->
		error({timeout, waiting_for_datagram_state_change})
	end,
	%% Send a CONNECT :protocol request to upgrade the stream to Websocket.
	{ok, ConnectStreamRef} = quicer:start_stream(Conn, #{}),
	{ok, EncodedRequest, _EncData, _EncSt} = cow_qpack:encode_field_section([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"webtransport">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, <<"/wt">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<"origin">>, <<"https://localhost">>}
	|ExtraHeaders], 0, cow_qpack:init(encoder)),
	{ok, _} = quicer:send(ConnectStreamRef, [
		<<1>>, %% HEADERS frame.
		cow_http3:encode_int(iolist_size(EncodedRequest)),
		EncodedRequest
	]),
	%% Receive a 200 response.
	{nofin, Data} = do_receive_data(ConnectStreamRef),
	{HLenEnc, HLenBits} = rfc9114_SUITE:do_guess_int_encoding(Data),
	<<
		1, %% HEADERS frame.
		HLenEnc:2, HLen:HLenBits,
		EncodedResponse:HLen/bytes
	>> = Data,
	{ok, DecodedResponse, _DecData, _DecSt}
		= cow_qpack:decode_field_section(EncodedResponse, 0, cow_qpack:init(decoder)),
	#{<<":status">> := <<"200">>} = maps:from_list(DecodedResponse),
	%% Retrieve the Session ID.
	{ok, SessionID} = quicer:get_stream_id(ConnectStreamRef),
	%% Accept QPACK streams to avoid conflicts with unidi streams from tests.
	Unidi1 = rfc9114_SUITE:do_accept_qpack_stream(Conn),
	Unidi2 = rfc9114_SUITE:do_accept_qpack_stream(Conn),
	%% Done.
	#{
		conn => Conn,
		connect_stream_ref => ConnectStreamRef,
		session_id => SessionID,
		resp_headers => DecodedResponse,
		enc_or_dec1 => Unidi1,
		enc_or_dec2 => Unidi2
	}.

do_receive_new_stream() ->
	receive
		{quic, new_stream, StreamRef, #{flags := Flags}} ->
			ok = quicer:setopt(StreamRef, active, true),
			case quicer:is_unidirectional(Flags) of
				true -> {unidi, StreamRef};
				false -> {bidi, StreamRef}
			end
	after 5000 ->
		error({timeout, waiting_for_stream})
	end.

do_receive_data(StreamRef) ->
	receive {quic, Data, StreamRef, #{flags := Flags}} ->
		IsFin = case Flags band ?QUIC_RECEIVE_FLAG_FIN of
			?QUIC_RECEIVE_FLAG_FIN -> fin;
			_ -> nofin
		end,
		{IsFin, Data}
	after 5000 ->
		error({timeout, waiting_for_data})
	end.

do_receive_datagram(Conn) ->
	receive {quic, <<0:2, QuarterID:6, Data/bits>>, Conn, Flags} when is_integer(Flags) ->
		{datagram, QuarterID * 4, Data}
	after 5000 ->
		ct:pal("~p", [process_info(self(), messages)]),
		error({timeout, waiting_for_datagram})
	end.

-else.

all() -> [].

groups() -> [].

-endif.
