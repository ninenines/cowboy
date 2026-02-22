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

%% QUIC transport using the pure Erlang erlang_quic library.

-module(cowboy_quic).

%% Connection.
-export([peername/1]).
-export([sockname/1]).
-export([peercert/1]).
-export([shutdown/2]).

%% Streams.
-export([start_bidi_stream/2]).
-export([start_unidi_stream/2]).
-export([setopt/4]).
-export([send/3]).
-export([send/4]).
-export([send_datagram/2]).
-export([shutdown_stream/2]).
-export([shutdown_stream/4]).

%% Messages.
-export([handle/1]).

-type connection_handle() :: reference().
-export_type([connection_handle/0]).

-type app_errno() :: non_neg_integer().

%% Connection.

-spec peername(connection_handle())
	-> {ok, {inet:ip_address(), inet:port_number()}}
	| {error, any()}.

peername(Conn) ->
	quic:peername(Conn).

-spec sockname(connection_handle())
	-> {ok, {inet:ip_address(), inet:port_number()}}
	| {error, any()}.

sockname(Conn) ->
	quic:sockname(Conn).

-spec peercert(connection_handle())
	-> {ok, public_key:der_encoded()}
	| {error, any()}.

peercert(Conn) ->
	quic:peercert(Conn).

-spec shutdown(connection_handle(), app_errno())
	-> ok | {error, any()}.

shutdown(Conn, ErrorCode) ->
	quic:close(Conn, ErrorCode).

%% Streams.

-spec start_bidi_stream(connection_handle(), iodata())
	-> {ok, cow_http3:stream_id()}
	| {error, any()}.

start_bidi_stream(Conn, InitialData) ->
	case quic:open_stream(Conn) of
		{ok, StreamID} ->
			case quic:send_data(Conn, StreamID, InitialData, false) of
				ok ->
					{ok, StreamID};
				Error ->
					Error
			end;
		Error ->
			Error
	end.

-spec start_unidi_stream(connection_handle(), iodata())
	-> {ok, cow_http3:stream_id()}
	| {error, any()}.

start_unidi_stream(Conn, InitialData) ->
	case quic:open_unidirectional_stream(Conn) of
		{ok, StreamID} ->
			case quic:send_data(Conn, StreamID, InitialData, false) of
				ok ->
					{ok, StreamID};
				Error ->
					Error
			end;
		Error ->
			Error
	end.

-spec setopt(connection_handle(), cow_http3:stream_id(), active, boolean())
	-> ok | {error, any()}.

setopt(Conn, _StreamID, active, _Value) ->
	%% erlang_quic uses process messages, always active
	%% Set connection-level options if needed
	quic:setopts(Conn, []).

-spec send(connection_handle(), cow_http3:stream_id(), iodata())
	-> ok | {error, any()}.

send(Conn, StreamID, Data) ->
	send(Conn, StreamID, Data, nofin).

-spec send(connection_handle(), cow_http3:stream_id(), iodata(), cow_http:fin())
	-> ok | {error, any()}.

send(Conn, StreamID, Data, IsFin) ->
	Fin = case IsFin of
		fin -> true;
		nofin -> false
	end,
	quic:send_data(Conn, StreamID, Data, Fin).

-spec send_datagram(connection_handle(), iodata())
	-> ok | {error, any()}.

send_datagram(Conn, Data) ->
	quic:send_datagram(Conn, Data).

-spec shutdown_stream(connection_handle(), cow_http3:stream_id())
	-> ok.

shutdown_stream(Conn, StreamID) ->
	_ = quic:reset_stream(Conn, StreamID, 0),
	ok.

-spec shutdown_stream(connection_handle(),
	cow_http3:stream_id(), both | receiving, app_errno())
	-> ok.

shutdown_stream(Conn, StreamID, _Dir, ErrorCode) ->
	_ = quic:reset_stream(Conn, StreamID, ErrorCode),
	ok.

%% Messages.
%%
%% Translate erlang_quic messages to cowboy_quic format.
%%
%% erlang_quic format:
%%   {quic, ConnRef, {stream_data, StreamId, Data, Fin}}
%%   {quic, ConnRef, {stream_opened, StreamId}}
%%   {quic, ConnRef, {stream_reset, StreamId, ErrorCode}}
%%   {quic, ConnRef, {closed, Reason}}
%%   {quic, ConnRef, {stop_sending, StreamId, ErrorCode}}
%%   {quic, ConnRef, {datagram, Data}}
%%
%% cowboy_quic format:
%%   {data, StreamID, fin|nofin, Data}
%%   {datagram, Data}
%%   {stream_started, StreamID, unidi|bidi}
%%   {stream_closed, StreamID, ErrorCode}
%%   closed
%%   {peer_send_shutdown, StreamID}

-spec handle({quic, reference(), term()})
	-> {data, cow_http3:stream_id(), cow_http:fin(), binary()}
	| {datagram, binary()}
	| {stream_started, cow_http3:stream_id(), unidi | bidi}
	| {stream_closed, cow_http3:stream_id(), app_errno()}
	| {goaway, cow_http3:stream_id()}
	| {transport_error, non_neg_integer(), binary()}
	| {send_ready, cow_http3:stream_id()}
	| closed
	| {peer_send_shutdown, cow_http3:stream_id()}
	| ok
	| unknown.

%% Stream data received.
handle({quic, _ConnRef, {stream_data, StreamID, Data, Fin}}) ->
	IsFin = case Fin of
		true -> fin;
		false -> nofin
	end,
	{data, StreamID, IsFin, Data};

%% Datagram received.
handle({quic, _ConnRef, {datagram, Data}}) ->
	{datagram, Data};

%% New stream opened by peer.
handle({quic, _ConnRef, {stream_opened, StreamID}}) ->
	%% Determine stream type from ID (bit 1: 0=bidi, 1=unidi)
	StreamType = case StreamID band 2 of
		0 -> bidi;
		2 -> unidi
	end,
	{stream_started, StreamID, StreamType};

%% Stream reset by peer.
handle({quic, _ConnRef, {stream_reset, StreamID, ErrorCode}}) ->
	{stream_closed, StreamID, ErrorCode};

%% Connection closed.
handle({quic, _ConnRef, {closed, _Reason}}) ->
	closed;

%% Peer initiated shutdown of sending.
handle({quic, _ConnRef, {stop_sending, StreamID, _ErrorCode}}) ->
	{peer_send_shutdown, StreamID};

%% Connection established (server receives this after handshake).
%% This is informational; the connection is already set up.
handle({quic, _ConnRef, {connected, _Info}}) ->
	ok;

%% Transport error received from peer or detected locally.
%% Forward to cowboy_http3 for proper connection termination.
handle({quic, _ConnRef, {transport_error, Code, Reason}}) ->
	{transport_error, Code, Reason};

%% GoAway received from peer - graceful shutdown initiated.
handle({quic, _ConnRef, {goaway, LastStreamID}}) ->
	{goaway, LastStreamID};

%% Session ticket received for 0-RTT resumption.
%% Currently informational; could be stored for client implementations.
handle({quic, _ConnRef, {session_ticket, _Ticket}}) ->
	ok;

%% Stream ready to send - flow control signal.
%% Forward to allow cowboy_http3 to resume sending on this stream.
handle({quic, _ConnRef, {send_ready, StreamID}}) ->
	{send_ready, StreamID};

%% Timer notification for internal QUIC timers.
%% Handled internally by erlang_quic.
handle({quic, _ConnRef, {timer, _NextTimeoutMs}}) ->
	ok;

%% Unknown message - let cowboy_http3 decide how to handle.
handle(_Msg) ->
	unknown.

%% EUnit tests for the handle/1 function.
-ifdef(TEST).

handle_stream_data_test() ->
	Conn = make_ref(),
	{data, 0, nofin, <<"data">>} = handle({quic, Conn, {stream_data, 0, <<"data">>, false}}).

handle_stream_data_fin_test() ->
	Conn = make_ref(),
	{data, 4, fin, <<"data">>} = handle({quic, Conn, {stream_data, 4, <<"data">>, true}}).

handle_stream_opened_bidi_test() ->
	Conn = make_ref(),
	%% Stream ID 0 has bit 1 = 0, so it's bidirectional
	{stream_started, 0, bidi} = handle({quic, Conn, {stream_opened, 0}}).

handle_stream_opened_unidi_test() ->
	Conn = make_ref(),
	%% Stream ID 2 has bit 1 = 1, so it's unidirectional
	{stream_started, 2, unidi} = handle({quic, Conn, {stream_opened, 2}}).

handle_stream_opened_client_initiated_bidi_test() ->
	Conn = make_ref(),
	%% Stream ID 4 (client-initiated, bidi) has bit 1 = 0
	{stream_started, 4, bidi} = handle({quic, Conn, {stream_opened, 4}}).

handle_stream_opened_client_initiated_unidi_test() ->
	Conn = make_ref(),
	%% Stream ID 6 (client-initiated, unidi) has bit 1 = 1
	{stream_started, 6, unidi} = handle({quic, Conn, {stream_opened, 6}}).

handle_datagram_test() ->
	Conn = make_ref(),
	{datagram, <<"dgram">>} = handle({quic, Conn, {datagram, <<"dgram">>}}).

handle_stream_reset_test() ->
	Conn = make_ref(),
	{stream_closed, 4, 256} = handle({quic, Conn, {stream_reset, 4, 256}}).

handle_closed_test() ->
	Conn = make_ref(),
	closed = handle({quic, Conn, {closed, normal}}).

handle_stop_sending_test() ->
	Conn = make_ref(),
	{peer_send_shutdown, 8} = handle({quic, Conn, {stop_sending, 8, 0}}).

handle_connected_test() ->
	Conn = make_ref(),
	ok = handle({quic, Conn, {connected, #{}}}).

handle_transport_error_test() ->
	Conn = make_ref(),
	{transport_error, 1, <<"error">>} = handle({quic, Conn, {transport_error, 1, <<"error">>}}).

handle_goaway_test() ->
	Conn = make_ref(),
	{goaway, 100} = handle({quic, Conn, {goaway, 100}}).

handle_session_ticket_test() ->
	Conn = make_ref(),
	ok = handle({quic, Conn, {session_ticket, <<"ticket">>}}).

handle_send_ready_test() ->
	Conn = make_ref(),
	{send_ready, 4} = handle({quic, Conn, {send_ready, 4}}).

handle_timer_test() ->
	Conn = make_ref(),
	ok = handle({quic, Conn, {timer, 1000}}).

handle_unknown_test() ->
	Conn = make_ref(),
	unknown = handle({quic, Conn, {unknown_event, data}}).

handle_completely_unknown_test() ->
	unknown = handle({something, completely, different}).

-endif. %% TEST
