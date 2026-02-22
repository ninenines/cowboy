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

%% A key difference between cowboy_http2 and cowboy_http3
%% is that HTTP/3 streams are QUIC streams and therefore
%% much of the connection state is handled outside of
%% Cowboy.

-module(cowboy_http3).

-include("cowboy_quic_adapter.hrl").

-export([init/4]).

%% Temporary callback to do sendfile over QUIC.
-export([send/2]).

%% @todo Graceful shutdown? Linger? Timeouts? Frame rates? PROXY header?
-type opts() :: #{
	compress_buffering => boolean(),
	compress_threshold => non_neg_integer(),
	connection_type => worker | supervisor,
	enable_connect_protocol => boolean(),
	env => cowboy_middleware:env(),
	logger => module(),
	max_decode_blocked_streams => 0..16#3fffffffffffffff,
	max_decode_table_size => 0..16#3fffffffffffffff,
	max_encode_blocked_streams => 0..16#3fffffffffffffff,
	max_encode_table_size => 0..16#3fffffffffffffff,
	max_ignored_frame_size_received => non_neg_integer() | infinity,
	metrics_callback => cowboy_metrics_h:metrics_callback(),
	metrics_req_filter => fun((cowboy_req:req()) -> map()),
	metrics_resp_headers_filter => fun((cowboy:http_headers()) -> cowboy:http_headers()),
	middlewares => [module()],
	shutdown_timeout => timeout(),
	stream_handlers => [module()],
	tracer_callback => cowboy_tracer_h:tracer_callback(),
	tracer_flags => [atom()],
	tracer_match_specs => cowboy_tracer_h:tracer_match_specs(),
	%% Open ended because configured stream handlers might add options.
	_ => _
}.
-export_type([opts/0]).

%% HTTP/3 or WebTransport stream.
%%
%% WebTransport sessions involve one bidirectional CONNECT stream
%% that must stay open (and can be used for signaling using the
%% Capsule Protocol) and an application-defined number of
%% unidirectional and bidirectional streams, as well as datagrams.
%%
%% WebTransport sessions run in the CONNECT request process and
%% all events related to the session is sent there as a message.
%% The pid of the process is kept in the state.
-record(stream, {
	id :: cow_http3:stream_id(),

	%% Whether the stream is currently in a special state.
	status :: header | {unidi, control | encoder | decoder}
		| normal | {data | ignore, non_neg_integer()} | stopping
		| {relaying, normal | {data, non_neg_integer()}, pid()}
		| {webtransport_session, normal | {ignore, non_neg_integer()}}
		| {webtransport_stream, cow_http3:stream_id()},

	%% Stream buffer.
	buffer = <<>> :: binary(),

	%% Stream state.
	state = undefined :: undefined | {module(), any()}
}).

-record(state, {
	parent :: pid(),
	ref :: ranch:ref(),
	conn :: ?QUIC_ADAPTER:connection_handle(),
	opts = #{} :: opts(),

	%% Remote address and port for the connection.
	peer = undefined :: {inet:ip_address(), inet:port_number()},

	%% Local address and port for the connection.
	sock = undefined :: {inet:ip_address(), inet:port_number()},

	%% Client certificate.
	cert :: undefined | binary(),

	%% HTTP/3 connection status for graceful shutdown.
	http3_status = connected :: connected | closing,

	%% HTTP/3 state machine.
	http3_machine :: cow_http3_machine:http3_machine(),

	%% Specially handled local unidi streams.
	local_control_id = undefined :: undefined | cow_http3:stream_id(),
	local_encoder_id = undefined :: undefined | cow_http3:stream_id(),
	local_decoder_id = undefined :: undefined | cow_http3:stream_id(),

	%% Bidirectional streams used for requests and responses,
	%% as well as unidirectional streams initiated by the client.
	streams = #{} :: #{cow_http3:stream_id() => #stream{}},

	%% Lingering streams that were recently reset. We may receive
	%% pending data or messages for these streams a short while
	%% after they have been reset.
	lingering_streams = [] :: [non_neg_integer()],

	%% Streams can spawn zero or more children which are then managed
	%% by this module if operating as a supervisor.
	children = cowboy_children:init() :: cowboy_children:children(),

	%% Stream reset rate limiting (CVE-2019-9514 protection).
	reset_rate_num = undefined :: undefined | pos_integer(),
	reset_rate_time = undefined :: undefined | integer()
}).

-spec init(pid(), ranch:ref(), ?QUIC_ADAPTER:connection_handle(), opts())
	-> no_return().

init(Parent, Ref, Conn, Opts) ->
	{ok, SettingsBin, HTTP3Machine0} = cow_http3_machine:init(server, Opts),
	%% Immediately open a control, encoder and decoder stream.
	%% @todo An endpoint MAY avoid creating an encoder stream if it will not be used (for example, if its encoder does not wish to use the dynamic table or if the maximum size of the dynamic table permitted by the peer is zero).
	%% @todo An endpoint MAY avoid creating a decoder stream if its decoder sets the maximum capacity of the dynamic table to zero.
	{ok, ControlID} = maybe_socket_error(undefined,
		?QUIC_ADAPTER:start_unidi_stream(Conn, [<<0>>, SettingsBin]),
		'A socket error occurred when opening the control stream.'),
	{ok, EncoderID} = maybe_socket_error(undefined,
		?QUIC_ADAPTER:start_unidi_stream(Conn, <<2>>),
		'A socket error occurred when opening the encoder stream.'),
	{ok, DecoderID} = maybe_socket_error(undefined,
		?QUIC_ADAPTER:start_unidi_stream(Conn, <<3>>),
		'A socket error occurred when opening the encoder stream.'),
	%% Set the control, encoder and decoder streams in the machine.
	HTTP3Machine = cow_http3_machine:init_unidi_local_streams(
		ControlID, EncoderID, DecoderID, HTTP3Machine0),
	%% Get the peername/sockname/cert.
	{ok, Peer} = maybe_socket_error(undefined, ?QUIC_ADAPTER:peername(Conn),
		'A socket error occurred when retrieving the peer name.'),
	{ok, Sock} = maybe_socket_error(undefined, ?QUIC_ADAPTER:sockname(Conn),
		'A socket error occurred when retrieving the sock name.'),
	CertResult = case ?QUIC_ADAPTER:peercert(Conn) of
		{error, no_peercert} ->
			{ok, undefined};
		Cert0 ->
			Cert0
	end,
	{ok, Cert} = maybe_socket_error(undefined, CertResult,
		'A socket error occurred when retrieving the client TLS certificate.'),
	%% Initialize rate limiting.
	CurrentTime = erlang:monotonic_time(millisecond),
	State0 = #state{parent=Parent, ref=Ref, conn=Conn,
		opts=Opts, peer=Peer, sock=Sock, cert=Cert,
		http3_machine=HTTP3Machine, local_control_id=ControlID,
		local_encoder_id=EncoderID, local_decoder_id=DecoderID},
	State = init_reset_rate_limiting(State0, CurrentTime),
	%% Quick! Let's go!
	loop(State).

loop(State0=#state{opts=Opts, children=Children}) ->
	receive
		Msg when element(1, Msg) =:= quic ->
			handle_quic_msg(State0, Msg);
		%% Timeouts.
		{timeout, Ref, {shutdown, Pid}} ->
			cowboy_children:shutdown_timeout(Children, Ref, Pid),
			loop(State0);
		%% Messages pertaining to a stream.
		{{Pid, StreamID}, Msg} when Pid =:= self() ->
			loop(info(State0, StreamID, Msg));
		{'$cowboy_relay_command', {Pid, StreamID}, RelayCommand} when Pid =:= self() ->
			loop(relay_command(State0, StreamID, RelayCommand));
		%% WebTransport commands.
		{'$webtransport_commands', SessionID, Commands} ->
			loop(webtransport_commands(State0, SessionID, Commands));
		%% Exit signal from children.
		Msg = {'EXIT', Pid, _} ->
			loop(down(State0, Pid, Msg));
		Msg ->
			cowboy:log(warning, "Received stray message ~p.", [Msg], Opts),
			loop(State0)
	end.

handle_quic_msg(State0=#state{opts=Opts}, Msg) ->
	case ?QUIC_ADAPTER:handle(Msg) of
		{data, StreamID, IsFin, Data} ->
			parse(State0, StreamID, Data, IsFin);
		{datagram, Data} ->
			parse_datagram(State0, Data);
		{stream_started, StreamID, StreamType} ->
			State = stream_new_remote(State0, StreamID, StreamType),
			loop(State);
		{stream_closed, StreamID, ErrorCode} ->
			State = stream_closed(State0, StreamID, ErrorCode),
			loop(State);
		{peer_send_shutdown, StreamID} ->
			State = stream_peer_send_shutdown(State0, StreamID),
			loop(State);
		{goaway, LastStreamID} ->
			goaway(State0, {goaway, LastStreamID});
		{transport_error, Code, Reason} ->
			Reason1 = {connection_error, {transport_error, Code},
				iolist_to_binary([<<"Transport error: ">>, Reason])},
			terminate(State0, Reason1);
		{send_ready, _StreamID} ->
			%% Flow control signal - stream is ready to send more data.
			%% Currently we send data immediately, so this is informational.
			loop(State0);
		closed ->
			%% @todo Different error reason if graceful?
			Reason = {socket_error, closed, 'The socket has been closed.'},
			terminate(State0, Reason);
		ok ->
			loop(State0);
		unknown ->
			cowboy:log(warning, "Received unknown QUIC message ~p.", [Msg], Opts),
			loop(State0)
	end.

parse(State, StreamID, Data, IsFin) ->
	case stream_get(State, StreamID) of
		Stream=#stream{buffer= <<>>} ->
			parse1(State, Stream, Data, IsFin);
		Stream=#stream{buffer=Buffer} ->
			Stream1 = Stream#stream{buffer= <<>>},
			parse1(stream_store(State, Stream1),
				Stream1, <<Buffer/binary, Data/binary>>, IsFin);
		%% Pending data for a stream that has been reset. Ignore.
		error ->
			case is_lingering_stream(State, StreamID) of
				true ->
					loop(State);
				false ->
					%% Stream not found. For erlang_quic, streams are created
					%% lazily when data arrives (no stream_opened notification).
					%% Determine stream type from ID and create the stream.
					StreamType = case StreamID band 2 of
						0 -> bidi;
						2 -> unidi
					end,
					State1 = stream_new_remote(State, StreamID, StreamType),
					Stream = stream_get(State1, StreamID),
					parse1(State1, Stream, Data, IsFin)
			end
	end.

parse1(State, Stream=#stream{status=header}, Data, IsFin) ->
	parse_unidirectional_stream_header(State, Stream, Data, IsFin);
parse1(State=#state{http3_machine=HTTP3Machine0},
		#stream{status={unidi, Type}, id=StreamID}, Data, IsFin)
		when Type =:= encoder; Type =:= decoder ->
	case cow_http3_machine:unidi_data(Data, IsFin, StreamID, HTTP3Machine0) of
		{ok, Instrs, HTTP3Machine} ->
			loop(send_instructions(State#state{http3_machine=HTTP3Machine}, Instrs));
		{error, Error={connection_error, _, _}, HTTP3Machine} ->
			terminate(State#state{http3_machine=HTTP3Machine}, Error)
	end;
%% Handle FIN on WebTransport session CONNECT stream.
%% When the CONNECT stream receives FIN, it's equivalent to a clean close.
parse1(State=#state{conn=Conn}, Stream=#stream{id=SessionID, status=
		{webtransport_session, _}}, <<>>, fin) ->
	webtransport_event(State, SessionID, {closed, 0, <<>>}),
	?QUIC_ADAPTER:shutdown_stream(Conn, SessionID),
	loop(webtransport_terminate_session(State, Stream));
parse1(State=#state{conn=Conn}, Stream=#stream{id=SessionID, status=
		{webtransport_session, normal}}, Data, IsFin) ->
	case cow_capsule:parse(Data) of
		{ok, wt_drain_session, Rest} ->
			webtransport_event(State, SessionID, close_initiated),
			parse1(State, Stream, Rest, IsFin);
		{ok, {wt_close_session, AppCode, AppMsg}, Rest} ->
			%% This event will be handled specially and lead
			%% to the termination of the session process.
			webtransport_event(State, SessionID, {closed, AppCode, AppMsg}),
			%% Shutdown the CONNECT stream immediately.
			?QUIC_ADAPTER:shutdown_stream(Conn, SessionID),
			%% @todo Will we receive a {stream_closed,...} after that?
			%% If any data is received past that point this is an error.
			%% @todo Don't crash, error out properly.
			<<>> = Rest,
			loop(webtransport_terminate_session(State, Stream));
		more ->
			loop(stream_store(State, Stream#stream{buffer=Data}));
		%% Ignore unhandled/unknown capsules.
		%% @todo Do this when cow_capsule includes some.
%		{ok, _, Rest} ->
%			parse1(State, Stream, Rest, IsFin);
%		{ok, Rest} ->
%			parse1(State, Stream, Rest, IsFin);
		%% @todo Make the max length configurable?
		{skip, Len} when Len =< 8192 ->
			loop(stream_store(State, Stream#stream{
				status={webtransport_session, {ignore, Len}}}));
		{skip, _Len} ->
			reset_stream(State, Stream, {stream_error, h3_message_error,
				'Capsule payload exceeds maximum allowed size.'});
		error ->
			reset_stream(State, Stream, {stream_error, h3_message_error,
				'Failed to parse capsule on WebTransport session stream.'})
	end;
parse1(State, Stream=#stream{status=
		{webtransport_session, {ignore, Len}}}, Data, IsFin) ->
	case Data of
		<<_:Len/unit:8, Rest/bits>> ->
			parse1(State, Stream#stream{status={webtransport_session, normal}}, Rest, IsFin);
		_ ->
			loop(stream_store(State, Stream#stream{
				status={webtransport_session, {ignore, Len - byte_size(Data)}}}))
	end;
parse1(State, #stream{id=StreamID, status={webtransport_stream, SessionID}}, Data, IsFin) ->
	webtransport_event(State, SessionID, {stream_data, StreamID, IsFin, Data}),
	%% No need to store the stream again, WT streams don't get changed here.
	loop(State);
parse1(State, Stream=#stream{status={data, Len}, id=StreamID}, Data, IsFin) ->
	DataLen = byte_size(Data),
	if
		DataLen < Len ->
			%% We don't have the full frame but this is the end of the
			%% data we have. So FrameIsFin is equivalent to IsFin here.
			loop(frame(State, Stream#stream{status={data, Len - DataLen}}, {data, Data}, IsFin));
		true ->
			<<Data1:Len/binary, Rest/bits>> = Data,
			FrameIsFin = is_fin(IsFin, Rest),
			parse(frame(State, Stream#stream{status=normal}, {data, Data1}, FrameIsFin),
				StreamID, Rest, IsFin)
	end;
%% This clause mirrors the {data, Len} clause.
parse1(State, Stream=#stream{status={relaying, {data, Len}, RelayPid}, id=StreamID},
		Data, IsFin) ->
	DataLen = byte_size(Data),
	if
		DataLen < Len ->
			%% We don't have the full frame but this is the end of the
			%% data we have. So FrameIsFin is equivalent to IsFin here.
			loop(frame(State, Stream#stream{status={relaying, {data, Len - DataLen}, RelayPid}},
				{data, Data}, IsFin));
		true ->
			<<Data1:Len/binary, Rest/bits>> = Data,
			FrameIsFin = is_fin(IsFin, Rest),
			parse(frame(State, Stream#stream{status={relaying, normal, RelayPid}},
				{data, Data1}, FrameIsFin), StreamID, Rest, IsFin)
	end;
parse1(State, Stream=#stream{status={ignore, Len}, id=StreamID}, Data, IsFin) ->
	DataLen = byte_size(Data),
	if
		DataLen < Len ->
			loop(stream_store(State, Stream#stream{status={ignore, Len - DataLen}}));
		true ->
			<<_:Len/binary, Rest/bits>> = Data,
			parse(stream_store(State, Stream#stream{status=normal}),
				StreamID, Rest, IsFin)
	end;
%% @todo Clause that discards receiving data for stopping streams.
%%       We may receive a few more frames after we abort receiving.
parse1(State=#state{opts=Opts}, Stream=#stream{status=Status0, id=StreamID}, Data, IsFin) ->
	case cow_http3:parse(Data) of
		{ok, Frame, Rest} ->
			FrameIsFin = is_fin(IsFin, Rest),
			parse(frame(State, Stream, Frame, FrameIsFin), StreamID, Rest, IsFin);
		%% The WebTransport stream header is not a real frame.
		{webtransport_stream_header, SessionID, Rest} ->
			become_webtransport_stream(State, Stream, bidi, SessionID, Rest, IsFin);
		{more, Frame = {data, _}, Len} ->
			%% We're at the end of the data so FrameIsFin is equivalent to IsFin.
			case IsFin of
				nofin when element(1, Status0) =:= relaying ->
					%% The stream will be stored at the end of processing commands.
					Status = setelement(2, Status0, {data, Len}),
					loop(frame(State, Stream#stream{status=Status}, Frame, nofin));
				nofin ->
					%% The stream will be stored at the end of processing commands.
					loop(frame(State, Stream#stream{status={data, Len}}, Frame, nofin));
				fin ->
					terminate(State, {connection_error, h3_frame_error,
						'Last frame on stream was truncated. (RFC9114 7.1)'})
			end;
		{more, ignore, Len} ->
			%% @todo This setting should be tested.
			%%
			%% While the default value doesn't warrant doing a streaming ignore
			%% (and could work just fine with the 'more' clause), this value
			%% is configurable and users may want to set it large.
			MaxIgnoredLen = maps:get(max_ignored_frame_size_received, Opts, 16384),
			%% We're at the end of the data so FrameIsFin is equivalent to IsFin.
			case IsFin of
				nofin when Len < MaxIgnoredLen ->
					%% We are not processing commands so we must store the stream.
					%% We also call ignored_frame here; we will not need to call
					%% it again when ignoring the rest of the data.
					Stream1 = Stream#stream{status={ignore, Len}},
					State1 = ignored_frame(State, Stream1),
					loop(stream_store(State1, Stream1));
				nofin ->
					terminate(State, {connection_error, h3_excessive_load,
						'Ignored frame larger than limit. (RFC9114 10.5)'});
				fin ->
					terminate(State, {connection_error, h3_frame_error,
						'Last frame on stream was truncated. (RFC9114 7.1)'})
			end;
		{ignore, Rest} ->
			parse(ignored_frame(State, Stream), StreamID, Rest, IsFin);
		Error = {connection_error, _, _} ->
			terminate(State, Error);
		more when Data =:= <<>> ->
			%% The buffer was already reset to <<>>.
			loop(stream_store(State, Stream));
		more ->
			%% We're at the end of the data so FrameIsFin is equivalent to IsFin.
			case IsFin of
				nofin ->
					loop(stream_store(State, Stream#stream{buffer=Data}));
				fin ->
					terminate(State, {connection_error, h3_frame_error,
						'Last frame on stream was truncated. (RFC9114 7.1)'})
			end
	end.

%% We may receive multiple frames in a single QUIC packet.
%% The FIN flag applies to the QUIC packet, not to the frame.
%% We must therefore only consider the frame to have a FIN
%% flag if there's no data remaining to be read.
is_fin(fin, <<>>) -> fin;
is_fin(_, _) -> nofin.

parse_unidirectional_stream_header(State0=#state{http3_machine=HTTP3Machine0},
		Stream0=#stream{id=StreamID}, Data, IsFin) ->
	case cow_http3:parse_unidi_stream_header(Data) of
		{ok, Type, Rest} when Type =:= control; Type =:= encoder; Type =:= decoder ->
			case cow_http3_machine:set_unidi_remote_stream_type(
					StreamID, Type, HTTP3Machine0) of
				{ok, HTTP3Machine} ->
					State = State0#state{http3_machine=HTTP3Machine},
					Stream = Stream0#stream{status={unidi, Type}},
					parse(stream_store(State, Stream), StreamID, Rest, IsFin);
				{error, Error={connection_error, _, _}, HTTP3Machine} ->
					terminate(State0#state{http3_machine=HTTP3Machine}, Error)
			end;
		%% @todo Perhaps do this in cow_http3_machine directly.
		{ok, push, _} ->
			terminate(State0, {connection_error, h3_stream_creation_error,
				'Only servers can push. (RFC9114 6.2.2)'});
		{ok, {webtransport, SessionID}, Rest} ->
			become_webtransport_stream(State0, Stream0, unidi, SessionID, Rest, IsFin);
		%% Unknown stream types must be ignored. We choose to abort the
		%% stream instead of reading and discarding the incoming data.
		{undefined, _} ->
			loop(stream_abort_receive(State0, Stream0, h3_stream_creation_error));
		%% Very unlikely to happen but WebTransport headers may be fragmented
		%% as they are more than one byte. The fin flag in this case is an error,
		%% but because it happens in WebTransport application data (the Session ID)
		%% we only reset the impacted stream and not the entire connection.
		more when IsFin =:= fin ->
			loop(stream_abort_receive(State0, Stream0, h3_stream_creation_error));
		more ->
			loop(stream_store(State0, Stream0#stream{buffer=Data}))
	end.

frame(State=#state{http3_machine=HTTP3Machine0},
		Stream=#stream{id=StreamID}, Frame, IsFin) ->
	case cow_http3_machine:frame(Frame, IsFin, StreamID, HTTP3Machine0) of
		{ok, HTTP3Machine} ->
			State#state{http3_machine=HTTP3Machine};
		{ok, {data, Data}, HTTP3Machine} ->
			data_frame(State#state{http3_machine=HTTP3Machine}, Stream, IsFin, Data);
		{ok, {headers, Headers, PseudoHeaders, BodyLen}, Instrs, HTTP3Machine} ->
			headers_frame(send_instructions(State#state{http3_machine=HTTP3Machine}, Instrs),
				Stream, IsFin, Headers, PseudoHeaders, BodyLen);
		{ok, {trailers, Trailers}, Instrs, HTTP3Machine} ->
			State1 = send_instructions(State#state{http3_machine=HTTP3Machine}, Instrs),
			trailers_frame(State1, Stream, Trailers);
		{ok, GoAway={goaway, _}, HTTP3Machine} ->
			goaway(State#state{http3_machine=HTTP3Machine}, GoAway);
		{error, Error={stream_error, _Reason, _Human}, Instrs, HTTP3Machine} ->
			State1 = send_instructions(State#state{http3_machine=HTTP3Machine}, Instrs),
			reset_stream(State1, Stream, Error);
		{error, Error={connection_error, _, _}, HTTP3Machine} ->
			terminate(State#state{http3_machine=HTTP3Machine}, Error)
	end.

data_frame(State, Stream=#stream{status={relaying, _, RelayPid}, id=StreamID}, IsFin, Data) ->
	RelayPid ! {'$cowboy_relay_data', {self(), StreamID}, IsFin, Data},
	stream_store(State, Stream);
data_frame(State=#state{opts=Opts},
		Stream=#stream{id=StreamID, state=StreamState0}, IsFin, Data) ->
	try cowboy_stream:data(StreamID, IsFin, Data, StreamState0) of
		{Commands, StreamState} ->
			commands(State, Stream#stream{state=StreamState}, Commands)
	catch Class:Exception:Stacktrace ->
		cowboy:log(cowboy_stream:make_error_log(data,
			[StreamID, IsFin, Data, StreamState0],
			Class, Exception, Stacktrace), Opts),
		reset_stream(State, Stream, {internal_error, {Class, Exception},
			'Unhandled exception in cowboy_stream:data/4.'})
	end.

trailers_frame(State, Stream=#stream{status={relaying, _, RelayPid}, id=StreamID}, Trailers) ->
	RelayPid ! {'$cowboy_relay_trailers', {self(), StreamID}, headers_to_map(Trailers, #{})},
	stream_store(State, Stream);
trailers_frame(State=#state{opts=Opts},
		Stream=#stream{id=StreamID, state=StreamState0}, Trailers) ->
	try cowboy_stream:info(StreamID, {trailers, headers_to_map(Trailers, #{})}, StreamState0) of
		{Commands, StreamState} ->
			commands(State, Stream#stream{state=StreamState}, Commands)
	catch Class:Exception:Stacktrace ->
		cowboy:log(cowboy_stream:make_error_log(info,
			[StreamID, {trailers, Trailers}, StreamState0],
			Class, Exception, Stacktrace), Opts),
		reset_stream(State, Stream, {internal_error, {Class, Exception},
			'Unhandled exception in cowboy_stream:info/3.'})
	end.

%% Regular CONNECT method (RFC 9110 Section 9.3.6).
%% Only :method and :authority pseudo-headers are allowed.
headers_frame(State=#state{ref=Ref, peer=Peer, sock=Sock, cert=Cert},
		Stream=#stream{id=StreamID}, IsFin, Headers,
		PseudoHeaders=#{method := <<"CONNECT">>, authority := Authority}, _)
		when map_size(PseudoHeaders) =:= 2 ->
	try cow_http_hd:parse_host(Authority) of
		{Host, Port0} ->
			Port = case Port0 of undefined -> 443; _ -> Port0 end,
			Req = #{
				ref => Ref,
				pid => self(),
				streamid => StreamID,
				peer => Peer,
				sock => Sock,
				cert => Cert,
				method => <<"CONNECT">>,
				scheme => <<"https">>,
				host => Host,
				port => Port,
				path => <<>>,
				qs => <<>>,
				version => 'HTTP/3',
				headers => headers_to_map(Headers, #{}),
				has_body => IsFin =:= nofin,
				body_length => undefined
			},
			headers_frame(State, Stream, Req)
	catch _:_ ->
		reset_stream(State, Stream, {stream_error, h3_message_error,
			'Invalid :authority pseudo-header in CONNECT request.'})
	end;
headers_frame(State, Stream, IsFin, Headers,
		PseudoHeaders=#{method := <<"TRACE">>}, _) ->
	early_error(State, Stream, IsFin, Headers, PseudoHeaders, 501,
		'The TRACE method is currently not implemented. (RFC9114 4.4, RFC7231 4.3.8)');
headers_frame(State, Stream, IsFin, Headers, PseudoHeaders=#{authority := Authority}, BodyLen) ->
	headers_frame_parse_host(State, Stream, IsFin, Headers, PseudoHeaders, BodyLen, Authority);
headers_frame(State, Stream, IsFin, Headers, PseudoHeaders, BodyLen) ->
	case lists:keyfind(<<"host">>, 1, Headers) of
		{_, Authority} ->
			headers_frame_parse_host(State, Stream, IsFin, Headers, PseudoHeaders, BodyLen, Authority);
		_ ->
			reset_stream(State, Stream, {stream_error, h3_message_error,
				'Requests translated from HTTP/1.1 must include a host header. (RFC7540 8.1.2.3, RFC7230 5.4)'})
	end.

headers_frame_parse_host(State=#state{ref=Ref, peer=Peer, sock=Sock, cert=Cert},
		Stream=#stream{id=StreamID}, IsFin, Headers,
		PseudoHeaders=#{method := Method, scheme := Scheme, path := PathWithQs},
		BodyLen, Authority) ->
	try cow_http_hd:parse_host(Authority) of
		{Host, Port0} ->
			Port = ensure_port(Scheme, Port0),
			try cow_http:parse_fullpath(PathWithQs) of
				{<<>>, _} ->
					reset_stream(State, Stream, {stream_error, h3_message_error,
						'The path component must not be empty. (RFC7540 8.1.2.3)'});
				{Path, Qs} ->
					Req0 = #{
						ref => Ref,
						pid => self(),
						streamid => StreamID,
						peer => Peer,
						sock => Sock,
						cert => Cert,
						method => Method,
						scheme => Scheme,
						host => Host,
						port => Port,
						path => Path,
						qs => Qs,
						version => 'HTTP/3',
						headers => headers_to_map(Headers, #{}),
						has_body => IsFin =:= nofin,
						body_length => BodyLen
					},
					%% We add the protocol information for extended CONNECTs.
					Req = case PseudoHeaders of
						#{protocol := Protocol} -> Req0#{protocol => Protocol};
						_ -> Req0
					end,
					headers_frame(State, Stream, Req)
			catch _:_ ->
				reset_stream(State, Stream, {stream_error, h3_message_error,
					'The :path pseudo-header is invalid. (RFC7540 8.1.2.3)'})
			end
	catch _:_ ->
		reset_stream(State, Stream, {stream_error, h3_message_error,
			'The :authority pseudo-header is invalid. (RFC7540 8.1.2.3)'})
	end.

%% @todo Copied from cowboy_http2.
%% @todo How to handle "http"?
ensure_port(<<"http">>, undefined) -> 80;
ensure_port(<<"https">>, undefined) -> 443;
ensure_port(_, Port) -> Port.

%% @todo Copied from cowboy_http2.
%% This function is necessary to properly handle duplicate headers
%% and the special-case cookie header.
headers_to_map([], Acc) ->
	Acc;
headers_to_map([{Name, Value}|Tail], Acc0) ->
	Acc = case Acc0 of
		%% The cookie header does not use proper HTTP header lists.
		#{Name := Value0} when Name =:= <<"cookie">> ->
			Acc0#{Name => << Value0/binary, "; ", Value/binary >>};
		#{Name := Value0} ->
			Acc0#{Name => << Value0/binary, ", ", Value/binary >>};
		_ ->
			Acc0#{Name => Value}
	end,
	headers_to_map(Tail, Acc).

%% @todo WebTransport CONNECT requests must have extra checks on settings.
%% @todo We may also need to defer them if we didn't get settings.
headers_frame(State=#state{opts=Opts}, Stream=#stream{id=StreamID}, Req) ->
	try cowboy_stream:init(StreamID, Req, Opts) of
		{Commands, StreamState} ->
			commands(State, Stream#stream{state=StreamState}, Commands)
	catch Class:Exception:Stacktrace ->
		cowboy:log(cowboy_stream:make_error_log(init,
			[StreamID, Req, Opts],
			Class, Exception, Stacktrace), Opts),
		reset_stream(State, Stream, {internal_error, {Class, Exception},
			'Unhandled exception in cowboy_stream:init/3.'})
	end.

early_error(State0=#state{ref=Ref, opts=Opts, peer=Peer},
		Stream=#stream{id=StreamID}, _IsFin, Headers, #{method := Method},
		StatusCode0, HumanReadable) ->
	%% We automatically terminate the stream but it is not an error
	%% per se (at least not in the first implementation).
	Reason = {stream_error, h3_no_error, HumanReadable},
	%% The partial Req is minimal for now. We only have one case
	%% where it can be called (when a method is completely disabled).
	PartialReq = #{
		ref => Ref,
		peer => Peer,
		method => Method,
		headers => headers_to_map(Headers, #{})
	},
	Resp = {response, StatusCode0, RespHeaders0=#{<<"content-length">> => <<"0">>}, <<>>},
	try cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts) of
		{response, StatusCode, RespHeaders, RespBody} ->
			send_response(State0, Stream, StatusCode, RespHeaders, RespBody)
	catch Class:Exception:Stacktrace ->
		cowboy:log(cowboy_stream:make_error_log(early_error,
			[StreamID, Reason, PartialReq, Resp, Opts],
			Class, Exception, Stacktrace), Opts),
		%% We still need to send an error response, so send what we initially
		%% wanted to send. It's better than nothing.
		send_headers(State0, Stream, fin, StatusCode0, RespHeaders0)
	end.

%% Datagrams.

parse_datagram(State, Data0) ->
	{SessionID, Data} = cow_http3:parse_datagram(Data0),
	case stream_get(State, SessionID) of
		#stream{status={webtransport_session, _}} ->
			webtransport_event(State, SessionID, {datagram, Data}),
			loop(State);
		_ ->
			%% Datagram for unknown/terminated session - silently discard.
			%% This can happen for datagrams arriving for sessions that
			%% have already been terminated, or for future sessions
			%% that haven't been established yet.
			loop(State)
	end.

%% Erlang messages.

down(State0=#state{opts=Opts, children=Children0}, Pid, Msg) ->
	State = case cowboy_children:down(Children0, Pid) of
		%% The stream was terminated already.
		{ok, undefined, Children} ->
			State0#state{children=Children};
		%% The stream is still running.
		{ok, StreamID, Children} ->
			info(State0#state{children=Children}, StreamID, Msg);
		%% The process was unknown.
		error ->
			cowboy:log(warning, "Received EXIT signal ~p for unknown process ~p.~n",
				[Msg, Pid], Opts),
			State0
	end,
	if
%% @todo
%		State#state.http2_status =:= closing, State#state.streams =:= #{} ->
%			terminate(State, {stop, normal, 'The connection is going away.'});
		true ->
			State
	end.

info(State=#state{opts=Opts, http3_machine=_HTTP3Machine}, StreamID, Msg) ->
	case stream_get(State, StreamID) of
		Stream=#stream{state=StreamState0} ->
			try cowboy_stream:info(StreamID, Msg, StreamState0) of
				{Commands, StreamState} ->
					commands(State, Stream#stream{state=StreamState}, Commands)
			catch Class:Exception:Stacktrace ->
				cowboy:log(cowboy_stream:make_error_log(info,
					[StreamID, Msg, StreamState0],
					Class, Exception, Stacktrace), Opts),
				reset_stream(State, Stream, {internal_error, {Class, Exception},
					'Unhandled exception in cowboy_stream:info/3.'})
			end;
		error ->
			case is_lingering_stream(State, StreamID) of
				true ->
					ok;
				false ->
					cowboy:log(warning, "Received message ~p for unknown stream ~p.",
						[Msg, StreamID], Opts)
			end,
			State
	end.

%% Stream handler commands.

commands(State, Stream, []) ->
	stream_store(State, Stream);
%% Error responses are sent only if a response wasn't sent already.
commands(State=#state{http3_machine=HTTP3Machine}, Stream=#stream{id=StreamID},
		[{error_response, StatusCode, Headers, Body}|Tail]) ->
	case cow_http3_machine:get_bidi_stream_local_state(StreamID, HTTP3Machine) of
		{ok, idle} ->
			commands(State, Stream, [{response, StatusCode, Headers, Body}|Tail]);
		_ ->
			commands(State, Stream, Tail)
	end;
%% Send an informational response.
commands(State0, Stream, [{inform, StatusCode, Headers}|Tail]) ->
	State = send_headers(State0, Stream, idle, StatusCode, Headers),
	commands(State, Stream, Tail);
%% Send response headers.
commands(State0, Stream, [{response, StatusCode, Headers, Body}|Tail]) ->
	State = send_response(State0, Stream, StatusCode, Headers, Body),
	commands(State, Stream, Tail);
%% Send response headers.
commands(State0, Stream, [{headers, StatusCode, Headers}|Tail]) ->
	State = send_headers(State0, Stream, nofin, StatusCode, Headers),
	commands(State, Stream, Tail);
%%% Send a response body chunk.
commands(State0=#state{conn=Conn}, Stream=#stream{id=StreamID}, [{data, IsFin, Data}|Tail]) ->
	_ = case Data of
		{sendfile, Offset, Bytes, Path} ->
			%% Temporary solution to do sendfile over QUIC.
			{ok, _} = ranch_transport:sendfile(?MODULE, {Conn, StreamID},
				Path, Offset, Bytes, []),
			ok = maybe_socket_error(State0,
				?QUIC_ADAPTER:send(Conn, StreamID, cow_http3:data(<<>>), IsFin));
		_ ->
			ok = maybe_socket_error(State0,
				?QUIC_ADAPTER:send(Conn, StreamID, cow_http3:data(Data), IsFin))
	end,
	State = maybe_send_is_fin(State0, Stream, IsFin),
	commands(State, Stream, Tail);
%%% Send trailers.
commands(State0=#state{conn=Conn, http3_machine=HTTP3Machine0},
		Stream=#stream{id=StreamID}, [{trailers, Trailers}|Tail]) ->
	State = case cow_http3_machine:prepare_trailers(
			StreamID, HTTP3Machine0, maps:to_list(Trailers)) of
		{trailers, HeaderBlock, Instrs, HTTP3Machine} ->
			State1 = send_instructions(State0#state{http3_machine=HTTP3Machine}, Instrs),
			ok = maybe_socket_error(State1,
				?QUIC_ADAPTER:send(Conn, StreamID, cow_http3:headers(HeaderBlock), fin)),
			State1;
		{no_trailers, HTTP3Machine} ->
			ok = maybe_socket_error(State0,
				?QUIC_ADAPTER:send(Conn, StreamID, cow_http3:data(<<>>), fin)),
			State0#state{http3_machine=HTTP3Machine}
	end,
	commands(State, Stream, Tail);
%% Server push is not implemented.
%%
%% HTTP/2 and HTTP/3 server push has been deprecated and removed by all major
%% browsers: Chrome removed support in v106 (2022), Firefox in v132 (Oct 2024).
%% The feature provided minimal real-world benefit and added complexity.
%% Therefore, push promises are intentionally not supported.
%%
%% Read the request body.
%%
%% Unlike HTTP/2, QUIC handles flow control automatically at the transport layer.
%% The QUIC stack sends MAX_STREAM_DATA frames to the peer when it's ready to
%% receive more data. Therefore, the {flow, Size} command is a no-op for HTTP/3.
commands(State, Stream, [{flow, _Size}|Tail]) ->
	commands(State, Stream, Tail);
%% Supervise a child process.
commands(State=#state{children=Children}, Stream=#stream{id=StreamID},
		[{spawn, Pid, Shutdown}|Tail]) ->
	 commands(State#state{children=cowboy_children:up(Children, Pid, StreamID, Shutdown)},
		Stream, Tail);
%% Error handling.
commands(State, Stream, [Error = {internal_error, _, _}|_Tail]) ->
	%% @todo Do we want to run the commands after an internal_error?
	%% @todo Do we even allow commands after?
	%% @todo Only reset when the stream still exists.
	reset_stream(State, Stream, Error);
%% Use a different protocol within the stream (CONNECT :protocol).
%% @todo Make sure we error out when the feature is disabled.
commands(State0, Stream0=#stream{id=StreamID},
		[{switch_protocol, Headers, cowboy_webtransport, WTState=#{}}|Tail]) ->
	State = info(stream_store(State0, Stream0), StreamID, {headers, 200, Headers}),
	#state{http3_machine=HTTP3Machine0} = State,
	Stream1 = #stream{state=StreamState} = stream_get(State, StreamID),
	%% The stream becomes a WT session at that point. It is the
	%% parent stream of all streams in this WT session. The
	%% cowboy_stream state is kept because it will be needed
	%% to terminate the stream properly.
	HTTP3Machine = cow_http3_machine:become_webtransport_session(StreamID, HTTP3Machine0),
	Stream = Stream1#stream{
		status={webtransport_session, normal},
		state={cowboy_webtransport, WTState#{stream_state => StreamState}}
	},
	%% @todo We must propagate the buffer to capsule handling if any.
	commands(State#state{http3_machine=HTTP3Machine}, Stream, Tail);
%% There are two data_delivery: stream_handlers and relay.
%% The former just has the data go through stream handlers
%% like normal requests. The latter relays data directly.
commands(State0, Stream0=#stream{id=StreamID},
		[{switch_protocol, Headers, _Mod, ModState=#{data_delivery := relay}}|Tail]) ->
	State = info(stream_store(State0, Stream0), StreamID, {headers, 200, Headers}),
	Stream1 = #stream{status=normal} = stream_get(State, StreamID),
	#{data_delivery_pid := RelayPid} = ModState,
	%% We do not set data_delivery_flow because it is managed by the
	%% QUIC library and we do not have an easy way to modify it.
	Stream = Stream1#stream{status={relaying, normal, RelayPid}},
	commands(State, Stream, Tail);
commands(State0, Stream0=#stream{id=StreamID},
		[{switch_protocol, Headers, _Mod, _ModState}|Tail]) ->
	State = info(stream_store(State0, Stream0), StreamID, {headers, 200, Headers}),
	Stream = stream_get(State, StreamID),
	commands(State, Stream, Tail);
%% Set options dynamically.
commands(State, Stream, [{set_options, _Opts}|Tail]) ->
	commands(State, Stream, Tail);
commands(State, Stream, [stop|_Tail]) ->
	%% @todo Do we want to run the commands after a stop?
	%% @todo Do we even allow commands after?
	stop_stream(State, Stream);
%% Log event.
commands(State=#state{opts=Opts}, Stream, [Log={log, _, _, _}|Tail]) ->
	cowboy:log(Log, Opts),
	commands(State, Stream, Tail).

send_response(State0=#state{conn=Conn, http3_machine=HTTP3Machine0},
		Stream=#stream{id=StreamID}, StatusCode, Headers, Body) ->
	Size = case Body of
		{sendfile, _, Bytes0, _} -> Bytes0;
		_ -> iolist_size(Body)
	end,
	case Size of
		0 ->
			State = send_headers(State0, Stream, fin, StatusCode, Headers),
			maybe_send_is_fin(State, Stream, fin);
		_ ->
			%% @todo Add a test for HEAD to make sure we don't send the body when
			%% returning {response...} from a stream handler (or {headers...} then {data...}).
			{ok, _IsFin, HeaderBlock, Instrs, HTTP3Machine}
				= cow_http3_machine:prepare_headers(StreamID, HTTP3Machine0, nofin,
					#{status => cow_http:status_to_integer(StatusCode)},
					headers_to_list(Headers)),
			State = send_instructions(State0#state{http3_machine=HTTP3Machine}, Instrs),
			%% @todo It might be better to do async sends.
			_ = case Body of
				{sendfile, Offset, Bytes, Path} ->
					ok = maybe_socket_error(State,
						?QUIC_ADAPTER:send(Conn, StreamID, cow_http3:headers(HeaderBlock))),
					%% Temporary solution to do sendfile over QUIC.
					{ok, _} = maybe_socket_error(State,
						ranch_transport:sendfile(?MODULE, {Conn, StreamID},
							Path, Offset, Bytes, [])),
					ok = maybe_socket_error(State,
						?QUIC_ADAPTER:send(Conn, StreamID, cow_http3:data(<<>>), fin));
				_ ->
					ok = maybe_socket_error(State,
						?QUIC_ADAPTER:send(Conn, StreamID, [
							cow_http3:headers(HeaderBlock),
							cow_http3:data(Body)
						], fin))
			end,
			maybe_send_is_fin(State, Stream, fin)
	end.

maybe_send_is_fin(State=#state{http3_machine=HTTP3Machine0},
		Stream=#stream{id=StreamID}, fin) ->
	HTTP3Machine = cow_http3_machine:close_bidi_stream_for_sending(StreamID, HTTP3Machine0),
	maybe_terminate_stream(State#state{http3_machine=HTTP3Machine}, Stream);
maybe_send_is_fin(State, _, _) ->
	State.

%% Temporary callback to do sendfile over QUIC.
-spec send({?QUIC_ADAPTER:connection_handle(), cow_http3:stream_id()},
	iodata()) -> ok | {error, any()}.

send({Conn, StreamID}, IoData) ->
	?QUIC_ADAPTER:send(Conn, StreamID, cow_http3:data(IoData)).

send_headers(State0=#state{conn=Conn, http3_machine=HTTP3Machine0},
		#stream{id=StreamID}, IsFin0, StatusCode, Headers) ->
	{ok, IsFin, HeaderBlock, Instrs, HTTP3Machine}
		= cow_http3_machine:prepare_headers(StreamID, HTTP3Machine0, IsFin0,
			#{status => cow_http:status_to_integer(StatusCode)},
			headers_to_list(Headers)),
	State = send_instructions(State0#state{http3_machine=HTTP3Machine}, Instrs),
	ok = maybe_socket_error(State,
		?QUIC_ADAPTER:send(Conn, StreamID, cow_http3:headers(HeaderBlock), IsFin)),
	State.

%% The set-cookie header is special; we can only send one cookie per header.
headers_to_list(Headers0=#{<<"set-cookie">> := SetCookies}) ->
	Headers = maps:to_list(maps:remove(<<"set-cookie">>, Headers0)),
	Headers ++ [{<<"set-cookie">>, Value} || Value <- SetCookies];
headers_to_list(Headers) ->
	maps:to_list(Headers).

%% @todo We would open unidi streams here if we only open on-demand.
%% No instructions.
send_instructions(State, undefined) ->
	State;
%% Decoder instructions.
send_instructions(State=#state{conn=Conn, local_decoder_id=DecoderID},
		{decoder_instructions, DecData}) ->
	ok = maybe_socket_error(State,
		?QUIC_ADAPTER:send(Conn, DecoderID, DecData)),
	State;
%% Encoder instructions.
send_instructions(State=#state{conn=Conn, local_encoder_id=EncoderID},
		{encoder_instructions, EncData}) ->
	ok = maybe_socket_error(State,
		?QUIC_ADAPTER:send(Conn, EncoderID, EncData)),
	State.

%% Relay data delivery commands.

relay_command(State, StreamID, DataCmd = {data, _, _}) ->
	Stream = stream_get(State, StreamID),
	commands(State, Stream, [DataCmd]);
relay_command(State=#state{conn=Conn}, StreamID, active) ->
	ok = maybe_socket_error(State,
		?QUIC_ADAPTER:setopt(Conn, StreamID, active, true)),
	State;
relay_command(State=#state{conn=Conn}, StreamID, passive) ->
	ok = maybe_socket_error(State,
		?QUIC_ADAPTER:setopt(Conn, StreamID, active, false)),
	State.

%% We mark the stream as being a WebTransport stream
%% and then continue parsing the data as a WebTransport
%% stream. This function is common for incoming unidi
%% and bidi streams.
become_webtransport_stream(State0=#state{http3_machine=HTTP3Machine0},
		Stream0=#stream{id=StreamID}, StreamType, SessionID, Rest, IsFin) ->
	case cow_http3_machine:become_webtransport_stream(StreamID, SessionID, HTTP3Machine0) of
		{ok, HTTP3Machine} ->
			State = State0#state{http3_machine=HTTP3Machine},
			Stream = Stream0#stream{status={webtransport_stream, SessionID}},
			webtransport_event(State, SessionID, {stream_open, StreamID, StreamType}),
			%% We don't need to parse the remaining data if there isn't any.
			case {Rest, IsFin} of
				{<<>>, nofin} -> loop(stream_store(State, Stream));
				_ -> parse(stream_store(State, Stream), StreamID, Rest, IsFin)
			end
		%% @todo Error conditions.
	end.

webtransport_event(State, SessionID, Event) ->
	#stream{
		status={webtransport_session, _},
		state={cowboy_webtransport, #{session_pid := SessionPid}}
	} = stream_get(State, SessionID),
	SessionPid ! {'$webtransport_event', SessionID, Event},
	ok.

webtransport_commands(State, SessionID, Commands) ->
	case stream_get(State, SessionID) of
		Session = #stream{status={webtransport_session, _}} ->
			wt_commands(State, Session, Commands);
		%% The stream has been terminated, ignore pending commands.
		error ->
			State
	end.

wt_commands(State, _, []) ->
	State;
wt_commands(State0=#state{conn=Conn, opts=Opts}, Session=#stream{id=SessionID},
		[{open_stream, OpenStreamRef, StreamType, InitialData}|Tail]) ->
	%% Because opening the stream involves sending a short header
	%% we necessarily write data. The InitialData variable allows
	%% providing additional data to be sent in the same packet.
	StartF = case StreamType of
		bidi -> start_bidi_stream;
		unidi -> start_unidi_stream
	end,
	Header = cow_http3:webtransport_stream_header(SessionID, StreamType),
	case ?QUIC_ADAPTER:StartF(Conn, [Header, InitialData]) of
		{ok, StreamID} ->
			webtransport_event(State0, SessionID,
				{opened_stream_id, OpenStreamRef, StreamID}),
			State = stream_new_local(State0, StreamID, StreamType,
				{webtransport_stream, SessionID}),
			wt_commands(State, Session, Tail);
		{error, Reason} ->
			cowboy:log(warning, "Failed to open WebTransport stream: ~p", [Reason], Opts),
			webtransport_event(State0, SessionID,
				{stream_open_failed, OpenStreamRef, Reason}),
			wt_commands(State0, Session, Tail)
	end;
wt_commands(State0=#state{conn=Conn}, Session=#stream{id=SessionID},
		[{close_stream, StreamID, Code}|Tail]) ->
	%% Verify that the stream belongs to this session before closing.
	case stream_get(State0, StreamID) of
		#stream{status={webtransport_stream, SessionID}} ->
			ErrorCode = cow_http3:error_to_code({wt_application_error, Code}),
			?QUIC_ADAPTER:shutdown_stream(Conn, StreamID, both, ErrorCode),
			State = stream_closed(State0, StreamID, 0),
			wt_commands(State, Session, Tail);
		_ ->
			%% Stream doesn't exist or doesn't belong to this session.
			wt_commands(State0, Session, Tail)
	end;
wt_commands(State=#state{conn=Conn, opts=Opts}, Session=#stream{id=SessionID},
		[{send, datagram, Data}|Tail]) ->
	%% Datagrams are unreliable by design, so we just log and continue on error.
	case ?QUIC_ADAPTER:send_datagram(Conn, cow_http3:datagram(SessionID, Data)) of
		ok ->
			ok;
		{error, Reason} ->
			cowboy:log(debug, "Failed to send WebTransport datagram: ~p", [Reason], Opts)
	end,
	wt_commands(State, Session, Tail);
wt_commands(State=#state{conn=Conn, opts=Opts}, Session=#stream{id=SessionID},
		[{send, StreamID, Data}|Tail]) ->
	%% Verify stream belongs to session before sending.
	case stream_get(State, StreamID) of
		#stream{status={webtransport_stream, SessionID}} ->
			case ?QUIC_ADAPTER:send(Conn, StreamID, Data, nofin) of
				ok ->
					ok;
				{error, Reason} ->
					cowboy:log(warning, "Failed to send on WebTransport stream ~p: ~p",
						[StreamID, Reason], Opts)
			end;
		_ ->
			cowboy:log(warning, "Attempted to send on unknown WebTransport stream ~p", [StreamID], Opts)
	end,
	wt_commands(State, Session, Tail);
wt_commands(State=#state{conn=Conn, opts=Opts}, Session=#stream{id=SessionID},
		[{send, StreamID, IsFin, Data}|Tail]) ->
	%% Verify stream belongs to session before sending.
	case stream_get(State, StreamID) of
		#stream{status={webtransport_stream, SessionID}} ->
			case ?QUIC_ADAPTER:send(Conn, StreamID, Data, IsFin) of
				ok ->
					ok;
				{error, Reason} ->
					cowboy:log(warning, "Failed to send on WebTransport stream ~p: ~p",
						[StreamID, Reason], Opts)
			end;
		_ ->
			cowboy:log(warning, "Attempted to send on unknown WebTransport stream ~p", [StreamID], Opts)
	end,
	wt_commands(State, Session, Tail);
wt_commands(State=#state{conn=Conn, opts=Opts}, Session=#stream{id=SessionID}, [initiate_close|Tail]) ->
	%% We must send a WT_DRAIN_SESSION capsule on the CONNECT stream.
	Capsule = cow_capsule:wt_drain_session(),
	case ?QUIC_ADAPTER:send(Conn, SessionID, Capsule, nofin) of
		ok ->
			ok;
		{error, Reason} ->
			cowboy:log(warning, "Failed to send WT_DRAIN_SESSION: ~p", [Reason], Opts)
	end,
	wt_commands(State, Session, Tail);
wt_commands(State0=#state{conn=Conn, opts=Opts}, Session=#stream{id=SessionID}, [Cmd|Tail])
		when Cmd =:= close; element(1, Cmd) =:= close ->
	%% We must send a WT_CLOSE_SESSION capsule on the CONNECT stream.
	{AppCode, AppMsg} = case Cmd of
		close -> {0, <<>>};
		{close, AppCode0} -> {AppCode0, <<>>};
		{close, AppCode0, AppMsg0} -> {AppCode0, AppMsg0}
	end,
	Capsule = cow_capsule:wt_close_session(AppCode, AppMsg),
	State = case ?QUIC_ADAPTER:send(Conn, SessionID, Capsule, fin) of
		ok ->
			webtransport_terminate_session(State0, Session);
		{error, Reason} ->
			%% Failed to send close capsule, force terminate the session.
			cowboy:log(warning, "Failed to send WT_CLOSE_SESSION: ~p, forcing termination", [Reason], Opts),
			?QUIC_ADAPTER:shutdown_stream(Conn, SessionID, both, cow_http3:error_to_code(h3_internal_error)),
			webtransport_terminate_session(State0, Session)
	end,
	%% Continue processing remaining commands even though session is terminated.
	wt_commands(State, Session, Tail).

webtransport_terminate_session(State=#state{conn=Conn, http3_machine=HTTP3Machine0,
		streams=Streams0, lingering_streams=Lingering0}, #stream{id=SessionID}) ->
	%% Reset/abort the WT streams.
	Streams = maps:filtermap(fun
		(_, #stream{id=StreamID, status={webtransport_session, _}})
				when StreamID =:= SessionID ->
			%% We remove the session stream but do the shutdown outside this function.
			false;
		(StreamID, #stream{status={webtransport_stream, StreamSessionID}})
				when StreamSessionID =:= SessionID ->
			?QUIC_ADAPTER:shutdown_stream(Conn, StreamID,
				both, cow_http3:error_to_code(wt_session_gone)),
			false;
		(_, _) ->
			true
	end, Streams0),
	%% Keep the streams in lingering state.
	%% We only keep up to 100 streams in this state. @todo Make it configurable?
	Terminated = maps:keys(Streams0) -- maps:keys(Streams),
	Lingering = lists:sublist(Terminated ++ Lingering0, 100),
	%% Update the HTTP3 state machine.
	HTTP3Machine = cow_http3_machine:close_webtransport_session(SessionID, HTTP3Machine0),
	State#state{
		http3_machine=HTTP3Machine,
		streams=Streams,
		lingering_streams=Lingering
	}.

stream_peer_send_shutdown(State=#state{conn=Conn}, StreamID) ->
	case stream_get(State, StreamID) of
		%% Cleanly terminating the CONNECT stream is equivalent
		%% to an application error code of 0 and empty message.
		Stream = #stream{status={webtransport_session, _}} ->
			webtransport_event(State, StreamID, {closed, 0, <<>>}),
			%% Shutdown the CONNECT stream fully.
			?QUIC_ADAPTER:shutdown_stream(Conn, StreamID),
			webtransport_terminate_session(State, Stream);
		_ ->
			State
	end.

reset_stream(State0=#state{conn=Conn, http3_machine=HTTP3Machine0},
		Stream=#stream{id=StreamID}, Error) ->
	Reason = case Error of
		{internal_error, _, _} -> h3_internal_error;
		{stream_error, Reason0, _} -> Reason0
	end,
	%% @todo Do we want to close both sides?
	%% @todo Should we close the send side if the receive side was already closed?
	?QUIC_ADAPTER:shutdown_stream(Conn, StreamID,
		both, cow_http3:error_to_code(Reason)),
	State1 = case cow_http3_machine:reset_stream(StreamID, HTTP3Machine0) of
		{ok, HTTP3Machine} ->
			terminate_stream(State0#state{http3_machine=HTTP3Machine}, Stream, Error);
		{error, not_found} ->
			terminate_stream(State0, Stream, Error)
	end,
	case reset_rate(State1) of
		{ok, State} ->
			State;
		error ->
			terminate(State1, {connection_error, h3_excessive_load,
				'Stream reset rate larger than configuration allows. Flood? (CVE-2019-9514)'})
	end.

%% Stream reset rate limiting (CVE-2019-9514 protection).

init_reset_rate_limiting(State=#state{opts=Opts}, CurrentTime) ->
	{ResetRateNum, ResetRatePeriod} = maps:get(max_reset_stream_rate, Opts, {10, 10000}),
	State#state{
		reset_rate_num=ResetRateNum,
		reset_rate_time=add_period(CurrentTime, ResetRatePeriod)
	}.

reset_rate(State0=#state{reset_rate_num=Num0, reset_rate_time=Time}) ->
	case Num0 - 1 of
		0 ->
			CurrentTime = erlang:monotonic_time(millisecond),
			if
				CurrentTime < Time ->
					error;
				true ->
					%% When the option has a period of infinity we cannot reach this clause.
					{ok, init_reset_rate_limiting(State0, CurrentTime)}
			end;
		Num ->
			{ok, State0#state{reset_rate_num=Num}}
	end.

add_period(_, infinity) -> infinity;
add_period(Time, Period) -> Time + Period.

stop_stream(State0=#state{http3_machine=HTTP3Machine}, Stream=#stream{id=StreamID}) ->
	%% We abort reading when stopping the stream but only
	%% if the client was not finished sending data.
	%% We mark the stream as 'stopping' either way.
	State = case cow_http3_machine:get_bidi_stream_remote_state(StreamID, HTTP3Machine) of
		{ok, fin} ->
			stream_store(State0, Stream#stream{status=stopping});
		{error, not_found} ->
			stream_store(State0, Stream#stream{status=stopping});
		_ ->
			stream_abort_receive(State0, Stream, h3_no_error)
	end,
	%% Then we may need to send a response or terminate it
	%% if the stream handler did not do so already.
	case cow_http3_machine:get_bidi_stream_local_state(StreamID, HTTP3Machine) of
		%% When the stream terminates normally (without resetting the stream)
		%% and no response was sent, we need to send a proper response back to the client.
		{ok, idle} ->
			info(State, StreamID, {response, 204, #{}, <<>>});
		%% When a response was sent but not terminated, we need to close the stream.
		%% We send a final DATA frame to complete the stream.
		{ok, nofin} ->
			info(State, StreamID, {data, fin, <<>>});
		%% When a response was sent fully we can terminate the stream,
		%% regardless of the stream being in half-closed or closed state.
		_ ->
			terminate_stream(State, Stream, normal)
	end.

maybe_terminate_stream(State, Stream=#stream{status=stopping}) ->
	terminate_stream(State, Stream, normal);
%% The Stream will be stored in the State at the end of commands processing.
maybe_terminate_stream(State, _) ->
	State.

terminate_stream(State=#state{streams=Streams0, children=Children0},
		#stream{id=StreamID, state=StreamState}, Reason) ->
	Streams = maps:remove(StreamID, Streams0),
	terminate_stream_handler(State, StreamID, Reason, StreamState),
	Children = cowboy_children:shutdown(Children0, StreamID),
	stream_linger(State#state{streams=Streams, children=Children}, StreamID).

terminate_stream_handler(#state{opts=Opts}, StreamID, Reason, StreamState) ->
	try
		cowboy_stream:terminate(StreamID, Reason, StreamState)
	catch Class:Exception:Stacktrace ->
		cowboy:log(cowboy_stream:make_error_log(terminate,
			[StreamID, Reason, StreamState],
			Class, Exception, Stacktrace), Opts)
	end.

ignored_frame(State=#state{http3_machine=HTTP3Machine0}, #stream{id=StreamID}) ->
	case cow_http3_machine:ignored_frame(StreamID, HTTP3Machine0) of
		{ok, HTTP3Machine} ->
			State#state{http3_machine=HTTP3Machine};
		{error, Error={connection_error, _, _}, HTTP3Machine} ->
			terminate(State#state{http3_machine=HTTP3Machine}, Error)
	end.

stream_abort_receive(State=#state{conn=Conn}, Stream=#stream{id=StreamID}, Reason) ->
	?QUIC_ADAPTER:shutdown_stream(Conn, StreamID,
		receiving, cow_http3:error_to_code(Reason)),
	stream_store(State, Stream#stream{status=stopping}).

%% Graceful connection shutdown.
%%
%% When we receive a GOAWAY frame from the client, we update our status
%% and begin graceful shutdown. When the server initiates shutdown,
%% we send a GOAWAY frame and begin graceful shutdown.
-spec goaway(#state{}, {goaway, _}) -> no_return().
goaway(State, {goaway, _LastStreamID}) ->
	%% Client initiated graceful shutdown.
	%% We should stop accepting new streams and finish existing ones.
	terminate(State#state{http3_status=closing}, {stop, goaway, 'The connection is going away.'}).

%% Send a GOAWAY frame to initiate graceful shutdown.
send_goaway(#state{conn=Conn, local_control_id=ControlID, streams=Streams}) ->
	%% Find the maximum client-initiated bidirectional stream ID.
	%% Client-initiated bidi streams have ID rem 4 == 0.
	LastStreamID = maps:fold(fun(StreamID, _, Max) ->
		case StreamID rem 4 of
			0 when StreamID > Max -> StreamID;
			_ -> Max
		end
	end, 0, Streams),
	GoAwayFrame = build_goaway_frame(LastStreamID),
	?QUIC_ADAPTER:send(Conn, ControlID, GoAwayFrame).

%% Build a GOAWAY frame (frame type 7).
build_goaway_frame(StreamID) ->
	EncodedStreamID = cow_http3:encode_int(StreamID),
	Len = byte_size(iolist_to_binary([EncodedStreamID])),
	[<<7>>, cow_http3:encode_int(Len), EncodedStreamID].

%% Function copied from cowboy_http.
maybe_socket_error(State, {error, closed}) ->
	terminate(State, {socket_error, closed, 'The socket has been closed.'});
maybe_socket_error(State, Reason) ->
	maybe_socket_error(State, Reason, 'An error has occurred on the socket.').

maybe_socket_error(_, Result = ok, _) ->
	Result;
maybe_socket_error(_, Result = {ok, _}, _) ->
	Result;
maybe_socket_error(State, {error, Reason}, Human) ->
	terminate(State, {socket_error, Reason, Human}).

-spec terminate(#state{} | undefined, _) -> no_return().
terminate(undefined, Reason) ->
	exit({shutdown, Reason});
terminate(State=#state{conn=Conn, http3_status=Status,
		streams=Streams, children=Children}, Reason) ->
	%% Send GOAWAY if we haven't already done so.
	%% Wrap in try/catch since the QUIC connection may already be closed.
	_ = case Status of
		connected ->
			%% We are terminating so it's OK if we can't send the GOAWAY anymore.
			try send_goaway(State) catch _:_ -> ok end;
		closing ->
			%% We already sent the GOAWAY frame.
			ok
	end,
	terminate_all_streams(State, maps:to_list(Streams), Reason),
	cowboy_children:terminate(Children),
	_ = ?QUIC_ADAPTER:shutdown(Conn, cow_http3:error_to_code(terminate_reason(Reason))),
	exit({shutdown, Reason}).

terminate_reason({connection_error, {transport_error, _Code}, _}) -> h3_internal_error;
terminate_reason({connection_error, Reason, _}) -> Reason;
terminate_reason({stop, _, _}) -> h3_no_error;
terminate_reason({socket_error, _, _}) -> h3_internal_error.

terminate_all_streams(_, [], _) ->
	ok;
terminate_all_streams(State, [{StreamID, #stream{state=StreamState}}|Tail], Reason) ->
	terminate_stream_handler(State, StreamID, Reason, StreamState),
	terminate_all_streams(State, Tail, Reason).

stream_get(#state{streams=Streams}, StreamID) ->
	maps:get(StreamID, Streams, error).

stream_new_local(State, StreamID, StreamType, Status) ->
	stream_new(State, StreamID, StreamType, unidi_local, Status).

stream_new_remote(State, StreamID, StreamType) ->
	Status = case StreamType of
		unidi -> header;
		bidi -> normal
	end,
	stream_new(State, StreamID, StreamType, unidi_remote, Status).

stream_new(State=#state{http3_machine=HTTP3Machine0, streams=Streams},
		StreamID, StreamType, UnidiType, Status) ->
	{HTTP3Machine, Status} = case StreamType of
		unidi ->
			{cow_http3_machine:init_unidi_stream(StreamID, UnidiType, HTTP3Machine0),
				Status};
		bidi ->
			{cow_http3_machine:init_bidi_stream(StreamID, HTTP3Machine0),
				Status}
	end,
	Stream = #stream{id=StreamID, status=Status},
	State#state{http3_machine=HTTP3Machine, streams=Streams#{StreamID => Stream}}.

%% Stream closed message for a local (write-only) unidi stream.
stream_closed(State=#state{local_control_id=StreamID}, StreamID, _) ->
	stream_closed1(State, StreamID);
stream_closed(State=#state{local_encoder_id=StreamID}, StreamID, _) ->
	stream_closed1(State, StreamID);
stream_closed(State=#state{local_decoder_id=StreamID}, StreamID, _) ->
	stream_closed1(State, StreamID);
stream_closed(State=#state{opts=Opts,
		streams=Streams0, children=Children0}, StreamID, ErrorCode) ->
	case maps:take(StreamID, Streams0) of
		%% In the WT session's case, streams will be
		%% removed in webtransport_terminate_session.
		{Stream=#stream{status={webtransport_session, _}}, _} ->
			webtransport_event(State, StreamID, closed_abruptly),
			webtransport_terminate_session(State, Stream);
		{#stream{state=undefined}, Streams} ->
			%% Unidi stream has no handler/children.
			stream_closed1(State#state{streams=Streams}, StreamID);
		%% We only stop bidi streams if the stream was closed with an error
		%% or the stream was already in the process of stopping.
		{#stream{status=Status, state=StreamState}, Streams}
				when Status =:= stopping; ErrorCode =/= 0 ->
			terminate_stream_handler(State, StreamID, closed, StreamState),
			Children = cowboy_children:shutdown(Children0, StreamID),
			stream_closed1(State#state{streams=Streams, children=Children}, StreamID);
		%% Don't remove a stream that terminated properly but
		%% has chosen to remain up (custom stream handlers).
		{_, _} ->
			stream_closed1(State, StreamID);
		%% Stream closed message for a stream that has been reset. Ignore.
		error ->
			case is_lingering_stream(State, StreamID) of
				true ->
					ok;
				false ->
					%% We avoid logging the data as it could be quite large.
					cowboy:log(warning, "Received stream_closed for unknown stream ~p. ~p ~p",
						[StreamID, self(), Streams0], Opts)
			end,
			State
	end.

stream_closed1(State=#state{http3_machine=HTTP3Machine0}, StreamID) ->
	case cow_http3_machine:close_stream(StreamID, HTTP3Machine0) of
		{ok, HTTP3Machine} ->
			State#state{http3_machine=HTTP3Machine};
		{error, Error={connection_error, _, _}, HTTP3Machine} ->
			terminate(State#state{http3_machine=HTTP3Machine}, Error)
	end.

stream_store(State=#state{streams=Streams}, Stream=#stream{id=StreamID}) ->
	State#state{streams=Streams#{StreamID => Stream}}.

stream_linger(State=#state{lingering_streams=Lingering0}, StreamID) ->
	%% We only keep up to 100 streams in this state. @todo Make it configurable?
	Lingering = [StreamID|lists:sublist(Lingering0, 100 - 1)],
	State#state{lingering_streams=Lingering}.

is_lingering_stream(#state{lingering_streams=Lingering}, StreamID) ->
	lists:member(StreamID, Lingering).
