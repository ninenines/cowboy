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

-record(stream, {
	id :: cow_http3:stream_id(),

	%% Whether the stream is currently in a special state.
	status :: header | {unidi, control | encoder | decoder}
		| normal | {data | ignore, non_neg_integer()} | stopping,

	%% Stream buffer.
	buffer = <<>> :: binary(),

	%% Stream state.
	state = undefined :: undefined | {module, any()}
}).

-record(state, {
	parent :: pid(),
	ref :: ranch:ref(),
	conn :: cowboy_quicer:quicer_connection_handle(),
	opts = #{} :: opts(),

	%% Remote address and port for the connection.
	peer = undefined :: {inet:ip_address(), inet:port_number()},

	%% Local address and port for the connection.
	sock = undefined :: {inet:ip_address(), inet:port_number()},

	%% Client certificate.
	cert :: undefined | binary(),

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
	children = cowboy_children:init() :: cowboy_children:children()
}).

-spec init(pid(), ranch:ref(), cowboy_quicer:quicer_connection_handle(), opts())
	-> no_return().

init(Parent, Ref, Conn, Opts) ->
	{ok, SettingsBin, HTTP3Machine0} = cow_http3_machine:init(server, Opts),
	%% Immediately open a control, encoder and decoder stream.
	%% @todo An endpoint MAY avoid creating an encoder stream if it will not be used (for example, if its encoder does not wish to use the dynamic table or if the maximum size of the dynamic table permitted by the peer is zero).
	%% @todo An endpoint MAY avoid creating a decoder stream if its decoder sets the maximum capacity of the dynamic table to zero.
	{ok, ControlID} = maybe_socket_error(undefined,
		cowboy_quicer:start_unidi_stream(Conn, [<<0>>, SettingsBin]),
		'A socket error occurred when opening the control stream.'),
	{ok, EncoderID} = maybe_socket_error(undefined,
		cowboy_quicer:start_unidi_stream(Conn, <<2>>),
		'A socket error occurred when opening the encoder stream.'),
	{ok, DecoderID} = maybe_socket_error(undefined,
		cowboy_quicer:start_unidi_stream(Conn, <<3>>),
		'A socket error occurred when opening the encoder stream.'),
	%% Set the control, encoder and decoder streams in the machine.
	HTTP3Machine = cow_http3_machine:init_unidi_local_streams(
		ControlID, EncoderID, DecoderID, HTTP3Machine0),
	%% Get the peername/sockname/cert.
	{ok, Peer} = maybe_socket_error(undefined, cowboy_quicer:peername(Conn),
		'A socket error occurred when retrieving the peer name.'),
	{ok, Sock} = maybe_socket_error(undefined, cowboy_quicer:sockname(Conn),
		'A socket error occurred when retrieving the sock name.'),
	CertResult = case cowboy_quicer:peercert(Conn) of
		{error, no_peercert} ->
			{ok, undefined};
		Cert0 ->
			Cert0
	end,
	{ok, Cert} = maybe_socket_error(undefined, CertResult,
		'A socket error occurred when retrieving the client TLS certificate.'),
	%% Quick! Let's go!
	loop(#state{parent=Parent, ref=Ref, conn=Conn,
		opts=Opts, peer=Peer, sock=Sock, cert=Cert,
		http3_machine=HTTP3Machine, local_control_id=ControlID,
		local_encoder_id=EncoderID, local_decoder_id=DecoderID}).

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
		%% Exit signal from children.
		Msg = {'EXIT', Pid, _} ->
			loop(down(State0, Pid, Msg));
		Msg ->
			cowboy:log(warning, "Received stray message ~p.", [Msg], Opts),
			loop(State0)
	end.

handle_quic_msg(State0=#state{opts=Opts}, Msg) ->
	case cowboy_quicer:handle(Msg) of
		{data, StreamID, IsFin, Data} ->
			parse(State0, StreamID, Data, IsFin);
		{stream_started, StreamID, StreamType} ->
			State = stream_new_remote(State0, StreamID, StreamType),
			loop(State);
		{stream_closed, StreamID, ErrorCode} ->
			State = stream_closed(State0, StreamID, ErrorCode),
			loop(State);
		closed ->
			%% @todo Different error reason if graceful?
			Reason = {socket_error, closed, 'The socket has been closed.'},
			terminate(State0, Reason);
		ok ->
			loop(State0);
		unknown ->
			cowboy:log(warning, "Received unknown QUIC message ~p.", [Msg], Opts),
			loop(State0);
		{socket_error, Reason} ->
			terminate(State0, {socket_error, Reason,
				'An error has occurred on the socket.'})
	end.

parse(State=#state{opts=Opts}, StreamID, Data, IsFin) ->
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
					ok;
				false ->
					%% We avoid logging the data as it could be quite large.
					cowboy:log(warning, "Received data for unknown stream ~p.",
						[StreamID], Opts)
			end,
			loop(State)
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
parse1(State=#state{opts=Opts}, Stream=#stream{id=StreamID}, Data, IsFin) ->
	case cow_http3:parse(Data) of
		{ok, Frame, Rest} ->
			FrameIsFin = is_fin(IsFin, Rest),
			parse(frame(State, Stream, Frame, FrameIsFin), StreamID, Rest, IsFin);
		{more, Frame = {data, _}, Len} ->
			%% We're at the end of the data so FrameIsFin is equivalent to IsFin.
			case IsFin of
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
		{ok, push, _} ->
			terminate(State0, {connection_error, h3_stream_creation_error,
				'Only servers can push. (RFC9114 6.2.2)'});
		%% Unknown stream types must be ignored. We choose to abort the
		%% stream instead of reading and discarding the incoming data.
		{undefined, _} ->
			loop(stream_abort_receive(State0, Stream0, h3_stream_creation_error))
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
		{ok, {trailers, _Trailers}, Instrs, HTTP3Machine} ->
			%% @todo Propagate trailers.
			send_instructions(State#state{http3_machine=HTTP3Machine}, Instrs);
		{ok, GoAway={goaway, _}, HTTP3Machine} ->
			goaway(State#state{http3_machine=HTTP3Machine}, GoAway);
		{error, Error={stream_error, _Reason, _Human}, Instrs, HTTP3Machine} ->
			State1 = send_instructions(State#state{http3_machine=HTTP3Machine}, Instrs),
			reset_stream(State1, Stream, Error);
		{error, Error={connection_error, _, _}, HTTP3Machine} ->
			terminate(State#state{http3_machine=HTTP3Machine}, Error)
	end.

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

headers_frame(State, Stream, IsFin, Headers,
		PseudoHeaders=#{method := <<"CONNECT">>}, _)
		when map_size(PseudoHeaders) =:= 2 ->
	early_error(State, Stream, IsFin, Headers, PseudoHeaders, 501,
		'The CONNECT method is currently not implemented. (RFC7231 4.3.6)');
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
				cowboy_quicer:send(Conn, StreamID, cow_http3:data(<<>>), IsFin));
		_ ->
			ok = maybe_socket_error(State0,
				cowboy_quicer:send(Conn, StreamID, cow_http3:data(Data), IsFin))
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
				cowboy_quicer:send(Conn, StreamID, cow_http3:headers(HeaderBlock), fin)),
			State1;
		{no_trailers, HTTP3Machine} ->
			ok = maybe_socket_error(State0,
				cowboy_quicer:send(Conn, StreamID, cow_http3:data(<<>>), fin)),
			State0#state{http3_machine=HTTP3Machine}
	end,
	commands(State, Stream, Tail);
%% Send a push promise.
%%
%% @todo Responses sent as a result of a push_promise request
%% must not send push_promise frames themselves.
%%
%% @todo We should not send push_promise frames when we are
%% in the closing http2_status.
%commands(State0=#state{socket=Socket, transport=Transport, http3_machine=HTTP3Machine0},
%		Stream, [{push, Method, Scheme, Host, Port, Path, Qs, Headers0}|Tail]) ->
%	Authority = case {Scheme, Port} of
%		{<<"http">>, 80} -> Host;
%		{<<"https">>, 443} -> Host;
%		_ -> iolist_to_binary([Host, $:, integer_to_binary(Port)])
%	end,
%	PathWithQs = iolist_to_binary(case Qs of
%		<<>> -> Path;
%		_ -> [Path, $?, Qs]
%	end),
%	PseudoHeaders = #{
%		method => Method,
%		scheme => Scheme,
%		authority => Authority,
%		path => PathWithQs
%	},
%	%% We need to make sure the header value is binary before we can
%	%% create the Req object, as it expects them to be flat.
%	Headers = maps:to_list(maps:map(fun(_, V) -> iolist_to_binary(V) end, Headers0)),
%	%% @todo
%	State = case cow_http2_machine:prepare_push_promise(StreamID, HTTP3Machine0,
%			PseudoHeaders, Headers) of
%		{ok, PromisedStreamID, HeaderBlock, HTTP3Machine} ->
%			Transport:send(Socket, cow_http2:push_promise(
%				StreamID, PromisedStreamID, HeaderBlock)),
%			headers_frame(State0#state{http3_machine=HTTP2Machine},
%				PromisedStreamID, fin, Headers, PseudoHeaders, 0);
%		{error, no_push} ->
%			State0
%	end,
%	commands(State, Stream, Tail);
%%% Read the request body.
%commands(State0=#state{flow=Flow, streams=Streams}, Stream, [{flow, Size}|Tail]) ->
commands(State, Stream, [{flow, _Size}|Tail]) ->
	%% @todo We should tell the QUIC stream to increase its window size.
%	#{StreamID := Stream=#stream{flow=StreamFlow}} = Streams,
%	State = update_window(State0#state{flow=Flow + Size,
%		streams=Streams#{StreamID => Stream#stream{flow=StreamFlow + Size}}},
%		StreamID),
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
						cowboy_quicer:send(Conn, StreamID, cow_http3:headers(HeaderBlock))),
					%% Temporary solution to do sendfile over QUIC.
					{ok, _} = maybe_socket_error(State,
						ranch_transport:sendfile(?MODULE, {Conn, StreamID},
							Path, Offset, Bytes, [])),
					ok = maybe_socket_error(State,
						cowboy_quicer:send(Conn, StreamID, cow_http3:data(<<>>), fin));
				_ ->
					ok = maybe_socket_error(State,
						cowboy_quicer:send(Conn, StreamID, [
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
-spec send({cowboy_quicer:quicer_connection_handle(), cow_http3:stream_id()},
	iodata()) -> ok | {error, any()}.

send({Conn, StreamID}, IoData) ->
	cowboy_quicer:send(Conn, StreamID, cow_http3:data(IoData)).

send_headers(State0=#state{conn=Conn, http3_machine=HTTP3Machine0},
		#stream{id=StreamID}, IsFin0, StatusCode, Headers) ->
	{ok, IsFin, HeaderBlock, Instrs, HTTP3Machine}
		= cow_http3_machine:prepare_headers(StreamID, HTTP3Machine0, IsFin0,
			#{status => cow_http:status_to_integer(StatusCode)},
			headers_to_list(Headers)),
	State = send_instructions(State0#state{http3_machine=HTTP3Machine}, Instrs),
	ok = maybe_socket_error(State,
		cowboy_quicer:send(Conn, StreamID, cow_http3:headers(HeaderBlock), IsFin)),
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
		cowboy_quicer:send(Conn, DecoderID, DecData)),
	State;
%% Encoder instructions.
send_instructions(State=#state{conn=Conn, local_encoder_id=EncoderID},
		{encoder_instructions, EncData}) ->
	ok = maybe_socket_error(State,
		cowboy_quicer:send(Conn, EncoderID, EncData)),
	State.

reset_stream(State0=#state{conn=Conn, http3_machine=HTTP3Machine0},
		Stream=#stream{id=StreamID}, Error) ->
	Reason = case Error of
		{internal_error, _, _} -> h3_internal_error;
		{stream_error, Reason0, _} -> Reason0
	end,
	%% @todo Do we want to close both sides?
	%% @todo Should we close the send side if the receive side was already closed?
	cowboy_quicer:shutdown_stream(Conn, StreamID,
		both, cow_http3:error_to_code(Reason)),
	State1 = case cow_http3_machine:reset_stream(StreamID, HTTP3Machine0) of
		{ok, HTTP3Machine} ->
			terminate_stream(State0#state{http3_machine=HTTP3Machine}, Stream, Error);
		{error, not_found} ->
			terminate_stream(State0, Stream, Error)
	end,
%% @todo
%	case reset_rate(State1) of
%		{ok, State} ->
%			State;
%		error ->
%			terminate(State1, {connection_error, enhance_your_calm,
%				'Stream reset rate larger than configuration allows. Flood? (CVE-2019-9514)'})
%	end.
	State1.

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
	cowboy_quicer:shutdown_stream(Conn, StreamID,
		receiving, cow_http3:error_to_code(Reason)),
	stream_store(State, Stream#stream{status=stopping}).

%% @todo Graceful connection shutdown.
%% We terminate the connection immediately if it hasn't fully been initialized.
-spec goaway(#state{}, {goaway, _}) -> no_return().
goaway(State, {goaway, _}) ->
	terminate(State, {stop, goaway, 'The connection is going away.'}).

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
terminate(State=#state{conn=Conn, %http3_status=Status,
		%http3_machine=HTTP3Machine,
		streams=Streams, children=Children}, Reason) ->
%	if
%		Status =:= connected; Status =:= closing_initiated ->
%% @todo
%			%% We are terminating so it's OK if we can't send the GOAWAY anymore.
%			_ = cowboy_quicer:send(Conn, ControlID, cow_http3:goaway(
%				cow_http3_machine:get_last_streamid(HTTP3Machine))),
		%% We already sent the GOAWAY frame.
%		Status =:= closing ->
%			ok
%	end,
	terminate_all_streams(State, maps:to_list(Streams), Reason),
	cowboy_children:terminate(Children),
%	terminate_linger(State),
	_ = cowboy_quicer:shutdown(Conn, cow_http3:error_to_code(terminate_reason(Reason))),
	exit({shutdown, Reason}).

terminate_reason({connection_error, Reason, _}) -> Reason;
terminate_reason({stop, _, _}) -> h3_no_error;
terminate_reason({socket_error, _, _}) -> h3_internal_error.
%terminate_reason({internal_error, _, _}) -> internal_error.

terminate_all_streams(_, [], _) ->
	ok;
terminate_all_streams(State, [{StreamID, #stream{state=StreamState}}|Tail], Reason) ->
	terminate_stream_handler(State, StreamID, Reason, StreamState),
	terminate_all_streams(State, Tail, Reason).

stream_get(#state{streams=Streams}, StreamID) ->
	maps:get(StreamID, Streams, error).

stream_new_remote(State=#state{http3_machine=HTTP3Machine0, streams=Streams},
		StreamID, StreamType) ->
	{HTTP3Machine, Status} = case StreamType of
		unidi ->
			{cow_http3_machine:init_unidi_stream(StreamID, unidi_remote, HTTP3Machine0),
				header};
		bidi ->
			{cow_http3_machine:init_bidi_stream(StreamID, HTTP3Machine0),
				normal}
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
