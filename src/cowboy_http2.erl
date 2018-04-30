%% Copyright (c) 2015-2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_http2).

-export([init/5]).
-export([init/9]).
-export([init/11]).

-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-type opts() :: #{
	connection_type => worker | supervisor,
	enable_connect_protocol => boolean(),
	env => cowboy_middleware:env(),
	inactivity_timeout => timeout(),
	initial_connection_window_size => 65535..16#7fffffff,
	initial_stream_window_size => 0..16#7fffffff,
	max_concurrent_streams => non_neg_integer() | infinity,
	max_decode_table_size => non_neg_integer(),
	max_encode_table_size => non_neg_integer(),
	max_frame_size_received => 16384..16777215,
	max_frame_size_sent => 16384..16777215 | infinity,
	middlewares => [module()],
	preface_timeout => timeout(),
	settings_timeout => timeout(),
	shutdown_timeout => timeout(),
	stream_handlers => [module()]
}.
-export_type([opts/0]).

-record(stream, {
	id = undefined :: cowboy_stream:streamid(),
	%% Stream handlers and their state.
	state = undefined :: {module(), any()} | flush,
	%% Request method.
	method = undefined :: binary(),
	%% Whether we finished sending data.
	local = idle :: idle | upgrade | cowboy_stream:fin() | flush,
	%% Local flow control window (how much we can send).
	local_window :: integer(),
	%% Buffered data waiting for the flow control window to increase.
	local_buffer = queue:new() :: queue:queue(
		{cowboy_stream:fin(), non_neg_integer(), iolist()
			| {sendfile, non_neg_integer(), pos_integer(), file:name_all()}}),
	local_buffer_size = 0 :: non_neg_integer(),
	local_trailers = undefined :: undefined | cowboy:http_headers(),
	%% Whether we finished receiving data.
	remote = nofin :: cowboy_stream:fin(),
	%% Remote flow control window (how much we accept to receive).
	remote_window :: integer(),
	%% Size expected and read from the request body.
	remote_expected_size = undefined :: undefined | non_neg_integer(),
	remote_read_size = 0 :: non_neg_integer(),
	%% Unparsed te header. Used to know if we can send trailers.
	te :: undefined | binary()
}).

-type stream() :: #stream{}.

-record(state, {
	parent = undefined :: pid(),
	ref :: ranch:ref(),
	socket = undefined :: inet:socket(),
	transport :: module(),
	opts = #{} :: opts(),

	%% Remote address and port for the connection.
	peer = undefined :: {inet:ip_address(), inet:port_number()},

	%% Local address and port for the connection.
	sock = undefined :: {inet:ip_address(), inet:port_number()},

	%% Client certificate (TLS only).
	cert :: undefined | binary(),

	%% Settings are separate for each endpoint. In addition, settings
	%% must be acknowledged before they can be expected to be applied.
	local_settings = #{
%		header_table_size => 4096,
%		enable_push => false, %% We are the server. Push is never enabled for clients.
%		max_concurrent_streams => infinity,
		initial_window_size => 65535,
		max_frame_size => 16384
%		max_header_list_size => infinity
	} :: map(),
	next_settings = undefined :: undefined | map(),
	next_settings_timer = undefined :: undefined | reference(),
	remote_settings = #{
		initial_window_size => 65535
	} :: map(),

	%% Connection-wide flow control window.
	local_window = 65535 :: integer(), %% How much we can send.
	remote_window = 65535 :: integer(), %% How much we accept to receive.

	%% Stream identifiers.
	client_streamid = 0 :: non_neg_integer(),
	server_streamid = 2 :: pos_integer(),

	%% Currently active HTTP/2 streams. Streams may be initiated either
	%% by the client or by the server through PUSH_PROMISE frames.
	streams = [] :: [stream()],

	%% HTTP/2 streams that have been reset recently by the server.
	%% We are expected to keep receiving additional frames after
	%% sending an RST_STREAM.
	lingering_streams = [] :: [cowboy_stream:streamid()],

	%% HTTP/2 streams that have been reset recently by the client.
	%% We keep a few of these around in order to reject subsequent
	%% frames on these streams.
	rst_lingering_streams = [] :: [cowboy_stream:streamid()],

	%% Streams can spawn zero or more children which are then managed
	%% by this module if operating as a supervisor.
	children = cowboy_children:init() :: cowboy_children:children(),

	%% The client starts by sending a sequence of bytes as a preface,
	%% followed by a potentially empty SETTINGS frame. Then the connection
	%% is established and continues normally. An exception is when a HEADERS
	%% frame is sent followed by CONTINUATION frames: no other frame can be
	%% sent in between.
	parse_state = undefined :: {preface, sequence, undefined | reference()}
		| {preface, settings, reference()}
		| normal
		| {continuation, cowboy_stream:streamid(), cowboy_stream:fin(), binary()},

	%% HPACK decoding and encoding state.
	decode_state = cow_hpack:init() :: cow_hpack:state(),
	encode_state = cow_hpack:init() :: cow_hpack:state()
}).

-spec init(pid(), ranch:ref(), inet:socket(), module(), cowboy:opts()) -> ok.
init(Parent, Ref, Socket, Transport, Opts) ->
	Peer0 = Transport:peername(Socket),
	Sock0 = Transport:sockname(Socket),
	Cert1 = case Transport:name() of
		ssl ->
			case ssl:peercert(Socket) of
				{error, no_peercert} ->
					{ok, undefined};
				Cert0 ->
					Cert0
			end;
		_ ->
			{ok, undefined}
	end,
	case {Peer0, Sock0, Cert1} of
		{{ok, Peer}, {ok, Sock}, {ok, Cert}} ->
			init(Parent, Ref, Socket, Transport, Opts, Peer, Sock, Cert, <<>>);
		{{error, Reason}, _, _} ->
			terminate(undefined, {socket_error, Reason,
				'A socket error occurred when retrieving the peer name.'});
		{_, {error, Reason}, _} ->
			terminate(undefined, {socket_error, Reason,
				'A socket error occurred when retrieving the sock name.'});
		{_, _, {error, Reason}} ->
			terminate(undefined, {socket_error, Reason,
				'A socket error occurred when retrieving the client TLS certificate.'})
	end.

-spec init(pid(), ranch:ref(), inet:socket(), module(), cowboy:opts(),
	{inet:ip_address(), inet:port_number()}, {inet:ip_address(), inet:port_number()},
	binary() | undefined, binary()) -> ok.
init(Parent, Ref, Socket, Transport, Opts, Peer, Sock, Cert, Buffer) ->
	State0 = #state{parent=Parent, ref=Ref, socket=Socket,
		transport=Transport, opts=Opts, peer=Peer, sock=Sock, cert=Cert,
		remote_window=maps:get(initial_connection_window_size, Opts, 65535),
		parse_state={preface, sequence, preface_timeout(Opts)}},
	State = settings_init(State0, Opts),
	preface(State),
	case Buffer of
		<<>> -> before_loop(State, Buffer);
		_ -> parse(State, Buffer)
	end.

%% @todo Add an argument for the request body.
-spec init(pid(), ranch:ref(), inet:socket(), module(), cowboy:opts(),
	{inet:ip_address(), inet:port_number()}, {inet:ip_address(), inet:port_number()},
	binary() | undefined, binary(), map() | undefined, cowboy_req:req()) -> ok.
init(Parent, Ref, Socket, Transport, Opts, Peer, Sock, Cert, Buffer, _Settings, Req) ->
	State0 = #state{parent=Parent, ref=Ref, socket=Socket,
		transport=Transport, opts=Opts, peer=Peer, sock=Sock, cert=Cert,
		remote_window=maps:get(initial_connection_window_size, Opts, 65535),
		parse_state={preface, sequence, preface_timeout(Opts)}},
	%% @todo Apply settings.
	%% StreamID from HTTP/1.1 Upgrade requests is always 1.
	%% The stream is always in the half-closed (remote) state.
	State1 = stream_handler_init(State0, 1, fin, upgrade, Req),
	%% We assume that the upgrade will be applied. A stream handler
	%% must not prevent the normal operations of the server.
	State2 = info(State1, 1, {switch_protocol, #{
		<<"connection">> => <<"Upgrade">>,
		<<"upgrade">> => <<"h2c">>
	}, ?MODULE, undefined}), %% @todo undefined or #{}?
	State = settings_init(State2, Opts),
	preface(State),
	case Buffer of
		<<>> -> before_loop(State, Buffer);
		_ -> parse(State, Buffer)
	end.

settings_init(State, Opts) ->
	S0 = setting_from_opt(#{}, Opts, max_decode_table_size,
		header_table_size, 4096),
	S1 = setting_from_opt(S0, Opts, max_concurrent_streams,
		max_concurrent_streams, infinity),
	S2 = setting_from_opt(S1, Opts, initial_stream_window_size,
		initial_window_size, 65535),
	S3 = setting_from_opt(S2, Opts, max_frame_size_received,
		max_frame_size, 16384),
	%% @todo max_header_list_size
	Settings = setting_from_opt(S3, Opts, enable_connect_protocol,
		enable_connect_protocol, false),
	%% Start a timer if necessary. The timer will trigger only
	%% if no SETTINGS ack was received in time.
	TRef = case maps:get(settings_timeout, Opts, 5000) of
		infinity -> undefined;
		SettingsTimeout -> erlang:start_timer(SettingsTimeout, self(), settings_timeout)
	end,
	State#state{next_settings=Settings, next_settings_timer=TRef}.

setting_from_opt(Settings, Opts, OptName, SettingName, Default) ->
	case maps:get(OptName, Opts, Default) of
		Default -> Settings;
		Value -> Settings#{SettingName => Value}
	end.

%% We send next_settings and use defaults until we get an ack.
%%
%% We also send a WINDOW_UPDATE frame for the connection when
%% the user specified an initial_connection_window_size.
preface(#state{socket=Socket, transport=Transport, opts=Opts, next_settings=Settings}) ->
	MaybeWindowUpdate = case maps:get(initial_connection_window_size, Opts, 65535) of
		65535 -> <<>>;
		Size -> cow_http2:window_update(Size - 65535)
	end,
	Transport:send(Socket, [cow_http2:settings(Settings), MaybeWindowUpdate]).

preface_timeout(Opts) ->
	case maps:get(preface_timeout, Opts, 5000) of
		infinity -> undefined;
		PrefaceTimeout -> erlang:start_timer(PrefaceTimeout, self(), preface_timeout)
	end.

%% @todo Add the timeout for last time since we heard of connection.
before_loop(State, Buffer) ->
	loop(State, Buffer).

loop(State=#state{parent=Parent, socket=Socket, transport=Transport,
		opts=Opts, children=Children, next_settings_timer=SettingsTRef,
		parse_state=PS}, Buffer) ->
	Transport:setopts(Socket, [{active, once}]),
	{OK, Closed, Error} = Transport:messages(),
	InactivityTimeout = maps:get(inactivity_timeout, Opts, 300000),
	receive
		%% Socket messages.
		{OK, Socket, Data} ->
			parse(State, << Buffer/binary, Data/binary >>);
		{Closed, Socket} ->
			terminate(State, {socket_error, closed, 'The socket has been closed.'});
		{Error, Socket, Reason} ->
			terminate(State, {socket_error, Reason, 'An error has occurred on the socket.'});
		%% System messages.
		{'EXIT', Parent, Reason} ->
			%% @todo We should exit gracefully.
			exit(Reason);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {State, Buffer});
		%% Timeouts.
		{timeout, Ref, {shutdown, Pid}} ->
			cowboy_children:shutdown_timeout(Children, Ref, Pid),
			loop(State, Buffer);
		{timeout, TRef, preface_timeout} ->
			case PS of
				{preface, _, TRef} ->
					terminate(State, {connection_error, protocol_error,
						'The preface was not received in a reasonable amount of time.'});
				_ ->
					loop(State, Buffer)
			end;
		{timeout, SettingsTRef, settings_timeout} ->
			terminate(State, {connection_error, settings_timeout,
				'The SETTINGS ack was not received within the configured time. (RFC7540 6.5.3)'});
		%% Messages pertaining to a stream.
		{{Pid, StreamID}, Msg} when Pid =:= self() ->
			loop(info(State, StreamID, Msg), Buffer);
		%% Exit signal from children.
		Msg = {'EXIT', Pid, _} ->
			loop(down(State, Pid, Msg), Buffer);
		%% Calls from supervisor module.
		{'$gen_call', From, Call} ->
			cowboy_children:handle_supervisor_call(Call, From, Children, ?MODULE),
			loop(State, Buffer);
		Msg ->
			error_logger:error_msg("Received stray message ~p.", [Msg]),
			loop(State, Buffer)
	after InactivityTimeout ->
		terminate(State, {internal_error, timeout, 'No message or data received before timeout.'})
	end.

parse(State=#state{socket=Socket, transport=Transport, parse_state={preface, sequence, TRef}}, Data) ->
	case Data of
		<< "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", Rest/bits >> ->
			parse(State#state{parse_state={preface, settings, TRef}}, Rest);
		_ when byte_size(Data) >= 24 ->
			Transport:close(Socket),
			exit({shutdown, {connection_error, protocol_error,
				'The connection preface was invalid. (RFC7540 3.5)'}});
		_ ->
			Len = byte_size(Data),
			<< Preface:Len/binary, _/bits >> = <<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n">>,
			case Data of
				Preface ->
					before_loop(State, Data);
				_ ->
					Transport:close(Socket),
					exit({shutdown, {connection_error, protocol_error,
						'The connection preface was invalid. (RFC7540 3.5)'}})
			end
	end;
%% @todo Perhaps instead of just more we can have {more, Len} to avoid all the checks.
parse(State=#state{local_settings=#{max_frame_size := MaxFrameSize},
		parse_state=ParseState}, Data) ->
	case cow_http2:parse(Data, MaxFrameSize) of
		{ok, Frame, Rest} ->
			case ParseState of
				normal ->
					parse(frame(State, Frame), Rest);
				{preface, settings, TRef} ->
					parse_settings_preface(State, Frame, Rest, TRef);
				{continuation, _, _, _} ->
					parse(continuation_frame(State, Frame), Rest)
			end;
		{ignore, _} when element(1, ParseState) =:= continuation ->
			terminate(State, {connection_error, protocol_error,
				'An invalid frame was received in the middle of a header block. (RFC7540 6.2)'});
		{ignore, Rest} ->
			parse(State, Rest);
		{stream_error, StreamID, Reason, Human, Rest} ->
			parse(stream_reset(State, StreamID, {stream_error, Reason, Human}), Rest);
		Error = {connection_error, _, _} ->
			terminate(State, Error);
		more ->
			before_loop(State, Data)
	end.

parse_settings_preface(State, Frame={settings, _}, Rest, TRef) ->
	ok = case TRef of
		undefined -> ok;
		_ -> erlang:cancel_timer(TRef, [{async, true}, {info, false}])
	end,
	parse(frame(State#state{parse_state=normal}, Frame), Rest);
parse_settings_preface(State, _, _, _) ->
	terminate(State, {connection_error, protocol_error,
		'The preface sequence must be followed by a SETTINGS frame. (RFC7540 3.5)'}).

%% @todo When we get a 'fin' we need to check if the stream had a 'fin' sent back
%% and terminate the stream if this is the end of it.

%% DATA frame.
frame(State=#state{client_streamid=LastStreamID}, {data, StreamID, _, _})
		when StreamID > LastStreamID ->
	terminate(State, {connection_error, protocol_error,
		'DATA frame received on a stream in idle state. (RFC7540 5.1)'});
frame(State=#state{remote_window=ConnWindow}, {data, _, _, Data})
		when byte_size(Data) > ConnWindow ->
	terminate(State, {connection_error, flow_control_error,
		'DATA frame overflowed the connection flow control window. (RFC7540 6.9, RFC7540 6.9.1)'});
frame(State0=#state{remote_window=ConnWindow, streams=Streams, lingering_streams=Lingering},
		{data, StreamID, IsFin, Data}) ->
	DataLen = byte_size(Data),
	State = State0#state{remote_window=ConnWindow - DataLen},
	case lists:keyfind(StreamID, #stream.id, Streams) of
		#stream{remote_window=StreamWindow} when StreamWindow < DataLen ->
			stream_reset(State, StreamID, {stream_error, flow_control_error,
				'DATA frame overflowed the stream flow control window. (RFC7540 6.9, RFC7540 6.9.1)'});
		Stream = #stream{remote=nofin} ->
			data_frame(State, Stream, IsFin, DataLen, Data);
		#stream{remote=fin} ->
			stream_reset(State, StreamID, {stream_error, stream_closed,
				'DATA frame received for a half-closed (remote) stream. (RFC7540 5.1)'});
		false ->
			%% After we send an RST_STREAM frame and terminate a stream,
			%% the client still might be sending us some more frames
			%% until it can process this RST_STREAM. We therefore ignore
			%% DATA frames received for such lingering streams.
			case lists:member(StreamID, Lingering) of
				true ->
					State0;
				false ->
					terminate(State, {connection_error, stream_closed,
						'DATA frame received for a closed stream. (RFC7540 5.1)'})
			end
	end;
%% HEADERS frame with invalid even-numbered streamid.
frame(State, {headers, StreamID, _, _, _}) when StreamID rem 2 =:= 0 ->
	terminate(State, {connection_error, protocol_error,
		'HEADERS frame received with even-numbered streamid. (RFC7540 5.1.1)'});
%% Either a HEADERS frame received on (half-)closed stream,
%% or a HEADERS frame containing the trailers.
frame(State=#state{client_streamid=LastStreamID, streams=Streams},
		{headers, StreamID, IsFin, IsHeadFin, HeaderBlockOrFragment})
		when StreamID =< LastStreamID ->
	case lists:keyfind(StreamID, #stream.id, Streams) of
		Stream = #stream{remote=nofin} when IsFin =:= fin ->
			case IsHeadFin of
				head_fin ->
					stream_decode_trailers(State, Stream, HeaderBlockOrFragment);
				head_nofin ->
					State#state{parse_state={continuation, StreamID, IsFin, HeaderBlockOrFragment}}
			end;
		%% We always close the connection here to avoid having to decode
		%% the headers to not waste resources on non-compliant clients.
		#stream{remote=nofin} when IsFin =:= nofin ->
			terminate(State, {connection_error, protocol_error,
				'Trailing HEADERS frame received without the END_STREAM flag set. (RFC7540 8.1, RFC7540 8.1.2.6)'});
		_ ->
			terminate(State, {connection_error, stream_closed,
				'HEADERS frame received on a stream in closed or half-closed state. (RFC7540 5.1)'})
	end;
%% Single HEADERS frame headers block.
frame(State, {headers, StreamID, IsFin, head_fin, HeaderBlock}) ->
	stream_decode_init(State, StreamID, IsFin, HeaderBlock);
%% HEADERS frame starting a headers block. Enter continuation mode.
frame(State, {headers, StreamID, IsFin, head_nofin, HeaderBlockFragment}) ->
	State#state{parse_state={continuation, StreamID, IsFin, HeaderBlockFragment}};
%% Single HEADERS frame headers block with priority.
frame(State, {headers, StreamID, IsFin, head_fin,
		_IsExclusive, _DepStreamID, _Weight, HeaderBlock}) ->
	%% @todo Handle priority.
	stream_decode_init(State, StreamID, IsFin, HeaderBlock);
%% HEADERS frame starting a headers block. Enter continuation mode.
frame(State, {headers, StreamID, IsFin, head_nofin,
		_IsExclusive, _DepStreamID, _Weight, HeaderBlockFragment}) ->
	%% @todo Handle priority.
	State#state{parse_state={continuation, StreamID, IsFin, HeaderBlockFragment}};
%% PRIORITY frame.
frame(State, {priority, _StreamID, _IsExclusive, _DepStreamID, _Weight}) ->
	%% @todo Handle priority.
	State;
%% RST_STREAM frame.
frame(State=#state{client_streamid=LastStreamID}, {rst_stream, StreamID, _})
		when StreamID > LastStreamID ->
	terminate(State, {connection_error, protocol_error,
		'RST_STREAM frame received on a stream in idle state. (RFC7540 5.1)'});
frame(State, {rst_stream, StreamID, Reason}) ->
	stream_terminate(stream_rst_linger(State, StreamID), StreamID,
		{stream_error, Reason, 'Stream reset requested by client.'});
%% SETTINGS frame.
frame(State0=#state{socket=Socket, transport=Transport, opts=Opts,
		remote_settings=Settings0}, {settings, Settings}) ->
	Transport:send(Socket, cow_http2:settings_ack()),
	State1 = State0#state{remote_settings=maps:merge(Settings0, Settings)},
	maps:fold(fun
		(header_table_size, NewSize, State=#state{encode_state=EncodeState0}) ->
			MaxSize = maps:get(max_encode_table_size, Opts, 4096),
			EncodeState = cow_hpack:set_max_size(min(NewSize, MaxSize), EncodeState0),
			State#state{encode_state=EncodeState};
		(initial_window_size, NewWindowSize, State) ->
			OldWindowSize = maps:get(initial_window_size, Settings0, 65535),
			update_streams_local_window(State, NewWindowSize - OldWindowSize);
		(_, _, State) ->
			State
	end, State1, Settings);
%% Ack for a previously sent SETTINGS frame.
frame(State0=#state{local_settings=Local0, next_settings=NextSettings,
		next_settings_timer=TRef}, settings_ack) ->
	ok = erlang:cancel_timer(TRef, [{async, true}, {info, false}]),
	Local = maps:merge(Local0, NextSettings),
	State1 = State0#state{local_settings=Local, next_settings=#{},
		next_settings_timer=undefined},
	maps:fold(fun
		(header_table_size, MaxSize, State=#state{decode_state=DecodeState0}) ->
			DecodeState = cow_hpack:set_max_size(MaxSize, DecodeState0),
			State#state{decode_state=DecodeState};
		(initial_window_size, NewWindowSize, State) ->
			OldWindowSize = maps:get(initial_window_size, Local0, 65535),
			update_streams_remote_window(State, NewWindowSize - OldWindowSize);
		(_, _, State) ->
			State
	end, State1, NextSettings);
%% Unexpected PUSH_PROMISE frame.
frame(State, {push_promise, _, _, _, _}) ->
	terminate(State, {connection_error, protocol_error,
		'PUSH_PROMISE frames MUST only be sent on a peer-initiated stream. (RFC7540 6.6)'});
%% PING frame.
frame(State=#state{socket=Socket, transport=Transport}, {ping, Opaque}) ->
	Transport:send(Socket, cow_http2:ping_ack(Opaque)),
	State;
%% Ack for a previously sent PING frame.
%%
%% @todo Might want to check contents but probably a waste of time.
frame(State, {ping_ack, _Opaque}) ->
	State;
%% GOAWAY frame.
frame(State, Frame={goaway, _, _, _}) ->
	terminate(State, {stop, Frame, 'Client is going away.'});
%% Connection-wide WINDOW_UPDATE frame.
frame(State=#state{local_window=ConnWindow}, {window_update, Increment})
		when ConnWindow + Increment > 16#7fffffff ->
	terminate(State, {connection_error, flow_control_error,
		'The flow control window must not be greater than 2^31-1. (RFC7540 6.9.1)'});
frame(State=#state{local_window=ConnWindow}, {window_update, Increment}) ->
	send_data(State#state{local_window=ConnWindow + Increment});
%% Stream-specific WINDOW_UPDATE frame.
frame(State=#state{client_streamid=LastStreamID}, {window_update, StreamID, _})
		when StreamID > LastStreamID ->
	terminate(State, {connection_error, protocol_error,
		'WINDOW_UPDATE frame received on a stream in idle state. (RFC7540 5.1)'});
frame(State0=#state{streams=Streams0, rst_lingering_streams=RstLingering},
		{window_update, StreamID, Increment}) ->
	case lists:keyfind(StreamID, #stream.id, Streams0) of
		#stream{local_window=StreamWindow} when StreamWindow + Increment > 16#7fffffff ->
			stream_reset(State0, StreamID, {stream_error, flow_control_error,
				'The flow control window must not be greater than 2^31-1. (RFC7540 6.9.1)'});
		Stream0 = #stream{local_window=StreamWindow} ->
			{State, Stream} = send_data(State0,
				Stream0#stream{local_window=StreamWindow + Increment}),
			Streams = lists:keystore(StreamID, #stream.id, Streams0, Stream),
			State#state{streams=Streams};
		false ->
			%% WINDOW_UPDATE frames may be received for a short period of time
			%% after a stream is closed. They must be ignored.
			case lists:member(StreamID, RstLingering) of
				false -> State0;
				true -> stream_closed(State0, StreamID, {stream_error, stream_closed,
					'WINDOW_UPDATE frame received after the stream was reset. (RFC7540 5.1)'})
			end
	end;
%% Unexpected CONTINUATION frame.
frame(State, {continuation, _, _, _}) ->
	terminate(State, {connection_error, protocol_error,
		'CONTINUATION frames MUST be preceded by a HEADERS frame. (RFC7540 6.10)'}).

data_frame(State, Stream0=#stream{id=StreamID, remote_window=StreamWindow,
		remote_read_size=StreamRead}, IsFin, DataLen, Data) ->
	Stream = Stream0#stream{remote=IsFin,
		remote_window=StreamWindow - DataLen,
		remote_read_size=StreamRead + DataLen},
	case is_body_size_valid(Stream) of
		true ->
			data_frame(State, Stream, IsFin, Data);
		false ->
			stream_reset(after_commands(State, Stream), StreamID, {stream_error, protocol_error,
				'The total size of DATA frames is different than the content-length. (RFC7540 8.1.2.6)'})
	end.

%% We ignore DATA frames for streams that are flushing out data.
data_frame(State, Stream=#stream{state=flush}, _, _) ->
	after_commands(State, Stream);
data_frame(State, Stream=#stream{id=StreamID, state=StreamState0}, IsFin, Data) ->
	try cowboy_stream:data(StreamID, IsFin, Data, StreamState0) of
		{Commands, StreamState} ->
			commands(State, Stream#stream{state=StreamState}, Commands)
	catch Class:Exception ->
		cowboy_stream:report_error(data,
			[StreamID, IsFin, Data, StreamState0],
			Class, Exception, erlang:get_stacktrace()),
		stream_reset(State, StreamID, {internal_error, {Class, Exception},
			'Unhandled exception in cowboy_stream:data/4.'})
	end.

%% It's always valid when no content-length header was specified.
is_body_size_valid(#stream{remote_expected_size=undefined}) ->
	true;
%% We didn't finish reading the body but the size is already larger than expected.
is_body_size_valid(#stream{remote=nofin, remote_expected_size=Expected,
		remote_read_size=Read}) when Read > Expected ->
	false;
is_body_size_valid(#stream{remote=nofin}) ->
	true;
is_body_size_valid(#stream{remote=fin, remote_expected_size=Expected,
		remote_read_size=Expected}) ->
	true;
%% We finished reading the body and the size read is not the one expected.
is_body_size_valid(_) ->
	false.

continuation_frame(State=#state{client_streamid=LastStreamID, streams=Streams,
		parse_state={continuation, StreamID, IsFin, HeaderBlockFragment0}},
		{continuation, StreamID, head_fin, HeaderBlockFragment1}) ->
	HeaderBlock = << HeaderBlockFragment0/binary, HeaderBlockFragment1/binary >>,
	case StreamID > LastStreamID of
		true -> %% New stream.
			stream_decode_init(State#state{parse_state=normal}, StreamID, IsFin, HeaderBlock);
		false -> %% Trailers.
			Stream = lists:keyfind(StreamID, #stream.id, Streams),
			stream_decode_trailers(State, Stream, HeaderBlock)
	end;
continuation_frame(State=#state{parse_state={continuation, StreamID, IsFin, HeaderBlockFragment0}},
		{continuation, StreamID, head_nofin, HeaderBlockFragment1}) ->
	State#state{parse_state={continuation, StreamID, IsFin,
		<< HeaderBlockFragment0/binary, HeaderBlockFragment1/binary >>}};
continuation_frame(State, _) ->
	terminate(State, {connection_error, protocol_error,
		'An invalid frame was received in the middle of a header block. (RFC7540 6.2)'}).

down(State=#state{children=Children0}, Pid, Msg) ->
	case cowboy_children:down(Children0, Pid) of
		%% The stream was terminated already.
		{ok, undefined, Children} ->
			State#state{children=Children};
		%% The stream is still running.
		{ok, StreamID, Children} ->
			info(State#state{children=Children}, StreamID, Msg);
		%% The process was unknown.
		error ->
			error_logger:error_msg("Received EXIT signal ~p for unknown process ~p.~n", [Msg, Pid]),
			State
	end.

info(State=#state{client_streamid=LastStreamID, streams=Streams}, StreamID, Msg) ->
	case lists:keyfind(StreamID, #stream.id, Streams) of
		#stream{state=flush} ->
			error_logger:error_msg("Received message ~p for terminated stream ~p.", [Msg, StreamID]),
			State;
		Stream = #stream{state=StreamState0} ->
			try cowboy_stream:info(StreamID, Msg, StreamState0) of
				{Commands, StreamState} ->
					commands(State, Stream#stream{state=StreamState}, Commands)
			catch Class:Exception ->
				cowboy_stream:report_error(info,
					[StreamID, Msg, StreamState0],
					Class, Exception, erlang:get_stacktrace()),
				stream_reset(State, StreamID, {internal_error, {Class, Exception},
					'Unhandled exception in cowboy_stream:info/3.'})
			end;
		false when StreamID =< LastStreamID ->
			%% Streams that were reset by the client or streams that are
			%% in the lingering state may still have Erlang messages going
			%% around. In these cases we do not want to log anything.
			State;
		false ->
			error_logger:error_msg("Received message ~p for unknown stream ~p.",
				[Msg, StreamID]),
			State
	end.

%% @todo Kill the stream if it tries to send a response, headers,
%% data or push promise when the stream is closed or half-closed.

commands(State, Stream, []) ->
	after_commands(State, Stream);
%% Error responses are sent only if a response wasn't sent already.
commands(State, Stream=#stream{local=idle}, [{error_response, StatusCode, Headers, Body}|Tail]) ->
	commands(State, Stream, [{response, StatusCode, Headers, Body}|Tail]);
commands(State, Stream, [{error_response, _, _, _}|Tail]) ->
	commands(State, Stream, Tail);
%% Send an informational response.
commands(State0, Stream=#stream{local=idle}, [{inform, StatusCode, Headers}|Tail]) ->
	State = send_headers(State0, Stream, StatusCode, Headers, nofin),
	commands(State, Stream, Tail);
%% Send response headers.
commands(State0, Stream0=#stream{local=idle},
		[{response, StatusCode, Headers, Body}|Tail]) ->
	{State, Stream} = send_response(State0, Stream0, StatusCode, Headers, Body),
	commands(State, Stream, Tail);
%% Send response headers.
commands(State0, Stream=#stream{method=Method, local=idle},
		[{headers, StatusCode, Headers}|Tail]) ->
	IsFin = case Method of
		<<"HEAD">> -> fin;
		_ -> nofin
	end,
	State = send_headers(State0, Stream, StatusCode, Headers, IsFin),
	commands(State, Stream#stream{local=IsFin}, Tail);
%% Send a response body chunk.
commands(State0, Stream0=#stream{local=nofin}, [{data, IsFin, Data}|Tail]) ->
	{State, Stream} = send_data(State0, Stream0, IsFin, Data),
	commands(State, Stream, Tail);
%% Send trailers.
commands(State0, Stream0=#stream{local=nofin, te=TE0}, [{trailers, Trailers}|Tail]) ->
	%% We only accept TE headers containing exactly "trailers" (RFC7540 8.1.2.1).
	TE = try cow_http_hd:parse_te(TE0) of
		{trailers, []} -> trailers;
		_ -> no_trailers
	catch _:_ ->
		%% If we can't parse the TE header, assume we can't send trailers.
		no_trailers
	end,
	{State, Stream} = case TE of
		trailers ->
			send_data(State0, Stream0, fin, {trailers, Trailers});
		no_trailers ->
			send_data(State0, Stream0, fin, <<>>)
	end,
	commands(State, Stream, Tail);
%% Send a file.
%% @todo Add the sendfile command.
%commands(State0, Stream0=#stream{local=nofin},
%		[{sendfile, IsFin, Offset, Bytes, Path}|Tail]) ->
%	{State, Stream} = send_data(State0, Stream0, IsFin, {sendfile, Offset, Bytes, Path}),
%	commands(State, Stream, Tail);
%% Send a push promise.
%%
%% @todo We need to keep track of what promises we made so that we don't
%% end up with an infinite loop of promises.
commands(State0=#state{socket=Socket, transport=Transport, server_streamid=PromisedStreamID,
		encode_state=EncodeState0}, Stream=#stream{id=StreamID},
		[{push, Method, Scheme, Host, Port, Path, Qs, Headers0}|Tail]) ->
	Authority = case {Scheme, Port} of
		{<<"http">>, 80} -> Host;
		{<<"https">>, 443} -> Host;
		_ -> iolist_to_binary([Host, $:, integer_to_binary(Port)])
	end,
	PathWithQs = iolist_to_binary(case Qs of
		<<>> -> Path;
		_ -> [Path, $?, Qs]
	end),
	%% We need to make sure the header value is binary before we can
	%% pass it to stream_req_init, as it expects them to be flat.
	Headers1 = maps:map(fun(_, V) -> iolist_to_binary(V) end, Headers0),
	Headers = Headers1#{
		<<":method">> => Method,
		<<":scheme">> => Scheme,
		<<":authority">> => Authority,
		<<":path">> => PathWithQs},
	{HeaderBlock, EncodeState} = headers_encode(Headers, EncodeState0),
	Transport:send(Socket, cow_http2:push_promise(StreamID, PromisedStreamID, HeaderBlock)),
	State = stream_req_init(State0#state{server_streamid=PromisedStreamID + 2,
		encode_state=EncodeState}, PromisedStreamID, fin, Headers1, #{
			method => Method,
			scheme => Scheme,
			authority => Authority,
			path => PathWithQs
		}),
	commands(State, Stream, Tail);
commands(State=#state{socket=Socket, transport=Transport, remote_window=ConnWindow},
		Stream=#stream{id=StreamID, remote_window=StreamWindow},
		[{flow, Size}|Tail]) ->
	Transport:send(Socket, [
		cow_http2:window_update(Size),
		cow_http2:window_update(StreamID, Size)
	]),
	commands(State#state{remote_window=ConnWindow + Size},
		Stream#stream{remote_window=StreamWindow + Size}, Tail);
%% Supervise a child process.
commands(State=#state{children=Children}, Stream=#stream{id=StreamID},
		[{spawn, Pid, Shutdown}|Tail]) ->
	 commands(State#state{children=cowboy_children:up(Children, Pid, StreamID, Shutdown)},
		Stream, Tail);
%% Error handling.
commands(State, Stream=#stream{id=StreamID}, [Error = {internal_error, _, _}|_Tail]) ->
	%% @todo Do we want to run the commands after an internal_error?
	%% @todo Do we even allow commands after?
	%% @todo Only reset when the stream still exists.
	stream_reset(after_commands(State, Stream), StreamID, Error);
%% Upgrade to HTTP/2. This is triggered by cowboy_http2 itself.
commands(State=#state{socket=Socket, transport=Transport},
		Stream=#stream{local=upgrade}, [{switch_protocol, Headers, ?MODULE, _}|Tail]) ->
	Transport:send(Socket, cow_http:response(101, 'HTTP/1.1', maps:to_list(Headers))),
	commands(State, Stream#stream{local=idle}, Tail);
%% Use a different protocol within the stream (CONNECT :protocol).
%% @todo Make sure we error out when the feature is disabled.
commands(State0, #stream{id=StreamID}, [{switch_protocol, Headers, _Mod, _ModState}|Tail]) ->
	State = #state{streams=Streams} = info(State0, StreamID, {headers, 200, Headers}),
	Stream = lists:keyfind(StreamID, #stream.id, Streams),
	commands(State, Stream, Tail);
commands(State, Stream=#stream{id=StreamID}, [stop|_Tail]) ->
	%% @todo Do we want to run the commands after a stop?
	%% @todo Do we even allow commands after?
	stream_terminate(after_commands(State, Stream), StreamID, normal).

after_commands(State=#state{streams=Streams0}, Stream=#stream{id=StreamID}) ->
	Streams = lists:keystore(StreamID, #stream.id, Streams0, Stream),
	State#state{streams=Streams}.

send_response(State0, Stream=#stream{method=Method}, StatusCode, Headers0, Body) ->
	if
		Method =:= <<"HEAD">>; Body =:= <<>> ->
			State = send_headers(State0, Stream, StatusCode, Headers0, fin),
			{State, Stream#stream{local=fin}};
		true ->
			State = send_headers(State0, Stream, StatusCode, Headers0, nofin),
			%% send_data works with both sendfile and iolists.
			send_data(State, Stream#stream{local=nofin}, fin, Body)
	end.

send_headers(State=#state{socket=Socket, transport=Transport, encode_state=EncodeState0},
		#stream{id=StreamID}, StatusCode, Headers0, IsFin) ->
	Headers = Headers0#{<<":status">> => status(StatusCode)},
	{HeaderBlock, EncodeState} = headers_encode(Headers, EncodeState0),
	Transport:send(Socket, cow_http2:headers(StreamID, IsFin, HeaderBlock)),
	State#state{encode_state=EncodeState}.

status(Status) when is_integer(Status) ->
	integer_to_binary(Status);
status(<< H, T, U, _/bits >>) when H >= $1, H =< $9, T >= $0, T =< $9, U >= $0, U =< $9 ->
	<< H, T, U >>.

%% @todo Should we ever want to implement the PRIORITY mechanism,
%% this would be the place to do it. Right now, we just go over
%% all streams and send what we can until either everything is
%% sent or we run out of space in the window.
send_data(State=#state{streams=Streams}) ->
	resume_streams(State, Streams, []).

%% When SETTINGS_INITIAL_WINDOW_SIZE changes we need to update
%% the local stream windows for all active streams and perhaps
%% resume sending data.
update_streams_local_window(State=#state{streams=Streams0}, Increment) ->
	Streams = [
		S#stream{local_window=StreamWindow + Increment}
	|| S=#stream{local_window=StreamWindow} <- Streams0],
	resume_streams(State, Streams, []).

%% When we receive an ack to a SETTINGS frame we sent we need to update
%% the remote stream windows for all active streams.
update_streams_remote_window(State=#state{streams=Streams0}, Increment) ->
	Streams = [
		S#stream{remote_window=StreamWindow + Increment}
	|| S=#stream{remote_window=StreamWindow} <- Streams0],
	State#state{streams=Streams}.

resume_streams(State, [], Acc) ->
	State#state{streams=lists:reverse(Acc)};
%% While technically we should never get < 0 here, let's be on the safe side.
resume_streams(State=#state{local_window=ConnWindow}, Streams, Acc)
		when ConnWindow =< 0 ->
	State#state{streams=lists:reverse(Acc, Streams)};
%% We rely on send_data/2 to do all the necessary checks about the stream.
resume_streams(State0, [Stream0|Tail], Acc) ->
	{State1, Stream} = send_data(State0, Stream0),
	case Stream of
		%% We are done flushing, remove the stream.
		%% Maybe skip the request body if it was not fully read.
		#stream{state=flush, local=fin} ->
			State = maybe_skip_body(State1, Stream, normal),
			resume_streams(State, Tail, Acc);
		%% Keep the stream. Either the stream handler is still running,
		%% or we are not finished flushing.
		_ ->
			resume_streams(State1, Tail, [Stream|Acc])
	end.

send_data(State, Stream=#stream{local=Local, local_buffer_size=0, local_trailers=Trailers})
		when (Trailers =/= undefined) andalso ((Local =:= idle) orelse (Local =:= nofin)) ->
	send_trailers(State, Stream#stream{local_trailers=undefined}, Trailers);
%% @todo It's possible that the stream terminates. We must remove it.
send_data(State=#state{local_window=ConnWindow},
		Stream=#stream{local=IsFin, local_window=StreamWindow, local_buffer_size=BufferSize})
		when ConnWindow =< 0; IsFin =:= fin; StreamWindow =< 0; BufferSize =:= 0 ->
	{State, Stream};
send_data(State0, Stream0=#stream{local_buffer=Q0, local_buffer_size=BufferSize}) ->
	%% We know there is an item in the queue.
	{{value, {IsFin, DataSize, Data}}, Q} = queue:out(Q0),
	{State, Stream} = send_data(State0,
		Stream0#stream{local_buffer=Q, local_buffer_size=BufferSize - DataSize},
		IsFin, Data, in_r),
	send_data(State, Stream).

send_data(State, Stream, IsFin, Data) ->
	send_data(State, Stream, IsFin, Data, in).

%% We can send trailers immediately if the queue is empty, otherwise we queue.
%% We always send trailer frames even if the window is empty.
send_data(State, Stream=#stream{local_buffer_size=0}, fin, {trailers, Trailers}, _) ->
	send_trailers(State, Stream, Trailers);
send_data(State, Stream, fin, {trailers, Trailers}, _) ->
	{State, Stream#stream{local_trailers=Trailers}};
%% Send data immediately if we can, buffer otherwise.
send_data(State=#state{local_window=ConnWindow},
		Stream=#stream{local_window=StreamWindow}, IsFin, Data, In)
		when ConnWindow =< 0; StreamWindow =< 0 ->
	{State, queue_data(Stream, IsFin, Data, In)};
send_data(State=#state{socket=Socket, transport=Transport, opts=Opts,
		remote_settings=RemoteSettings, local_window=ConnWindow},
		Stream=#stream{id=StreamID, local_window=StreamWindow}, IsFin, Data, In) ->
	RemoteMaxFrameSize = maps:get(max_frame_size, RemoteSettings, 16384),
	ConfiguredMaxFrameSize = maps:get(max_frame_size_sent, Opts, infinity),
	MaxSendSize = min(
		min(ConnWindow, StreamWindow),
		min(RemoteMaxFrameSize, ConfiguredMaxFrameSize)
	),
	case Data of
		{sendfile, Offset, Bytes, Path} when Bytes =< MaxSendSize ->
			Transport:send(Socket, cow_http2:data_header(StreamID, IsFin, Bytes)),
			Transport:sendfile(Socket, Path, Offset, Bytes),
			{State#state{local_window=ConnWindow - Bytes},
				Stream#stream{local=IsFin, local_window=StreamWindow - Bytes}};
		{sendfile, Offset, Bytes, Path} ->
			Transport:send(Socket, cow_http2:data_header(StreamID, nofin, MaxSendSize)),
			Transport:sendfile(Socket, Path, Offset, MaxSendSize),
			send_data(State#state{local_window=ConnWindow - MaxSendSize},
				Stream#stream{local_window=StreamWindow - MaxSendSize},
				IsFin, {sendfile, Offset + MaxSendSize, Bytes - MaxSendSize, Path}, In);
		Iolist0 ->
			IolistSize = iolist_size(Iolist0),
			if
				IolistSize =< MaxSendSize ->
					Transport:send(Socket, cow_http2:data(StreamID, IsFin, Iolist0)),
					{State#state{local_window=ConnWindow - IolistSize},
						Stream#stream{local=IsFin, local_window=StreamWindow - IolistSize}};
				true ->
					{Iolist, More} = cowboy_iolists:split(MaxSendSize, Iolist0),
					Transport:send(Socket, cow_http2:data(StreamID, nofin, Iolist)),
					send_data(State#state{local_window=ConnWindow - MaxSendSize},
						Stream#stream{local_window=StreamWindow - MaxSendSize},
						IsFin, More, In)
			end
	end.

send_trailers(State=#state{socket=Socket, transport=Transport, encode_state=EncodeState0},
		Stream=#stream{id=StreamID}, Trailers) ->
	{HeaderBlock, EncodeState} = headers_encode(Trailers, EncodeState0),
	Transport:send(Socket, cow_http2:headers(StreamID, fin, HeaderBlock)),
	{State#state{encode_state=EncodeState}, Stream#stream{local=fin}}.

queue_data(Stream=#stream{local_buffer=Q0, local_buffer_size=Size0}, IsFin, Data, In) ->
	DataSize = case Data of
		{sendfile, _, Bytes, _} -> Bytes;
		Iolist -> iolist_size(Iolist)
	end,
	Q = queue:In({IsFin, DataSize, Data}, Q0),
	Stream#stream{local_buffer=Q, local_buffer_size=Size0 + DataSize}.

%% The set-cookie header is special; we can only send one cookie per header.
headers_encode(Headers0=#{<<"set-cookie">> := SetCookies}, EncodeState) ->
	Headers1 = maps:to_list(maps:remove(<<"set-cookie">>, Headers0)),
	Headers = Headers1 ++ [{<<"set-cookie">>, Value} || Value <- SetCookies],
	cow_hpack:encode(Headers, EncodeState);
headers_encode(Headers0, EncodeState) ->
	Headers = maps:to_list(Headers0),
	cow_hpack:encode(Headers, EncodeState).

-spec terminate(#state{}, _) -> no_return().
terminate(undefined, Reason) ->
	exit({shutdown, Reason});
terminate(#state{socket=Socket, transport=Transport, parse_state={preface, _, _}}, Reason) ->
	Transport:close(Socket),
	exit({shutdown, Reason});
terminate(#state{socket=Socket, transport=Transport, client_streamid=LastStreamID,
		streams=Streams, children=Children}, Reason) ->
	%% @todo We might want to optionally send the Reason value
	%% as debug data in the GOAWAY frame here. Perhaps more.
	Transport:send(Socket, cow_http2:goaway(LastStreamID, terminate_reason(Reason), <<>>)),
	terminate_all_streams(Streams, Reason),
	cowboy_children:terminate(Children),
	Transport:close(Socket),
	exit({shutdown, Reason}).

terminate_reason({connection_error, Reason, _}) -> Reason;
terminate_reason({stop, _, _}) -> no_error;
terminate_reason({socket_error, _, _}) -> internal_error;
terminate_reason({internal_error, _, _}) -> internal_error.

terminate_all_streams([], _) ->
	ok;
%% This stream was already terminated and is now just flushing the data out. Skip it.
terminate_all_streams([#stream{state=flush}|Tail], Reason) ->
	terminate_all_streams(Tail, Reason);
terminate_all_streams([#stream{id=StreamID, state=StreamState}|Tail], Reason) ->
	stream_call_terminate(StreamID, Reason, StreamState),
	terminate_all_streams(Tail, Reason).

%% Stream functions.

stream_decode_init(State=#state{decode_state=DecodeState0}, StreamID, IsFin, HeaderBlock) ->
	try cow_hpack:decode(HeaderBlock, DecodeState0) of
		{Headers, DecodeState} ->
			stream_enforce_concurrency_limit(State#state{decode_state=DecodeState},
				StreamID, IsFin, Headers)
	catch _:_ ->
		terminate(State, {connection_error, compression_error,
			'Error while trying to decode HPACK-encoded header block. (RFC7540 4.3)'})
	end.

stream_enforce_concurrency_limit(State=#state{opts=Opts, streams=Streams},
		StreamID, IsFin, Headers) ->
	MaxConcurrentStreams = maps:get(max_concurrent_streams, Opts, infinity),
	case length(Streams) < MaxConcurrentStreams of
		true ->
			stream_pseudo_headers_init(State, StreamID, IsFin, Headers);
		false ->
			stream_refused(State, StreamID,
				'Maximum number of concurrent streams has been reached. (RFC7540 5.1.2)')
	end.

stream_pseudo_headers_init(State=#state{local_settings=LocalSettings},
		StreamID, IsFin, Headers0) ->
	IsExtendedConnectEnabled = maps:get(enable_connect_protocol, LocalSettings, false),
	case pseudo_headers(Headers0, #{}) of
		{ok, PseudoHeaders=#{method := <<"CONNECT">>, scheme := _,
			authority := _, path := _, protocol := _}, Headers}
			when IsExtendedConnectEnabled ->
			stream_regular_headers_init(State, StreamID, IsFin, Headers, PseudoHeaders);
		{ok, #{method := <<"CONNECT">>, scheme := _,
			authority := _, path := _}, _}
			when IsExtendedConnectEnabled ->
			stream_malformed(State, StreamID,
				'The :protocol pseudo-header MUST be sent with an extended CONNECT. (draft_h2_websockets 4)');
		{ok, #{protocol := _}, _} ->
			stream_malformed(State, StreamID,
				'The :protocol pseudo-header is only defined for the extended CONNECT. (draft_h2_websockets 4)');
		%% @todo Add clause for CONNECT requests (no scheme/path).
		{ok, PseudoHeaders=#{method := <<"CONNECT">>}, _} ->
			stream_early_error(State, StreamID, IsFin, 501, PseudoHeaders,
				'The CONNECT method is currently not implemented. (RFC7231 4.3.6)');
		{ok, PseudoHeaders=#{method := <<"TRACE">>}, _} ->
			stream_early_error(State, StreamID, IsFin, 501, PseudoHeaders,
				'The TRACE method is currently not implemented. (RFC7231 4.3.8)');
		{ok, PseudoHeaders=#{method := _, scheme := _, authority := _, path := _}, Headers} ->
			stream_regular_headers_init(State, StreamID, IsFin, Headers, PseudoHeaders);
		{ok, _, _} ->
			stream_malformed(State, StreamID,
				'A required pseudo-header was not found. (RFC7540 8.1.2.3)');
		{error, HumanReadable} ->
			stream_malformed(State, StreamID, HumanReadable)
	end.

pseudo_headers([{<<":method">>, _}|_], #{method := _}) ->
	{error, 'Multiple :method pseudo-headers were found. (RFC7540 8.1.2.3)'};
pseudo_headers([{<<":method">>, Method}|Tail], PseudoHeaders) ->
	pseudo_headers(Tail, PseudoHeaders#{method => Method});
pseudo_headers([{<<":scheme">>, _}|_], #{scheme := _}) ->
	{error, 'Multiple :scheme pseudo-headers were found. (RFC7540 8.1.2.3)'};
pseudo_headers([{<<":scheme">>, Scheme}|Tail], PseudoHeaders) ->
	pseudo_headers(Tail, PseudoHeaders#{scheme => Scheme});
pseudo_headers([{<<":authority">>, _}|_], #{authority := _}) ->
	{error, 'Multiple :authority pseudo-headers were found. (RFC7540 8.1.2.3)'};
pseudo_headers([{<<":authority">>, Authority}|Tail], PseudoHeaders) ->
	pseudo_headers(Tail, PseudoHeaders#{authority => Authority});
pseudo_headers([{<<":path">>, _}|_], #{path := _}) ->
	{error, 'Multiple :path pseudo-headers were found. (RFC7540 8.1.2.3)'};
pseudo_headers([{<<":path">>, Path}|Tail], PseudoHeaders) ->
	pseudo_headers(Tail, PseudoHeaders#{path => Path});
pseudo_headers([{<<":protocol">>, _}|_], #{protocol := _}) ->
	{error, 'Multiple :protocol pseudo-headers were found. (RFC7540 8.1.2.3)'};
pseudo_headers([{<<":protocol">>, Protocol}|Tail], PseudoHeaders) ->
	pseudo_headers(Tail, PseudoHeaders#{protocol => Protocol});
pseudo_headers([{<<":", _/bits>>, _}|_], _) ->
	{error, 'An unknown or invalid pseudo-header was found. (RFC7540 8.1.2.1)'};
pseudo_headers(Headers, PseudoHeaders) ->
	{ok, PseudoHeaders, Headers}.

stream_regular_headers_init(State, StreamID, IsFin, Headers, PseudoHeaders) ->
	case regular_headers(Headers) of
		ok ->
			stream_req_init(State, StreamID, IsFin,
				headers_to_map(Headers, #{}), PseudoHeaders);
		{error, HumanReadable} ->
			stream_malformed(State, StreamID, HumanReadable)
	end.

regular_headers([{<<":", _/bits>>, _}|_]) ->
	{error, 'Pseudo-headers were found after regular headers. (RFC7540 8.1.2.1)'};
regular_headers([{<<"connection">>, _}|_]) ->
	{error, 'The connection header is not allowed. (RFC7540 8.1.2.2)'};
regular_headers([{<<"keep-alive">>, _}|_]) ->
	{error, 'The keep-alive header is not allowed. (RFC7540 8.1.2.2)'};
regular_headers([{<<"proxy-authenticate">>, _}|_]) ->
	{error, 'The proxy-authenticate header is not allowed. (RFC7540 8.1.2.2)'};
regular_headers([{<<"proxy-authorization">>, _}|_]) ->
	{error, 'The proxy-authorization header is not allowed. (RFC7540 8.1.2.2)'};
regular_headers([{<<"transfer-encoding">>, _}|_]) ->
	{error, 'The transfer-encoding header is not allowed. (RFC7540 8.1.2.2)'};
regular_headers([{<<"upgrade">>, _}|_]) ->
	{error, 'The upgrade header is not allowed. (RFC7540 8.1.2.2)'};
regular_headers([{<<"te">>, Value}|_]) when Value =/= <<"trailers">> ->
	{error, 'The te header with a value other than "trailers" is not allowed. (RFC7540 8.1.2.2)'};
regular_headers([{Name, _}|Tail]) ->
	case cowboy_bstr:to_lower(Name) of
		Name -> regular_headers(Tail);
		_ -> {error, 'Header names must be lowercase. (RFC7540 8.1.2)'}
	end;
regular_headers([]) ->
	ok.

%% This function is necessary to properly handle duplicate headers
%% and the special-case cookie header.
headers_to_map([], Acc) ->
	Acc;
headers_to_map([{Name, Value}|Tail], Acc0) ->
	Acc = case Acc0 of
		%% The cookie header does not use proper HTTP header lists.
		#{Name := Value0} when Name =:= <<"cookie">> -> Acc0#{Name => << Value0/binary, "; ", Value/binary >>};
		#{Name := Value0} -> Acc0#{Name => << Value0/binary, ", ", Value/binary >>};
		_ -> Acc0#{Name => Value}
	end,
	headers_to_map(Tail, Acc).

stream_req_init(State, StreamID, IsFin, Headers, PseudoHeaders) ->
	case Headers of
		#{<<"content-length">> := BinLength} when BinLength =/= <<"0">>, IsFin =:= fin ->
			stream_malformed(State, StreamID,
				'HEADERS frame with the END_STREAM flag contains a non-zero content-length. (RFC7540 8.1.2.6)');
		_ when IsFin =:= fin ->
			stream_req_init(State, StreamID, IsFin, Headers, PseudoHeaders, 0);
		#{<<"content-length">> := <<"0">>} ->
			stream_req_init(State, StreamID, IsFin, Headers, PseudoHeaders, 0);
		#{<<"content-length">> := BinLength} ->
			try
				stream_req_init(State, StreamID, IsFin, Headers, PseudoHeaders,
					cow_http_hd:parse_content_length(BinLength))
			catch _:_ ->
				stream_malformed(State, StreamID,
					'The content-length header is invalid. (RFC7230 3.3.2)')
			end;
		_ ->
			stream_req_init(State, StreamID, IsFin, Headers, PseudoHeaders, undefined)
	end.

stream_req_init(State=#state{ref=Ref, peer=Peer, sock=Sock, cert=Cert},
		StreamID, IsFin, Headers, PseudoHeaders=#{method := Method, scheme := Scheme,
			authority := Authority, path := PathWithQs}, BodyLength) ->
	try cow_http_hd:parse_host(Authority) of
		{Host, Port} ->
			try cow_http:parse_fullpath(PathWithQs) of
				{<<>>, _} ->
					stream_malformed(State, StreamID,
						'The path component must not be empty. (RFC7540 8.1.2.3)');
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
						version => 'HTTP/2',
						headers => Headers,
						has_body => IsFin =:= nofin,
						body_length => BodyLength
					},
					%% We add the protocol information for extended CONNECTs.
					Req = case PseudoHeaders of
						#{protocol := Protocol} ->
							Req0#{protocol => Protocol};
						_ ->
							Req0
					end,
					stream_handler_init(State, StreamID, IsFin, idle, Req)
			catch _:_ ->
				stream_malformed(State, StreamID,
					'The :path pseudo-header is invalid. (RFC7540 8.1.2.3)')
			end
	catch _:_ ->
		stream_malformed(State, StreamID,
			'The :authority pseudo-header is invalid. (RFC7540 8.1.2.3)')
	end.

stream_closed(State=#state{socket=Socket, transport=Transport}, StreamID, _) ->
	Transport:send(Socket, cow_http2:rst_stream(StreamID, stream_closed)),
	State.

stream_malformed(State=#state{socket=Socket, transport=Transport}, StreamID, _) ->
	Transport:send(Socket, cow_http2:rst_stream(StreamID, protocol_error)),
	State.

stream_refused(State=#state{socket=Socket, transport=Transport}, StreamID, _) ->
	Transport:send(Socket, cow_http2:rst_stream(StreamID, refused_stream)),
	State.

stream_early_error(State0=#state{ref=Ref, opts=Opts, peer=Peer,
		local_settings=#{initial_window_size := RemoteWindow},
		remote_settings=#{initial_window_size := LocalWindow},
		streams=Streams}, StreamID, IsFin, StatusCode0,
		#{method := Method}, HumanReadable) ->
	%% We automatically terminate the stream but it is not an error
	%% per se (at least not in the first implementation).
	Reason = {stream_error, no_error, HumanReadable},
	%% The partial Req is minimal for now. We only have one case
	%% where it can be called (when a method is completely disabled).
	PartialReq = #{
		ref => Ref,
		peer => Peer,
		method => Method
	},
	Resp = {response, StatusCode0, RespHeaders0=#{<<"content-length">> => <<"0">>}, <<>>},
	%% We need a stream to talk to the send_* functions.
	Stream0 = #stream{id=StreamID, state=flush, method=Method,
		remote=IsFin, local=idle,
		local_window=LocalWindow, remote_window=RemoteWindow},
	try cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts) of
		{response, StatusCode, RespHeaders, RespBody} ->
			case send_response(State0, Stream0, StatusCode, RespHeaders, RespBody) of
				{State, #stream{local=fin}} ->
					State;
				{State, Stream} ->
					State#state{streams=[Stream|Streams]}
			end
	catch Class:Exception ->
		cowboy_stream:report_error(early_error,
			[StreamID, Reason, PartialReq, Resp, Opts],
			Class, Exception, erlang:get_stacktrace()),
		%% We still need to send an error response, so send what we initially
		%% wanted to send. It's better than nothing.
		send_headers(State0, Stream0, StatusCode0, RespHeaders0, fin)
	end.

stream_handler_init(State=#state{opts=Opts,
		local_settings=#{initial_window_size := RemoteWindow},
		remote_settings=#{initial_window_size := LocalWindow}},
		StreamID, RemoteIsFin, LocalIsFin,
		Req=#{method := Method, headers := Headers, body_length := BodyLength}) ->
	try cowboy_stream:init(StreamID, Req, Opts) of
		{Commands, StreamState} ->
			commands(State#state{client_streamid=StreamID},
				#stream{id=StreamID, state=StreamState,
					method=Method, remote=RemoteIsFin, local=LocalIsFin,
					local_window=LocalWindow, remote_window=RemoteWindow,
					remote_expected_size=BodyLength,
					te=maps:get(<<"te">>, Headers, undefined)},
				Commands)
	catch Class:Exception ->
		cowboy_stream:report_error(init,
			[StreamID, Req, Opts],
			Class, Exception, erlang:get_stacktrace()),
		stream_reset(State, StreamID, {internal_error, {Class, Exception},
			'Unhandled exception in cowboy_stream:init/3.'})
	end.

stream_decode_trailers(State=#state{decode_state=DecodeState0}, Stream, HeaderBlock) ->
	try cow_hpack:decode(HeaderBlock, DecodeState0) of
		{Headers, DecodeState} ->
			stream_trailers_is_body_size_valid(State#state{decode_state=DecodeState},
				Stream#stream{remote=fin}, Headers)
	catch _:_ ->
		terminate(State, {connection_error, compression_error,
			'Error while trying to decode HPACK-encoded header block. (RFC7540 4.3)'})
	end.

stream_trailers_is_body_size_valid(State, Stream=#stream{id=StreamID}, Headers) ->
	case is_body_size_valid(Stream) of
		true ->
			stream_reject_pseudo_headers_in_trailers(State, Stream, Headers);
		false ->
			stream_reset(after_commands(State, Stream), StreamID, {stream_error, protocol_error,
				'The total size of DATA frames is different than the content-length. (RFC7540 8.1.2.6)'})
	end.

stream_reject_pseudo_headers_in_trailers(State, Stream=#stream{id=StreamID}, Headers) ->
	case has_pseudo_header(Headers) of
		false ->
			%% @todo There's probably a number of regular headers forbidden too.
			%% @todo Propagate trailers.
			after_commands(State, Stream);
		true ->
			stream_reset(after_commands(State, Stream), StreamID, {stream_error, protocol_error,
				'Trailer header blocks must not contain pseudo-headers. (RFC7540 8.1.2.1)'})
	end.

has_pseudo_header([]) ->
	false;
has_pseudo_header([{<<":", _/bits>>, _}|_]) ->
	true;
has_pseudo_header([_|Tail]) ->
	has_pseudo_header(Tail).

%% @todo Don't send an RST_STREAM if one was already sent.
stream_reset(State=#state{socket=Socket, transport=Transport}, StreamID, StreamError) ->
	Reason = case StreamError of
		{internal_error, _, _} -> internal_error;
		{stream_error, Reason0, _} -> Reason0
	end,
	Transport:send(Socket, cow_http2:rst_stream(StreamID, Reason)),
	stream_terminate(stream_linger(State, StreamID), StreamID, StreamError).

%% We only keep up to 100 streams in this state. @todo Make it configurable?
stream_linger(State=#state{lingering_streams=Lingering0}, StreamID) ->
	Lingering = [StreamID|lists:sublist(Lingering0, 100 - 1)],
	State#state{lingering_streams=Lingering}.

%% We only keep up to 10 streams in this state. @todo Make it configurable?
stream_rst_linger(State=#state{rst_lingering_streams=Lingering0}, StreamID) ->
	Lingering = [StreamID|lists:sublist(Lingering0, 10 - 1)],
	State#state{rst_lingering_streams=Lingering}.

stream_terminate(State0=#state{streams=Streams0, children=Children0}, StreamID, Reason) ->
	case lists:keytake(StreamID, #stream.id, Streams0) of
		%% When the stream terminates normally (without sending RST_STREAM)
		%% and no response was sent, we need to send a proper response back to the client.
		{value, Stream=#stream{local=idle}, Streams} when Reason =:= normal ->
			State1 = #state{streams=Streams1} = info(State0, StreamID, {response, 204, #{}, <<>>}),
			State = maybe_skip_body(State1, Stream, Reason),
			#stream{state=StreamState} = lists:keyfind(StreamID, #stream.id, Streams1),
			stream_call_terminate(StreamID, Reason, StreamState),
			Children = cowboy_children:shutdown(Children0, StreamID),
			State#state{streams=Streams, children=Children};
		%% When a response was sent but not terminated, we need to close the stream.
		{value, Stream=#stream{local=nofin, local_buffer_size=0}, Streams}
				when Reason =:= normal ->
			State1 = #state{streams=Streams1} = info(State0, StreamID, {data, fin, <<>>}),
			State = maybe_skip_body(State1, Stream, Reason),
			#stream{state=StreamState} = lists:keyfind(StreamID, #stream.id, Streams1),
			stream_call_terminate(StreamID, Reason, StreamState),
			Children = cowboy_children:shutdown(Children0, StreamID),
			State#state{streams=Streams, children=Children};
		%% Unless there is still data in the buffer. We can however reset
		%% a few fields and set a special local state to avoid confusion.
		%%
		%% We do not reset the stream in this case (to skip the body)
		%% because we are still sending data via the buffer. We will
		%% reset the stream if necessary once the buffer is empty.
		{value, Stream=#stream{state=StreamState, local=nofin}, Streams} ->
			stream_call_terminate(StreamID, Reason, StreamState),
			Children = cowboy_children:shutdown(Children0, StreamID),
			State0#state{streams=[Stream#stream{state=flush, local=flush}|Streams],
				children=Children};
		%% Otherwise we sent or received an RST_STREAM and/or the stream is already closed.
		{value, Stream=#stream{state=StreamState}, Streams} ->
			State = maybe_skip_body(State0, Stream, Reason),
			stream_call_terminate(StreamID, Reason, StreamState),
			Children = cowboy_children:shutdown(Children0, StreamID),
			State#state{streams=Streams, children=Children};
		%% The stream doesn't exist. This can occur for various reasons.
		%% It can happen before the stream has been created, or because
		%% the cowboy_stream:init call failed, in which case doing nothing
		%% is correct.
		false ->
			State0
	end.

%% When the stream stops normally without reading the request
%% body fully we need to tell the client to stop sending it.
%% We do this by sending an RST_STREAM with reason NO_ERROR. (RFC7540 8.1.0)
maybe_skip_body(State=#state{socket=Socket, transport=Transport},
		#stream{id=StreamID, remote=nofin}, normal) ->
	Transport:send(Socket, cow_http2:rst_stream(StreamID, no_error)),
	stream_linger(State, StreamID);
maybe_skip_body(State, _, _) ->
	State.

stream_call_terminate(StreamID, Reason, StreamState) ->
	try
		cowboy_stream:terminate(StreamID, Reason, StreamState)
	catch Class:Exception ->
		cowboy_stream:report_error(terminate,
			[StreamID, Reason, StreamState],
			Class, Exception, erlang:get_stacktrace())
	end.

%% System callbacks.

-spec system_continue(_, _, {#state{}, binary()}) -> ok.
system_continue(_, _, {State, Buffer}) ->
	loop(State, Buffer).

-spec system_terminate(any(), _, _, {#state{}, binary()}) -> no_return().
system_terminate(Reason, _, _, {State, _}) ->
	%% @todo We should exit gracefully, if possible.
	terminate(State, Reason).

-spec system_code_change(Misc, _, _, _) -> {ok, Misc} when Misc::{#state{}, binary()}.
system_code_change(Misc, _, _, _) ->
	{ok, Misc}.
