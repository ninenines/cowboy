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

-export([init/6]).
-export([init/10]).
-export([init/12]).

-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-type opts() :: #{
	active_n => pos_integer(),
	compress_buffering => boolean(),
	compress_threshold => non_neg_integer(),
	connection_type => worker | supervisor,
	connection_window_margin_size => 0..16#7fffffff,
	connection_window_update_threshold => 0..16#7fffffff,
	enable_connect_protocol => boolean(),
	env => cowboy_middleware:env(),
	idle_timeout => timeout(),
	inactivity_timeout => timeout(),
	initial_connection_window_size => 65535..16#7fffffff,
	initial_stream_window_size => 0..16#7fffffff,
	linger_timeout => timeout(),
	logger => module(),
	max_concurrent_streams => non_neg_integer() | infinity,
	max_connection_buffer_size => non_neg_integer(),
	max_connection_window_size => 0..16#7fffffff,
	max_decode_table_size => non_neg_integer(),
	max_encode_table_size => non_neg_integer(),
	max_frame_size_received => 16384..16777215,
	max_frame_size_sent => 16384..16777215 | infinity,
	max_received_frame_rate => {pos_integer(), timeout()},
	max_reset_stream_rate => {pos_integer(), timeout()},
	max_stream_buffer_size => non_neg_integer(),
	max_stream_window_size => 0..16#7fffffff,
	metrics_callback => cowboy_metrics_h:metrics_callback(),
	metrics_req_filter => fun((cowboy_req:req()) -> map()),
	metrics_resp_headers_filter => fun((cowboy:http_headers()) -> cowboy:http_headers()),
	middlewares => [module()],
	preface_timeout => timeout(),
	proxy_header => boolean(),
	sendfile => boolean(),
	settings_timeout => timeout(),
	shutdown_timeout => timeout(),
	stream_handlers => [module()],
	stream_window_data_threshold => 0..16#7fffffff,
	stream_window_margin_size => 0..16#7fffffff,
	stream_window_update_threshold => 0..16#7fffffff,
	tracer_callback => cowboy_tracer_h:tracer_callback(),
	tracer_flags => [atom()],
	tracer_match_specs => cowboy_tracer_h:tracer_match_specs(),
	%% Open ended because configured stream handlers might add options.
	_ => _
}.
-export_type([opts/0]).

-record(stream, {
	%% Whether the stream is currently stopping.
	status = running :: running | stopping,

	%% Flow requested for this stream.
	flow = 0 :: non_neg_integer(),

	%% Stream state.
	state :: {module, any()}
}).

-record(state, {
	parent = undefined :: pid(),
	ref :: ranch:ref(),
	socket = undefined :: inet:socket(),
	transport :: module(),
	proxy_header :: undefined | ranch_proxy_header:proxy_info(),
	opts = #{} :: opts(),

	%% Timer for idle_timeout.
	timer = undefined :: undefined | reference(),

	%% Remote address and port for the connection.
	peer = undefined :: {inet:ip_address(), inet:port_number()},

	%% Local address and port for the connection.
	sock = undefined :: {inet:ip_address(), inet:port_number()},

	%% Client certificate (TLS only).
	cert :: undefined | binary(),

	%% HTTP/2 state machine.
	http2_status :: sequence | settings | upgrade | connected | closing,
	http2_machine :: cow_http2_machine:http2_machine(),

	%% HTTP/2 frame rate flood protection.
	frame_rate_num :: undefined | pos_integer(),
	frame_rate_time :: undefined | integer(),

	%% HTTP/2 reset stream flood protection.
	reset_rate_num :: undefined | pos_integer(),
	reset_rate_time :: undefined | integer(),

	%% Flow requested for all streams.
	flow = 0 :: non_neg_integer(),

	%% Currently active HTTP/2 streams. Streams may be initiated either
	%% by the client or by the server through PUSH_PROMISE frames.
	streams = #{} :: #{cow_http2:streamid() => #stream{}},

	%% Streams can spawn zero or more children which are then managed
	%% by this module if operating as a supervisor.
	children = cowboy_children:init() :: cowboy_children:children()
}).

-spec init(pid(), ranch:ref(), inet:socket(), module(),
	ranch_proxy_header:proxy_info() | undefined, cowboy:opts()) -> ok.
init(Parent, Ref, Socket, Transport, ProxyHeader, Opts) ->
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
			init(Parent, Ref, Socket, Transport, ProxyHeader, Opts, Peer, Sock, Cert, <<>>);
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

-spec init(pid(), ranch:ref(), inet:socket(), module(),
	ranch_proxy_header:proxy_info() | undefined, cowboy:opts(),
	{inet:ip_address(), inet:port_number()}, {inet:ip_address(), inet:port_number()},
	binary() | undefined, binary()) -> ok.
init(Parent, Ref, Socket, Transport, ProxyHeader, Opts, Peer, Sock, Cert, Buffer) ->
	{ok, Preface, HTTP2Machine} = cow_http2_machine:init(server, Opts),
	State = set_timeout(init_rate_limiting(#state{parent=Parent, ref=Ref, socket=Socket,
		transport=Transport, proxy_header=ProxyHeader,
		opts=Opts, peer=Peer, sock=Sock, cert=Cert,
		http2_status=sequence, http2_machine=HTTP2Machine})),
	Transport:send(Socket, Preface),
	setopts_active(State),
	case Buffer of
		<<>> -> loop(State, Buffer);
		_ -> parse(State, Buffer)
	end.

init_rate_limiting(State=#state{opts=Opts}) ->
	{FrameRateNum, FrameRatePeriod} = maps:get(max_received_frame_rate, Opts, {10000, 10000}),
	{ResetRateNum, ResetRatePeriod} = maps:get(max_reset_stream_rate, Opts, {10, 10000}),
	CurrentTime = erlang:monotonic_time(millisecond),
	State#state{
		frame_rate_num=FrameRateNum, frame_rate_time=add_period(CurrentTime, FrameRatePeriod),
		reset_rate_num=ResetRateNum, reset_rate_time=add_period(CurrentTime, ResetRatePeriod)
	}.

add_period(_, infinity) -> infinity;
add_period(Time, Period) -> Time + Period.

%% @todo Add an argument for the request body.
-spec init(pid(), ranch:ref(), inet:socket(), module(),
	ranch_proxy_header:proxy_info() | undefined, cowboy:opts(),
	{inet:ip_address(), inet:port_number()}, {inet:ip_address(), inet:port_number()},
	binary() | undefined, binary(), map() | undefined, cowboy_req:req()) -> ok.
init(Parent, Ref, Socket, Transport, ProxyHeader, Opts, Peer, Sock, Cert, Buffer,
		_Settings, Req=#{method := Method}) ->
	{ok, Preface, HTTP2Machine0} = cow_http2_machine:init(server, Opts),
	{ok, StreamID, HTTP2Machine}
		= cow_http2_machine:init_upgrade_stream(Method, HTTP2Machine0),
	State0 = #state{parent=Parent, ref=Ref, socket=Socket,
		transport=Transport, proxy_header=ProxyHeader,
		opts=Opts, peer=Peer, sock=Sock, cert=Cert,
		http2_status=upgrade, http2_machine=HTTP2Machine},
	State1 = headers_frame(State0#state{
		http2_machine=HTTP2Machine}, StreamID, Req),
	%% We assume that the upgrade will be applied. A stream handler
	%% must not prevent the normal operations of the server.
	State2 = info(State1, 1, {switch_protocol, #{
		<<"connection">> => <<"Upgrade">>,
		<<"upgrade">> => <<"h2c">>
	}, ?MODULE, undefined}), %% @todo undefined or #{}?
	State = set_timeout(init_rate_limiting(State2#state{http2_status=sequence})),
	Transport:send(Socket, Preface),
	setopts_active(State),
	case Buffer of
		<<>> -> loop(State, Buffer);
		_ -> parse(State, Buffer)
	end.

%% Because HTTP/2 has flow control and Cowboy has other rate limiting
%% mechanisms implemented, a very large active_n value should be fine,
%% as long as the stream handlers do their work in a timely manner.
setopts_active(#state{socket=Socket, transport=Transport, opts=Opts}) ->
	N = maps:get(active_n, Opts, 100),
	Transport:setopts(Socket, [{active, N}]).

loop(State=#state{parent=Parent, socket=Socket, transport=Transport,
		opts=Opts, timer=TimerRef, children=Children}, Buffer) ->
	Messages = Transport:messages(),
	InactivityTimeout = maps:get(inactivity_timeout, Opts, 300000),
	receive
		%% Socket messages.
		{OK, Socket, Data} when OK =:= element(1, Messages) ->
			parse(set_timeout(State), << Buffer/binary, Data/binary >>);
		{Closed, Socket} when Closed =:= element(2, Messages) ->
			terminate(State, {socket_error, closed, 'The socket has been closed.'});
		{Error, Socket, Reason} when Error =:= element(3, Messages) ->
			terminate(State, {socket_error, Reason, 'An error has occurred on the socket.'});
		{Passive, Socket} when Passive =:= element(4, Messages);
				%% Hardcoded for compatibility with Ranch 1.x.
				Passive =:= tcp_passive; Passive =:= ssl_passive ->
			setopts_active(State),
			loop(State, Buffer);
		%% System messages.
		{'EXIT', Parent, Reason} ->
			%% @todo Graceful shutdown here as well?
			terminate(State, {stop, {exit, Reason}, 'Parent process terminated.'});
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {State, Buffer});
		%% Timeouts.
		{timeout, TimerRef, idle_timeout} ->
			terminate(State, {stop, timeout,
				'Connection idle longer than configuration allows.'});
		{timeout, Ref, {shutdown, Pid}} ->
			cowboy_children:shutdown_timeout(Children, Ref, Pid),
			loop(State, Buffer);
		{timeout, TRef, {cow_http2_machine, Name}} ->
			loop(timeout(State, Name, TRef), Buffer);
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
			cowboy:log(warning, "Received stray message ~p.", [Msg], Opts),
			loop(State, Buffer)
	after InactivityTimeout ->
		terminate(State, {internal_error, timeout, 'No message or data received before timeout.'})
	end.

set_timeout(State=#state{opts=Opts, timer=TimerRef0}) ->
	ok = case TimerRef0 of
		undefined -> ok;
		_ -> erlang:cancel_timer(TimerRef0, [{async, true}, {info, false}])
	end,
	TimerRef = case maps:get(idle_timeout, Opts, 60000) of
		infinity -> undefined;
		Timeout -> erlang:start_timer(Timeout, self(), idle_timeout)
	end,
	State#state{timer=TimerRef}.

%% HTTP/2 protocol parsing.

parse(State=#state{http2_status=sequence}, Data) ->
	case cow_http2:parse_sequence(Data) of
		{ok, Rest} ->
			parse(State#state{http2_status=settings}, Rest);
		more ->
			loop(State, Data);
		Error = {connection_error, _, _} ->
			terminate(State, Error)
	end;
parse(State=#state{http2_status=Status, http2_machine=HTTP2Machine, streams=Streams}, Data) ->
	MaxFrameSize = cow_http2_machine:get_local_setting(max_frame_size, HTTP2Machine),
	case cow_http2:parse(Data, MaxFrameSize) of
		{ok, Frame, Rest} ->
			parse(frame_rate(State, Frame), Rest);
		{ignore, Rest} ->
			parse(frame_rate(State, ignore), Rest);
		{stream_error, StreamID, Reason, Human, Rest} ->
			parse(reset_stream(State, StreamID, {stream_error, Reason, Human}), Rest);
		Error = {connection_error, _, _} ->
			terminate(State, Error);
		%% Terminate the connection if we are closing and all streams have completed.
		more when Status =:= closing, Streams =:= #{} ->
			terminate(State, {stop, normal, 'The connection is going away.'});
		more ->
			loop(State, Data)
	end.

%% Frame rate flood protection.

frame_rate(State0=#state{opts=Opts, frame_rate_num=Num0, frame_rate_time=Time}, Frame) ->
	{Result, State} = case Num0 - 1 of
		0 ->
			CurrentTime = erlang:monotonic_time(millisecond),
			if
				CurrentTime < Time ->
					{error, State0};
				true ->
					%% When the option has a period of infinity we cannot reach this clause.
					{Num, Period} = maps:get(max_received_frame_rate, Opts, {1000, 10000}),
					{ok, State0#state{frame_rate_num=Num, frame_rate_time=CurrentTime + Period}}
			end;
		Num ->
			{ok, State0#state{frame_rate_num=Num}}
	end,
	case {Result, Frame} of
		{ok, ignore} -> ignored_frame(State);
		{ok, _} -> frame(State, Frame);
		{error, _} -> terminate(State, {connection_error, enhance_your_calm,
			'Frame rate larger than configuration allows. Flood? (CVE-2019-9512, CVE-2019-9515, CVE-2019-9518)'})
	end.

%% Frames received.

%% We do nothing when receiving a lingering DATA frame.
%% We already removed the stream flow from the connection
%% flow and are therefore already accounting for the window
%% being reduced by these frames.
frame(State=#state{http2_machine=HTTP2Machine0}, Frame) ->
	case cow_http2_machine:frame(Frame, HTTP2Machine0) of
		{ok, HTTP2Machine} ->
			maybe_ack(State#state{http2_machine=HTTP2Machine}, Frame);
		{ok, {data, StreamID, IsFin, Data}, HTTP2Machine} ->
			data_frame(State#state{http2_machine=HTTP2Machine}, StreamID, IsFin, Data);
		{ok, {headers, StreamID, IsFin, Headers, PseudoHeaders, BodyLen}, HTTP2Machine} ->
			headers_frame(State#state{http2_machine=HTTP2Machine},
				StreamID, IsFin, Headers, PseudoHeaders, BodyLen);
		{ok, {trailers, _StreamID, _Trailers}, HTTP2Machine} ->
			%% @todo Propagate trailers.
			State#state{http2_machine=HTTP2Machine};
		{ok, {rst_stream, StreamID, Reason}, HTTP2Machine} ->
			rst_stream_frame(State#state{http2_machine=HTTP2Machine}, StreamID, Reason);
		{ok, GoAway={goaway, _, _, _}, HTTP2Machine} ->
			goaway(State#state{http2_machine=HTTP2Machine}, GoAway);
		{send, SendData, HTTP2Machine} ->
			%% We may need to send an alarm for each of the streams sending data.
			lists:foldl(
				fun({StreamID, _, _}, S) -> maybe_send_data_alarm(S, HTTP2Machine0, StreamID) end,
				send_data(maybe_ack(State#state{http2_machine=HTTP2Machine}, Frame), SendData, []),
				SendData);
		{error, {stream_error, StreamID, Reason, Human}, HTTP2Machine} ->
			reset_stream(State#state{http2_machine=HTTP2Machine},
				StreamID, {stream_error, Reason, Human});
		{error, Error={connection_error, _, _}, HTTP2Machine} ->
			terminate(State#state{http2_machine=HTTP2Machine}, Error)
	end.

%% We use this opportunity to mark the HTTP/2 status as connected
%% if we were still waiting for a SETTINGS frame.
maybe_ack(State=#state{http2_status=settings}, Frame) ->
	maybe_ack(State#state{http2_status=connected}, Frame);
maybe_ack(State=#state{socket=Socket, transport=Transport}, Frame) ->
	case Frame of
		{settings, _} -> Transport:send(Socket, cow_http2:settings_ack());
		{ping, Opaque} -> Transport:send(Socket, cow_http2:ping_ack(Opaque));
		_ -> ok
	end,
	State.

data_frame(State0=#state{opts=Opts, flow=Flow, streams=Streams}, StreamID, IsFin, Data) ->
	case Streams of
		#{StreamID := Stream=#stream{status=running, flow=StreamFlow, state=StreamState0}} ->
			try cowboy_stream:data(StreamID, IsFin, Data, StreamState0) of
				{Commands, StreamState} ->
					%% Remove the amount of data received from the flow.
					%% We may receive more data than we requested. We ensure
					%% that the flow value doesn't go lower than 0.
					Size = byte_size(Data),
					State = update_window(State0#state{flow=max(0, Flow - Size),
						streams=Streams#{StreamID => Stream#stream{
							flow=max(0, StreamFlow - Size), state=StreamState}}},
						StreamID),
					commands(State, StreamID, Commands)
			catch Class:Exception:Stacktrace ->
				cowboy:log(cowboy_stream:make_error_log(data,
					[StreamID, IsFin, Data, StreamState0],
					Class, Exception, Stacktrace), Opts),
				reset_stream(State0, StreamID, {internal_error, {Class, Exception},
					'Unhandled exception in cowboy_stream:data/4.'})
			end;
		%% We ignore DATA frames for streams that are stopping.
		#{} ->
			State0
	end.

headers_frame(State, StreamID, IsFin, Headers,
		PseudoHeaders=#{method := <<"CONNECT">>}, _)
		when map_size(PseudoHeaders) =:= 2 ->
	early_error(State, StreamID, IsFin, Headers, PseudoHeaders, 501,
		'The CONNECT method is currently not implemented. (RFC7231 4.3.6)');
headers_frame(State, StreamID, IsFin, Headers,
		PseudoHeaders=#{method := <<"TRACE">>}, _) ->
	early_error(State, StreamID, IsFin, Headers, PseudoHeaders, 501,
		'The TRACE method is currently not implemented. (RFC7231 4.3.8)');
headers_frame(State, StreamID, IsFin, Headers, PseudoHeaders=#{authority := Authority}, BodyLen) ->
	headers_frame_parse_host(State, StreamID, IsFin, Headers, PseudoHeaders, BodyLen, Authority);
headers_frame(State, StreamID, IsFin, Headers, PseudoHeaders, BodyLen) ->
	case lists:keyfind(<<"host">>, 1, Headers) of
		{_, Authority} ->
			headers_frame_parse_host(State, StreamID, IsFin, Headers, PseudoHeaders, BodyLen, Authority);
		_ ->
			reset_stream(State, StreamID, {stream_error, protocol_error,
				'Requests translated from HTTP/1.1 must include a host header. (RFC7540 8.1.2.3, RFC7230 5.4)'})
	end.

headers_frame_parse_host(State=#state{ref=Ref, peer=Peer, sock=Sock, cert=Cert, proxy_header=ProxyHeader},
		StreamID, IsFin, Headers, PseudoHeaders=#{method := Method, scheme := Scheme, path := PathWithQs},
		BodyLen, Authority) ->
	try cow_http_hd:parse_host(Authority) of
		{Host, Port0} ->
			Port = ensure_port(Scheme, Port0),
			try cow_http:parse_fullpath(PathWithQs) of
				{<<>>, _} ->
					reset_stream(State, StreamID, {stream_error, protocol_error,
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
						version => 'HTTP/2',
						headers => headers_to_map(Headers, #{}),
						has_body => IsFin =:= nofin,
						body_length => BodyLen
					},
					%% We add the PROXY header information if any.
					Req1 = case ProxyHeader of
						undefined -> Req0;
						_ -> Req0#{proxy_header => ProxyHeader}
					end,
					%% We add the protocol information for extended CONNECTs.
					Req = case PseudoHeaders of
						#{protocol := Protocol} -> Req1#{protocol => Protocol};
						_ -> Req1
					end,
					headers_frame(State, StreamID, Req)
			catch _:_ ->
				reset_stream(State, StreamID, {stream_error, protocol_error,
					'The :path pseudo-header is invalid. (RFC7540 8.1.2.3)'})
			end
	catch _:_ ->
		reset_stream(State, StreamID, {stream_error, protocol_error,
			'The :authority pseudo-header is invalid. (RFC7540 8.1.2.3)'})
	end.

ensure_port(<<"http">>, undefined) -> 80;
ensure_port(<<"https">>, undefined) -> 443;
ensure_port(_, Port) -> Port.

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

headers_frame(State=#state{opts=Opts, streams=Streams}, StreamID, Req) ->
	try cowboy_stream:init(StreamID, Req, Opts) of
		{Commands, StreamState} ->
			commands(State#state{
				streams=Streams#{StreamID => #stream{state=StreamState}}},
				StreamID, Commands)
	catch Class:Exception:Stacktrace ->
		cowboy:log(cowboy_stream:make_error_log(init,
			[StreamID, Req, Opts],
			Class, Exception, Stacktrace), Opts),
		reset_stream(State, StreamID, {internal_error, {Class, Exception},
			'Unhandled exception in cowboy_stream:init/3.'})
	end.

early_error(State0=#state{ref=Ref, opts=Opts, peer=Peer},
		StreamID, _IsFin, Headers, #{method := Method},
		StatusCode0, HumanReadable) ->
	%% We automatically terminate the stream but it is not an error
	%% per se (at least not in the first implementation).
	Reason = {stream_error, no_error, HumanReadable},
	%% The partial Req is minimal for now. We only have one case
	%% where it can be called (when a method is completely disabled).
	%% @todo Fill in the other elements.
	PartialReq = #{
		ref => Ref,
		peer => Peer,
		method => Method,
		headers => headers_to_map(Headers, #{})
	},
	Resp = {response, StatusCode0, RespHeaders0=#{<<"content-length">> => <<"0">>}, <<>>},
	try cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts) of
		{response, StatusCode, RespHeaders, RespBody} ->
			send_response(State0, StreamID, StatusCode, RespHeaders, RespBody)
	catch Class:Exception:Stacktrace ->
		cowboy:log(cowboy_stream:make_error_log(early_error,
			[StreamID, Reason, PartialReq, Resp, Opts],
			Class, Exception, Stacktrace), Opts),
		%% We still need to send an error response, so send what we initially
		%% wanted to send. It's better than nothing.
		send_headers(State0, StreamID, fin, StatusCode0, RespHeaders0)
	end.

rst_stream_frame(State=#state{streams=Streams0, children=Children0}, StreamID, Reason) ->
	case maps:take(StreamID, Streams0) of
		{#stream{state=StreamState}, Streams} ->
			terminate_stream_handler(State, StreamID, Reason, StreamState),
			Children = cowboy_children:shutdown(Children0, StreamID),
			State#state{streams=Streams, children=Children};
		error ->
			State
	end.

ignored_frame(State=#state{http2_machine=HTTP2Machine0}) ->
	case cow_http2_machine:ignored_frame(HTTP2Machine0) of
		{ok, HTTP2Machine} ->
			State#state{http2_machine=HTTP2Machine};
		{error, Error={connection_error, _, _}, HTTP2Machine} ->
			terminate(State#state{http2_machine=HTTP2Machine}, Error)
	end.

%% HTTP/2 timeouts.

timeout(State=#state{http2_machine=HTTP2Machine0}, Name, TRef) ->
	case cow_http2_machine:timeout(Name, TRef, HTTP2Machine0) of
		{ok, HTTP2Machine} ->
			State#state{http2_machine=HTTP2Machine};
		{error, Error={connection_error, _, _}, HTTP2Machine} ->
			terminate(State#state{http2_machine=HTTP2Machine}, Error)
	end.

%% Erlang messages.

down(State=#state{opts=Opts, children=Children0}, Pid, Msg) ->
	case cowboy_children:down(Children0, Pid) of
		%% The stream was terminated already.
		{ok, undefined, Children} ->
			State#state{children=Children};
		%% The stream is still running.
		{ok, StreamID, Children} ->
			info(State#state{children=Children}, StreamID, Msg);
		%% The process was unknown.
		error ->
			cowboy:log(warning, "Received EXIT signal ~p for unknown process ~p.~n",
				[Msg, Pid], Opts),
			State
	end.

info(State=#state{opts=Opts, http2_machine=HTTP2Machine, streams=Streams}, StreamID, Msg) ->
	case Streams of
		#{StreamID := Stream=#stream{state=StreamState0}} ->
			try cowboy_stream:info(StreamID, Msg, StreamState0) of
				{Commands, StreamState} ->
					commands(State#state{streams=Streams#{StreamID => Stream#stream{state=StreamState}}},
						StreamID, Commands)
			catch Class:Exception:Stacktrace ->
				cowboy:log(cowboy_stream:make_error_log(info,
					[StreamID, Msg, StreamState0],
					Class, Exception, Stacktrace), Opts),
				reset_stream(State, StreamID, {internal_error, {Class, Exception},
					'Unhandled exception in cowboy_stream:info/3.'})
			end;
		_ ->
			case cow_http2_machine:is_lingering_stream(StreamID, HTTP2Machine) of
				true ->
					ok;
				false ->
					cowboy:log(warning, "Received message ~p for unknown stream ~p.",
						[Msg, StreamID], Opts)
			end,
			State
	end.

%% Stream handler commands.
%%
%% @todo Kill the stream if it tries to send a response, headers,
%% data or push promise when the stream is closed or half-closed.

commands(State, _, []) ->
	State;
%% Error responses are sent only if a response wasn't sent already.
commands(State=#state{http2_machine=HTTP2Machine}, StreamID,
		[{error_response, StatusCode, Headers, Body}|Tail]) ->
	case cow_http2_machine:get_stream_local_state(StreamID, HTTP2Machine) of
		{ok, idle, _} ->
			commands(State, StreamID, [{response, StatusCode, Headers, Body}|Tail]);
		_ ->
			commands(State, StreamID, Tail)
	end;
%% Send an informational response.
commands(State0, StreamID, [{inform, StatusCode, Headers}|Tail]) ->
	State = send_headers(State0, StreamID, idle, StatusCode, Headers),
	commands(State, StreamID, Tail);
%% Send response headers.
commands(State0, StreamID, [{response, StatusCode, Headers, Body}|Tail]) ->
	State = send_response(State0, StreamID, StatusCode, Headers, Body),
	commands(State, StreamID, Tail);
%% Send response headers.
commands(State0, StreamID, [{headers, StatusCode, Headers}|Tail]) ->
	State = send_headers(State0, StreamID, nofin, StatusCode, Headers),
	commands(State, StreamID, Tail);
%% Send a response body chunk.
commands(State0, StreamID, [{data, IsFin, Data}|Tail]) ->
	State = maybe_send_data(State0, StreamID, IsFin, Data, []),
	commands(State, StreamID, Tail);
%% Send trailers.
commands(State0, StreamID, [{trailers, Trailers}|Tail]) ->
	State = maybe_send_data(State0, StreamID, fin, {trailers, maps:to_list(Trailers)}, []),
	commands(State, StreamID, Tail);
%% Send a push promise.
%%
%% @todo Responses sent as a result of a push_promise request
%% must not send push_promise frames themselves.
%%
%% @todo We should not send push_promise frames when we are
%% in the closing http2_status.
commands(State0=#state{socket=Socket, transport=Transport, http2_machine=HTTP2Machine0},
		StreamID, [{push, Method, Scheme, Host, Port, Path, Qs, Headers0}|Tail]) ->
	Authority = case {Scheme, Port} of
		{<<"http">>, 80} -> Host;
		{<<"https">>, 443} -> Host;
		_ -> iolist_to_binary([Host, $:, integer_to_binary(Port)])
	end,
	PathWithQs = iolist_to_binary(case Qs of
		<<>> -> Path;
		_ -> [Path, $?, Qs]
	end),
	PseudoHeaders = #{
		method => Method,
		scheme => Scheme,
		authority => Authority,
		path => PathWithQs
	},
	%% We need to make sure the header value is binary before we can
	%% create the Req object, as it expects them to be flat.
	Headers = maps:to_list(maps:map(fun(_, V) -> iolist_to_binary(V) end, Headers0)),
	State = case cow_http2_machine:prepare_push_promise(StreamID, HTTP2Machine0,
			PseudoHeaders, Headers) of
		{ok, PromisedStreamID, HeaderBlock, HTTP2Machine} ->
			Transport:send(Socket, cow_http2:push_promise(
				StreamID, PromisedStreamID, HeaderBlock)),
			headers_frame(State0#state{http2_machine=HTTP2Machine},
				PromisedStreamID, fin, Headers, PseudoHeaders, 0);
		{error, no_push} ->
			State0
	end,
	commands(State, StreamID, Tail);
%% Read the request body.
commands(State0=#state{flow=Flow, streams=Streams}, StreamID, [{flow, Size}|Tail]) ->
	#{StreamID := Stream=#stream{flow=StreamFlow}} = Streams,
	State = update_window(State0#state{flow=Flow + Size,
		streams=Streams#{StreamID => Stream#stream{flow=StreamFlow + Size}}},
		StreamID),
	commands(State, StreamID, Tail);
%% Supervise a child process.
commands(State=#state{children=Children}, StreamID, [{spawn, Pid, Shutdown}|Tail]) ->
	 commands(State#state{children=cowboy_children:up(Children, Pid, StreamID, Shutdown)},
		StreamID, Tail);
%% Error handling.
commands(State, StreamID, [Error = {internal_error, _, _}|_Tail]) ->
	%% @todo Do we want to run the commands after an internal_error?
	%% @todo Do we even allow commands after?
	%% @todo Only reset when the stream still exists.
	reset_stream(State, StreamID, Error);
%% Upgrade to HTTP/2. This is triggered by cowboy_http2 itself.
commands(State=#state{socket=Socket, transport=Transport, http2_status=upgrade},
		StreamID, [{switch_protocol, Headers, ?MODULE, _}|Tail]) ->
	%% @todo This 101 response needs to be passed through stream handlers.
	Transport:send(Socket, cow_http:response(101, 'HTTP/1.1', maps:to_list(Headers))),
	commands(State, StreamID, Tail);
%% Use a different protocol within the stream (CONNECT :protocol).
%% @todo Make sure we error out when the feature is disabled.
commands(State0, StreamID, [{switch_protocol, Headers, _Mod, _ModState}|Tail]) ->
	State = info(State0, StreamID, {headers, 200, Headers}),
	commands(State, StreamID, Tail);
%% Set options dynamically.
commands(State, StreamID, [{set_options, _Opts}|Tail]) ->
	commands(State, StreamID, Tail);
commands(State, StreamID, [stop|_Tail]) ->
	%% @todo Do we want to run the commands after a stop?
	%% @todo Do we even allow commands after?
	stop_stream(State, StreamID);
%% Log event.
commands(State=#state{opts=Opts}, StreamID, [Log={log, _, _, _}|Tail]) ->
	cowboy:log(Log, Opts),
	commands(State, StreamID, Tail).

%% Tentatively update the window after the flow was updated.

update_window(State=#state{socket=Socket, transport=Transport,
		http2_machine=HTTP2Machine0, flow=Flow, streams=Streams}, StreamID) ->
	#{StreamID := #stream{flow=StreamFlow}} = Streams,
	{Data1, HTTP2Machine2} = case cow_http2_machine:ensure_window(Flow, HTTP2Machine0) of
		ok -> {<<>>, HTTP2Machine0};
		{ok, Increment1, HTTP2Machine1} -> {cow_http2:window_update(Increment1), HTTP2Machine1}
	end,
	{Data2, HTTP2Machine} = case cow_http2_machine:ensure_window(StreamID, StreamFlow, HTTP2Machine2) of
		ok -> {<<>>, HTTP2Machine2};
		{ok, Increment2, HTTP2Machine3} -> {cow_http2:window_update(StreamID, Increment2), HTTP2Machine3}
	end,
	case {Data1, Data2} of
		{<<>>, <<>>} -> ok;
		_ -> Transport:send(Socket, [Data1, Data2])
	end,
	State#state{http2_machine=HTTP2Machine}.

%% Send the response, trailers or data.

send_response(State0=#state{http2_machine=HTTP2Machine0}, StreamID, StatusCode, Headers, Body) ->
	Size = case Body of
		{sendfile, _, Bytes, _} -> Bytes;
		_ -> iolist_size(Body)
	end,
	case Size of
		0 ->
			State = send_headers(State0, StreamID, fin, StatusCode, Headers),
			maybe_terminate_stream(State, StreamID, fin);
		_ ->
			%% @todo Add a test for HEAD to make sure we don't send the body when
			%% returning {response...} from a stream handler (or {headers...} then {data...}).
			{ok, _IsFin, HeaderBlock, HTTP2Machine}
				= cow_http2_machine:prepare_headers(StreamID, HTTP2Machine0, nofin,
					#{status => cow_http:status_to_integer(StatusCode)},
					headers_to_list(Headers)),
			maybe_send_data(State0#state{http2_machine=HTTP2Machine}, StreamID, fin, Body,
				[cow_http2:headers(StreamID, nofin, HeaderBlock)])
	end.

send_headers(State=#state{socket=Socket, transport=Transport,
		http2_machine=HTTP2Machine0}, StreamID, IsFin0, StatusCode, Headers) ->
	{ok, IsFin, HeaderBlock, HTTP2Machine}
		= cow_http2_machine:prepare_headers(StreamID, HTTP2Machine0, IsFin0,
			#{status => cow_http:status_to_integer(StatusCode)},
			headers_to_list(Headers)),
	Transport:send(Socket, cow_http2:headers(StreamID, IsFin, HeaderBlock)),
	State#state{http2_machine=HTTP2Machine}.

%% The set-cookie header is special; we can only send one cookie per header.
headers_to_list(Headers0=#{<<"set-cookie">> := SetCookies}) ->
	Headers = maps:to_list(maps:remove(<<"set-cookie">>, Headers0)),
	Headers ++ [{<<"set-cookie">>, Value} || Value <- SetCookies];
headers_to_list(Headers) ->
	maps:to_list(Headers).

maybe_send_data(State0=#state{socket=Socket, transport=Transport,
		http2_machine=HTTP2Machine0}, StreamID, IsFin, Data0, Prefix) ->
	Data = case is_tuple(Data0) of
		false -> {data, Data0};
		true -> Data0
	end,
	case cow_http2_machine:send_or_queue_data(StreamID, HTTP2Machine0, IsFin, Data) of
		{ok, HTTP2Machine} ->
			%% If we have prefix data (like a HEADERS frame) we need to send it
			%% even if we do not send any DATA frames.
			case Prefix of
				[] -> ok;
				_ -> Transport:send(Socket, Prefix)
			end,
			maybe_send_data_alarm(State0#state{http2_machine=HTTP2Machine}, HTTP2Machine0, StreamID);
		{send, SendData, HTTP2Machine} ->
			State = #state{http2_status=Status, streams=Streams}
				= send_data(State0#state{http2_machine=HTTP2Machine}, SendData, Prefix),
			%% Terminate the connection if we are closing and all streams have completed.
			if
				Status =:= closing, Streams =:= #{} ->
					terminate(State, {stop, normal, 'The connection is going away.'});
				true ->
					maybe_send_data_alarm(State, HTTP2Machine0, StreamID)
			end
	end.

send_data(State0=#state{socket=Socket, transport=Transport, opts=Opts}, SendData, Prefix) ->
	{Acc, State} = prepare_data(State0, SendData, [], Prefix),
	_ = [case Data of
		{sendfile, Offset, Bytes, Path} ->
			%% When sendfile is disabled we explicitly use the fallback.
			_ = case maps:get(sendfile, Opts, true) of
				true -> Transport:sendfile(Socket, Path, Offset, Bytes);
				false -> ranch_transport:sendfile(Transport, Socket, Path, Offset, Bytes, [])
			end;
		_ ->
			Transport:send(Socket, Data)
	end || Data <- Acc],
	State.

prepare_data(State, [], Acc, []) ->
	{lists:reverse(Acc), State};
prepare_data(State, [], Acc, Buffer) ->
	{lists:reverse([lists:reverse(Buffer)|Acc]), State};
prepare_data(State0, [{StreamID, IsFin, SendData}|Tail], Acc0, Buffer0) ->
	{Acc, Buffer, State} = prepare_data(State0, StreamID, IsFin, SendData, Acc0, Buffer0),
	prepare_data(State, Tail, Acc, Buffer).

prepare_data(State0, StreamID, IsFin, [], Acc, Buffer) ->
	State = maybe_terminate_stream(State0, StreamID, IsFin),
	{Acc, Buffer, State};
prepare_data(State0, StreamID, IsFin, [FrameData|Tail], Acc, Buffer) ->
	FrameIsFin = case Tail of
		[] -> IsFin;
		_ -> nofin
	end,
	case prepare_data_frame(State0, StreamID, FrameIsFin, FrameData) of
		{{MoreData, Sendfile}, State} when is_tuple(Sendfile) ->
			case Buffer of
				[] ->
					prepare_data(State, StreamID, IsFin, Tail,
						[Sendfile, MoreData|Acc], []);
				_ ->
					prepare_data(State, StreamID, IsFin, Tail,
						[Sendfile, lists:reverse([MoreData|Buffer])|Acc], [])
			end;
		{MoreData, State} ->
			prepare_data(State, StreamID, IsFin, Tail,
				Acc, [MoreData|Buffer])
	end.

prepare_data_frame(State, StreamID, IsFin, {data, Data}) ->
	{cow_http2:data(StreamID, IsFin, Data),
		State};
prepare_data_frame(State, StreamID, IsFin, Sendfile={sendfile, _, Bytes, _}) ->
	{{cow_http2:data_header(StreamID, IsFin, Bytes), Sendfile},
		State};
%% The stream is terminated in cow_http2_machine:prepare_trailers.
prepare_data_frame(State=#state{http2_machine=HTTP2Machine0},
		StreamID, nofin, {trailers, Trailers}) ->
	{ok, HeaderBlock, HTTP2Machine}
		= cow_http2_machine:prepare_trailers(StreamID, HTTP2Machine0, Trailers),
	{cow_http2:headers(StreamID, fin, HeaderBlock),
		State#state{http2_machine=HTTP2Machine}}.

%% After we have sent or queued data we may need to set or clear an alarm.
%% We do this by comparing the HTTP2Machine buffer state before/after for
%% the relevant streams.
maybe_send_data_alarm(State=#state{opts=Opts, http2_machine=HTTP2Machine}, HTTP2Machine0, StreamID) ->
	ConnBufferSizeBefore = cow_http2_machine:get_connection_local_buffer_size(HTTP2Machine0),
	ConnBufferSizeAfter = cow_http2_machine:get_connection_local_buffer_size(HTTP2Machine),
	{ok, StreamBufferSizeBefore} = cow_http2_machine:get_stream_local_buffer_size(StreamID, HTTP2Machine0),
	%% When the stream ends up closed after it finished sending data,
	%% we do not want to trigger an alarm. We act as if the buffer
	%% size did not change.
	StreamBufferSizeAfter = case cow_http2_machine:get_stream_local_buffer_size(StreamID, HTTP2Machine) of
		{ok, BSA} -> BSA;
		{error, closed} -> StreamBufferSizeBefore
	end,
	MaxConnBufferSize = maps:get(max_connection_buffer_size, Opts, 16000000),
	MaxStreamBufferSize = maps:get(max_stream_buffer_size, Opts, 8000000),
	%% I do not want to document these internal events yet. I am not yet
	%% convinced it should be {alarm, Name, on|off} and not {internal_event, E}
	%% or something else entirely. Though alarms are probably right.
	if
		ConnBufferSizeBefore >= MaxConnBufferSize, ConnBufferSizeAfter < MaxConnBufferSize ->
			connection_alarm(State, connection_buffer_full, off);
		ConnBufferSizeBefore < MaxConnBufferSize, ConnBufferSizeAfter >= MaxConnBufferSize ->
			connection_alarm(State, connection_buffer_full, on);
		StreamBufferSizeBefore >= MaxStreamBufferSize, StreamBufferSizeAfter < MaxStreamBufferSize ->
			stream_alarm(State, StreamID, stream_buffer_full, off);
		StreamBufferSizeBefore < MaxStreamBufferSize, StreamBufferSizeAfter >= MaxStreamBufferSize ->
			stream_alarm(State, StreamID, stream_buffer_full, on);
		true ->
			State
	end.

connection_alarm(State0=#state{streams=Streams}, Name, Value) ->
	lists:foldl(fun(StreamID, State) ->
		stream_alarm(State, StreamID, Name, Value)
	end, State0, maps:keys(Streams)).

stream_alarm(State, StreamID, Name, Value) ->
	info(State, StreamID, {alarm, Name, Value}).

%% Terminate a stream or the connection.

%% We may have to cancel streams even if we receive multiple
%% GOAWAY frames as the LastStreamID value may be lower than
%% the one previously received.
goaway(State0=#state{socket=Socket, transport=Transport, http2_machine=HTTP2Machine,
		http2_status=Status, streams=Streams0}, {goaway, LastStreamID, Reason, _})
		when Status =:= connected; Status =:= closing ->
	Streams = goaway_streams(State0, maps:to_list(Streams0), LastStreamID,
		{stop, {goaway, Reason}, 'The connection is going away.'}, []),
	State = State0#state{streams=maps:from_list(Streams)},
	case Status of
		connected ->
			Transport:send(Socket, cow_http2:goaway(
				cow_http2_machine:get_last_streamid(HTTP2Machine),
				no_error, <<>>)),
			State#state{http2_status=closing};
		_ ->
			State
	end;
%% We terminate the connection immediately if it hasn't fully been initialized.
goaway(State, {goaway, _, Reason, _}) ->
	terminate(State, {stop, {goaway, Reason}, 'The connection is going away.'}).

%% Cancel client-initiated streams that are above LastStreamID.
goaway_streams(_, [], _, _, Acc) ->
	Acc;
goaway_streams(State, [{StreamID, #stream{state=StreamState}}|Tail], LastStreamID, Reason, Acc)
		when StreamID > LastStreamID, (StreamID rem 2) =:= 0 ->
	terminate_stream_handler(State, StreamID, Reason, StreamState),
	goaway_streams(State, Tail, LastStreamID, Reason, Acc);
goaway_streams(State, [Stream|Tail], LastStreamID, Reason, Acc) ->
	goaway_streams(State, Tail, LastStreamID, Reason, [Stream|Acc]).

-spec terminate(#state{}, _) -> no_return().
terminate(undefined, Reason) ->
	exit({shutdown, Reason});
terminate(State=#state{socket=Socket, transport=Transport, http2_status=Status,
		http2_machine=HTTP2Machine, streams=Streams, children=Children}, Reason)
		when Status =:= connected; Status =:= closing ->
	%% @todo We might want to optionally send the Reason value
	%% as debug data in the GOAWAY frame here. Perhaps more.
	case Status of
		connected ->
			Transport:send(Socket, cow_http2:goaway(
				cow_http2_machine:get_last_streamid(HTTP2Machine),
				terminate_reason(Reason), <<>>));
		%% We already sent the GOAWAY frame.
		closing ->
			ok
	end,
	terminate_all_streams(State, maps:to_list(Streams), Reason),
	cowboy_children:terminate(Children),
	terminate_linger(State),
	exit({shutdown, Reason});
terminate(#state{socket=Socket, transport=Transport}, Reason) ->
	Transport:close(Socket),
	exit({shutdown, Reason}).

terminate_reason({connection_error, Reason, _}) -> Reason;
terminate_reason({stop, _, _}) -> no_error;
terminate_reason({socket_error, _, _}) -> internal_error;
terminate_reason({internal_error, _, _}) -> internal_error.

terminate_all_streams(_, [], _) ->
	ok;
terminate_all_streams(State, [{StreamID, #stream{state=StreamState}}|Tail], Reason) ->
	terminate_stream_handler(State, StreamID, Reason, StreamState),
	terminate_all_streams(State, Tail, Reason).

%% This code is copied from cowboy_http.
terminate_linger(State=#state{socket=Socket, transport=Transport, opts=Opts}) ->
	case Transport:shutdown(Socket, write) of
		ok ->
			case maps:get(linger_timeout, Opts, 1000) of
				0 ->
					ok;
				infinity ->
					terminate_linger_before_loop(State, undefined, Transport:messages());
				Timeout ->
					TimerRef = erlang:start_timer(Timeout, self(), linger_timeout),
					terminate_linger_before_loop(State, TimerRef, Transport:messages())
			end;
		{error, _} ->
			ok
	end.

terminate_linger_before_loop(State, TimerRef, Messages) ->
	%% We may already be in active mode when we do this
	%% but it's OK because we are shutting down anyway.
	case setopts_active(State) of
		ok ->
			terminate_linger_loop(State, TimerRef, Messages);
		{error, _} ->
			ok
	end.

terminate_linger_loop(State=#state{socket=Socket}, TimerRef, Messages) ->
	receive
		{OK, Socket, _} when OK =:= element(1, Messages) ->
			terminate_linger_loop(State, TimerRef, Messages);
		{Closed, Socket} when Closed =:= element(2, Messages) ->
			ok;
		{Error, Socket, _} when Error =:= element(3, Messages) ->
			ok;
		{Passive, Socket} when Passive =:= tcp_passive; Passive =:= ssl_passive ->
			terminate_linger_before_loop(State, TimerRef, Messages);
		{timeout, TimerRef, linger_timeout} ->
			ok;
		_ ->
			terminate_linger_loop(State, TimerRef, Messages)
	end.

%% @todo Don't send an RST_STREAM if one was already sent.
reset_stream(State0=#state{socket=Socket, transport=Transport,
		http2_machine=HTTP2Machine0}, StreamID, Error) ->
	Reason = case Error of
		{internal_error, _, _} -> internal_error;
		{stream_error, Reason0, _} -> Reason0
	end,
	Transport:send(Socket, cow_http2:rst_stream(StreamID, Reason)),
	State1 = case cow_http2_machine:reset_stream(StreamID, HTTP2Machine0) of
		{ok, HTTP2Machine} ->
			terminate_stream(State0#state{http2_machine=HTTP2Machine}, StreamID, Error);
		{error, not_found} ->
			terminate_stream(State0, StreamID, Error)
	end,
	case reset_rate(State1) of
		{ok, State} ->
			State;
		error ->
			terminate(State1, {connection_error, enhance_your_calm,
				'Stream reset rate larger than configuration allows. Flood? (CVE-2019-9514)'})
	end.

reset_rate(State0=#state{opts=Opts, reset_rate_num=Num0, reset_rate_time=Time}) ->
	case Num0 - 1 of
		0 ->
			CurrentTime = erlang:monotonic_time(millisecond),
			if
				CurrentTime < Time ->
					error;
				true ->
					%% When the option has a period of infinity we cannot reach this clause.
					{Num, Period} = maps:get(max_reset_stream_rate, Opts, {10, 10000}),
					{ok, State0#state{reset_rate_num=Num, reset_rate_time=CurrentTime + Period}}
			end;
		Num ->
			{ok, State0#state{reset_rate_num=Num}}
	end.

stop_stream(State=#state{http2_machine=HTTP2Machine}, StreamID) ->
	case cow_http2_machine:get_stream_local_state(StreamID, HTTP2Machine) of
		%% When the stream terminates normally (without sending RST_STREAM)
		%% and no response was sent, we need to send a proper response back to the client.
		%% We delay the termination of the stream until the response is fully sent.
		{ok, idle, _} ->
			info(stopping(State, StreamID), StreamID, {response, 204, #{}, <<>>});
		%% When a response was sent but not terminated, we need to close the stream.
		%% We delay the termination of the stream until the response is fully sent.
		{ok, nofin, fin} ->
			stopping(State, StreamID);
		%% We only send a final DATA frame if there isn't one queued yet.
		{ok, nofin, _} ->
			info(stopping(State, StreamID), StreamID, {data, fin, <<>>});
		%% When a response was sent fully we can terminate the stream,
		%% regardless of the stream being in half-closed or closed state.
		_ ->
			terminate_stream(State, StreamID)
	end.

stopping(State=#state{streams=Streams}, StreamID) ->
	#{StreamID := Stream} = Streams,
	State#state{streams=Streams#{StreamID => Stream#stream{status=stopping}}}.

%% If we finished sending data and the stream is stopping, terminate it.
maybe_terminate_stream(State=#state{streams=Streams}, StreamID, fin) ->
	case Streams of
		#{StreamID := #stream{status=stopping}} ->
			terminate_stream(State, StreamID);
		_ ->
			State
	end;
maybe_terminate_stream(State, _, _) ->
	State.

%% When the stream stops normally without reading the request
%% body fully we need to tell the client to stop sending it.
%% We do this by sending an RST_STREAM with reason NO_ERROR. (RFC7540 8.1.0)
terminate_stream(State0=#state{socket=Socket, transport=Transport,
		http2_machine=HTTP2Machine0}, StreamID) ->
	State = case cow_http2_machine:get_stream_local_state(StreamID, HTTP2Machine0) of
		{ok, fin, _} ->
			Transport:send(Socket, cow_http2:rst_stream(StreamID, no_error)),
			{ok, HTTP2Machine} = cow_http2_machine:reset_stream(StreamID, HTTP2Machine0),
			State0#state{http2_machine=HTTP2Machine};
		{error, closed} ->
			State0
	end,
	terminate_stream(State, StreamID, normal).

%% We remove the stream flow from the connection flow. Any further
%% data received for this stream is therefore fully contained within
%% the extra window we allocated for this stream.
terminate_stream(State=#state{flow=Flow, streams=Streams0, children=Children0}, StreamID, Reason) ->
	case maps:take(StreamID, Streams0) of
		{#stream{flow=StreamFlow, state=StreamState}, Streams} ->
			terminate_stream_handler(State, StreamID, Reason, StreamState),
			Children = cowboy_children:shutdown(Children0, StreamID),
			State#state{flow=Flow - StreamFlow, streams=Streams, children=Children};
		error ->
			State
	end.

terminate_stream_handler(#state{opts=Opts}, StreamID, Reason, StreamState) ->
	try
		cowboy_stream:terminate(StreamID, Reason, StreamState)
	catch Class:Exception:Stacktrace ->
		cowboy:log(cowboy_stream:make_error_log(terminate,
			[StreamID, Reason, StreamState],
			Class, Exception, Stacktrace), Opts)
	end.

%% System callbacks.

-spec system_continue(_, _, {#state{}, binary()}) -> ok.
system_continue(_, _, {State, Buffer}) ->
	loop(State, Buffer).

-spec system_terminate(any(), _, _, {#state{}, binary()}) -> no_return().
system_terminate(Reason, _, _, {State, _}) ->
	%% @todo Graceful shutdown here as well?
	terminate(State, {stop, {exit, Reason}, 'sys:terminate/2,3 was called.'}).

-spec system_code_change(Misc, _, _, _) -> {ok, Misc} when Misc::{#state{}, binary()}.
system_code_change(Misc, _, _, _) ->
	{ok, Misc}.
