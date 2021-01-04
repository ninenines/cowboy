%% Copyright (c) 2020, Viktor Söderqvist <viktor.soderqvist@est.tech>
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

%% Handles the CONNECT method. In HTTP/2 for a stream; in HTTP/1 it takes over
%% the connection.
-module(cowboy_tunnel).

-behaviour(cowboy_sub_protocol).
-export([upgrade/4]).
-export([upgrade/5]).

-export([is_connect_request/1]).
-export([takeover/7]).
-export([loop/2]).

-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-type commands() :: [{data, iodata()}
	| stop
	| {active, boolean()}
	| {set_options, map()}
	| {shutdown_reason, any()}
].
-export_type([commands/0]).

-type call_result(State) :: {commands(), State} | {commands(), State, hibernate}.

-type terminate_reason() :: normal | stop | timeout
	| {error, closed | atom()}
	| {crash, error | exit | throw, any()}.

%% Init/2 and terminate/3 overlap with the cowboy_handler behaviour. If a module
%% is a callback module for both, there will be a compiler warning.

%% -callback init(Req, any())
%% 	-> {ok | module(), Req, any()}
%% 	| {module(), Req, any(), any()}
%% 	when Req::cowboy_req:req().

-callback tunnel_init(State) -> call_result(State) when State::any().

-callback tunnel_handle(nofin|fin, binary(), State)
	-> call_result(State) when State::any().

-callback tunnel_info(any(), State) -> call_result(State) when State::any().

%% -callback terminate(any(), cowboy_req:req(), any()) -> ok.
%% -optional_callbacks([terminate/3]).

-type opts() :: #{
	active_n => pos_integer(),
	idle_timeout => timeout(),
	req_filter => fun((cowboy_req:req()) -> map())
}.
-export_type([opts/0]).

-record(state, {
	parent :: undefined | pid(),
	ref :: ranch:ref(),
	socket = undefined :: inet:socket() | {pid(), cowboy_stream:streamid()} | undefined,
	transport = undefined :: module() | undefined,
	http_response_sent = false :: boolean(),
	opts = #{} :: opts(),
	active = true :: boolean(),
	handler :: module(),
	timeout_ref = undefined :: undefined | reference(),
	messages = undefined :: undefined | {atom(), atom(), atom()}
		| {atom(), atom(), atom(), atom()},
	hibernate = false :: boolean(),
	req = #{} :: map(),
	shutdown_reason = normal :: any()
}).

-spec is_connect_request(cowboy_req:req()) -> boolean().
is_connect_request(#{method := <<"CONNECT">>, version := 'HTTP/2',
		protocol := _}) ->
	false;
is_connect_request(#{method := Method}) ->
	Method =:= <<"CONNECT">>.

%% Stream process.

-spec upgrade(Req, Env, module(), any()) -> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade(Req, Env, Handler, HandlerState) ->
	upgrade(Req, Env, Handler, HandlerState, #{}).

-spec upgrade(Req, Env, module(), any(), opts()) -> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
%% @todo Immediately crash if a response has already been sent.
upgrade(Req, Env, Handler, HandlerState, Opts) ->
	FilteredReq = case maps:get(req_filter, Opts, undefined) of
		undefined -> maps:with([method, version, host, port, peer], Req);
		FilterFun -> FilterFun(Req)
	end,
	State = #state{opts=Opts, handler=Handler, req=FilteredReq},
	case is_connect_request(Req) of
		true ->
			takeover_stream(State, Req, HandlerState, Env);
		false ->
			{ok, cowboy_req:reply(400, Req), Env}
	end.

-spec takeover_stream(#state{}, Req, any(), Env) -> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
takeover_stream(State,
		Req=#{version := 'HTTP/1.1', pid := Pid, streamid := StreamID},
		HandlerState, Env) ->
	Pid ! {{Pid, StreamID}, {takeover, ?MODULE, {State, HandlerState}}},
	{ok, Req, Env};
%% For HTTP/2 we do not let the process die, we instead keep it
%% for the tunnel stream. This is because in HTTP/2 we only
%% have a stream; we don't take over the whole connection.
takeover_stream(State, #{ref := Ref, pid := Pid, streamid := StreamID},
		HandlerState, _Env) ->
	takeover(Pid, Ref, {Pid, StreamID}, undefined, undefined, <<>>,
		{State, HandlerState}).


%% Connection process.

-spec takeover(pid(), ranch:ref(), inet:socket() | {pid(), cowboy_stream:streamid()},
	module() | undefined, any(), binary(),
	{#state{}, any()}) -> no_return().
takeover(Parent, Ref, Socket, Transport, _Opts, _Buffer, {State0, HandlerState}) ->
	%% @todo Remove from ranch as we do for websockets? Better not...
	%%ranch:remove_connection(Ref),
	Messages = case Transport of
		undefined -> undefined;
		_ -> Transport:messages()
	end,
	State = loop_timeout(State0#state{parent=Parent,
		ref=Ref, socket=Socket, transport=Transport,
		messages=Messages}),
	handler_call(State, HandlerState, tunnel_init, undefined).

send_connect_successful(#state{socket=Stream={Pid, _}, transport=undefined}) ->
	Pid ! {Stream, {headers, 200, #{}}};
send_connect_successful(#state{socket=Socket, transport=Transport}) ->
	Transport:send(Socket, cow_http:response(200, 'HTTP/1.1', [])).

send_connect_error(#state{socket=Stream={Pid, _}, transport=undefined}) ->
	Pid ! {Stream, {response, 503, #{}, <<"Failed to connect to peer">>}};
send_connect_error(#state{socket=Socket, transport=Transport}) ->
	Transport:send(Socket, cow_http:response(503, 'HTTP/1.1', [])).

after_init(State=#state{active=Active}) ->
	%% Send 2xx or 5xx response to the CONNECT request
	send_connect_successful(State),
	case Active of
		true ->
			%% Enable active,N for HTTP/1.1, and auto read_body for
			%% HTTP/2. We must do this only after calling
			%% tunnel_init/1 to give the handler a chance to disable
			%% active mode immediately.
			setopts_active(State),
			maybe_read_body(State);
		false ->
			ok
	end.

%% We have two ways of reading the client data. For HTTP/1.1
%% we have full control of the socket and can therefore use active,N.
%% For HTTP/2 we are just a stream, and are instead using read_body
%% (automatic mode). Technically HTTP/2 will only go passive after
%% receiving the next data message, while HTTP/1.1 goes passive
%% immediately but there might still be data to be processed in
%% the message queue.

setopts_active(#state{transport=undefined}) ->
	ok;
setopts_active(#state{socket=Socket, transport=Transport, opts=Opts}) ->
	N = maps:get(active_n, Opts, 100),
	Transport:setopts(Socket, [{active, N}]).

maybe_read_body(#state{socket=Stream={Pid, _}, transport=undefined, active=true}) ->
	%% @todo Keep Ref around.
	ReadBodyRef = make_ref(),
	Pid ! {Stream, {read_body, self(), ReadBodyRef, auto, infinity}},
	ok;
maybe_read_body(_) ->
	ok.

active(State) ->
	setopts_active(State),
	maybe_read_body(State),
	State#state{active=true}.

passive(State=#state{transport=undefined}) ->
	%% Unfortunately we cannot currently cancel read_body.
	%% But that's OK, we will just stop reading the body
	%% after the next message.
	State#state{active=false};
passive(State=#state{socket=Socket, transport=Transport, messages=Messages}) ->
	Transport:setopts(Socket, [{active, false}]),
	flush_passive(Socket, Messages),
	State#state{active=false}.

flush_passive(Socket, Messages) ->
	receive
		{Passive, Socket} when Passive =:= element(4, Messages);
				%% Hardcoded for compatibility with Ranch 1.x.
				Passive =:= tcp_passive; Passive =:= ssl_passive ->
			flush_passive(Socket, Messages)
	after 0 ->
		ok
	end.

before_loop(State=#state{hibernate=true}, HandlerState) ->
	proc_lib:hibernate(?MODULE, loop,
		[State#state{hibernate=false}, HandlerState]);
before_loop(State, HandlerState) ->
	loop(State, HandlerState).

-spec loop_timeout(#state{}) -> #state{}.
loop_timeout(State=#state{opts=Opts, timeout_ref=PrevRef}) ->
	_ = case PrevRef of
		undefined -> ignore;
		PrevRef -> erlang:cancel_timer(PrevRef)
	end,
	case maps:get(idle_timeout, Opts, 60000) of
		infinity ->
			State#state{timeout_ref=undefined};
		Timeout ->
			TRef = erlang:start_timer(Timeout, self(), ?MODULE),
			State#state{timeout_ref=TRef}
	end.

-spec loop(#state{}, any()) -> no_return().
loop(State=#state{parent=Parent, socket=Socket, messages=Messages,
		timeout_ref=TRef}, HandlerState) ->
	receive
		%% Socket messages. (HTTP/1.1)
		{OK, Socket, Data} when OK =:= element(1, Messages) ->
			State2 = loop_timeout(State),
			data(State2, HandlerState, nofin, Data);
		{Closed, Socket} when Closed =:= element(2, Messages) ->
			%% This means client has closed for writing.
			%% Continue reading from peer until receiving FIN.
			State2 = loop_timeout(State),
			data(State2, HandlerState, fin, <<>>);
			%%terminate(State, HandlerState, {error, closed});
		{Error, Socket, Reason} when Error =:= element(3, Messages) ->
			terminate(State, HandlerState, {error, Reason});
		{Passive, Socket} when Passive =:= element(4, Messages);
				%% Hardcoded for compatibility with Ranch 1.x.
				Passive =:= tcp_passive; Passive =:= ssl_passive ->
			setopts_active(State),
			loop(State, HandlerState);
		%% Body reading messages. (HTTP/2)
		{request_body, _Ref, IsFin, Data} ->
			maybe_read_body(State),
			State2 = loop_timeout(State),
			data(State2, HandlerState, IsFin, Data);
		%% Timeouts.
		{timeout, TRef, ?MODULE} ->
			%% _ = transport_send(State, fin, <<>>),
			terminate(State, HandlerState, timeout);
		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
			before_loop(State, HandlerState);
		%% System messages.
		{'EXIT', Parent, Reason} ->
			%% @todo We should exit gracefully.
			exit(Reason);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
				{State, HandlerState});
		%% Calls from supervisor module.
		{'$gen_call', From, Call} ->
			cowboy_children:handle_supervisor_call(Call, From, [], ?MODULE),
			before_loop(State, HandlerState);
		Message ->
			handler_call(State, HandlerState, tunnel_info, Message)
	end.

data(State, HandlerState, IsFin, Data) ->
	handler_call(State, HandlerState, tunnel_handle, {IsFin, Data}).

handler_call(State=#state{handler=Handler}, HandlerState, Callback, Message) ->
	try case Callback of
		tunnel_init ->
			Handler:tunnel_init(HandlerState);
		tunnel_handle ->
			{IsFin, Data} = Message,
			Handler:tunnel_handle(IsFin, Data, HandlerState);
		tunnel_info ->
			Handler:tunnel_info(Message, HandlerState)
	end of
		{Commands, HandlerState2} when is_list(Commands) ->
			handler_call_result(State,
				HandlerState2, Commands);
		{Commands, HandlerState2, hibernate} when is_list(Commands) ->
			handler_call_result(State#state{hibernate=true},
				HandlerState2, Commands)
	catch Class:Reason:Stacktrace ->
		handler_terminate(State, HandlerState, {crash, Class, Reason}),
		erlang:raise(Class, Reason, Stacktrace)
	end.

-spec handler_call_result(#state{}, any(), commands()) -> no_return().
handler_call_result(State0, HandlerState, Commands) ->
	case commands(Commands, State0, []) of
		{ok, State} ->
			before_loop(State, HandlerState);
		{stop, State} ->
			terminate(State, HandlerState, stop);
		{Error = {error, _}, State} ->
			terminate(State, HandlerState, Error)
	end.

commands([], State=#state{http_response_sent = false}, Data) ->
	after_init(State),
	commands([], State#state{http_response_sent = true}, Data);
commands([], State, []) ->
	{ok, State};
commands([], State, Data) ->
	Result = transport_send(State, nofin, lists:reverse(Data)),
	{Result, State};
commands([{active, Active}|Tail], State0=#state{active=Active0}, Data) when is_boolean(Active) ->
	State = if
		Active, not Active0 ->
			active(State0);
		Active0, not Active ->
			passive(State0);
		true ->
			State0
	end,
	commands(Tail, State#state{active=Active}, Data);
commands([{set_options, SetOpts}|Tail], State0=#state{opts=Opts}, Data) ->
	State = case SetOpts of
		#{idle_timeout := IdleTimeout} ->
			loop_timeout(State0#state{opts=Opts#{idle_timeout => IdleTimeout}});
		_ ->
			State0
	end,
	commands(Tail, State, Data);
commands([{shutdown_reason, ShutdownReason}|Tail], State, Data) ->
	commands(Tail, State#state{shutdown_reason=ShutdownReason}, Data);
commands([{data, Data}|Tail], State, DataAcc) ->
	commands(Tail, State, [Data|DataAcc]);
commands([stop|_Tail], State, Data) ->
	%% @todo Handle the case when http_response_sent = false
	_ = transport_send(State, fin, lists:reverse(Data)),
	{stop, State}.

transport_send(#state{socket=Stream={Pid, _}, transport=undefined}, IsFin, Data) ->
	Pid ! {Stream, {data, IsFin, Data}},
	ok;
transport_send(#state{socket=Socket, transport=Transport}, IsFin, Data) ->
	Result = Transport:send(Socket, Data),
	case IsFin of
		fin -> _ = Transport:shutdown(Socket, write);
		nofin -> ok
	end,
	Result.

transport_close(#state{transport=undefined}) ->
	ok;
transport_close(#state{socket=Socket, transport=Transport}) ->
	Transport:close(Socket).

-spec terminate(#state{}, any(), terminate_reason()) -> no_return().
terminate(State=#state{http_response_sent=false}, HandlerState, Reason) ->
	send_connect_error(State),
	terminate(State#state{http_response_sent = true}, HandlerState, Reason);
terminate(State=#state{shutdown_reason=Shutdown}, HandlerState, Reason) ->
	handler_terminate(State, HandlerState, Reason),
	_ = transport_close(State),
	case Shutdown of
		normal -> exit(normal);
		_ -> exit({shutdown, Shutdown})
	end.

handler_terminate(#state{handler=Handler, req=Req}, HandlerState, Reason) ->
	cowboy_handler:terminate(Reason, Req, HandlerState, Handler).

%% System callbacks.

-spec system_continue(_, _, {#state{}, any()}) -> no_return().
system_continue(_, _, {State, HandlerState}) ->
	loop(State, HandlerState).

-spec system_terminate(any(), _, _, {#state{}, any()}) -> no_return().
system_terminate(Reason, _, _, {State, HandlerState}) ->
	%% @todo We should exit gracefully, if possible.
	terminate(State, HandlerState, Reason).

-spec system_code_change(Misc, _, _, _)
	-> {ok, Misc} when Misc::{#state{}, any()}.
system_code_change(Misc, _, _, _) ->
	{ok, Misc}.
