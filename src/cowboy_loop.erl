%% Copyright (c) 2011-2014, Loïc Hoguin <essen@ninenines.eu>
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

%% When using loop handlers, we are receiving data from the socket because we
%% want to know when the socket gets closed. This is generally not an issue
%% because these kinds of requests are generally not pipelined, and don't have
%% a body. If they do have a body, this body is often read in the
%% <em>init/2</em> callback and this is no problem. Otherwise, this data
%% accumulates in a buffer until we reach a certain threshold of 5000 bytes
%% by default. This can be configured through the <em>loop_max_buffer</em>
%% environment value. The request will be terminated with an
%% <em>{error, overflow}</em> reason if this threshold is reached.
-module(cowboy_loop).
-behaviour(cowboy_sub_protocol).
-behaviour(cowboy_sys).

-export([upgrade/6]).
-export([loop/4]).
-export([sys_continue/2]).
-export([sys_terminate/3]).

-callback init(Req, any())
	-> {ok | module(), Req, any()}
	| {module(), Req, any(), hibernate}
	| {module(), Req, any(), timeout()}
	| {module(), Req, any(), timeout(), hibernate}
	when Req::cowboy_req:req().
-callback info(any(), Req, State)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {shutdown, Req, State}
	when Req::cowboy_req:req(), State::any().
%% @todo optional -callback terminate(terminate_reason(), cowboy_req:req(), state()) -> ok.

-record(state, {
	env :: cowboy_middleware:env(),
	parent :: pid(),
	hibernate = false :: boolean(),
	buffer_size = 0 :: non_neg_integer(),
	max_buffer = 5000 :: non_neg_integer() | infinity,
	timeout = infinity :: timeout(),
	timeout_ref = undefined :: undefined | reference(),
	resp_sent = false :: boolean()
}).

-spec upgrade(Req, Env, module(), any(), timeout(), run | hibernate)
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), any()}, any(), ?MODULE, Req, {#state{}, module(), any()}}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade(Req, Env, Handler, HandlerState, Timeout, run) ->
	{_, Parent} = lists:keyfind(parent, 1, Env),
	State = #state{env=Env, parent=Parent, max_buffer=get_max_buffer(Env),
		timeout=Timeout},
	State2 = timeout(State),
	after_call(Req, State2, Handler, HandlerState);
upgrade(Req, Env, Handler, HandlerState, Timeout, hibernate) ->
	Parent = proplists:get_value(parent, Env, self()),
	State = #state{env=Env, parent=Parent, max_buffer=get_max_buffer(Env),
		hibernate=true, timeout=Timeout},
	State2 = timeout(State),
	after_call(Req, State2, Handler, HandlerState).

get_max_buffer(Env) ->
	case lists:keyfind(loop_max_buffer, 1, Env) of
		false -> 5000;
		{_, MaxBuffer} -> MaxBuffer
	end.

%% Update the state if the response was sent in the callback.
after_call(Req, State=#state{resp_sent=false}, Handler,
		HandlerState) ->
	receive
		{cowboy_req, resp_sent} ->
			before_loop(Req, State#state{resp_sent=true}, Handler, HandlerState)
	after 0 ->
		before_loop(Req, State, Handler, HandlerState)
	end;
after_call(Req, State, Handler, HandlerState) ->
	before_loop(Req, State, Handler, HandlerState).

before_loop(Req, State=#state{hibernate=true}, Handler, HandlerState) ->
	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
	Transport:setopts(Socket, [{active, once}]),
	{suspend, ?MODULE, loop, [Req, State#state{hibernate=false}, Handler, HandlerState]};
before_loop(Req, State, Handler, HandlerState) ->
	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
	Transport:setopts(Socket, [{active, once}]),
	loop(Req, State, Handler, HandlerState).

%% Almost the same code can be found in cowboy_websocket.
timeout(State=#state{timeout=infinity}) ->
	State#state{timeout_ref=undefined};
timeout(State=#state{timeout=Timeout,
		timeout_ref=PrevRef}) ->
	_ = case PrevRef of
		undefined -> ignore;
		PrevRef -> erlang:cancel_timer(PrevRef)
	end,
	TRef = erlang:start_timer(Timeout, self(), ?MODULE),
	State#state{timeout_ref=TRef}.

-spec loop(Req, #state{}, module(), any())
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), any()}, any(), ?MODULE, Req, {#state{}, module(), any()}}
	when Req::cowboy_req:req().
loop(Req, State=#state{parent=Parent, buffer_size=NbBytes,
		max_buffer=Threshold, timeout_ref=TRef,
		resp_sent=RespSent}, Handler, HandlerState) ->
	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
	{OK, Closed, Error} = Transport:messages(),
	receive
		{OK, Socket, Data} ->
			NbBytes2 = NbBytes + byte_size(Data),
			if	NbBytes2 > Threshold ->
					_ = if RespSent -> ok; true ->
						cowboy_req:reply(500, Req)
					end,
					cowboy_handler:terminate({error, overflow}, Req, HandlerState, Handler),
					exit(normal);
				true ->
					Req2 = cowboy_req:append_buffer(Data, Req),
					State2 = timeout(State#state{buffer_size=NbBytes2}),
					before_loop(Req2, State2, Handler, HandlerState)
			end;
		{Closed, Socket} ->
			terminate(Req, State, Handler, HandlerState, {error, closed});
		{Error, Socket, Reason} ->
			terminate(Req, State, Handler, HandlerState, {error, Reason});
		{timeout, TRef, ?MODULE} ->
			after_loop(Req, State, Handler, HandlerState, timeout);
		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
			loop(Req, State, Handler, HandlerState);
		{system, From, Msg} ->
			{system, From, Msg, ?MODULE, Req, {State, Handler, HandlerState}};
		{'EXIT', Parent, Reason} ->
			sys_terminate(Reason, Req, {State, Handler, HandlerState});
		Message ->
			%% We set the socket back to {active, false} mode in case
			%% the handler is going to call recv. We also flush any
			%% data received after that and put it into the buffer.
			%% We do not check the size here, if data keeps coming
			%% we'll error out on the next packet received.
			Transport:setopts(Socket, [{active, false}]),
			Req2 = receive {OK, Socket, Data} ->
				cowboy_req:append_buffer(Data, Req)
			after 0 ->
				Req
			end,
			call(Req2, State, Handler, HandlerState, Message)
	end.

call(Req, State=#state{resp_sent=RespSent},
		Handler, HandlerState, Message) ->
	try Handler:info(Message, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			after_call(Req2, State, Handler, HandlerState2);
		{ok, Req2, HandlerState2, hibernate} ->
			after_call(Req2, State#state{hibernate=true}, Handler, HandlerState2);
		{shutdown, Req2, HandlerState2} ->
			after_loop(Req2, State, Handler, HandlerState2, shutdown)
	catch Class:Reason ->
		Stacktrace = erlang:get_stacktrace(),
		if RespSent -> ok; true ->
			cowboy_req:maybe_reply(Stacktrace, Req)
		end,
		cowboy_handler:terminate({crash, Class, Reason}, Req, HandlerState, Handler),
		exit([
			{class, Class},
			{reason, Reason},
			{mfa, {Handler, info, 3}},
			{stacktrace, Stacktrace},
			{req, cowboy_req:to_list(Req)},
			{state, HandlerState}
		])
	end.

%% It is sometimes important to make a socket passive as it was initially
%% and as it is expected to be by cowboy_protocol, right after we're done
%% with loop handling. The browser may freely pipeline a bunch of requests
%% if previous one was, say, a JSONP long-polling request.
after_loop(Req, State, Handler, HandlerState, Reason) ->
	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
	Transport:setopts(Socket, [{active, false}]),
	{OK, _Closed, _Error} = Transport:messages(),
	Req2 = receive
		{OK, Socket, Data} ->
			cowboy_req:append_buffer(Data, Req)
	after 0 ->
		Req
	end,
	terminate(Req2, State, Handler, HandlerState, Reason).

terminate(Req, #state{env=Env, timeout_ref=TRef},
		Handler, HandlerState, Reason) ->
	_ = case TRef of
		undefined -> ignore;
		TRef -> erlang:cancel_timer(TRef)
	end,
	flush_timeouts(),
	Result = cowboy_handler:terminate(Reason, Req, HandlerState, Handler),
	{ok, Req, [{result, Result}|Env]}.

flush_timeouts() ->
	receive
		{timeout, TRef, ?MODULE} when is_reference(TRef) ->
			flush_timeouts()
	after 0 ->
		ok
	end.

-spec sys_continue(Req, {#state{}, module(), any()})
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), any()}, any(), ?MODULE, Req, {#state{}, module(), any()}}
	when Req::cowboy_req:req().
sys_continue(Req, {State, Handler, HandlerState}) ->
	loop(Req, State, Handler, HandlerState).

-spec sys_terminate(any(), Req, {#state{}, module(), any()}) -> no_return()
	when Req::cowboy_req:req().
sys_terminate(Reason, Req, {_, Handler, HandlerState}) ->
	_ = cowboy_handler:terminate(Reason, Req, HandlerState, Handler),
	exit(Reason).
