%% Copyright (c) 2011-2013, Loïc Hoguin <essen@ninenines.eu>
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

%% @doc Handler middleware.
%%
%% Execute the handler given by the <em>handler</em> and <em>handler_opts</em>
%% environment values. The result of this execution is added to the
%% environment under the <em>result</em> value.
%%
%% When using loop handlers, we are receiving data from the socket because we
%% want to know when the socket gets closed. This is generally not an issue
%% because these kinds of requests are generally not pipelined, and don't have
%% a body. If they do have a body, this body is often read in the
%% <em>init/3</em> callback and this is no problem. Otherwise, this data
%% accumulates in a buffer until we reach a certain threshold of 5000 bytes
%% by default. This can be configured through the <em>loop_max_buffer</em>
%% environment value. The request will be terminated with an
%% <em>{error, overflow}</em> reason if this threshold is reached.
%%
%% @see cowboy_http_handler
-module(cowboy_handler).
-behaviour(cowboy_middleware).

-export([execute/2]).
-export([handler_loop/4]).

-record(state, {
	env :: cowboy_middleware:env(),
	hibernate = false :: boolean(),
	loop_buffer_size = 0 :: non_neg_integer(),
	loop_max_buffer = 5000 :: non_neg_integer() | infinity,
	loop_timeout = infinity :: timeout(),
	loop_timeout_ref = undefined :: undefined | reference(),
	resp_sent = false :: boolean()
}).

%% @private
-spec execute(Req, Env)
	-> {ok, Req, Env} | {error, 500, Req}
	| {suspend, ?MODULE, handler_loop, [any()]}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env) ->
	{_, Handler} = lists:keyfind(handler, 1, Env),
	{_, HandlerOpts} = lists:keyfind(handler_opts, 1, Env),
	MaxBuffer = case lists:keyfind(loop_max_buffer, 1, Env) of
		false -> 5000;
		{_, MaxBuffer0} -> MaxBuffer0
	end,
	handler_init(Req, #state{env=Env, loop_max_buffer=MaxBuffer},
		Handler, HandlerOpts).

-spec handler_init(Req, #state{}, module(), any())
	-> {ok, Req, cowboy_middleware:env()}
	| {error, 500, Req} | {suspend, module(), atom(), [any()]}
	when Req::cowboy_req:req().
handler_init(Req, State, Handler, HandlerOpts) ->
	Transport = cowboy_req:get(transport, Req),
	try Handler:init({Transport:name(), http}, Req, HandlerOpts) of
		{ok, Req2, HandlerState} ->
			handler_handle(Req2, State, Handler, HandlerState);
		{loop, Req2, HandlerState} ->
			handler_after_callback(Req2, State, Handler, HandlerState);
		{loop, Req2, HandlerState, hibernate} ->
			handler_after_callback(Req2, State#state{hibernate=true},
				Handler, HandlerState);
		{loop, Req2, HandlerState, Timeout} ->
			State2 = handler_loop_timeout(State#state{loop_timeout=Timeout}),
			handler_after_callback(Req2, State2, Handler, HandlerState);
		{loop, Req2, HandlerState, Timeout, hibernate} ->
			State2 = handler_loop_timeout(State#state{
				hibernate=true, loop_timeout=Timeout}),
			handler_after_callback(Req2, State2, Handler, HandlerState);
		{shutdown, Req2, HandlerState} ->
			terminate_request(Req2, State, Handler, HandlerState,
				{normal, shutdown});
		%% @todo {upgrade, transport, Module}
		{upgrade, protocol, Module} ->
			upgrade_protocol(Req, State, Handler, HandlerOpts, Module);
		{upgrade, protocol, Module, Req2, HandlerOpts2} ->
			upgrade_protocol(Req2, State, Handler, HandlerOpts2, Module)
	catch Class:Reason ->
		cowboy_req:maybe_reply(500, Req),
		erlang:Class([
			{reason, Reason},
			{mfa, {Handler, init, 3}},
			{stacktrace, erlang:get_stacktrace()},
			{req, cowboy_req:to_list(Req)},
			{opts, HandlerOpts}
		])
	end.

-spec upgrade_protocol(Req, #state{}, module(), any(), module())
	-> {ok, Req, Env}
	| {suspend, module(), atom(), any()}
	| {halt, Req}
	| {error, cowboy:http_status(), Req}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade_protocol(Req, #state{env=Env},
		Handler, HandlerOpts, Module) ->
	Module:upgrade(Req, Env, Handler, HandlerOpts).

-spec handler_handle(Req, #state{}, module(), any())
	-> {ok, Req, cowboy_middleware:env()}
	| {error, 500, Req}
	when Req::cowboy_req:req().
handler_handle(Req, State, Handler, HandlerState) ->
	try Handler:handle(Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			terminate_request(Req2, State, Handler, HandlerState2,
				{normal, shutdown})
	catch Class:Reason ->
		cowboy_req:maybe_reply(500, Req),
		handler_terminate(Req, Handler, HandlerState, Reason),
		erlang:Class([
			{reason, Reason},
			{mfa, {Handler, handle, 2}},
			{stacktrace, erlang:get_stacktrace()},
			{req, cowboy_req:to_list(Req)},
			{state, HandlerState}
		])
	end.

%% Update the state if the response was sent in the callback.
-spec handler_after_callback(Req, #state{}, module(), any())
	-> {ok, Req, cowboy_middleware:env()}
	| {error, 500, Req} | {suspend, module(), atom(), [any()]}
	when Req::cowboy_req:req().
handler_after_callback(Req, State=#state{resp_sent=false}, Handler,
		HandlerState) ->
	receive
		{cowboy_req, resp_sent} ->
			handler_before_loop(Req, State#state{resp_sent=true}, Handler,
				HandlerState)
	after 0 ->
		handler_before_loop(Req, State, Handler, HandlerState)
	end;
handler_after_callback(Req, State, Handler, HandlerState) ->
	handler_before_loop(Req, State, Handler, HandlerState).

%% We don't listen for Transport closes because that would force us
%% to receive data and buffer it indefinitely.
-spec handler_before_loop(Req, #state{}, module(), any())
	-> {ok, Req, cowboy_middleware:env()}
	| {error, 500, Req} | {suspend, module(), atom(), [any()]}
	when Req::cowboy_req:req().
handler_before_loop(Req, State=#state{hibernate=true}, Handler, HandlerState) ->
	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
	Transport:setopts(Socket, [{active, once}]),
	{suspend, ?MODULE, handler_loop,
		[Req, State#state{hibernate=false}, Handler, HandlerState]};
handler_before_loop(Req, State, Handler, HandlerState) ->
	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
	Transport:setopts(Socket, [{active, once}]),
	handler_loop(Req, State, Handler, HandlerState).

%% Almost the same code can be found in cowboy_websocket.
-spec handler_loop_timeout(#state{}) -> #state{}.
handler_loop_timeout(State=#state{loop_timeout=infinity}) ->
	State#state{loop_timeout_ref=undefined};
handler_loop_timeout(State=#state{loop_timeout=Timeout,
		loop_timeout_ref=PrevRef}) ->
	_ = case PrevRef of
		undefined -> ignore;
		PrevRef -> erlang:cancel_timer(PrevRef)
	end,
	TRef = erlang:start_timer(Timeout, self(), ?MODULE),
	State#state{loop_timeout_ref=TRef}.

%% @private
-spec handler_loop(Req, #state{}, module(), any())
	-> {ok, Req, cowboy_middleware:env()}
	| {error, 500, Req} | {suspend, module(), atom(), [any()]}
	when Req::cowboy_req:req().
handler_loop(Req, State=#state{loop_buffer_size=NbBytes,
		loop_max_buffer=Threshold, loop_timeout_ref=TRef},
		Handler, HandlerState) ->
	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
	{OK, Closed, Error} = Transport:messages(),
	receive
		{OK, Socket, Data} ->
			NbBytes2 = NbBytes + byte_size(Data),
			if	NbBytes2 > Threshold ->
					_ = handler_terminate(Req, Handler, HandlerState,
						{error, overflow}),
					cowboy_req:maybe_reply(500, Req),
					exit(normal);
				true ->
					Req2 = cowboy_req:append_buffer(Data, Req),
					State2 = handler_loop_timeout(State#state{
						loop_buffer_size=NbBytes2}),
					handler_before_loop(Req2, State2, Handler, HandlerState)
			end;
		{Closed, Socket} ->
			terminate_request(Req, State, Handler, HandlerState,
				{error, closed});
		{Error, Socket, Reason} ->
			terminate_request(Req, State, Handler, HandlerState,
				{error, Reason});
		{timeout, TRef, ?MODULE} ->
			handler_after_loop(Req, State, Handler, HandlerState,
				{normal, timeout});
		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
			handler_before_loop(Req, State, Handler, HandlerState);
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
			handler_call(Req2, State, Handler, HandlerState, Message)
	end.

-spec handler_call(Req, #state{}, module(), any(), any())
	-> {ok, Req, cowboy_middleware:env()}
	| {error, 500, Req} | {suspend, module(), atom(), [any()]}
	when Req::cowboy_req:req().
handler_call(Req, State=#state{resp_sent=RespSent},
		Handler, HandlerState, Message) ->
	try Handler:info(Message, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			handler_after_loop(Req2, State, Handler, HandlerState2,
				{normal, shutdown});
		{loop, Req2, HandlerState2} ->
			handler_after_callback(Req2, State, Handler, HandlerState2);
		{loop, Req2, HandlerState2, hibernate} ->
			handler_after_callback(Req2, State#state{hibernate=true},
				Handler, HandlerState2)
	catch Class:Reason ->
		if RespSent ->
			ok;
		true ->
			cowboy_req:maybe_reply(500, Req)
		end,
		handler_terminate(Req, Handler, HandlerState, Reason),
		erlang:Class([
			{reason, Reason},
			{mfa, {Handler, info, 3}},
			{stacktrace, erlang:get_stacktrace()},
			{req, cowboy_req:to_list(Req)},
			{state, HandlerState}
		])
	end.

%% It is sometimes important to make a socket passive as it was initially
%% and as it is expected to be by cowboy_protocol, right after we're done
%% with loop handling. The browser may freely pipeline a bunch of requests
%% if previous one was, say, a JSONP long-polling request.
-spec handler_after_loop(Req, #state{}, module(), any(),
	{normal, timeout | shutdown} | {error, atom()}) ->
	{ok, Req, cowboy_middleware:env()} when Req::cowboy_req:req().
handler_after_loop(Req, State, Handler, HandlerState, Reason) ->
	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
	Transport:setopts(Socket, [{active, false}]),
	{OK, _Closed, _Error} = Transport:messages(),
	Req2 = receive
		{OK, Socket, Data} ->
			cowboy_req:append_buffer(Data, Req)
	after 0 ->
		Req
	end,
	terminate_request(Req2, State, Handler, HandlerState, Reason).

-spec terminate_request(Req, #state{}, module(), any(),
	{normal, timeout | shutdown} | {error, atom()}) ->
	{ok, Req, cowboy_middleware:env()} when Req::cowboy_req:req().
terminate_request(Req, #state{env=Env}, Handler, HandlerState, Reason) ->
	HandlerRes = handler_terminate(Req, Handler, HandlerState, Reason),
	{ok, Req, [{result, HandlerRes}|Env]}.

-spec handler_terminate(cowboy_req:req(), module(), any(),
	{normal, timeout | shutdown} | {error, atom()}) -> ok.
handler_terminate(Req, Handler, HandlerState, Reason) ->
	try
		Handler:terminate(Reason, cowboy_req:lock(Req), HandlerState)
	catch Class:Reason2 ->
		erlang:Class([
			{reason, Reason2},
			{mfa, {Handler, terminate, 3}},
			{stacktrace, erlang:get_stacktrace()},
			{req, cowboy_req:to_list(Req)},
			{state, HandlerState},
			{terminate_reason, Reason}
		])
	end.
