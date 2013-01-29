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
%% @see cowboy_http_handler
-module(cowboy_handler).
-behaviour(cowboy_middleware).

-export([execute/2]).
-export([handler_loop/4]).

-record(state, {
	env :: cowboy_middleware:env(),
	hibernate = false :: boolean(),
	loop_timeout = infinity :: timeout(),
	loop_timeout_ref :: undefined | reference(),
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
	handler_init(Req, #state{env=Env}, Handler, HandlerOpts).

-spec handler_init(Req, #state{}, module(), any())
	-> {ok, Req, cowboy_middleware:env()}
	| {error, 500, Req} | {suspend, module(), function(), [any()]}
	when Req::cowboy_req:req().
handler_init(Req, State, Handler, HandlerOpts) ->
	Transport = cowboy_req:get(transport, Req),
	try Handler:init({Transport:name(), http}, Req, HandlerOpts) of
		{ok, Req2, HandlerState} ->
			handler_handle(Req2, State, Handler, HandlerState);
		{loop, Req2, HandlerState} ->
			handler_before_loop(Req2, State#state{hibernate=false},
				Handler, HandlerState);
		{loop, Req2, HandlerState, hibernate} ->
			handler_before_loop(Req2, State#state{hibernate=true},
				Handler, HandlerState);
		{loop, Req2, HandlerState, Timeout} ->
			handler_before_loop(Req2, State#state{loop_timeout=Timeout},
				Handler, HandlerState);
		{loop, Req2, HandlerState, Timeout, hibernate} ->
			handler_before_loop(Req2, State#state{
				hibernate=true, loop_timeout=Timeout}, Handler, HandlerState);
		{shutdown, Req2, HandlerState} ->
			terminate_request(Req2, State, Handler, HandlerState,
				{normal, shutdown});
		%% @todo {upgrade, transport, Module}
		{upgrade, protocol, Module} ->
			upgrade_protocol(Req, State, Handler, HandlerOpts, Module);
		{upgrade, protocol, Module, Req2, HandlerOpts2} ->
			upgrade_protocol(Req2, State, Handler, HandlerOpts2, Module)
	catch Class:Reason ->
		error_logger:error_msg(
			"** Cowboy handler ~p terminating in ~p/~p~n"
			"   for the reason ~p:~p~n"
			"** Options were ~p~n"
			"** Request was ~p~n"
			"** Stacktrace: ~p~n~n",
			[Handler, init, 3, Class, Reason, HandlerOpts,
				cowboy_req:to_list(Req), erlang:get_stacktrace()]),
		error_terminate(Req, State)
	end.

-spec upgrade_protocol(Req, #state{}, module(), any(), module())
	-> {ok, Req, Env}
	| {suspend, module(), atom(), any()}
	| {halt, Req}
	| {error, cowboy_http:status(), Req}
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
		error_logger:error_msg(
			"** Cowboy handler ~p terminating in ~p/~p~n"
			"   for the reason ~p:~p~n"
			"** Handler state was ~p~n"
			"** Request was ~p~n"
			"** Stacktrace: ~p~n~n",
			[Handler, handle, 2, Class, Reason, HandlerState,
				cowboy_req:to_list(Req), erlang:get_stacktrace()]),
		handler_terminate(Req, Handler, HandlerState, Reason),
		error_terminate(Req, State)
	end.

%% We don't listen for Transport closes because that would force us
%% to receive data and buffer it indefinitely.
-spec handler_before_loop(Req, #state{}, module(), any())
	-> {ok, Req, cowboy_middleware:env()}
	| {error, 500, Req} | {suspend, module(), function(), [any()]}
	when Req::cowboy_req:req().
handler_before_loop(Req, State=#state{hibernate=true}, Handler, HandlerState) ->
	State2 = handler_loop_timeout(State),
	{suspend, ?MODULE, handler_loop,
		[Req, State2#state{hibernate=false}, Handler, HandlerState]};
handler_before_loop(Req, State, Handler, HandlerState) ->
	State2 = handler_loop_timeout(State),
	handler_loop(Req, State2, Handler, HandlerState).

%% Almost the same code can be found in cowboy_websocket.
-spec handler_loop_timeout(#state{}) -> #state{}.
handler_loop_timeout(State=#state{loop_timeout=infinity}) ->
	State#state{loop_timeout_ref=undefined};
handler_loop_timeout(State=#state{loop_timeout=Timeout,
		loop_timeout_ref=PrevRef}) ->
	_ = case PrevRef of undefined -> ignore; PrevRef ->
		erlang:cancel_timer(PrevRef) end,
	TRef = erlang:start_timer(Timeout, self(), ?MODULE),
	State#state{loop_timeout_ref=TRef}.

%% @private
-spec handler_loop(Req, #state{}, module(), any())
	-> {ok, Req, cowboy_middleware:env()}
	| {error, 500, Req} | {suspend, module(), function(), [any()]}
	when Req::cowboy_req:req().
handler_loop(Req, State=#state{loop_timeout_ref=TRef}, Handler, HandlerState) ->
	receive
		{cowboy_req, resp_sent} ->
			handler_loop(Req, State#state{resp_sent=true},
				Handler, HandlerState);
		{timeout, TRef, ?MODULE} ->
			terminate_request(Req, State, Handler, HandlerState,
				{normal, timeout});
		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
			handler_loop(Req, State, Handler, HandlerState);
		Message ->
			handler_call(Req, State, Handler, HandlerState, Message)
	end.

-spec handler_call(Req, #state{}, module(), any(), any())
	-> {ok, Req, cowboy_middleware:env()}
	| {error, 500, Req} | {suspend, module(), function(), [any()]}
	when Req::cowboy_req:req().
handler_call(Req, State, Handler, HandlerState, Message) ->
	try Handler:info(Message, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			terminate_request(Req2, State, Handler, HandlerState2,
				{normal, shutdown});
		{loop, Req2, HandlerState2} ->
			handler_before_loop(Req2, State, Handler, HandlerState2);
		{loop, Req2, HandlerState2, hibernate} ->
			handler_before_loop(Req2, State#state{hibernate=true},
				Handler, HandlerState2)
	catch Class:Reason ->
		error_logger:error_msg(
			"** Cowboy handler ~p terminating in ~p/~p~n"
			"   for the reason ~p:~p~n"
			"** Handler state was ~p~n"
			"** Request was ~p~n"
			"** Stacktrace: ~p~n~n",
			[Handler, info, 3, Class, Reason, HandlerState,
				cowboy_req:to_list(Req), erlang:get_stacktrace()]),
		handler_terminate(Req, Handler, HandlerState, Reason),
		error_terminate(Req, State)
	end.

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
		error_logger:error_msg(
			"** Cowboy handler ~p terminating in ~p/~p~n"
			"   for the reason ~p:~p~n"
			"** Handler state was ~p~n"
			"** Request was ~p~n"
			"** Stacktrace: ~p~n~n",
			[Handler, terminate, 3, Class, Reason2, HandlerState,
				cowboy_req:to_list(Req), erlang:get_stacktrace()])
	end.

%% Only send an error reply if there is no resp_sent message.
-spec error_terminate(Req, #state{})
	-> {error, 500, Req} | {halt, Req} when Req::cowboy_req:req().
error_terminate(Req, #state{resp_sent=true}) ->
	%% Close the connection, but do not attempt sending a reply.
	{halt, cowboy_req:set([{connection, close}, {resp_state, done}], Req)};
error_terminate(Req, _) ->
	{error, 500, Req}.
