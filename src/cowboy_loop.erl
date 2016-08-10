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

-export([upgrade/6]).
-export([loop/4]).

-callback init(Req, any())
	-> {ok | module(), Req, any()}
	| {module(), Req, any(), hibernate}
	| {module(), Req, any(), timeout()}
	| {module(), Req, any(), timeout(), hibernate}
	when Req::cowboy_req:req().
-callback info(any(), Req, State)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {stop, Req, State}
	when Req::cowboy_req:req(), State::any().

-callback terminate(any(), cowboy_req:req(), any()) -> ok.
-optional_callbacks([terminate/3]).

-record(state, {
	env :: cowboy_middleware:env(),
	hibernate = false :: boolean(),
	buffer_size = 0 :: non_neg_integer(),
	max_buffer = 5000 :: non_neg_integer() | infinity,
	timeout = infinity :: timeout(),
	timeout_ref = undefined :: undefined | reference()
}).

-spec upgrade(Req, Env, module(), any(), timeout(), run | hibernate)
	-> {ok, Req, Env} | {suspend, module(), atom(), [any()]}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade(Req, Env, Handler, HandlerState, Timeout, Hibernate) ->
	State = #state{env=Env, max_buffer=get_max_buffer(Env), timeout=Timeout,
		hibernate=Hibernate =:= hibernate},
	State2 = timeout(State),
	before_loop(Req, State2, Handler, HandlerState).

get_max_buffer(#{loop_max_buffer := MaxBuffer}) -> MaxBuffer;
get_max_buffer(_) -> 5000.

before_loop(Req, State=#state{hibernate=true}, Handler, HandlerState) ->

	%% @todo Yeah we can't get the socket anymore.
	%% Everything changes since we are a separate process now.
	%% Proper flow control at the connection level should be implemented
	%% instead of what we have here.

%	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
%	Transport:setopts(Socket, [{active, once}]),
	{suspend, ?MODULE, loop, [Req, State#state{hibernate=false}, Handler, HandlerState]};
before_loop(Req, State, Handler, HandlerState) ->

	%% Same here.

%	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
%	Transport:setopts(Socket, [{active, once}]),
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
	-> {ok, Req, cowboy_middleware:env()} | {suspend, module(), atom(), [any()]}
	when Req::cowboy_req:req().
loop(Req, State=#state{timeout_ref=TRef}, Handler, HandlerState) ->
	receive
		{timeout, TRef, ?MODULE} ->
			terminate(Req, State, Handler, HandlerState, timeout);
		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
			loop(Req, State, Handler, HandlerState);
		Message ->
			call(Req, State, Handler, HandlerState, Message)
	end.

call(Req, State, Handler, HandlerState, Message) ->
	try Handler:info(Message, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			before_loop(Req2, State, Handler, HandlerState2);
		{ok, Req2, HandlerState2, hibernate} ->
			before_loop(Req2, State#state{hibernate=true}, Handler, HandlerState2);
		{stop, Req2, HandlerState2} ->
			terminate(Req2, State, Handler, HandlerState2, stop)
	catch Class:Reason ->
		cowboy_handler:terminate({crash, Class, Reason}, Req, HandlerState, Handler),
		erlang:raise(Class, Reason, erlang:get_stacktrace())
	end.

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
