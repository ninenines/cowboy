%% Copyright (c) 2011-2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_loop).
-behaviour(cowboy_sub_protocol).

-ifdef(OTP_RELEASE).
-compile({nowarn_deprecated_function, [{erlang, get_stacktrace, 0}]}).
-endif.

-export([upgrade/4]).
-export([upgrade/5]).
-export([loop/4]).

-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-callback init(Req, any())
	-> {ok | module(), Req, any()}
	| {module(), Req, any(), any()}
	when Req::cowboy_req:req().

-callback info(any(), Req, State)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {stop, Req, State}
	when Req::cowboy_req:req(), State::any().

-callback terminate(any(), cowboy_req:req(), any()) -> ok.
-optional_callbacks([terminate/3]).

-spec upgrade(Req, Env, module(), any())
	-> {ok, Req, Env} | {suspend, ?MODULE, loop, [any()]}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade(Req, Env, Handler, HandlerState) ->
	loop(Req, Env, Handler, HandlerState).

-spec upgrade(Req, Env, module(), any(), hibernate)
	-> {suspend, ?MODULE, loop, [any()]}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade(Req, Env, Handler, HandlerState, hibernate) ->
	suspend(Req, Env, Handler, HandlerState).

-spec loop(Req, Env, module(), any())
	-> {ok, Req, Env} | {suspend, ?MODULE, loop, [any()]}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
%% @todo Handle system messages.
loop(Req=#{pid := Parent}, Env, Handler, HandlerState) ->
	receive
		%% System messages.
		{'EXIT', Parent, Reason} ->
			terminate(Req, Env, Handler, HandlerState, Reason);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
				{Req, Env, Handler, HandlerState});
		%% Calls from supervisor module.
		{'$gen_call', From, Call} ->
			cowboy_children:handle_supervisor_call(Call, From, [], ?MODULE),
			loop(Req, Env, Handler, HandlerState);
		Message ->
			call(Req, Env, Handler, HandlerState, Message)
	end.

call(Req0, Env, Handler, HandlerState0, Message) ->
	try Handler:info(Message, Req0, HandlerState0) of
		{ok, Req, HandlerState} ->
			loop(Req, Env, Handler, HandlerState);
		{ok, Req, HandlerState, hibernate} ->
			suspend(Req, Env, Handler, HandlerState);
		{stop, Req, HandlerState} ->
			terminate(Req, Env, Handler, HandlerState, stop)
	catch Class:Reason ->
		StackTrace = erlang:get_stacktrace(),
		cowboy_handler:terminate({crash, Class, Reason}, Req0, HandlerState0, Handler),
		erlang:raise(Class, Reason, StackTrace)
	end.

suspend(Req, Env, Handler, HandlerState) ->
	{suspend, ?MODULE, loop, [Req, Env, Handler, HandlerState]}.

terminate(Req, Env, Handler, HandlerState, Reason) ->
	Result = cowboy_handler:terminate(Reason, Req, HandlerState, Handler),
	{ok, Req, Env#{result => Result}}.

%% System callbacks.

-spec system_continue(_, _, {Req, Env, module(), any()})
	-> {ok, Req, Env} | {suspend, ?MODULE, loop, [any()]}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
system_continue(_, _, {Req, Env, Handler, HandlerState}) ->
	loop(Req, Env, Handler, HandlerState).

-spec system_terminate(any(), _, _, {Req, Env, module(), any()})
	-> {ok, Req, Env} when Req::cowboy_req:req(), Env::cowboy_middleware:env().
system_terminate(Reason, _, _, {Req, Env, Handler, HandlerState}) ->
	terminate(Req, Env, Handler, HandlerState, Reason).

-spec system_code_change(Misc, _, _, _) -> {ok, Misc}
	when Misc::{cowboy_req:req(), cowboy_middleware:env(), module(), any()}.
system_code_change(Misc, _, _, _) ->
	{ok, Misc}.
