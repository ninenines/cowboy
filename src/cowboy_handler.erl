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

%% Handler middleware.
%%
%% Execute the handler given by the <em>handler</em> and <em>handler_opts</em>
%% environment values. The result of this execution is added to the
%% environment under the <em>result</em> value.
-module(cowboy_handler).
-behaviour(cowboy_middleware).

-export([execute/2]).
-export([terminate/5]).

-spec execute(Req, Env) -> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env) ->
	{_, Handler} = lists:keyfind(handler, 1, Env),
	{_, HandlerOpts} = lists:keyfind(handler_opts, 1, Env),
	try Handler:init(Req, HandlerOpts) of
		{http, Req2, State} ->
			handle(Req2, Env, Handler, State);
		{shutdown, Req2, State} ->
			terminate(Req2, Env, Handler, State, {normal, shutdown});
		{Mod, Req2, State} ->
			upgrade(Req2, Env, Handler, State, infinity, run, Mod);
		{Mod, Req2, State, hibernate} ->
			upgrade(Req2, Env, Handler, State, infinity, hibernate, Mod);
		{Mod, Req2, State, Timeout} ->
			upgrade(Req2, Env, Handler, State, Timeout, run, Mod);
		{Mod, Req2, State, Timeout, hibernate} ->
			upgrade(Req2, Env, Handler, State, Timeout, hibernate, Mod)
	catch Class:Reason ->
		Stacktrace = erlang:get_stacktrace(),
		cowboy_req:maybe_reply(Stacktrace, Req),
		erlang:Class([
			{reason, Reason},
			{mfa, {Handler, init, 2}},
			{stacktrace, Stacktrace},
			{req, cowboy_req:to_list(Req)},
			{opts, HandlerOpts}
		])
	end.

handle(Req, Env, Handler, State) ->
	try Handler:handle(Req, State) of
		{ok, Req2, State2} ->
			terminate(Req2, Env, Handler, State2, {normal, shutdown})
	catch Class:Reason ->
		Stacktrace = erlang:get_stacktrace(),
		cowboy_req:maybe_reply(Stacktrace, Req),
		_ = terminate(Req, Env, Handler, State, Reason),
		erlang:Class([
			{reason, Reason},
			{mfa, {Handler, handle, 2}},
			{stacktrace, Stacktrace},
			{req, cowboy_req:to_list(Req)},
			{state, State}
		])
	end.

upgrade(Req, Env, Handler, State, Timeout, Hibernate, Mod) ->
	Mod2 = case Mod of
		long_polling -> cowboy_long_polling;
		rest -> cowboy_rest;
		ws -> cowboy_websocket;
		_ when Mod =/= http -> Mod
	end,
	Mod2:upgrade(Req, Env, Handler, State, Timeout, Hibernate).

-spec terminate(Req, Env, module(), any(), {normal, shutdown} | {error, atom()} | any())
	-> {ok, Req, Env} when Req::cowboy_req:req(), Env::cowboy_middleware:env().
terminate(Req, Env, Handler, State, Reason) ->
	Result = case erlang:function_exported(Handler, terminate, 3) of
		true ->
			try
				Handler:terminate(Reason, cowboy_req:lock(Req), State)
			catch Class:Reason2 ->
				erlang:Class([
					{reason, Reason2},
					{mfa, {Handler, terminate, 3}},
					{stacktrace, erlang:get_stacktrace()},
					{req, cowboy_req:to_list(Req)},
					{state, State},
					{terminate_reason, Reason}
				])
			end;
		false ->
			ok
	end,
	{ok, Req, [{result, Result}|Env]}.
