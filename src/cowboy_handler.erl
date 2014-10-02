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
-export([terminate/4]).
-export([code_change/6]).
-export([format_status/5]).

-callback init(Req, any())
	-> {ok | module(), Req, any()}
	| {module(), Req, any(), hibernate}
	| {module(), Req, any(), timeout()}
	| {module(), Req, any(), timeout(), hibernate}
	when Req::cowboy_req:req().
%% @todo optional -callback terminate(terminate_reason(), cowboy_req:req(), state()) -> ok.
%% @todo optional -callback code_change(any(), cowboy_req:req(), any(), any())
%%	-> {ok, cowboy_req:req(), any()}.
%% @todo optional -callback format_status(normal | terminate,
%%		[[{any(), any()}], cowboy_req:req(), any()]) -> any().

-spec execute(Req, Env) -> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env) ->
	{_, Handler} = lists:keyfind(handler, 1, Env),
	{_, HandlerOpts} = lists:keyfind(handler_opts, 1, Env),
	try Handler:init(Req, HandlerOpts) of
		{ok, Req2, State} ->
			Result = terminate(normal, Req2, State, Handler),
			{ok, Req2, [{result, Result}|Env]};
		{Mod, Req2, State} ->
			Mod:upgrade(Req2, Env, Handler, State, infinity, run);
		{Mod, Req2, State, hibernate} ->
			Mod:upgrade(Req2, Env, Handler, State, infinity, hibernate);
		{Mod, Req2, State, Timeout} ->
			Mod:upgrade(Req2, Env, Handler, State, Timeout, run);
		{Mod, Req2, State, Timeout, hibernate} ->
			Mod:upgrade(Req2, Env, Handler, State, Timeout, hibernate)
	catch Class:Reason ->
		Stacktrace = erlang:get_stacktrace(),
		cowboy_req:maybe_reply(Stacktrace, Req),
		terminate({crash, Class, Reason}, Req, HandlerOpts, Handler),
		erlang:exit([
			{class, Class},
			{reason, Reason},
			{mfa, {Handler, init, 2}},
			{stacktrace, Stacktrace},
			{req, cowboy_req:to_list(Req)},
			{opts, HandlerOpts}
		])
	end.

-spec terminate(any(), Req, any(), module()) -> ok when Req::cowboy_req:req().
terminate(Reason, Req, State, Handler) ->
	case erlang:function_exported(Handler, terminate, 3) of
		true ->
			try
				Handler:terminate(Reason, cowboy_req:lock(Req), State)
			catch Class:Reason2 ->
				erlang:exit([
					{class, Class},
					{reason, Reason2},
					{mfa, {Handler, terminate, 3}},
					{stacktrace, erlang:get_stacktrace()},
					{req, cowboy_req:to_list(Req)},
					{state, terminate_state(Req, State, Handler)},
					{terminate_reason, Reason}
				])
			end;
		false ->
			ok
	end.

terminate_state(Req, State, Handler) ->
	case erlang:function_exported(Handler, format_status, 2) of
		true ->
			call_format_status(terminate, get(), Req, State, Handler);
		false ->
			default_format_status(terminate, State)
	end.

-spec code_change(any(), cowboy_req:req(), any(), module(), any(), module())
	-> {ok, cowboy_req:req(), any()}.
code_change(OldVsn, Req, State, Handler, Extra, Handler) ->
	case erlang:function_exported(Handler, code_change, 4) of
		true ->
			handler_code_change(OldVsn, Req, State, Extra, Handler);
		false ->
			{ok, Req, State}
	end;
code_change(_OldVsn, Req, State, _Module, _Extra, _Handler) ->
	{ok, Req, State}.

handler_code_change(OldVsn, Req, State, Extra, Handler) ->
	{ok, _Req2, _State2} = Handler:code_change(OldVsn, Req, State, Extra).

-spec format_status(normal | terminate, [{term(), term()}], cowboy_req:req(),
	any(), module()) -> any().
format_status(Opt, PDict, Req, State, Handler) ->
	case erlang:function_exported(Handler, format_status, 2) of
		true ->
			call_format_status(Opt, PDict, Req, State, Handler);
		false ->
			default_format_status(Opt, State)
	end.

call_format_status(Opt, PDict, Req, State, Handler) ->
	try Handler:format_status(Opt, [PDict, cowboy_req:lock(Req), State]) of
		Status ->
			Status
	catch
		_:_ ->
			default_format_status(Opt, State)
	end.

default_format_status(normal, State) ->
	[{data, [{"Handler state", State}]}];
default_format_status(terminate, State) ->
	State.
