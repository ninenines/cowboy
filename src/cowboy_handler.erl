%% Copyright (c) 2011-2024, Loïc Hoguin <essen@ninenines.eu>
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

-callback init(Req, any())
	-> {ok | module(), Req, any()}
	| {module(), Req, any(), any()}
	when Req::cowboy_req:req().

-callback terminate(any(), map(), any()) -> ok.
-optional_callbacks([terminate/3]).

-spec execute(Req, Env) -> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env=#{handler := Handler, handler_opts := HandlerOpts}) ->
	try Handler:init(Req, HandlerOpts) of
		{ok, Req2, State} ->
			Result = terminate(normal, Req2, State, Handler),
			{ok, Req2, Env#{result => Result}};
		{Mod, Req2, State} ->
			Mod:upgrade(Req2, Env, Handler, State);
		{Mod, Req2, State, Opts} ->
			Mod:upgrade(Req2, Env, Handler, State, Opts)
	catch Class:Reason:Stacktrace ->
		terminate({crash, Class, Reason}, Req, HandlerOpts, Handler),
		erlang:raise(Class, Reason, Stacktrace)
	end.

-spec terminate(any(), Req | undefined, any(), module()) -> ok when Req::cowboy_req:req().
terminate(Reason, Req, State, Handler) ->
	case erlang:function_exported(Handler, terminate, 3) of
		true ->
			Handler:terminate(Reason, Req, State);
		false ->
			ok
	end.
