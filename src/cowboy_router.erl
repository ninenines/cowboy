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

%% @doc Routing middleware.
%%
%% Resolve the handler to be used for the request based on the
%% routing information found in the <em>dispatch</em> environment value.
%% When found, the handler module and associated data are added to
%% the environment as the <em>handler</em> and <em>handler_opts</em> values
%% respectively.
%%
%% If the route cannot be found, processing stops with either
%% a 400 or a 404 reply.
%%
%% @see cowboy_dispatcher
-module(cowboy_router).
-behaviour(cowboy_middleware).

-export([execute/2]).

%% @private
-spec execute(Req, Env)
	-> {ok, Req, Env} | {error, 400 | 404, Req}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req, Env) ->
	{_, Dispatch} = lists:keyfind(dispatch, 1, Env),
	[Host, Path] = cowboy_req:get([host, path], Req),
	case cowboy_dispatcher:match(Dispatch, Host, Path) of
		{ok, Handler, HandlerOpts, Bindings, HostInfo, PathInfo} ->
			Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
			{ok, Req2, [{handler, Handler}, {handler_opts, HandlerOpts}|Env]};
		{error, notfound, host} ->
			{error, 400, Req};
		{error, badrequest, path} ->
			{error, 400, Req};
		{error, notfound, path} ->
			{error, 404, Req}
	end.
