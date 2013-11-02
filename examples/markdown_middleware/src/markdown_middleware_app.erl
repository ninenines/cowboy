%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(markdown_middleware_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/[...]", cowboy_static, {priv_dir, markdown_middleware, ""}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]},
		{middlewares, [cowboy_router, markdown_converter, cowboy_handler]}
	]),
	markdown_middleware_sup:start_link().

stop(_State) ->
	ok.
