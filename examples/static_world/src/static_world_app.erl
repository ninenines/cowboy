%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(static_world_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, static_world, "index.html"}},
			{"/[...]", cowboy_static, {priv_dir, static_world, "",
				[{mimetypes, cow_mimetypes, all}]}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
	static_world_sup:start_link().

stop(_State) ->
	ok.
