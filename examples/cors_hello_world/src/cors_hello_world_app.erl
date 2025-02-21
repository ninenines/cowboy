%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(cors_hello_world_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", toppage_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{
		middlewares => [cors_hello_world_middleware, cowboy_router, cowboy_handler],
		env => #{dispatch => Dispatch}
	}),
	cors_hello_world_sup:start_link().

stop(_State) ->
	ok.
