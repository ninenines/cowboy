%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(ssl_hello_world_app).
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
	PrivDir = code:priv_dir(ssl_hello_world),
	{ok, _} = cowboy:start_https(https, 100, [
		{port, 8443},
		{cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
		{certfile, PrivDir ++ "/ssl/server.crt"},
		{keyfile, PrivDir ++ "/ssl/server.key"}
	], [{env, [{dispatch, Dispatch}]}]),
	ssl_hello_world_sup:start_link().

stop(_State) ->
	ok.
