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
	{ok, _} = cowboy:start_https(https, 100, [
		{port, 8443},
		{cacertfile, "priv/ssl/cowboy-ca.crt"},
		{certfile, "priv/ssl/server.crt"},
		{keyfile, "priv/ssl/server.key"}
	], [{env, [{dispatch, Dispatch}]}]),
	ssl_hello_world_sup:start_link().

stop(_State) ->
	ok.
