%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(spdy_hello_world_app).
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
	PrivDir = code:priv_dir(spdy_hello_world),
	{ok, _} = cowboy:start_spdy(spdy, 100, [
		{port, 8443},
		{cacertfile, PrivDir ++ "/spdy/cowboy-ca.crt"},
		{certfile, PrivDir ++ "/spdy/server.crt"},
		{keyfile, PrivDir ++ "/spdy/server.key"}
	], [{env, [{dispatch, Dispatch}]}]),
	spdy_hello_world_sup:start_link().

stop(_State) ->
	ok.
