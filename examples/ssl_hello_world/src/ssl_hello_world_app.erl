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
			{"/", toppage_h, []}
		]}
	]),
	PrivDir = code:priv_dir(ssl_hello_world),
	{ok, _} = cowboy:start_tls(https, [
		{port, 8443},
		{certfile, PrivDir ++ "/ssl/cert.pem"},
		{keyfile, PrivDir ++ "/ssl/key.pem"}
	], #{env => #{dispatch => Dispatch}}),
	ssl_hello_world_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(https).
