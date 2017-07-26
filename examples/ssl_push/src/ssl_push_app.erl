%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(ssl_push_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
      {"/static/[...]", cowboy_static, {priv_dir,  ssl_push, "static_files"}},
			{"/", toppage_handler, []}
		]}
	]),
	PrivDir = code:priv_dir(ssl_push),
	{ok, _} = cowboy:start_tls(https, [
		{port, 8443},
		{cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
		{certfile, PrivDir ++ "/ssl/server.crt"},
		{keyfile, PrivDir ++ "/ssl/server.key"}
	], #{env => #{dispatch => Dispatch}}),
	ssl_push_sup:start_link().

stop(_State) ->
	ok.
