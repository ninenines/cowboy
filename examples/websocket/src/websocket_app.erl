%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(websocket_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, [
				{directory, {priv_dir, websocket, []}},
				{file, <<"index.html">>},
				{mimetypes, [{<<".html">>, [<<"text/html">>]}]}
			]},
			{"/websocket", ws_handler, []},
			{"/static/[...]", cowboy_static, [
				{directory, {priv_dir, websocket, [<<"static">>]}},
				{mimetypes, [{<<".js">>, [<<"application/javascript">>]}]}
			]}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]),
	websocket_sup:start_link().

stop(_State) ->
	ok.
