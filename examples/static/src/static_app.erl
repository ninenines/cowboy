%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(static_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = [
		{'_', [
			{['...'], cowboy_http_static, [
				{directory, {priv_dir, static, []}}
			]} 
		]}
	],
	{ok, _} = cowboy:start_listener(http, 100,
		cowboy_tcp_transport, [{port, 8080}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	static_sup:start_link().

stop(_State) ->
	ok.
