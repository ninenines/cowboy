%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(rest_hello_world_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = [
		{'_', [
			{[], toppage_handler, []}
		]}
	],
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{dispatch, Dispatch}
	]),
	rest_hello_world_sup:start_link().

stop(_State) ->
	ok.
