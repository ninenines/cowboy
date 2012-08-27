%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(echo_post_app).
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
	echo_post_sup:start_link().

stop(_State) ->
	ok.
