%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(compress_response_app).
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
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{compress, true},
		{env, [{dispatch, Dispatch}]}
	]),
	compress_response_sup:start_link().

stop(_State) ->
	ok.
