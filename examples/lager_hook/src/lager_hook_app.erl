%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(lager_hook_app).
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
		{env, [{dispatch, Dispatch}]}
	]),
	error_logger_lager_hook_h:replace(),
	lager_hook_sup:start_link().

stop(_State) ->
	ok.
