%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(error_hook_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', []}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]},
		{onresponse, fun error_hook_responder:respond/4}
	]),
	error_hook_sup:start_link().

stop(_State) ->
	ok.
