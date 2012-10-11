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
			{['...'], cowboy_static, [
				{directory, {priv_dir, static, []}},
				{mimetypes, {fun mimetypes:path_to_mimes/2, default}}
			]} 
		]}
	],
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{dispatch, Dispatch}
	]),
	static_sup:start_link().

stop(_State) ->
	ok.
