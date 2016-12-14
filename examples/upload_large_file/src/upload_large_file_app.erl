%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(upload_large_file_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, upload_large_file, "index.html"}},
			{"/upload", upload_large_file_handler, []}
		]}
	]),
	
	{ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
	
	upload_large_file_sup:start_link().

stop(_State) ->
	ok.
