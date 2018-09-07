%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(file_server_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/[...]", cowboy_static, {priv_dir, file_server, "", [
				{mimetypes, cow_mimetypes, all},
				{dir_handler, directory_h}
			]}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch},
		middlewares => [cowboy_router, directory_lister, cowboy_handler]
	}),
	file_server_sup:start_link().

stop(_State) ->
	ok.
