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
			{"/", toppage_h, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h]
	}),
	compress_response_sup:start_link().

stop(_State) ->
	ok.
