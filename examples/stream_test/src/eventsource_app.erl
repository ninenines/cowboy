%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(eventsource_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/eventsource", eventsource_handler, []},
			{"/", cowboy_static, {priv_file, eventsource, "index.html"}}
		]}
	]),
  Streams =  [eventsource_dummy_stream, cowboy_stream_h],
	{ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{
    stream_handlers => Streams,
		env => #{dispatch => Dispatch}
	}),
	eventsource_sup:start_link().

stop(_State) ->
	ok.
