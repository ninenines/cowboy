%% Feel free to use, reuse and abuse the code in this file.

-module(websocket).

%% API.
-export([start/0]).

%% %% API.
%% start() ->
%%     io:format("dbg start~n"),
%%     ok = application:start(ranch),
%%     application:start(cowboy),
%%     application:start(websocket).


start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(websocket).
