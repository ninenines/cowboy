%% Feel free to use, reuse and abuse the code in this file.

-module(rest_hello_world).

%% API.
-export([start/0]).

%% API.

start() ->
	ok = application:start(cowboy),
	ok = application:start(rest_hello_world).
