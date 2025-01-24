%% This module is the fastest way of producing a Hello world!

-module(stream_hello_h).

-export([init/3]).
-export([terminate/3]).

init(_, _, State) ->
	{[
		{response, 200, #{<<"content-length">> => <<"12">>}, <<"Hello world!">>},
		stop
	], State}.

terminate(_, _, _) ->
	ok.
