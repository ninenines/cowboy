%% This module implements a loop handler that reports
%% its pid to its tester and then waits 500 milliseconds.

-module(loop_system_h).

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

init(Req, _) ->
	[{<<"from">>, From}] = cowboy_req:parse_qs(Req),
	{Pid, Tag} = binary_to_term(From),
	Pid ! {Tag, self()},
	{cowboy_loop, Req, state, 500}.

info(_Info, Req, State) ->
	{ok, Req, State}.

terminate(timeout, _, _) ->
	ok.
