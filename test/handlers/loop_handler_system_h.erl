%% This module implements a loop handler that sends
%% a message to loop_system_tester and then waits
%% for a timeout. That process will attempt to use
%% system messages on the process. The result of
%% the request will be a 204 reply.

-module(loop_handler_system_h).

-export([init/2]).
-export([info/3]).
-export([terminate/3]).
-export([code_change/4]).
-export([format_status/2]).

init(Req, _) ->
	loop_tester_system ! {loop_handler_system_h, self()},
	{cowboy_loop, Req, 0, 500}.

info(_Info, Req, State) ->
	{shutdown, cowboy_req:reply(500, Req), State}.

terminate(_, _, _) ->
	ok.

code_change(_OldVsn, Req, _State, State2) ->
	{ok, Req, State2}.

format_status(normal, [_PDict, _Req, State]) ->
	[{data, [{"Handler state", {formatted, State}}]}];
format_status(terminate, [_PDict, _Req, State]) ->
	{formatted, State}.
