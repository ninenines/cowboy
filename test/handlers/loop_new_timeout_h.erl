%% This module implements a loop handler that changes
%% the timeout value to 500ms after the first message
%% then sends itself another message after 1000ms.
%% It is expected to timeout, that is, reply a 299.

-module(loop_new_timeout_h).

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

init(Req, _) ->
	self() ! message,
	{cowboy_loop, Req, 2}.

info(message, Req, State) ->
	erlang:send_after(1000, self(), message),
	{ok, Req, State, 500};
info(timeout, Req, State) ->
	{stop, cowboy_req:reply(<<"299 OK!">>, Req), State}.

terminate(stop, _, _) ->
	ok.
