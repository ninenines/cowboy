%% This module implements a loop handler that first
%% sets a timeout, then hibernates, then ensures
%% that the timeout initially set no longer triggers.
%% If everything goes fine a 200 is returned. If the
%% timeout triggers again a 299 is.

-module(loop_handler_timeout_hibernate_h).

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

init(Req, _) ->
	self() ! message1,
	{cowboy_loop, Req, undefined, 100}.

info(message1, Req, State) ->
	erlang:send_after(200, self(), message2),
	{ok, Req, State, hibernate};
info(message2, Req, State) ->
	erlang:send_after(200, self(), message3),
	%% Don't set a timeout now.
	{ok, Req, State};
info(message3, Req, State) ->
	{stop, cowboy_req:reply(200, Req), State};
info(timeout, Req, State) ->
	{stop, cowboy_req:reply(<<"299 OK!">>, Req), State}.

terminate(stop, _, _) ->
	ok.
