%% This module implements a loop handler for long-polling.
%% It starts by sending itself a message after 200ms,
%% then sends another after that for a total of 3 messages.
%% When it receives the last message, it sends a 102 reply back.

-module(long_polling_h).

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

init(Req, _) ->
	erlang:send_after(200, self(), timeout),
	{cowboy_loop, Req, 2, hibernate}.

info(timeout, Req, 0) ->
	%% Send an unused status code to make sure there's no
	%% conflict with whatever Cowboy may send itself.
	{stop, cowboy_req:reply(<<"299 OK!">>, Req), 0};
info(timeout, Req, Count) ->
	erlang:send_after(200, self(), timeout),
	{ok, Req, Count - 1, hibernate}.

terminate(stop, _, 0) ->
	ok;
terminate({error, overflow}, _, _) ->
	ok.
