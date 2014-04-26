%% This module implements a loop handler for long-polling.
%% It starts by sending itself a message after 200ms,
%% then sends another after that for a total of 3 messages.
%% When it receives the last message, it sends a 102 reply back.

-module(long_polling_h).
-behaviour(cowboy_loop_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

init(_, Req, _) ->
	erlang:send_after(200, self(), timeout),
	{loop, Req, 2, 5000, hibernate}.

info(timeout, Req, 0) ->
	{ok, Req2} = cowboy_req:reply(102, Req),
	{ok, Req2, 0};
info(timeout, Req, Count) ->
	erlang:send_after(200, self(), timeout),
	{loop, Req, Count - 1, hibernate}.

terminate({normal, shutdown}, _, 0) ->
	ok;
terminate({error, overflow}, _, _) ->
	ok.
