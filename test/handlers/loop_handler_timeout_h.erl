%% This module implements a loop handler that sends
%% itself a timeout that will intentionally arrive
%% too late, as it configures itself to only wait
%% 200ms before closing the connection in init/2.
%% This results in a 204 reply being sent back by Cowboy.

-module(loop_handler_timeout_h).

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

init(Req, _) ->
	erlang:send_after(1000, self(), timeout),
	{cowboy_loop, Req, undefined, hibernate}.

info(timeout, Req, State) ->
	{stop, cowboy_req:reply(500, Req), State}.

terminate(timeout, _, _) ->
	ok.
