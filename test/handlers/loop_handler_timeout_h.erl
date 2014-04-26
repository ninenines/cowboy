%% This module implements a loop handler that sends
%% itself a timeout that will intentionally arrive
%% too late, as it configures itself to only wait
%% 200ms before closing the connection in init/3.
%% This results in a 204 reply being sent back by Cowboy.

-module(loop_handler_timeout_h).
-behaviour(cowboy_loop_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

init(_, Req, _) ->
	erlang:send_after(1000, self(), timeout),
	{loop, Req, undefined, 200, hibernate}.

info(timeout, Req, State) ->
	{ok, Req2} = cowboy_req:reply(500, Req),
	{ok, Req2, State}.

terminate({normal, timeout}, _, _) ->
	ok.
