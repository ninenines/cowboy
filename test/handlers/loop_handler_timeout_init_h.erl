%% This module implements a loop handler that reads
%% the request query for a timeout value, then sends
%% itself a message after 1000ms. It replies a 200 when
%% the message does not timeout and a 299 otherwise.

-module(loop_handler_timeout_init_h).

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

init(Req, _) ->
	#{timeout := Timeout} = cowboy_req:match_qs([{timeout, int}], Req),
	erlang:send_after(500, self(), message),
	{cowboy_loop, Req, undefined, Timeout}.

info(message, Req, State) ->
	{stop, cowboy_req:reply(200, Req), State};
info(timeout, Req, State) ->
	{stop, cowboy_req:reply(<<"299 OK!">>, Req), State}.

terminate(stop, _, _) ->
	ok.
