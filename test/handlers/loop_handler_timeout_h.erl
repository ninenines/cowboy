%% This module implements a loop handler that sends
%% itself a timeout that will intentionally arrive
%% after the HTTP/1.1 request_timeout. The protocol
%% is not supposed to close the connection when a
%% request is ongoing, and therefore this handler
%% will eventually send a 200 reply.

-module(loop_handler_timeout_h).

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

init(Req, _) ->
	erlang:send_after(6000, self(), timeout),
	{cowboy_loop, Req, #{hibernate => true}}.

info(timeout, Req, State) ->
	{stop, cowboy_req:reply(200, #{}, <<"Good!">>, Req), State}.

terminate(stop, _, _) ->
	ok.
