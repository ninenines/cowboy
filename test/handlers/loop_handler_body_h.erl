%% This module implements a loop handler that reads
%% the request body after sending itself a message,
%% checks that its size is exactly 100000 bytes,
%% then sends a 200 reply back.

-module(loop_handler_body_h).

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

init(Req, _) ->
	self() ! timeout,
	{cowboy_loop, Req, undefined, hibernate}.

info(timeout, Req0, State) ->
	{ok, Body, Req} = cowboy_req:read_body(Req0),
	100000 = byte_size(Body),
	{stop, cowboy_req:reply(200, Req), State}.

terminate(stop, _, _) ->
	ok.
