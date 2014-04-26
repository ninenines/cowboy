%% This module implements a loop handler that reads
%% the request body after sending itself a message,
%% checks that its size is exactly 100000 bytes,
%% then sends a 200 reply back.

-module(loop_handler_body_h).
-behaviour(cowboy_loop_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

init(_, Req, _) ->
	self() ! timeout,
	{loop, Req, undefined, 5000, hibernate}.

info(timeout, Req, State) ->
	{ok, Body, Req2} = cowboy_req:body(Req),
	100000 = byte_size(Body),
	{ok, Req3} = cowboy_req:reply(200, Req2),
	{ok, Req3, State}.

terminate({normal, shutdown}, _, _) ->
	ok.
