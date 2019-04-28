%% This module implements a loop handler that reads
%% 1000 bytes of the request body after sending itself
%% a message, then terminates the stream.

-module(loop_handler_abort_h).

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

init(Req, _) ->
	self() ! timeout,
	{cowboy_loop, Req, undefined, hibernate}.

info(timeout, Req0, State) ->
	{_Status, Body, Req} = cowboy_req:read_body(Req0, #{length => 1000}),
	1000 = byte_size(Body),
	{stop, cowboy_req:reply(200, Req), State}.

terminate(stop, _, _) ->
	ok.
