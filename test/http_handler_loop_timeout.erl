%% Feel free to use, reuse and abuse the code in this file.

-module(http_handler_loop_timeout).
-behaviour(cowboy_loop_handler).
-export([init/3, info/3, terminate/2]).

init({_, http}, Req, _) ->
	erlang:send_after(1000, self(), error_timeout),
	{loop, Req, undefined, 500, hibernate}.

info(error_timeout, Req, State) ->
	{ok, Req2} = cowboy_req:reply(500, Req),
	{ok, Req2, State}.

terminate(_, _) ->
	ok.
