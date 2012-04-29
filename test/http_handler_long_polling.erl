%% Feel free to use, reuse and abuse the code in this file.

-module(http_handler_long_polling).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, info/3, terminate/2]).

init({_Transport, http}, Req, _Opts) ->
	erlang:send_after(500, self(), timeout),
	{loop, Req, 5, 5000, hibernate}.

handle(_Req, _State) ->
	exit(badarg).

info(timeout, Req, 0) ->
	{ok, Req2} = cowboy_http_req:reply(102, Req),
	{ok, Req2, 0};
info(timeout, Req, State) ->
	erlang:send_after(500, self(), timeout),
	{loop, Req, State - 1, hibernate}.

terminate(_Req, _State) ->
	ok.
