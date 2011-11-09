%% Feel free to use, reuse and abuse the code in this file.

-module(chunked_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Transport, http}, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{ok, Req2} = cowboy_http_req:chunked_reply(200, Req),
	cowboy_http_req:chunk("chunked_handler\r\n", Req2),
	cowboy_http_req:chunk("works fine!", Req2),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.
