%% Feel free to use, reuse and abuse the code in this file.

-module(streamed_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({_Transport, http}, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{ok, Req2} = cowboy_req:streamed_reply(200, Req),
	timer:sleep(100),
	cowboy_req:stream("streamed_handler\r\n", Req2),
	timer:sleep(100),
	cowboy_req:stream("works fine!", Req2),
	{ok, Req2, State}.

terminate(_, _, _) ->
	ok.
