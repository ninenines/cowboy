%% Feel free to use, reuse and abuse the code in this file.

-module(http_streamed).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({_Transport, http}, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	Req2 = cowboy_req:set([{resp_state, waiting_stream}], Req),
	{ok, Req3} = cowboy_req:chunked_reply(200, Req2),
	timer:sleep(100),
	cowboy_req:chunk("streamed_handler\r\n", Req3),
	timer:sleep(100),
	cowboy_req:chunk("works fine!", Req3),
	{ok, Req3, State}.

terminate(_, _, _) ->
	ok.
