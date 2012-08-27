%% Feel free to use, reuse and abuse the code in this file.

-module(http_handler_echo_body).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_, http}, Req, _) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{true, Req1} = cowboy_req:has_body(Req),
	{ok, Body, Req2} = cowboy_req:body(Req1),
	{Size, Req3} = cowboy_req:body_length(Req2),
	Size = byte_size(Body),
	{ok, Req4} = cowboy_req:reply(200, [], Body, Req3),
	{ok, Req4, State}.

terminate(_, _) ->
	ok.
