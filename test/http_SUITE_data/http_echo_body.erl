%% Feel free to use, reuse and abuse the code in this file.

-module(http_echo_body).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({_, http}, Req, _) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	true = cowboy_req:has_body(Req),
	{ok, Req3} = case cowboy_req:body(Req, [{length, 1000000}]) of
		{ok, Body, Req2} -> handle_body(Req2, Body);
		{more, _, Req2} -> handle_badlength(Req2)
	end,
	{ok, Req3, State}.

handle_badlength(Req) ->
	{ok, Req2} = cowboy_req:reply(413, [], <<"Request entity too large">>, Req),
	{ok, Req2}.

handle_body(Req, Body) ->
	{Size, Req2} = cowboy_req:body_length(Req),
	Size = byte_size(Body),
	{ok, Req3} = cowboy_req:reply(200, [], Body, Req2),
	{ok, Req3}.

terminate(_, _, _) ->
	ok.
