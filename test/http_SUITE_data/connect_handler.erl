%% Feel free to use, reuse and abuse the code in this file.

-module(connect_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init(_Trans, Req, _Opts) ->
	{ok, Req, nostate}.

handle(Req, nostate) ->
	{ReqMethod, _} = cowboy_req:method(Req),
	Host = cowboy_req:get(host, Req),
	{ok, Req2} = cowboy_req:reply(200, [], [ReqMethod, Host], Req),
	{ok, Req2, nostate}.

terminate(_, _, _) ->
	ok.
