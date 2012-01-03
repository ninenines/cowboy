%% Feel free to use, reuse and abuse the code in this file.

-module(http_handler_stream_body).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-record(state, {headers, body, reply}).

init({_Transport, http}, Req, Opts) ->
	Headers = proplists:get_value(headers, Opts, []),
	Body = proplists:get_value(body, Opts, "http_handler_stream_body"),
	Reply = proplists:get_value(reply, Opts),
	{ok, Req, #state{headers=Headers, body=Body, reply=Reply}}.

handle(Req, State=#state{headers=_Headers, body=Body, reply=set_resp}) ->
	{ok, Transport, Socket} = cowboy_http_req:transport(Req),
	SFun = fun() -> Transport:send(Socket, Body), sent end,
	SLen = iolist_size(Body),
	{ok, Req2} = cowboy_http_req:set_resp_body_fun(SLen, SFun, Req),
	{ok, Req3} = cowboy_http_req:reply(200, Req2),
	{ok, Req3, State}.

terminate(_Req, _State) ->
	ok.
