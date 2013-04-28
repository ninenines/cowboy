%% Feel free to use, reuse and abuse the code in this file.

-module(http_stream_body).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-record(state, {headers, body, reply}).

init({_Transport, http}, Req, Opts) ->
	Headers = proplists:get_value(headers, Opts, []),
	Body = proplists:get_value(body, Opts, "http_handler_stream_body"),
	Reply = proplists:get_value(reply, Opts),
	{ok, Req, #state{headers=Headers, body=Body, reply=Reply}}.

handle(Req, State=#state{headers=_Headers, body=Body, reply=Reply}) ->
	SFun = fun(Socket, Transport) -> Transport:send(Socket, Body) end,
	Req2 = case Reply of
		set_resp ->
			SLen = iolist_size(Body),
			cowboy_req:set_resp_body_fun(SLen, SFun, Req);
		set_resp_close ->
			cowboy_req:set_resp_body_fun(SFun, Req);
		set_resp_chunked ->
			%% Here Body should be a list of chunks, not a binary.
			SFun2 = fun(SendFun) -> lists:foreach(SendFun, Body) end,
			cowboy_req:set_resp_body_fun(chunked, SFun2, Req)
	end,
	{ok, Req3} = cowboy_req:reply(200, Req2),
	{ok, Req3, State}.

terminate(_, _, _) ->
	ok.
