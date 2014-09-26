%% Feel free to use, reuse and abuse the code in this file.

-module(http_stream_body).

-export([init/2]).
-export([handle/2]).

init(Req, Opts) ->
	{http, Req, Opts}.

handle(Req, State) ->
	Body = proplists:get_value(body, State, "http_handler_stream_body"),
	Reply = proplists:get_value(reply, State),
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
	{ok, cowboy_req:reply(200, Req2), State}.
