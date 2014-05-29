%% Feel free to use, reuse and abuse the code in this file.

-module(http_push).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-record(state, {headers, body}).

init({_Transport, http}, Req, Opts) ->
	Headers = proplists:get_value(headers, Opts, []),
	Body = proplists:get_value(body, Opts, "http_handler"),
	{ok, Req, #state{headers=Headers, body=Body}}.

handle(Req, State=#state{headers=Headers, body=Body}) ->
    io:format("handle push~n", []),
    {ok, _Req2} = cowboy_req:push_reply(200, <<"/push/data">>, Headers, <<"data">>, Req),
    timer:sleep(3000),
	{ok, Req3} = cowboy_req:reply(200, Headers, Body, Req),
	{ok, Req3, State}.

terminate(_, _, _) ->
	ok.
