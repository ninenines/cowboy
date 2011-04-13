%% Feel free to use, reuse and abuse the code in this file.

-module(http_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-record(state, {headers, body}).

init({_Transport, http}, Req, Opts) ->
	Headers = proplists:get_value(headers, Opts, []),
	Body = proplists:get_value(body, Opts, "http_handler"),
	{ok, Req, #state{headers=Headers, body=Body}}.

handle(Req, State=#state{headers=Headers, body=Body}) ->
	{ok, Req2} = cowboy_http_req:reply(200, Headers, Body, Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.
