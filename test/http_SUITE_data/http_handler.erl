%% Feel free to use, reuse and abuse the code in this file.

-module(http_handler).

-export([init/2]).
-export([handle/2]).

init(Req, Opts) ->
	{http, Req, Opts}.

handle(Req, State) ->
	Headers = proplists:get_value(headers, State, []),
	Body = proplists:get_value(body, State, "http_handler"),
	{ok, cowboy_req:reply(200, Headers, Body, Req), State}.
