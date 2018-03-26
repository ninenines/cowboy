%% Feel free to use, reuse and abuse the code in this file.

-module(http_handler).

-export([init/2]).

init(Req, Opts) ->
	Headers = proplists:get_value(headers, Opts, #{}),
	Body = proplists:get_value(body, Opts, "http_handler"),
	{ok, cowboy_req:reply(200, Headers, Body, Req), Opts}.
