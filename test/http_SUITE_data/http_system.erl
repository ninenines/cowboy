%% Feel free to use, reuse and abuse the code in this file.

-module(http_system).

-export([init/2]).

init(Req, Opts) ->
	http_system_tester ! {http_system, self()},
	timer:sleep(100),
	Headers = proplists:get_value(headers, Opts, []),
	Body = proplists:get_value(body, Opts, "http_system"),
	{ok, cowboy_req:reply(200, Headers, Body, Req), Opts}.
