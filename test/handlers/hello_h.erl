%% This module sends a hello world response.

-module(hello_h).

-export([init/2]).

init(Req, Opts) ->
	{ok, cowboy_req:reply(200, #{}, <<"Hello world!">>, Req), Opts}.
