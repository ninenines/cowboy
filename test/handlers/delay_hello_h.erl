%% This module sends a hello world response after a delay.

-module(delay_hello_h).

-export([init/2]).

init(Req, Delay) ->
	timer:sleep(Delay),
	{ok, cowboy_req:reply(200, #{}, <<"Hello world!">>, Req), Delay}.
