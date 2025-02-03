%% This module reads the request body fully and send a 204 response.

-module(read_body_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok, Req} = read_body(Req0),
	{ok, cowboy_req:reply(200, #{}, Req), Opts}.

read_body(Req0) ->
	case cowboy_req:read_body(Req0) of
		{ok, _, Req} -> {ok, Req};
		{more, _, Req} -> read_body(Req)
	end.
