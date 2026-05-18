%% This handler replies with a fixed-size body. The default size is
%% large enough to leave unacknowledged data in the kernel send buffer
%% when the peer is not reading.

-module(long_body_h).

-export([init/2]).

init(Req, Opts) ->
	Size = maps:get(size, Opts, 32768),
	{ok, cowboy_req:reply(200, #{}, binary:copy(<<"x">>, Size), Req), Opts}.
