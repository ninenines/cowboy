%% This module crashes immediately.

-module(crash_h).

-behaviour(cowboy_handler).

-export([init/2]).

-spec init(_, _) -> no_return().
init(_, no_reply) ->
	ct_helper:ignore(?MODULE, init, 2),
	error(crash);
init(Req, reply) ->
	_ = cowboy_req:reply(200, Req),
	ct_helper:ignore(?MODULE, init, 2),
	error(crash).
