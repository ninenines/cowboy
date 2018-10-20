%% This module crashes on execution

-module(crash_h).

-behaviour(cowboy_handler).

-export([init/2]).

init(_Req, _Opts) ->
	ct_helper:ignore(?MODULE, init, 2),
	error(crash).
