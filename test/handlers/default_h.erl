%% This module does not do anything.

-module(default_h).

-export([init/2]).

init(Req, Opts) ->
	{ok, Req, Opts}.
