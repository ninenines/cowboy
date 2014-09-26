-module(rest_empty_resource).

-export([init/2]).

init(Req, Opts) ->
	{rest, Req, Opts}.
