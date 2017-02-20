-module(rest_empty_resource).

-export([init/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.
