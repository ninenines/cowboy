-module(rest_empty_resource).
-export([init/3]).

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.
