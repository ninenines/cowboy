-module(system_default_h).

-export([init/2]).

init(Req, Upgrade) ->
	system_default_tester ! {?MODULE, self()},
	{Upgrade, Req, undefined, 500}.
