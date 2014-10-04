-module(system_default_handler_h).

-export([init/2]).

init(Req, Upgrade) ->
	system_default_handler_tester ! {?MODULE, self()},
	{Upgrade, Req, undefined, 500}.
