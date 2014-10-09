-module(system_h).

-export([init/2]).

init(Req, {Upgrade, Opt}) ->
	[{<<"from">>, From}] = cowboy_req:parse_qs(Req),
	{Pid, Tag} = binary_to_term(From),
	Pid ! {Tag, self()},
	case Opt of
		run ->
			{Upgrade, Req, state};
		hibernate ->
			{Upgrade, Req, state, hibernate}
	end.
