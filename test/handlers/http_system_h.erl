%% Feel free to use, reuse and abuse the code in this file.

-module(http_system_h).

-export([init/2]).

init(Req, Opts) ->
	[{<<"from">>, From}] = cowboy_req:parse_qs(Req),
	{Pid, Tag} = binary_to_term(From),
	Pid ! {Tag, self()},
	timer:sleep(500),
	{ok, Req, Opts}.
