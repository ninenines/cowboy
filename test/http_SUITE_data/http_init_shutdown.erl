%% Feel free to use, reuse and abuse the code in this file.

-module(http_init_shutdown).

-export([init/2]).

init(Req, _) ->
	Req2 = cowboy_req:reply(<<"666 Init Shutdown Testing">>,
		[{<<"connection">>, <<"close">>}], Req),
	{shutdown, Req2, undefined}.
