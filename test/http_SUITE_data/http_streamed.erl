%% Feel free to use, reuse and abuse the code in this file.

-module(http_streamed).

-export([init/2]).

init(Req, Opts) ->
	Req2 = cowboy_req:set([{resp_state, waiting_stream}], Req),
	Req3 = cowboy_req:chunked_reply(200, Req2),
	timer:sleep(100),
	cowboy_req:chunk("streamed_handler\r\n", Req3),
	timer:sleep(100),
	cowboy_req:chunk("works fine!", Req3),
	{ok, Req3, Opts}.
