%% Feel free to use, reuse and abuse the code in this file.

-module(http_chunked).

-export([init/2]).

init(Req, Opts) ->
	Req2 = cowboy_req:chunked_reply(200, Req),
	%% Try an empty chunk to make sure the stream doesn't get closed.
	cowboy_req:chunk([<<>>], Req2),
	timer:sleep(100),
	cowboy_req:chunk("chunked_handler\r\n", Req2),
	timer:sleep(100),
	cowboy_req:chunk("works fine!", Req2),
	{ok, Req2, Opts}.
