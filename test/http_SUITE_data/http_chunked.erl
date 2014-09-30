%% Feel free to use, reuse and abuse the code in this file.

-module(http_chunked).

-export([init/2]).

init(Req, Opts) ->
	Req2 = cowboy_req:chunked_reply(200, Req),
	timer:sleep(100),
	cowboy_req:chunk("chunked_handler\r\n", Req2),
	timer:sleep(100),
	cowboy_req:chunk("works fine!", Req2),
	{ok, Req2, Opts}.
