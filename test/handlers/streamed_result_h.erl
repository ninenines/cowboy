-module(streamed_result_h).

-export([init/2]).

init(Req, Opts) ->
	N = list_to_integer(binary_to_list(cowboy_req:binding(n, Req))),
	Interval = list_to_integer(binary_to_list(cowboy_req:binding(interval, Req))),
	chunked(N, Interval, Req, Opts).

chunked(N, Interval, Req0, Opts) ->
	Req = cowboy_req:stream_reply(200, Req0),
	{ok, loop(N, Interval, Req), Opts}.

loop(0, _Interval, Req) ->
	ok = cowboy_req:stream_body("Finished!\n", fin, Req),
	Req;
loop(N, Interval, Req) ->
	ok = cowboy_req:stream_body(iolist_to_binary([integer_to_list(N), <<"\n">>]), nofin, Req),
	timer:sleep(Interval),
	loop(N-1, Interval, Req).
