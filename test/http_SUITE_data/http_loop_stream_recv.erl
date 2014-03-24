%% Feel free to use, reuse and abuse the code in this file.

-module(http_loop_stream_recv).
-export([init/3]).
-export([info/3]).
-export([terminate/3]).

init({_, http}, Req, _) ->
	receive after 100 -> ok end,
	self() ! stream,
	{loop, Req, undefined, 100}.

info(stream, Req, undefined) ->
	stream(Req, 1, <<>>).

stream(Req, ID, Acc) ->
	case cowboy_req:stream_body(Req) of
		{ok, Data, Req2} ->
			parse_id(Req2, ID, << Acc/binary, Data/binary >>);
		{done, Req2} ->
			{ok, Req3} = cowboy_req:reply(200, Req2),
			{ok, Req3, undefined}
	end.

parse_id(Req, ID, Data) ->
	case Data of
		<< ID:32, Rest/bits >> ->
			parse_id(Req, ID + 1, Rest);
		_ ->
			stream(Req, ID, Data)
	end.

terminate({normal, shutdown}, _, _) ->
	ok.
