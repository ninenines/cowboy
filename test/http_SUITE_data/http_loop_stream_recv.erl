%% Feel free to use, reuse and abuse the code in this file.

-module(http_loop_stream_recv).
-export([init/3]).
-export([info/3]).
-export([terminate/3]).

init({_, http}, Req, _) ->
	receive after 100 -> ok end,
	self() ! stream,
	{loop, Req, 1, 100}.

info(stream, Req, Id) ->
	case stream_next(Req) of
		{ok, Id, Req2} ->
			info(stream, Req2, Id+1);
		{done, Req2} ->
			{ok, Req3} = cowboy_req:reply(200, Req2),
			{ok, Req3, Id}
	end.

terminate({normal, shutdown}, _, _) ->
	ok.

stream_next(Req) ->
	stream_next(<<>>, Req).

stream_next(Buffer, Req) ->
	case cowboy_req:stream_body(Req) of
		{ok, Packet, Req2} ->
			case <<Buffer/binary, Packet/binary>> of
				<<Id:32>> ->
					{ok, Id, Req2};
				Buffer2 when byte_size(Buffer2) < 4 ->
					stream_next(Buffer2, Req2);
				_InvalidBuffer ->
					{error, invalid_chunk}
			end;
		Other ->
			Other
	end.
