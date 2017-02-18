%% Feel free to use, reuse and abuse the code in this file.

-module(http_loop_stream_recv).

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

init(Req, _) ->
	receive after 100 -> ok end,
	self() ! stream,
	{cowboy_loop, Req, undefined}.

info(stream, Req, undefined) ->
	stream(Req, 1, <<>>).

stream(Req, ID, Acc) ->
	case cowboy_req:read_body(Req) of
		{ok, <<>>, Req2} ->
			{stop, cowboy_req:reply(200, Req2), undefined};
		{_, Data, Req2} ->
			parse_id(Req2, ID, << Acc/binary, Data/binary >>)
	end.

parse_id(Req, ID, Data) ->
	case Data of
		<< ID:32, Rest/bits >> ->
			parse_id(Req, ID + 1, Rest);
		_ ->
			stream(Req, ID, Data)
	end.

terminate(stop, _, _) ->
	ok.
