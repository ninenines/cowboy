%% Feel free to use, reuse and abuse the code in this file.

-module(http_multipart_stream).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init(_, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	Req2 = multipart(Req),
	{ok, Req3} = cowboy_req:reply(200, Req2),
	{ok, Req3, State}.

terminate(_, _, _) ->
	ok.

multipart(Req) ->
	case cowboy_req:part(Req) of
		{ok, [{<<"content-length">>, BinLength}], Req2} ->
			Length = list_to_integer(binary_to_list(BinLength)),
			{Length, Req3} = stream_body(Req2, 0),
			multipart(Req3);
		{done, Req2} ->
			Req2
	end.

stream_body(Req, N) ->
	case cowboy_req:part_body(Req) of
		{ok, Data, Req2} ->
			{N + byte_size(Data), Req2};
		{more, Data, Req2} ->
			stream_body(Req2, N + byte_size(Data))
	end.
