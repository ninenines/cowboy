%% Feel free to use, reuse and abuse the code in this file.

-module(http_multipart_stream).

-export([init/2]).

init(Req, Opts) ->
	Req2 = multipart(Req),
	{ok, cowboy_req:reply(200, Req2), Opts}.

multipart(Req) ->
	case cowboy_req:read_part(Req) of
		{ok, [{<<"content-length">>, BinLength}], Req2} ->
			Length = binary_to_integer(BinLength),
			{Length, Req3} = stream_body(Req2, 0),
			multipart(Req3);
		{done, Req2} ->
			Req2
	end.

stream_body(Req, N) ->
	case cowboy_req:read_part_body(Req) of
		{ok, Data, Req2} ->
			{N + byte_size(Data), Req2};
		{more, Data, Req2} ->
			stream_body(Req2, N + byte_size(Data))
	end.
