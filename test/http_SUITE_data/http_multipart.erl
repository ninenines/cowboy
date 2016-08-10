%% Feel free to use, reuse and abuse the code in this file.

-module(http_multipart).

-export([init/2]).

init(Req, Opts) ->
	{Result, Req2} = acc_multipart(Req, []),
	{ok, cowboy_req:reply(200, #{}, term_to_binary(Result), Req2), Opts}.

acc_multipart(Req, Acc) ->
	case cowboy_req:read_part(Req) of
		{ok, Headers, Req2} ->
			{ok, Body, Req3} = cowboy_req:read_part_body(Req2),
			acc_multipart(Req3, [{Headers, Body}|Acc]);
		{done, Req2} ->
			{lists:reverse(Acc), Req2}
	end.
