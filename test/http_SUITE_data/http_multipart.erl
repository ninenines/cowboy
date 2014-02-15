%% Feel free to use, reuse and abuse the code in this file.

-module(http_multipart).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({_Transport, http}, Req, []) ->
	{ok, Req, {}}.

handle(Req, State) ->
	{Result, Req2} = acc_multipart(Req, []),
	{ok, Req3} = cowboy_req:reply(200, [], term_to_binary(Result), Req2),
	{ok, Req3, State}.

terminate(_, _, _) ->
	ok.

acc_multipart(Req, Acc) ->
	case cowboy_req:part(Req) of
		{ok, Headers, Req2} ->
			{ok, Body, Req3} = cowboy_req:part_body(Req2),
			acc_multipart(Req3, [{Headers, Body}|Acc]);
		{done, Req2} ->
			{lists:reverse(Acc), Req2}
	end.
