%% Feel free to use, reuse and abuse the code in this file.

-module(http_handler_multipart).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Transport, http}, Req, []) ->
	{ok, Req, {}}.

handle(Req, State) ->
	{Result, Req2} = acc_multipart(Req),
	{ok, Req3} = cowboy_req:reply(200, [], term_to_binary(Result), Req2),
	{ok, Req3, State}.

terminate(_Req, _State) ->
	ok.

acc_multipart(Req) ->
	acc_multipart(cowboy_req:multipart_data(Req), []).

acc_multipart({headers, Headers, Req}, Acc) ->
	acc_multipart(cowboy_req:multipart_data(Req), [{Headers, []}|Acc]);
acc_multipart({body, Data, Req}, [{Headers, BodyAcc}|Acc]) ->
	acc_multipart(cowboy_req:multipart_data(Req), [{Headers, [Data|BodyAcc]}|Acc]);
acc_multipart({end_of_part, Req}, [{Headers, BodyAcc}|Acc]) ->
	acc_multipart(cowboy_req:multipart_data(Req),
		[{Headers, list_to_binary(lists:reverse(BodyAcc))}|Acc]);
acc_multipart({eof, Req}, Acc) ->
	{lists:reverse(Acc), Req}.
