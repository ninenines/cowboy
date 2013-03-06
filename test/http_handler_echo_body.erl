%% Feel free to use, reuse and abuse the code in this file.

-module(http_handler_echo_body).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({_, http}, Req, _) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	true = cowboy_req:has_body(Req),
	{ok, Req3} = case cowboy_req:body(1000000, Req) of
		{error, chunked} -> handle_chunked(Req);
		{error, badlength} -> handle_badlength(Req);
		{ok, Body, Req2} -> handle_body(Req2, Body)
	end,
	{ok, Req3, State}.

handle_chunked(Req) ->
	{ok, Data, Req2} = read_body(Req, <<>>, 1000000),
	{ok, Req3} = cowboy_req:reply(200, [], Data, Req2),
	{ok, Req3}.

handle_badlength(Req) ->
	{ok, Req2} = cowboy_req:reply(413, [], <<"Request entity too large">>, Req),
	{ok, Req2}.

handle_body(Req, Body) ->
	{Size, Req2} = cowboy_req:body_length(Req),
	Size = byte_size(Body),
	{ok, Req3} = cowboy_req:reply(200, [], Body, Req2),
	{ok, Req3}.

terminate(_, _, _) ->
	ok.

% Read chunked request content
read_body(Req, Acc, BodyLengthRemaining) ->
	case cowboy_req:stream_body(Req) of
		{ok, Data, Req2} ->
			BodyLengthRem = BodyLengthRemaining - byte_size(Data),
			read_body(Req2, << Acc/binary, Data/binary >>, BodyLengthRem);
		{done, Req2} ->
			{ok, Acc, Req2}
	end.
