-module(upload_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_, Req, _Opts) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{ok, Headers, Req2} = cowboy_req:part(Req),
	{ok, Data, Req3} = cowboy_req:part_body(Req2),
	{file, <<"inputfile">>, Filename, ContentType, _TE}
		= cow_multipart:form_data(Headers),
	io:format("Received file ~p of content-type ~p as follow:~n~p~n~n",
		[Filename, ContentType, Data]),
	{ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.
