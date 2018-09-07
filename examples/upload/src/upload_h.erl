%% Feel free to use, reuse and abuse the code in this file.

%% @doc Upload handler.
-module(upload_h).

-export([init/2]).

init(Req, Opts) ->
	{ok, Headers, Req2} = cowboy_req:read_part(Req),
	{ok, Data, Req3} = cowboy_req:read_part_body(Req2),
	{file, <<"inputfile">>, Filename, ContentType}
		= cow_multipart:form_data(Headers),
	io:format("Received file ~p of content-type ~p as follow:~n~p~n~n",
		[Filename, ContentType, Data]),
	{ok, Req3, Opts}.
