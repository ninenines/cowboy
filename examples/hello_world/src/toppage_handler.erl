%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(200, [], <<"Hello world!">>, Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.
