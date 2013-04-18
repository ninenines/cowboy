%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	% fail here becouse of wrong pattern matching
	ok = cowboy_req:reply(200, [], <<"Hello world!">>, Req),
	{ok, Req, State}.

terminate(_Reason, _Req, _State) ->
	ok.
