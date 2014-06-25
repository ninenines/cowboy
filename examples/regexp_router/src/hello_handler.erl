%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello handler.
-module(hello_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{User, Req2} = cowboy_req:binding(user, Req),
	HelloText = case User of
		undefined -> <<"Hello everyone!!!">>;
		_ -> <<"Hello, ", User/binary ,"!">>
	end,
	{ok, Req3} = cowboy_req:reply(200, [], HelloText, Req2),
	{ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.
