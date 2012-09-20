%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	{HasBody, Req3} = cowboy_req:has_body(Req2),
	{ok, Req4} = maybe_echo(Method, HasBody, Req3),
	{ok, Req4, State}.

maybe_echo(<<"POST">>, true, Req) ->
	{ok, PostVals, Req2} = cowboy_req:body_qs(Req),
	Echo = proplists:get_value(<<"echo">>, PostVals),
	echo(Echo, Req2);
maybe_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
echo(Echo, Req) ->
	cowboy_req:reply(200,
		[{<<"Content-Encoding">>, <<"utf-8">>}], Echo, Req).

terminate(_Req, _State) ->
	ok.
