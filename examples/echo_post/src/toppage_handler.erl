%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(toppage_handler).

-export([init/2]).

init(Req, Opts) ->
	Method = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req),
	Req2 = maybe_echo(Method, HasBody, Req),
	{ok, Req2, Opts}.

maybe_echo(<<"POST">>, true, Req) ->
	{ok, PostVals, Req2} = cowboy_req:body_qs(Req),
	Echo = proplists:get_value(<<"echo">>, PostVals),
	echo(Echo, Req2),
	Req2;
maybe_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req),
	Req;
maybe_echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req),
	Req.

echo(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
echo(Echo, Req) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, Echo, Req).
