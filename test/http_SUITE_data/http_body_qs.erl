%% Feel free to use, reuse and abuse the code in this file.

-module(http_body_qs).

-export([init/2]).

init(Req, Opts) ->
	Method = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req),
	{ok, maybe_echo(Method, HasBody, Req), Opts}.

maybe_echo(<<"POST">>, true, Req) ->
	try cowboy_req:read_urlencoded_body(Req) of
		{ok, PostVals, Req2} ->
			echo(proplists:get_value(<<"echo">>, PostVals), Req2)
	catch _:_ ->
		echo(badlength, Req)
	end;
maybe_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, #{}, <<"Missing body.">>, Req);
maybe_echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

echo(badlength, Req) ->
	cowboy_req:reply(413, #{}, <<"POST body bigger than 16000 bytes">>, Req);
echo(undefined, Req) ->
	cowboy_req:reply(400, #{}, <<"Missing echo parameter.">>, Req);
echo(Echo, Req) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, Echo, Req).
