%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(toppage_h).

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	#{echo := Echo} = cowboy_req:match_qs([{echo, [], undefined}], Req0),
	Req = echo(Method, Echo, Req0),
	{ok, Req, Opts}.

echo(<<"GET">>, undefined, Req) ->
	cowboy_req:reply(400, #{}, <<"Missing echo parameter.">>, Req);
echo(<<"GET">>, Echo, Req) ->
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, Echo, Req);
echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).
