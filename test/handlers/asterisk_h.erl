%% This module echoes back the value the test is interested in.

-module(asterisk_h).

-export([init/2]).

init(Req, Opts) ->
	echo(cowboy_req:header(<<"x-echo">>, Req), Req, Opts).

echo(undefined, Req, Opts) ->
	{ok, cowboy_req:reply(200, Req), Opts};
echo(What, Req, Opts) ->
	F = binary_to_atom(What, latin1),
	Value = case cowboy_req:F(Req) of
		V when is_integer(V) -> integer_to_binary(V);
		V -> V
	end,
	{ok, cowboy_req:reply(200, #{}, Value, Req), Opts}.
