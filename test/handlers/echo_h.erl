%% This module echoes back the value the test is interested in.

-module(echo_h).

-export([init/2]).

init(Req, Opts) ->
	echo(cowboy_req:binding(key, Req), Req, Opts).

echo(What, Req, Opts) ->
	F = binary_to_atom(What, latin1),
	Value = case cowboy_req:F(Req) of
		V when is_integer(V) -> integer_to_binary(V);
		V when is_atom(V) -> atom_to_binary(V, latin1);
		V when is_list(V); is_tuple(V) -> io_lib:format("~p", [V]);
		V -> V
	end,
	cowboy_req:reply(200, #{}, Value, Req),
	{ok, Req, Opts}.
