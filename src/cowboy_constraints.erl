%% Copyright (c) 2014-2017, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(cowboy_constraints).

-export([validate/2]).
-export([reverse/2]).
-export([format_error/1]).

-type constraint() :: int | nonempty | fun().
-export_type([constraint/0]).

-type reason() :: {constraint(), any(), any()}.
-export_type([reason/0]).

-spec validate(binary(), constraint() | [constraint()])
	-> {ok, any()} | {error, reason()}.
validate(Value, Constraints) when is_list(Constraints) ->
	apply_list(forward, Value, Constraints);
validate(Value, Constraint) ->
	apply_list(forward, Value, [Constraint]).

-spec reverse(any(), constraint() | [constraint()])
	-> {ok, binary()} | {error, reason()}.
reverse(Value, Constraints) when is_list(Constraints) ->
	apply_list(reverse, Value, Constraints);
reverse(Value, Constraint) ->
	apply_list(reverse, Value, [Constraint]).

-spec format_error(reason()) -> iodata().
format_error({Constraint, Reason, Value}) ->
	apply_constraint(format_error, {Reason, Value}, Constraint).

apply_list(_, Value, []) ->
	{ok, Value};
apply_list(Type, Value0, [Constraint|Tail]) ->
	case apply_constraint(Type, Value0, Constraint) of
		{ok, Value} ->
			apply_list(Type, Value, Tail);
		{error, Reason} ->
			{error, {Constraint, Reason, Value0}}
	end.

%% @todo {int, From, To}, etc.
apply_constraint(Type, Value, int) ->
	int(Type, Value);
apply_constraint(Type, Value, nonempty) ->
	nonempty(Type, Value);
apply_constraint(Type, Value, F) when is_function(F) ->
	F(Type, Value).

%% Constraint functions.

int(forward, Value) ->
	try
		{ok, binary_to_integer(Value)}
	catch _:_ ->
		{error, not_an_integer}
	end;
int(reverse, Value) ->
	try
		{ok, integer_to_binary(Value)}
	catch _:_ ->
		{error, not_an_integer}
	end;
int(format_error, {not_an_integer, Value}) ->
	io_lib:format("The value ~p is not an integer.", [Value]).

nonempty(Type, <<>>) when Type =/= format_error ->
	{error, empty};
nonempty(Type, Value) when Type =/= format_error, is_binary(Value) ->
	{ok, Value};
nonempty(format_error, {empty, Value}) ->
	io_lib:format("The value ~p is empty.", [Value]).

-ifdef(TEST).

validate_test() ->
	F = fun(_, Value) ->
		try
			{ok, binary_to_atom(Value, latin1)}
		catch _:_ ->
			{error, not_a_binary}
		end
	end,
	%% Value, Constraints, Result.
	Tests = [
		{<<>>, [], <<>>},
		{<<"123">>, int, 123},
		{<<"123">>, [int], 123},
		{<<"123">>, [nonempty, int], 123},
		{<<"123">>, [int, nonempty], 123},
		{<<>>, nonempty, error},
		{<<>>, [nonempty], error},
		{<<"hello">>, F, hello},
		{<<"hello">>, [F], hello},
		{<<"123">>, [F, int], error},
		{<<"123">>, [int, F], error},
		{<<"hello">>, [nonempty, F], hello},
		{<<"hello">>, [F, nonempty], hello}
	],
	[{lists:flatten(io_lib:format("~p, ~p", [V, C])), fun() ->
		case R of
			error -> {error, _} = validate(V, C);
			_ -> {ok, R} = validate(V, C)
		end
	end} || {V, C, R} <- Tests].

reverse_test() ->
	F = fun(_, Value) ->
		try
			{ok, atom_to_binary(Value, latin1)}
		catch _:_ ->
			{error, not_an_atom}
		end
	end,
	%% Value, Constraints, Result.
	Tests = [
		{<<>>, [], <<>>},
		{123, int, <<"123">>},
		{123, [int], <<"123">>},
		{123, [nonempty, int], <<"123">>},
		{123, [int, nonempty], <<"123">>},
		{<<>>, nonempty, error},
		{<<>>, [nonempty], error},
		{hello, F, <<"hello">>},
		{hello, [F], <<"hello">>},
		{123, [F, int], error},
		{123, [int, F], error},
		{hello, [nonempty, F], <<"hello">>},
		{hello, [F, nonempty], <<"hello">>}
	],
	[{lists:flatten(io_lib:format("~p, ~p", [V, C])), fun() ->
		case R of
			error -> {error, _} = reverse(V, C);
			_ -> {ok, R} = reverse(V, C)
		end
	end} || {V, C, R} <- Tests].

int_format_error_test() ->
	{error, Reason} = validate(<<"string">>, int),
	Bin = iolist_to_binary(format_error(Reason)),
	true = is_binary(Bin),
	ok.

nonempty_format_error_test() ->
	{error, Reason} = validate(<<>>, nonempty),
	Bin = iolist_to_binary(format_error(Reason)),
	true = is_binary(Bin),
	ok.

fun_format_error_test() ->
	F = fun
		(format_error, {test, <<"value">>}) ->
			formatted;
		(_, _) ->
			{error, test}
	end,
	{error, Reason} = validate(<<"value">>, F),
	formatted = format_error(Reason),
	ok.

-endif.
