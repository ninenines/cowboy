%% Copyright (c) 2014, Loïc Hoguin <essen@ninenines.eu>
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

-type constraint() :: int | nonempty | fun().
-export_type([constraint/0]).

-spec validate(binary(), [constraint()]) -> true | {true, any()} | false.
validate(Value, [Constraint]) ->
	apply_constraint(Value, Constraint);
validate(Value, Constraints) when is_list(Constraints) ->
	validate_list(Value, Constraints, original);
validate(Value, Constraint) ->
	apply_constraint(Value, Constraint).

validate_list(_, [], original) ->
	true;
validate_list(Value, [], modified) ->
	{true, Value};
validate_list(Value, [Constraint|Tail], State) ->
	case apply_constraint(Value, Constraint) of
		true ->
			validate_list(Value, Tail, State);
		{true, Value2} ->
			validate_list(Value2, Tail, modified);
		false ->
			false
	end.

%% @todo {int, From, To}, etc.
apply_constraint(Value, int) ->
	int(Value);
apply_constraint(Value, nonempty) ->
	nonempty(Value);
apply_constraint(Value, F) when is_function(F) ->
	F(Value).

%% Constraint functions.

int(Value) when is_binary(Value) ->
	try {true, list_to_integer(binary_to_list(Value))}
	catch _:_ -> false
	end.

nonempty(<<>>) -> false;
nonempty(Value) when is_binary(Value) -> true.
