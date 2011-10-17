%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
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

%% @doc Core HTTP parsing API.
-module(cowboy_http).

%% Parsing.
-export([list/2, nonempty_list/2, token/2]).

%% Interpretation.
-export([connection_to_atom/1]).

-include("include/http.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Parsing.

%% @doc Parse a non-empty list of the given type.
-spec nonempty_list(binary(), fun()) -> [any(), ...] | {error, badarg}.
nonempty_list(Data, Fun) ->
	case list(Data, Fun, []) of
		{error, badarg} -> {error, badarg};
		[] -> {error, badarg};
		L -> lists:reverse(L)
	end.

%% @doc Parse a list of the given type.
-spec list(binary(), fun()) -> list() | {error, badarg}.
list(Data, Fun) ->
	case list(Data, Fun, []) of
		{error, badarg} -> {error, badarg};
		L -> lists:reverse(L)
	end.

-spec list(binary(), fun(), [binary()]) -> [any()] | {error, badarg}.
list(<<>>, _Fun, Acc) ->
	Acc;
%% From the RFC:
%% <blockquote>Wherever this construct is used, null elements are allowed,
%% but do not contribute to the count of elements present.
%% That is, "(element), , (element) " is permitted, but counts
%% as only two elements. Therefore, where at least one element is required,
%% at least one non-null element MUST be present.</blockquote>
list(<< $,, Rest/bits >>, Fun, Acc) ->
	list(Rest, Fun, Acc);
list(Data, Fun, Acc) ->
	Fun(Data,
		fun (R, <<>>) -> list_separator(R,
				fun (D) -> list(D, Fun, Acc) end);
			(R, I) -> list_separator(R,
				fun (D) -> list(D, Fun, [I|Acc]) end)
		end).

-spec list_separator(binary(), fun()) -> any().
list_separator(<<>>, Fun) ->
	Fun(<<>>);
list_separator(<< $,, Rest/bits >>, Fun) ->
	Fun(Rest);
list_separator(<< C, Rest/bits >>, Fun)
		when C =:= $\s; C =:= $\t ->
	list_separator(Rest, Fun);
list_separator(_Data, _Fun) ->
	{error, badarg}.

%% @doc Parse a token.
-spec token(binary(), fun()) -> any().
token(<< C, Rest/bits >>, Fun)
		when C =:= $\s; C =:= $\t ->
	token(Rest, Fun);
token(Data, Fun) ->
	token(Data, Fun, <<>>).

-spec token(binary(), fun(), binary()) -> any().
token(<<>>, Fun, Acc) ->
	Fun(<<>>, Acc);
token(Data = << C, _Rest/bits >>, Fun, Acc)
		when C =:= $(; C =:= $); C =:= $<; C =:= $>; C =:= $@;
			 C =:= $,; C =:= $;; C =:= $:; C =:= $\\; C =:= $";
			 C =:= $/; C =:= $[; C =:= $]; C =:= $?; C =:= $=;
			 C =:= ${; C =:= $}; C =:= $\s; C =:= $\t;
			 C < 32; C =:= 127 ->
	Fun(Data, Acc);
token(<< C, Rest/bits >>, Fun, Acc) ->
	token(Rest, Fun, << Acc/binary, C >>).

%% Interpretation.

%% @doc Walk through a tokens list and return whether
%% the connection is keepalive or closed.
-spec connection_to_atom([binary()]) -> keepalive | close.
connection_to_atom([]) ->
	keepalive;
connection_to_atom([<<"keep-alive">>|_Tail]) ->
	keepalive;
connection_to_atom([<<"close">>|_Tail]) ->
	close;
connection_to_atom([Connection|Tail]) ->
	case cowboy_bstr:to_lower(Connection) of
		<<"close">> -> close;
		<<"keep-alive">> -> keepalive;
		_Any -> connection_to_atom(Tail)
	end.

%% Tests.

-ifdef(TEST).

nonempty_token_list_test_() ->
	%% {Value, Result}
	Tests = [
		{<<>>, {error, badarg}},
		{<<" ">>, {error, badarg}},
		{<<" , ">>, {error, badarg}},
		{<<",,,">>, {error, badarg}},
		{<<"a b">>, {error, badarg}},
		{<<"a , , , ">>, [<<"a">>]},
		{<<" , , , a">>, [<<"a">>]},
		{<<"a, , b">>, [<<"a">>, <<"b">>]},
		{<<"close">>, [<<"close">>]},
		{<<"keep-alive, upgrade">>, [<<"keep-alive">>, <<"upgrade">>]}
	],
	[{V, fun() -> R = nonempty_list(V, fun token/2) end} || {V, R} <- Tests].

connection_to_atom_test_() ->
	%% {Tokens, Result}
	Tests = [
		{[<<"close">>], close},
		{[<<"ClOsE">>], close},
		{[<<"Keep-Alive">>], keepalive},
		{[<<"Keep-Alive">>, <<"Upgrade">>], keepalive}
	],
	[{lists:flatten(io_lib:format("~p", [T])),
		fun() -> R = connection_to_atom(T) end} || {T, R} <- Tests].

-endif.
