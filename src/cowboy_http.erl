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

-module(cowboy_http).

%% Parsing.
-export([parse_tokens_list/1]).

%% Interpretation.
-export([connection_to_atom/1]).

-include("include/http.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Parsing.

%% @doc Parse a list of tokens, as is often found in HTTP headers.
%%
%% From the RFC:
%% <blockquote>Wherever this construct is used, null elements are allowed,
%% but do not contribute to the count of elements present.
%% That is, "(element), , (element) " is permitted, but counts
%% as only two elements. Therefore, where at least one element is required,
%% at least one non-null element MUST be present.</blockquote>
-spec parse_tokens_list(binary()) -> [binary()] | {error, badarg}.
parse_tokens_list(Value) ->
	case parse_tokens_list(Value, ws_or_sep, <<>>, []) of
		{error, badarg} ->
			{error, badarg};
		L when length(L) =:= 0 ->
			{error, badarg};
		L ->
			lists:reverse(L)
	end.

-spec parse_tokens_list(binary(), token | ws | ws_or_sep, binary(),
	[binary()]) -> [binary()] | {error, badarg}.
parse_tokens_list(<<>>, token, Token, Acc) ->
	[Token|Acc];
parse_tokens_list(<< C, Rest/bits >>, token, Token, Acc)
		when C =:= $\s; C =:= $\t ->
	parse_tokens_list(Rest, ws, <<>>, [Token|Acc]);
parse_tokens_list(<< $,, Rest/bits >>, token, Token, Acc) ->
	parse_tokens_list(Rest, ws_or_sep, <<>>, [Token|Acc]);
parse_tokens_list(<< C, Rest/bits >>, token, Token, Acc) ->
	parse_tokens_list(Rest, token, << Token/binary, C >>, Acc);
parse_tokens_list(<< C, Rest/bits >>, ws, <<>>, Acc)
		when C =:= $\s; C =:= $\t ->
	parse_tokens_list(Rest, ws, <<>>, Acc);
parse_tokens_list(<< $,, Rest/bits >>, ws, <<>>, Acc) ->
	parse_tokens_list(Rest, ws_or_sep, <<>>, Acc);
parse_tokens_list(<<>>, ws_or_sep, <<>>, Acc) ->
	Acc;
parse_tokens_list(<< C, Rest/bits >>, ws_or_sep, <<>>, Acc)
		when C =:= $\s; C =:= $\t ->
	parse_tokens_list(Rest, ws_or_sep, <<>>, Acc);
parse_tokens_list(<< $,, Rest/bits >>, ws_or_sep, <<>>, Acc) ->
	parse_tokens_list(Rest, ws_or_sep, <<>>, Acc);
parse_tokens_list(<< C, Rest/bits >>, ws_or_sep, <<>>, Acc) ->
	parse_tokens_list(Rest, token, << C >>, Acc);
parse_tokens_list(_Value, _State, _Token, _Acc) ->
	{error, badarg}.

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

parse_tokens_list_test_() ->
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
	[{V, fun() -> R = parse_tokens_list(V) end} || {V, R} <- Tests].

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
