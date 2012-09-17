%% Copyright 2007 Mochi Media, Inc.
%% Copyright 2011 Thomas Burdick <thomas.burdick@gmail.com>
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

%% @doc HTTP Cookie parsing and generating (RFC 2965).

-module(cowboy_cookies).

%% API.
-export([parse_cookie/1]).
-export([cookie/3]).
-export([cookie/2]).

%% Types.
-type kv() :: {Name::binary(), Value::binary()}.
-type kvlist() :: [kv()].
-type cookie_option() :: {max_age, integer()}
				| {local_time, calendar:datetime()}
				| {domain, binary()} | {path, binary()}
				| {secure, true | false} | {http_only, true | false}.

-export_type([kv/0]).
-export_type([kvlist/0]).
-export_type([cookie_option/0]).

-define(QUOTE, $\").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API.

%% @doc Parse the contents of a Cookie header field, ignoring cookie
%% attributes, and return a simple property list.
-spec parse_cookie(binary()) -> kvlist().
parse_cookie(<<>>) ->
	[];
parse_cookie(Cookie) when is_binary(Cookie) ->
	parse_cookie(Cookie, []).

%% @equiv cookie(Key, Value, [])
-spec cookie(binary(), binary()) -> kv().
cookie(Key, Value) when is_binary(Key) andalso is_binary(Value) ->
	cookie(Key, Value, []).

%% @doc Generate a Set-Cookie header field tuple.
-spec cookie(binary(), binary(), [cookie_option()]) -> kv().
cookie(Key, Value, Options) when is_binary(Key)
		andalso is_binary(Value) andalso is_list(Options) ->
	Cookie = <<(any_to_binary(Key))/binary, "=",
		(quote(Value))/binary, "; Version=1">>,
	%% Set-Cookie:
	%%    Comment, Domain, Max-Age, Path, Secure, Version
	ExpiresPart =
		case proplists:get_value(max_age, Options) of
			undefined ->
				<<"">>;
			RawAge ->
				When = case proplists:get_value(local_time, Options) of
						undefined ->
							calendar:local_time();
						LocalTime ->
							LocalTime
					end,
				Age = case RawAge < 0 of
						true ->
							0;
						false ->
							RawAge
					end,
				AgeBinary = quote(Age),
				CookieDate = age_to_cookie_date(Age, When),
				<<"; Expires=", CookieDate/binary,
				"; Max-Age=", AgeBinary/binary>>
		end,
	SecurePart =
		case proplists:get_value(secure, Options) of
			true ->
				<<"; Secure">>;
			_ ->
				<<"">>
		end,
	DomainPart =
		case proplists:get_value(domain, Options) of
			undefined ->
				<<"">>;
			Domain ->
				<<"; Domain=", (quote(Domain))/binary>>
		end,
	PathPart =
		case proplists:get_value(path, Options) of
			undefined ->
				<<"">>;
			Path ->
				<<"; Path=", (quote(Path, true))/binary>>
		end,
	HttpOnlyPart =
		case proplists:get_value(http_only, Options) of
			true ->
				<<"; HttpOnly">>;
			_ ->
				<<"">>
		end,
	CookieParts = <<Cookie/binary, ExpiresPart/binary, SecurePart/binary,
		DomainPart/binary, PathPart/binary, HttpOnlyPart/binary>>,
	{<<"Set-Cookie">>, CookieParts}.

%% Internal.

%% @doc Check if a character is a white space character.
-spec is_whitespace(char()) -> boolean().
is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\r) -> true;
is_whitespace($\n) -> true;
is_whitespace(_) -> false.

%% @doc Check if a character is a separator.
-spec is_separator(char()) -> boolean().
is_separator(C) when C < 32 -> true;
is_separator($\s) -> true;
is_separator($\t) -> true;
is_separator($() -> true;
is_separator($)) -> true;
is_separator($<) -> true;
is_separator($>) -> true;
is_separator($@) -> true;
is_separator($,) -> true;
is_separator($;) -> true;
is_separator($:) -> true;
is_separator($\\) -> true;
is_separator(?QUOTE) -> true;
is_separator($/) -> true;
is_separator($[) -> true;
is_separator($]) -> true;
is_separator($?) -> true;
is_separator($=) -> true;
is_separator(${) -> true;
is_separator($}) -> true;
is_separator(_) -> false.

%% @doc Check if a binary has an ASCII separator character.
-spec has_separator(binary(), boolean()) -> boolean().
has_separator(<<>>, _) ->
	false;
has_separator(<<$/, Rest/binary>>, true) ->
	has_separator(Rest, true);
has_separator(<<C, Rest/binary>>, IgnoreSlash) ->
	case is_separator(C) of
		true ->
			true;
		false ->
			has_separator(Rest, IgnoreSlash)
	end.

%% @doc Convert to a binary and raise an error if quoting is required. Quoting
%% is broken in different ways for different browsers. Its better to simply
%% avoiding doing it at all.
%% @end
-spec quote(term(), boolean()) -> binary().
quote(V0, IgnoreSlash) ->
	V = any_to_binary(V0),
	case has_separator(V, IgnoreSlash) of
		true ->
			erlang:error({cookie_quoting_required, V});
		false ->
			V
	end.

%% @equiv quote(Bin, false)
-spec quote(term()) -> binary().
quote(V0) ->
	quote(V0, false).

-spec add_seconds(integer(), calendar:datetime()) -> calendar:datetime().
add_seconds(Secs, LocalTime) ->
	Greg = calendar:datetime_to_gregorian_seconds(LocalTime),
	calendar:gregorian_seconds_to_datetime(Greg + Secs).

-spec age_to_cookie_date(integer(), calendar:datetime()) -> binary().
age_to_cookie_date(Age, LocalTime) ->
	cowboy_clock:rfc2109(add_seconds(Age, LocalTime)).

-spec parse_cookie(binary(), kvlist()) -> kvlist().
parse_cookie(<<>>, Acc) ->
	lists:reverse(Acc);
parse_cookie(String, Acc) ->
	{{Token, Value}, Rest} = read_pair(String),
	Acc1 = case Token of
			<<"">> ->
				Acc;
			<<"$", _R/binary>> ->
				Acc;
			_ ->
				[{Token, Value} | Acc]
		end,
	parse_cookie(Rest, Acc1).

-spec read_pair(binary()) -> {{binary(), binary()}, binary()}.
read_pair(String) ->
	{Token, Rest} = read_token(skip_whitespace(String)),
	{Value, Rest1} = read_value(skip_whitespace(Rest)),
	{{Token, Value}, skip_past_separator(Rest1)}.

-spec read_value(binary()) -> {binary(), binary()}.
read_value(<<"=",  Value/binary>>) ->
	Value1 = skip_whitespace(Value),
	case Value1 of
		<<?QUOTE, _R/binary>> ->
			read_quoted(Value1);
		_ ->
			read_token(Value1)
	end;
read_value(String) ->
	{<<"">>, String}.

-spec read_quoted(binary()) -> {binary(), binary()}.
read_quoted(<<?QUOTE, String/binary>>) ->
	read_quoted(String, <<"">>).

-spec read_quoted(binary(), binary()) -> {binary(), binary()}.
read_quoted(<<"">>, Acc) ->
	{Acc, <<"">>};
read_quoted(<<?QUOTE, Rest/binary>>, Acc) ->
	{Acc, Rest};
read_quoted(<<$\\, Any, Rest/binary>>, Acc) ->
	read_quoted(Rest, <<Acc/binary, Any>>);
read_quoted(<<C, Rest/binary>>, Acc) ->
	read_quoted(Rest, <<Acc/binary, C>>).

%% @doc Drop characters while a function returns true.
-spec binary_dropwhile(fun((char()) -> boolean()), binary()) -> binary().
binary_dropwhile(_F, <<"">>) ->
	<<"">>;
binary_dropwhile(F, String) ->
	<<C, Rest/binary>> = String,
	case F(C) of
		true ->
			binary_dropwhile(F, Rest);
		false ->
			String
	end.

%% @doc Remove leading whitespace.
-spec skip_whitespace(binary()) -> binary().
skip_whitespace(String) ->
	binary_dropwhile(fun is_whitespace/1, String).

%% @doc Split a binary when the current character causes F to return true.
-spec binary_splitwith(fun((char()) -> boolean()), binary(), binary())
	-> {binary(), binary()}.
binary_splitwith(_F, Head, <<>>) ->
	{Head, <<>>};
binary_splitwith(F, Head, Tail) ->
	<<C, NTail/binary>> = Tail,
	case F(C) of
		true ->
			{Head, Tail};
		false ->
			binary_splitwith(F, <<Head/binary, C>>, NTail)
	end.

%% @doc Split a binary with a function returning true or false on each char.
-spec binary_splitwith(fun((char()) -> boolean()), binary())
	-> {binary(), binary()}.
binary_splitwith(F, String) ->
	binary_splitwith(F, <<>>, String).

%% @doc Split the binary when the next separator is found.
-spec read_token(binary()) -> {binary(), binary()}.
read_token(String) ->
	binary_splitwith(fun is_separator/1, String).

%% @doc Return string after ; or , characters.
-spec skip_past_separator(binary()) -> binary().
skip_past_separator(<<"">>) ->
	<<"">>;
skip_past_separator(<<";", Rest/binary>>) ->
	Rest;
skip_past_separator(<<",", Rest/binary>>) ->
	Rest;
skip_past_separator(<<_C, Rest/binary>>) ->
	skip_past_separator(Rest).

-spec any_to_binary(binary() | string() | atom() | integer()) -> binary().
any_to_binary(V) when is_binary(V) ->
	V;
any_to_binary(V) when is_list(V) ->
	erlang:list_to_binary(V);
any_to_binary(V) when is_atom(V) ->
	erlang:atom_to_binary(V, latin1);
any_to_binary(V) when is_integer(V) ->
	list_to_binary(integer_to_list(V)).

%% Tests.

-ifdef(TEST).

quote_test() ->
	%% ?assertError eunit macro is not compatible with coverage module
	_ = try quote(<<":wq">>)
	catch error:{cookie_quoting_required, <<":wq">>} -> ok
	end,
	?assertEqual(<<"foo">>,quote(foo)),
	_ = try quote(<<"/test/slashes/">>)
	catch error:{cookie_quoting_required, <<"/test/slashes/">>} -> ok
	end,
	ok.

parse_cookie_test() ->
	%% RFC example
	C1 = <<"$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\";
	Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\";
	Shipping=\"FedEx\"; $Path=\"/acme\"">>,
	?assertEqual(
	[{<<"Customer">>,<<"WILE_E_COYOTE">>},
		{<<"Part_Number">>,<<"Rocket_Launcher_0001">>},
		{<<"Shipping">>,<<"FedEx">>}],
	parse_cookie(C1)),
	%% Potential edge cases
	?assertEqual(
	[{<<"foo">>, <<"x">>}],
	parse_cookie(<<"foo=\"\\x\"">>)),
	?assertEqual(
	[],
	parse_cookie(<<"=">>)),
	?assertEqual(
	[{<<"foo">>, <<"">>}, {<<"bar">>, <<"">>}],
	parse_cookie(<<"  foo ; bar  ">>)),
	?assertEqual(
	[{<<"foo">>, <<"">>}, {<<"bar">>, <<"">>}],
	parse_cookie(<<"foo=;bar=">>)),
	?assertEqual(
	[{<<"foo">>, <<"\";">>}, {<<"bar">>, <<"">>}],
	parse_cookie(<<"foo = \"\\\";\";bar ">>)),
	?assertEqual(
	[{<<"foo">>, <<"\";bar">>}],
	parse_cookie(<<"foo=\"\\\";bar">>)),
	?assertEqual(
	[],
	parse_cookie(<<"">>)),
	?assertEqual(
	[{<<"foo">>, <<"bar">>}, {<<"baz">>, <<"wibble">>}],
	parse_cookie(<<"foo=bar , baz=wibble ">>)),
	ok.

domain_test() ->
	?assertEqual(
	{<<"Set-Cookie">>,
		<<"Customer=WILE_E_COYOTE; "
		"Version=1; "
		"Domain=acme.com; "
		"HttpOnly">>},
	cookie(<<"Customer">>, <<"WILE_E_COYOTE">>,
			[{http_only, true}, {domain, <<"acme.com">>}])),
	ok.

local_time_test() ->
	{<<"Set-Cookie">>, B} = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>,
							[{max_age, 111}, {secure, true}]),

	?assertMatch(
	[<<"Customer=WILE_E_COYOTE">>,
		<<" Version=1">>,
		<<" Expires=", _R/binary>>,
		<<" Max-Age=111">>,
		<<" Secure">>],
	binary:split(B, <<";">>, [global])),
	ok.

-spec cookie_test() -> no_return(). %% Not actually true, just a bad option.
cookie_test() ->
	C1 = {<<"Set-Cookie">>,
		<<"Customer=WILE_E_COYOTE; "
		"Version=1; "
		"Path=/acme">>},
	C1 = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>, [{path, <<"/acme">>}]),

	C1 = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>,
				[{path, <<"/acme">>}, {badoption, <<"negatory">>}]),

	{<<"Set-Cookie">>,<<"=NoKey; Version=1">>}
		= cookie(<<"">>, <<"NoKey">>, []),
	{<<"Set-Cookie">>,<<"=NoKey; Version=1">>}
		= cookie(<<"">>, <<"NoKey">>),
	LocalTime = calendar:universal_time_to_local_time(
		{{2007, 5, 15}, {13, 45, 33}}),
	C2 = {<<"Set-Cookie">>,
		<<"Customer=WILE_E_COYOTE; "
		"Version=1; "
		"Expires=Tue, 15 May 2007 13:45:33 GMT; "
		"Max-Age=0">>},
	C2 = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>,
				[{max_age, -111}, {local_time, LocalTime}]),
	C3 = {<<"Set-Cookie">>,
		<<"Customer=WILE_E_COYOTE; "
		"Version=1; "
		"Expires=Wed, 16 May 2007 13:45:50 GMT; "
		"Max-Age=86417">>},
	C3 = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>,
				[{max_age, 86417}, {local_time, LocalTime}]),
	ok.

-endif.
