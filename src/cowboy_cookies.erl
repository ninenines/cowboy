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

%% @doc HTTP Cookie parsing and generating (RFC 2109, RFC 2965).

-module(cowboy_cookies).

%% API.
-export([parse_cookie/1, cookie/3, cookie/2]).

%% Types.
-type kv() :: {Name::binary(), Value::binary()}.
-type kvlist() :: [kv()].
-type cookie_option() :: {max_age, integer()}
                   | {local_time, {calendar:date(), calendar:time()}}
                   | {domain, binary()} | {path, binary()}
                   | {secure, true | false} | {http_only, true | false}.
-export_type([kv/0, kvlist/0, cookie_option/0]).

-define(QUOTE, $\").

-include_lib("eunit/include/eunit.hrl").


%% ----------------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------------

%% @doc Parse the contents of a Cookie header field, ignoring cookie
%% attributes, and return a simple property list.
-spec parse_cookie(binary()) -> kvlist().
parse_cookie(<<>>) ->
    [];
parse_cookie(Cookie) ->
    parse_cookie(Cookie, []).



%% @doc Short-hand for <code>cookie(Key, Value, [])</code>.
-spec cookie(Key::binary(), Value::binary()) -> kvlist().
cookie(Key, Value) ->
    cookie(Key, Value, []).

%% @doc Generate a Set-Cookie header field tuple.
-spec cookie(Key::binary(), Value::binary(), Options::[cookie_option()]) ->
    kvlist().
cookie(Key, Value, Options) ->
    Cookie = <<(any_to_binary(Key))/binary, "=", (quote(Value))/binary, "; Version=1">>,
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
                <<"; Path=", (quote(Path))/binary>>
        end,
    HttpOnlyPart =
        case proplists:get_value(http_only, Options) of
            true ->
                <<"; HttpOnly">>;
            _ ->
                <<"">>
        end,
    CookieParts = <<Cookie/binary, ExpiresPart/binary, SecurePart/binary, DomainPart/binary, PathPart/binary, HttpOnlyPart/binary>>,
    {<<"Set-Cookie">>, CookieParts}.


%% ----------------------------------------------------------------------------
%% Private
%% ----------------------------------------------------------------------------

%% @doc Check if a character is a white space character.
is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\r) -> true;
is_whitespace($\n) -> true;
is_whitespace(_) -> false.

%% @doc Check if a character is a seperator.
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

%% @doc Check if a binary has an ASCII seperator character.
has_seperator(<<>>) ->
    false;
has_seperator(<<$/, Rest/binary>>) ->
    has_seperator(Rest);
has_seperator(<<C, Rest/binary>>) ->
    case is_separator(C) of
        true ->
            true;
        false ->
            has_seperator(Rest)
    end.

%% @doc Convert to a binary and raise an error if quoting is required. Quoting
%% is broken in different ways for different browsers. Its better to simply
%% avoiding doing it at all.
%% @end
-spec quote(term()) -> binary().
quote(V0) ->
    V = any_to_binary(V0),
    case has_seperator(V) of
        true ->
            erlang:error({cookie_quoting_required, V});
        false ->
            V
    end.

%% Return a date in the form of: Wdy, DD-Mon-YYYY HH:MM:SS GMT
%% See also: rfc2109: 10.1.2
rfc2109_cookie_expires_date(LocalTime) ->
    {{YYYY,MM,DD},{Hour,Min,Sec}} =
        case calendar:local_time_to_universal_time_dst(LocalTime) of
            [Gmt]   -> Gmt;
            [_,Gmt] -> Gmt
        end,
    Wday = calendar:day_of_the_week({YYYY,MM,DD}),
    DayBin = cowboy_clock:pad_int(DD),
    YearBin = list_to_binary(integer_to_list(YYYY)),
    HourBin = cowboy_clock:pad_int(Hour),
    MinBin = cowboy_clock:pad_int(Min),
    SecBin = cowboy_clock:pad_int(Sec),
    WeekDay = cowboy_clock:weekday(Wday),
    Month = cowboy_clock:month(MM),
    <<WeekDay/binary, ", ",
      DayBin/binary, " ", Month/binary, " ",
      YearBin/binary, " ",
      HourBin/binary, ":",
      MinBin/binary, ":",
      SecBin/binary, " GMT">>.

add_seconds(Secs, LocalTime) ->
    Greg = calendar:datetime_to_gregorian_seconds(LocalTime),
    calendar:gregorian_seconds_to_datetime(Greg + Secs).

age_to_cookie_date(Age, LocalTime) ->
    rfc2109_cookie_expires_date(add_seconds(Age, LocalTime)).

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

read_pair(String) ->
    {Token, Rest} = read_token(skip_whitespace(String)),
    {Value, Rest1} = read_value(skip_whitespace(Rest)),
    {{Token, Value}, skip_past_separator(Rest1)}.

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

read_quoted(<<?QUOTE, String/binary>>) ->
    read_quoted(String, <<"">>).

read_quoted(<<"">>, Acc) ->
    {Acc, <<"">>};
read_quoted(<<?QUOTE, Rest/binary>>, Acc) ->
    {Acc, Rest};
read_quoted(<<$\\, Any, Rest/binary>>, Acc) ->
    read_quoted(Rest, <<Acc/binary, Any>>);
read_quoted(<<C, Rest/binary>>, Acc) ->
    read_quoted(Rest, <<Acc/binary, C>>).


%% @doc Drop characters while a function returns true.
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
skip_whitespace(String) ->
    binary_dropwhile(fun is_whitespace/1, String).

%% @doc Split a binary when the current character causes F to return true.
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
binary_splitwith(F, String) ->
    binary_splitwith(F, <<>>, String). 

%% @doc Split the binary when the next seperator is found.
read_token(String) ->
    binary_splitwith(fun is_separator/1, String).

%% @doc Return string after ; or , characters.
skip_past_separator(<<"">>) ->
    <<"">>;
skip_past_separator(<<";", Rest/binary>>) ->
    Rest;
skip_past_separator(<<",", Rest/binary>>) ->
    Rest;
skip_past_separator(<<_C, Rest/binary>>) ->
    skip_past_separator(Rest).

any_to_binary(V) when is_binary(V) ->
    V;
any_to_binary(V) when is_list(V) ->
    erlang:list_to_binary(V);
any_to_binary(V) when is_atom(V) ->
    erlang:atom_to_binary(V, latin1);
any_to_binary(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V)).

%% ----------------------------------------------------------------------------
%% Tests
%% ----------------------------------------------------------------------------

-ifdef(TEST).

quote_test() ->
    %% ?assertError eunit macro is not compatible with coverage module
    try quote(<<":wq">>)
    catch error:{cookie_quoting_required, <<":wq">>} -> ok
    end,
    ?assertEqual(
       <<"foo">>,
       quote(foo)),
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

cookie_test() ->
    C1 = {<<"Set-Cookie">>,
          <<"Customer=WILE_E_COYOTE; "
          "Version=1; "
          "Path=/acme">>},
    C1 = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>, [{path, <<"/acme">>}]),

    C1 = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>,
                [{path, <<"/acme">>}, {badoption, <<"negatory">>}]),

    C1 = cookie('Customer', 'WILE_E_COYOTE', [{path, '/acme'}]),

    C1 = cookie("Customer", "WILE_E_COYOTE", [{path, "/acme"}]),

    {<<"Set-Cookie">>,<<"=NoKey; Version=1">>} = cookie(<<"">>, <<"NoKey">>, []),
    {<<"Set-Cookie">>,<<"=NoKey; Version=1">>} = cookie(<<"">>, <<"NoKey">>),
    LocalTime = calendar:universal_time_to_local_time({{2007, 5, 15}, {13, 45, 33}}),
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
