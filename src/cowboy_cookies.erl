%% @author Emad El-Haraty <emad@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc HTTP Cookie parsing and generating (RFC 2109, RFC 2965).

-module(cowboy_cookies).
-export([parse_cookie/1, cookie/3, cookie/2]).

-define(QUOTE, $\").

-define(IS_WHITESPACE(C),
        (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).

%% RFC 2616 separators (called tspecials in RFC 2068)
-define(IS_SEPARATOR(C),
        (C < 32 orelse
         C =:= $\s orelse C =:= $\t orelse
         C =:= $( orelse C =:= $) orelse C =:= $< orelse C =:= $> orelse
         C =:= $@ orelse C =:= $, orelse C =:= $; orelse C =:= $: orelse
         C =:= $\\ orelse C =:= $\" orelse C =:= $/ orelse
         C =:= $[ orelse C =:= $] orelse C =:= $? orelse C =:= $= orelse
         C =:= ${ orelse C =:= $})).

%% @type proplist() = [{Key::string(), Value::string()}].
%% @type header() = {Name::string(), Value::string()}.
%% @type int_seconds() = integer().

%% @spec cookie(Key::string(), Value::string()) -> header()
%% @doc Short-hand for <code>cookie(Key, Value, [])</code>.
cookie(Key, Value) ->
    cookie(Key, Value, []).

%% @spec cookie(Key::string(), Value::string(), Options::[Option]) -> header()
%% where Option = {max_age, int_seconds()} | {local_time, {date(), time()}}
%%                | {domain, string()} | {path, string()}
%%                | {secure, true | false} | {http_only, true | false}
%%
%% @doc Generate a Set-Cookie header field tuple.
cookie(Key, Value, Options) ->
    Cookie = [any_to_list(Key), "=", quote(Value), "; Version=1"],
    %% Set-Cookie:
    %%    Comment, Domain, Max-Age, Path, Secure, Version
    %% Set-Cookie2:
    %%    Comment, CommentURL, Discard, Domain, Max-Age, Path, Port, Secure,
    %%    Version
    ExpiresPart =
        case proplists:get_value(max_age, Options) of
            undefined ->
                "";
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
                ["; Expires=", age_to_cookie_date(Age, When),
                 "; Max-Age=", quote(Age)]
        end,
    SecurePart =
        case proplists:get_value(secure, Options) of
            true ->
                "; Secure";
            _ ->
                ""
        end,
    DomainPart =
        case proplists:get_value(domain, Options) of
            undefined ->
                "";
            Domain ->
                ["; Domain=", quote(Domain)]
        end,
    PathPart =
        case proplists:get_value(path, Options) of
            undefined ->
                "";
            Path ->
                ["; Path=", quote(Path)]
        end,
    HttpOnlyPart =
        case proplists:get_value(http_only, Options) of
            true ->
                "; HttpOnly";
            _ ->
                ""
        end,
    CookieParts = [Cookie, ExpiresPart, SecurePart, DomainPart, PathPart, HttpOnlyPart],
    {"Set-Cookie", lists:flatten(CookieParts)}.


%% Every major browser incorrectly handles quoted strings in a
%% different and (worse) incompatible manner.  Instead of wasting time
%% writing redundant code for each browser, we restrict cookies to
%% only contain characters that browsers handle compatibly.
%%
%% By replacing the definition of quote with this, we generate
%% RFC-compliant cookies:
%%
%%     quote(V) ->
%%         Fun = fun(?QUOTE, Acc) -> [$\\, ?QUOTE | Acc];
%%                  (Ch, Acc) -> [Ch | Acc]
%%               end,
%%         [?QUOTE | lists:foldr(Fun, [?QUOTE], V)].

%% Convert to a string and raise an error if quoting is required.
quote(V0) ->
    V = any_to_list(V0),
    lists:all(fun(Ch) -> Ch =:= $/ orelse not ?IS_SEPARATOR(Ch) end, V)
        orelse erlang:error({cookie_quoting_required, V}),
    V.


%% Return a date in the form of: Wdy, DD-Mon-YYYY HH:MM:SS GMT
%% See also: rfc2109: 10.1.2
rfc2109_cookie_expires_date(LocalTime) ->
    {{YYYY,MM,DD},{Hour,Min,Sec}} =
        case calendar:local_time_to_universal_time_dst(LocalTime) of
            [Gmt]   -> Gmt;
            [_,Gmt] -> Gmt
        end,
    DayNumber = calendar:day_of_the_week({YYYY,MM,DD}),
    lists:flatten(
      io_lib:format("~s, ~2.2.0w-~3.s-~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
                    [httpd_util:day(DayNumber),DD,httpd_util:month(MM),YYYY,Hour,Min,Sec])).

add_seconds(Secs, LocalTime) ->
    Greg = calendar:datetime_to_gregorian_seconds(LocalTime),
    calendar:gregorian_seconds_to_datetime(Greg + Secs).

age_to_cookie_date(Age, LocalTime) ->
    rfc2109_cookie_expires_date(add_seconds(Age, LocalTime)).

%% @spec parse_cookie(string()) -> [{K::string(), V::string()}]
%% @doc Parse the contents of a Cookie header field, ignoring cookie
%% attributes, and return a simple property list.
parse_cookie("") ->
    [];
parse_cookie(Cookie) ->
    parse_cookie(Cookie, []).

%% Internal API

parse_cookie([], Acc) ->
    lists:reverse(Acc);
parse_cookie(String, Acc) ->
    {{Token, Value}, Rest} = read_pair(String),
    Acc1 = case Token of
               "" ->
                   Acc;
               "$" ++ _ ->
                   Acc;
               _ ->
                   [{Token, Value} | Acc]
           end,
    parse_cookie(Rest, Acc1).

read_pair(String) ->
    {Token, Rest} = read_token(skip_whitespace(String)),
    {Value, Rest1} = read_value(skip_whitespace(Rest)),
    {{Token, Value}, skip_past_separator(Rest1)}.

read_value([$= | Value]) ->
    Value1 = skip_whitespace(Value),
    case Value1 of
        [?QUOTE | _] ->
            read_quoted(Value1);
        _ ->
            read_token(Value1)
    end;
read_value(String) ->
    {"", String}.

read_quoted([?QUOTE | String]) ->
    read_quoted(String, []).

read_quoted([], Acc) ->
    {lists:reverse(Acc), []};
read_quoted([?QUOTE | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
read_quoted([$\\, Any | Rest], Acc) ->
    read_quoted(Rest, [Any | Acc]);
read_quoted([C | Rest], Acc) ->
    read_quoted(Rest, [C | Acc]).

skip_whitespace(String) ->
    F = fun (C) -> ?IS_WHITESPACE(C) end,
    lists:dropwhile(F, String).

read_token(String) ->
    F = fun (C) -> not ?IS_SEPARATOR(C) end,
    lists:splitwith(F, String).

skip_past_separator([]) ->
    [];
skip_past_separator([$; | Rest]) ->
    Rest;
skip_past_separator([$, | Rest]) ->
    Rest;
skip_past_separator([_ | Rest]) ->
    skip_past_separator(Rest).

any_to_list(V) when is_list(V) ->
    V;
any_to_list(V) when is_atom(V) ->
    atom_to_list(V);
any_to_list(V) when is_binary(V) ->
    binary_to_list(V);
any_to_list(V) when is_integer(V) ->
    integer_to_list(V).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

quote_test() ->
    %% ?assertError eunit macro is not compatible with coverage module
    try quote(":wq")
    catch error:{cookie_quoting_required, ":wq"} -> ok
    end,
    ?assertEqual(
       "foo",
       quote(foo)),
    ok.

parse_cookie_test() ->
    %% RFC example
    C1 = "$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\";
    Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\";
    Shipping=\"FedEx\"; $Path=\"/acme\"",
    ?assertEqual(
       [{"Customer","WILE_E_COYOTE"},
        {"Part_Number","Rocket_Launcher_0001"},
        {"Shipping","FedEx"}],
       parse_cookie(C1)),
    %% Potential edge cases
    ?assertEqual(
       [{"foo", "x"}],
       parse_cookie("foo=\"\\x\"")),
    ?assertEqual(
       [],
       parse_cookie("=")),
    ?assertEqual(
       [{"foo", ""}, {"bar", ""}],
       parse_cookie("  foo ; bar  ")),
    ?assertEqual(
       [{"foo", ""}, {"bar", ""}],
       parse_cookie("foo=;bar=")),
    ?assertEqual(
       [{"foo", "\";"}, {"bar", ""}],
       parse_cookie("foo = \"\\\";\";bar ")),
    ?assertEqual(
       [{"foo", "\";bar"}],
       parse_cookie("foo=\"\\\";bar")),
    ?assertEqual(
       [],
       parse_cookie([])),
    ?assertEqual(
       [{"foo", "bar"}, {"baz", "wibble"}],
       parse_cookie("foo=bar , baz=wibble ")),
    ok.

domain_test() ->
    ?assertEqual(
       {"Set-Cookie",
        "Customer=WILE_E_COYOTE; "
        "Version=1; "
        "Domain=acme.com; "
        "HttpOnly"},
       cookie("Customer", "WILE_E_COYOTE",
              [{http_only, true}, {domain, "acme.com"}])),
    ok.

local_time_test() ->
    {"Set-Cookie", S} = cookie("Customer", "WILE_E_COYOTE",
                               [{max_age, 111}, {secure, true}]),
    ?assertMatch(
       ["Customer=WILE_E_COYOTE",
        " Version=1",
        " Expires=" ++ _,
        " Max-Age=111",
        " Secure"],
       string:tokens(S, ";")),
    ok.

cookie_test() ->
    C1 = {"Set-Cookie",
          "Customer=WILE_E_COYOTE; "
          "Version=1; "
          "Path=/acme"},
    C1 = cookie("Customer", "WILE_E_COYOTE", [{path, "/acme"}]),
    C1 = cookie("Customer", "WILE_E_COYOTE",
                [{path, "/acme"}, {badoption, "negatory"}]),
    C1 = cookie('Customer', 'WILE_E_COYOTE', [{path, '/acme'}]),
    C1 = cookie(<<"Customer">>, <<"WILE_E_COYOTE">>, [{path, <<"/acme">>}]),

    {"Set-Cookie","=NoKey; Version=1"} = cookie("", "NoKey", []),
    {"Set-Cookie","=NoKey; Version=1"} = cookie("", "NoKey"),
    LocalTime = calendar:universal_time_to_local_time({{2007, 5, 15}, {13, 45, 33}}),
    C2 = {"Set-Cookie",
          "Customer=WILE_E_COYOTE; "
          "Version=1; "
          "Expires=Tue, 15-May-2007 13:45:33 GMT; "
          "Max-Age=0"},
    C2 = cookie("Customer", "WILE_E_COYOTE",
                [{max_age, -111}, {local_time, LocalTime}]),
    C3 = {"Set-Cookie",
          "Customer=WILE_E_COYOTE; "
          "Version=1; "
          "Expires=Wed, 16-May-2007 13:45:50 GMT; "
          "Max-Age=86417"},
    C3 = cookie("Customer", "WILE_E_COYOTE",
                [{max_age, 86417}, {local_time, LocalTime}]),
    ok.

-endif.
