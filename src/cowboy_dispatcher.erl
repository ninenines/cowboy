%% Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>
%% Copyright (c) 2011, Anthony Ramine <nox@dev-extend.eu>
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

%% @doc Dispatch requests according to a hostname and path.
-module(cowboy_dispatcher).

%% API.
-export([split_host/1]).
-export([split_path/2]).
-export([match/3]).

-type bindings() :: list({atom(), binary()}).
-type tokens() :: list(binary()).
-type match_rule() :: '_' | '*' | list(binary() | '_' | '...' | atom()).
-type dispatch_path() :: list({match_rule(), module(), any()}).
-type dispatch_rule() :: {Host::match_rule(), Path::dispatch_path()}.
-type dispatch_rules() :: list(dispatch_rule()).

-export_type([bindings/0]).
-export_type([tokens/0]).
-export_type([dispatch_rules/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API.

%% @doc Split a hostname into a list of tokens.
-spec split_host(binary())
	-> {tokens(), binary(), undefined | inet:port_number()}.
split_host(<<>>) ->
	{[], <<>>, undefined};
split_host(Host) ->
	case binary:split(Host, <<":">>) of
		[Host] ->
			{binary:split(Host, <<".">>, [global, trim]), Host, undefined};
		[Host2, Port] ->
			{binary:split(Host2, <<".">>, [global, trim]), Host2,
				list_to_integer(binary_to_list(Port))}
	end.

%% @doc Split a path into a list of path segments.
%%
%% Following RFC2396, this function may return path segments containing any
%% character, including <em>/</em> if, and only if, a <em>/</em> was escaped
%% and part of a path segment.
-spec split_path(binary(), fun((binary()) -> binary())) ->
		{tokens(), binary(), binary()}.
split_path(Path, URLDec) ->
	case binary:split(Path, <<"?">>) of
		[Path] -> {do_split_path(Path, <<"/">>, URLDec), Path, <<>>};
		[<<>>, Qs] -> {[], <<>>, Qs};
		[Path2, Qs] -> {do_split_path(Path2, <<"/">>, URLDec), Path2, Qs}
	end.

-spec do_split_path(binary(), <<_:8>>, fun((binary()) -> binary())) -> tokens().
do_split_path(RawPath, Separator, URLDec) ->
	EncodedPath = case binary:split(RawPath, Separator, [global, trim]) of
		[<<>>|Path] -> Path;
		Path -> Path
	end,
	[URLDec(Token) || Token <- EncodedPath].

%% @doc Match hostname tokens and path tokens against dispatch rules.
%%
%% It is typically used for matching tokens for the hostname and path of
%% the request against a global dispatch rule for your listener.
%%
%% Dispatch rules are a list of <em>{Hostname, PathRules}</em> tuples, with
%% <em>PathRules</em> being a list of <em>{Path, HandlerMod, HandlerOpts}</em>.
%%
%% <em>Hostname</em> and <em>Path</em> are match rules and can be either the
%% atom <em>'_'</em>, which matches everything for a single token, the atom
%% <em>'*'</em>, which matches everything for the rest of the tokens, or a
%% list of tokens. Each token can be either a binary, the atom <em>'_'</em>,
%% the atom '...' or a named atom. A binary token must match exactly,
%% <em>'_'</em> matches everything for a single token, <em>'...'</em> matches
%% everything for the rest of the tokens and a named atom will bind the
%% corresponding token value and return it.
%%
%% The list of hostname tokens is reversed before matching. For example, if
%% we were to match "www.ninenines.eu", we would first match "eu", then
%% "ninenines", then "www". This means that in the context of hostnames,
%% the <em>'...'</em> atom matches properly the lower levels of the domain
%% as would be expected.
%%
%% When a result is found, this function will return the handler module and
%% options found in the dispatch list, a key-value list of bindings and
%% the tokens that were matched by the <em>'...'</em> atom for both the
%% hostname and path.
-spec match(Host::tokens(), Path::tokens(), dispatch_rules())
	-> {ok, module(), any(), bindings(),
		HostInfo::undefined | tokens(),
		PathInfo::undefined | tokens()}
	| {error, notfound, host} | {error, notfound, path}.
match(_Host, _Path, []) ->
	{error, notfound, host};
match(_Host, Path, [{'_', PathMatchs}|_Tail]) ->
	match_path(Path, PathMatchs, [], undefined);
match(Host, Path, [{HostMatch, PathMatchs}|Tail]) ->
	case try_match(host, Host, HostMatch) of
		false ->
			match(Host, Path, Tail);
		{true, HostBinds, undefined} ->
			match_path(Path, PathMatchs, HostBinds, undefined);
		{true, HostBinds, HostInfo} ->
			match_path(Path, PathMatchs, HostBinds, lists:reverse(HostInfo))
	end.

-spec match_path(tokens(), dispatch_path(), bindings(),
	HostInfo::undefined | tokens())
	-> {ok, module(), any(), bindings(),
		HostInfo::undefined | tokens(),
		PathInfo::undefined | tokens()}
	| {error, notfound, path}.
match_path(_Path, [], _HostBinds, _HostInfo) ->
	{error, notfound, path};
match_path(_Path, [{'_', Handler, Opts}|_Tail], HostBinds, HostInfo) ->
	{ok, Handler, Opts, HostBinds, HostInfo, undefined};
match_path('*', [{'*', Handler, Opts}|_Tail], HostBinds, HostInfo) ->
	{ok, Handler, Opts, HostBinds, HostInfo, undefined};
match_path(Path, [{PathMatch, Handler, Opts}|Tail], HostBinds, HostInfo) ->
	case try_match(path, Path, PathMatch) of
		false ->
			match_path(Path, Tail, HostBinds, HostInfo);
		{true, PathBinds, PathInfo} ->
			{ok, Handler, Opts, HostBinds ++ PathBinds, HostInfo, PathInfo}
	end.

%% Internal.

-spec try_match(host | path, tokens(), match_rule())
	-> {true, bindings(), undefined | tokens()} | false.
try_match(host, List, Match) ->
	list_match(lists:reverse(List), lists:reverse(Match), []);
try_match(path, List, Match) ->
	list_match(List, Match, []).

-spec list_match(tokens(), match_rule(), bindings())
	-> {true, bindings(), undefined | tokens()} | false.
%% Atom '...' matches any trailing path, stop right now.
list_match(List, ['...'], Binds) ->
	{true, Binds, List};
%% Atom '_' matches anything, continue.
list_match([_E|Tail], ['_'|TailMatch], Binds) ->
	list_match(Tail, TailMatch, Binds);
%% Both values match, continue.
list_match([E|Tail], [E|TailMatch], Binds) ->
	list_match(Tail, TailMatch, Binds);
%% Bind E to the variable name V and continue.
list_match([E|Tail], [V|TailMatch], Binds) when is_atom(V) ->
	list_match(Tail, TailMatch, [{V, E}|Binds]);
%% Match complete.
list_match([], [], Binds) ->
	{true, Binds, undefined};
%% Values don't match, stop.
list_match(_List, _Match, _Binds) ->
	false.

%% Tests.

-ifdef(TEST).

split_host_test_() ->
	%% {Host, Result}
	Tests = [
		{<<"">>, {[], <<"">>, undefined}},
		{<<".........">>, {[], <<".........">>, undefined}},
		{<<"*">>, {[<<"*">>], <<"*">>, undefined}},
		{<<"cowboy.ninenines.eu">>,
			{[<<"cowboy">>, <<"ninenines">>, <<"eu">>],
			 <<"cowboy.ninenines.eu">>, undefined}},
		{<<"ninenines..eu">>,
			{[<<"ninenines">>, <<>>, <<"eu">>],
			 <<"ninenines..eu">>, undefined}},
		{<<"ninenines.eu">>,
			{[<<"ninenines">>, <<"eu">>], <<"ninenines.eu">>, undefined}},
		{<<"ninenines.eu:8080">>,
			{[<<"ninenines">>, <<"eu">>], <<"ninenines.eu">>, 8080}},
		{<<"a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z">>,
			{[<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>, <<"f">>, <<"g">>,
			  <<"h">>, <<"i">>, <<"j">>, <<"k">>, <<"l">>, <<"m">>, <<"n">>,
			  <<"o">>, <<"p">>, <<"q">>, <<"r">>, <<"s">>, <<"t">>, <<"u">>,
			  <<"v">>, <<"w">>, <<"x">>, <<"y">>, <<"z">>],
			 <<"a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z">>,
			 undefined}}
	],
	[{H, fun() -> R = split_host(H) end} || {H, R} <- Tests].

split_host_fail_test_() ->
	Tests = [
		<<"ninenines.eu:owns">>,
		<<"ninenines.eu: owns">>,
		<<"ninenines.eu:42fun">>,
		<<"ninenines.eu: 42fun">>,
		<<"ninenines.eu:42 fun">>,
		<<"ninenines.eu:fun 42">>,
		<<"ninenines.eu: 42">>,
		<<":owns">>,
		<<":42 fun">>
	],
	[{H, fun() -> case catch split_host(H) of
		{'EXIT', _Reason} -> ok
	end end} || H <- Tests].

split_path_test_() ->
	%% {Path, Result, QueryString}
	Tests = [
		{<<"?">>, [], <<"">>, <<"">>},
		{<<"???">>, [], <<"">>, <<"??">>},
		{<<"/">>, [], <<"/">>, <<"">>},
		{<<"/extend//cowboy">>, [<<"extend">>, <<>>, <<"cowboy">>],
			<<"/extend//cowboy">>, <<>>},
		{<<"/users">>, [<<"users">>], <<"/users">>, <<"">>},
		{<<"/users?">>, [<<"users">>], <<"/users">>, <<"">>},
		{<<"/users?a">>, [<<"users">>], <<"/users">>, <<"a">>},
		{<<"/users/42/friends?a=b&c=d&e=notsure?whatever">>,
			[<<"users">>, <<"42">>, <<"friends">>],
			<<"/users/42/friends">>, <<"a=b&c=d&e=notsure?whatever">>},
		{<<"/users/a+b/c%21d?e+f=g+h">>,
			[<<"users">>, <<"a b">>, <<"c!d">>],
			<<"/users/a+b/c%21d">>, <<"e+f=g+h">>}
	],
	URLDecode = fun(Bin) -> cowboy_http:urldecode(Bin, crash) end,
	[{P, fun() -> {R, RawP, Qs} = split_path(P, URLDecode) end}
		|| {P, R, RawP, Qs} <- Tests].

match_test_() ->
	Dispatch = [
		{[<<"www">>, '_', <<"ninenines">>, <<"eu">>], [
			{[<<"users">>, '_', <<"mails">>], match_any_subdomain_users, []}
		]},
		{[<<"ninenines">>, <<"eu">>], [
			{[<<"users">>, id, <<"friends">>], match_extend_users_friends, []},
			{'_', match_extend, []}
		]},
		{[<<"ninenines">>, var], [
			{[<<"threads">>, var], match_duplicate_vars,
				[we, {expect, two}, var, here]}
		]},
		{[<<"erlang">>, ext], [
			{'_', match_erlang_ext, []}
		]},
		{'_', [
			{[<<"users">>, id, <<"friends">>], match_users_friends, []},
			{'_', match_any, []}
		]}
	],
	%% {Host, Path, Result}
	Tests = [
		{[<<"any">>], [], {ok, match_any, [], []}},
		{[<<"www">>, <<"any">>, <<"ninenines">>, <<"eu">>],
			[<<"users">>, <<"42">>, <<"mails">>],
			{ok, match_any_subdomain_users, [], []}},
		{[<<"www">>, <<"ninenines">>, <<"eu">>],
			[<<"users">>, <<"42">>, <<"mails">>], {ok, match_any, [], []}},
		{[<<"www">>, <<"ninenines">>, <<"eu">>], [], {ok, match_any, [], []}},
		{[<<"www">>, <<"any">>, <<"ninenines">>, <<"eu">>],
			[<<"not_users">>, <<"42">>, <<"mails">>], {error, notfound, path}},
		{[<<"ninenines">>, <<"eu">>], [], {ok, match_extend, [], []}},
		{[<<"ninenines">>, <<"eu">>], [<<"users">>, <<"42">>, <<"friends">>],
			{ok, match_extend_users_friends, [], [{id, <<"42">>}]}},
		{[<<"erlang">>, <<"fr">>], '_',
			{ok, match_erlang_ext, [], [{ext, <<"fr">>}]}},
		{[<<"any">>], [<<"users">>, <<"444">>, <<"friends">>],
			{ok, match_users_friends, [], [{id, <<"444">>}]}},
		{[<<"ninenines">>, <<"fr">>], [<<"threads">>, <<"987">>],
			{ok, match_duplicate_vars, [we, {expect, two}, var, here],
			[{var, <<"fr">>}, {var, <<"987">>}]}}
	],
	[{lists:flatten(io_lib:format("~p, ~p", [H, P])), fun() ->
		{ok, Handler, Opts, Binds, undefined, undefined} = match(H, P, Dispatch)
	end} || {H, P, {ok, Handler, Opts, Binds}} <- Tests].

match_info_test_() ->
	Dispatch = [
		{[<<"www">>, <<"ninenines">>, <<"eu">>], [
			{[<<"pathinfo">>, <<"is">>, <<"next">>, '...'], match_path, []}
		]},
		{['...', <<"ninenines">>, <<"eu">>], [
			{'_', match_any, []}
		]}
	],
	Tests = [
		{[<<"ninenines">>, <<"eu">>], [],
			{ok, match_any, [], [], [], undefined}},
		{[<<"bugs">>, <<"ninenines">>, <<"eu">>], [],
			{ok, match_any, [], [], [<<"bugs">>], undefined}},
		{[<<"cowboy">>, <<"bugs">>, <<"ninenines">>, <<"eu">>], [],
			{ok, match_any, [], [], [<<"cowboy">>, <<"bugs">>], undefined}},
		{[<<"www">>, <<"ninenines">>, <<"eu">>],
			[<<"pathinfo">>, <<"is">>, <<"next">>],
			{ok, match_path, [], [], undefined, []}},
		{[<<"www">>, <<"ninenines">>, <<"eu">>],
			[<<"pathinfo">>, <<"is">>, <<"next">>, <<"path_info">>],
			{ok, match_path, [], [], undefined, [<<"path_info">>]}},
		{[<<"www">>, <<"ninenines">>, <<"eu">>],
			[<<"pathinfo">>, <<"is">>, <<"next">>, <<"foo">>, <<"bar">>],
			{ok, match_path, [], [], undefined, [<<"foo">>, <<"bar">>]}}
	],
	[{lists:flatten(io_lib:format("~p, ~p", [H, P])), fun() ->
		R = match(H, P, Dispatch)
	end} || {H, P, R} <- Tests].

-endif.
