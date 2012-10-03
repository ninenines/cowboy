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
-export([match/3]).

-type bindings() :: [{atom(), binary()}].
-type tokens() :: [binary()].
-type match_rule() :: '_' | '*' | [binary() | '_' | '...' | atom()].
-type dispatch_path() :: [{match_rule(), module(), any()}].
-type dispatch_rule() :: {Host::match_rule(), Path::dispatch_path()}.
-type dispatch_rules() :: [dispatch_rule()].

-export_type([bindings/0]).
-export_type([tokens/0]).
-export_type([dispatch_rules/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API.

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
-spec match(dispatch_rules(), Host::binary() | tokens(), Path::binary())
	-> {ok, module(), any(), bindings(),
		HostInfo::undefined | tokens(),
		PathInfo::undefined | tokens()}
	| {error, notfound, host} | {error, notfound, path}.
match([], _, _) ->
	{error, notfound, host};
match([{'_', PathMatchs}|_Tail], _, Path) ->
	match_path(PathMatchs, undefined, Path, []);
match([{HostMatch, PathMatchs}|Tail], Tokens, Path)
		when is_list(Tokens) ->
	case list_match(Tokens, lists:reverse(HostMatch), []) of
		false ->
			match(Tail, Tokens, Path);
		{true, Bindings, undefined} ->
			match_path(PathMatchs, undefined, Path, Bindings);
		{true, Bindings, HostInfo} ->
			match_path(PathMatchs, lists:reverse(HostInfo),
				Path, Bindings)
	end;
match(Dispatch, Host, Path) ->
	match(Dispatch, split_host(Host), Path).

-spec match_path(dispatch_path(),
	HostInfo::undefined | tokens(), binary() | tokens(), bindings())
	-> {ok, module(), any(), bindings(),
		HostInfo::undefined | tokens(),
		PathInfo::undefined | tokens()}
	| {error, notfound, path}.
match_path([], _, _, _) ->
	{error, notfound, path};
match_path([{'_', Handler, Opts}|_Tail], HostInfo, _, Bindings) ->
	{ok, Handler, Opts, Bindings, HostInfo, undefined};
match_path([{'*', Handler, Opts}|_Tail], HostInfo, '*', Bindings) ->
	{ok, Handler, Opts, Bindings, HostInfo, undefined};
match_path([{PathMatch, Handler, Opts}|Tail], HostInfo, Tokens,
		Bindings) when is_list(Tokens) ->
	case list_match(Tokens, PathMatch, []) of
		false ->
			match_path(Tail, HostInfo, Tokens, Bindings);
		{true, PathBinds, PathInfo} ->
			{ok, Handler, Opts, Bindings ++ PathBinds, HostInfo, PathInfo}
	end;
match_path(Dispatch, HostInfo, Path, Bindings) ->
	match_path(Dispatch, HostInfo, split_path(Path), Bindings).

%% Internal.

%% @doc Split a hostname into a list of tokens.
-spec split_host(binary()) -> tokens().
split_host(Host) ->
	split_host(Host, []).

split_host(Host, Acc) ->
	case binary:match(Host, <<".">>) of
		nomatch when Host =:= <<>> ->
			Acc;
		nomatch ->
			[Host|Acc];
		{Pos, _} ->
			<< Segment:Pos/binary, _:8, Rest/bits >> = Host,
			false = byte_size(Segment) == 0,
			split_host(Rest, [Segment|Acc])
	end.

%% @doc Split a path into a list of path segments.
%%
%% Following RFC2396, this function may return path segments containing any
%% character, including <em>/</em> if, and only if, a <em>/</em> was escaped
%% and part of a path segment.
-spec split_path(binary()) -> tokens().
split_path(<< $/, Path/bits >>) ->
	split_path(Path, []).

split_path(Path, Acc) ->
	case binary:match(Path, <<"/">>) of
		nomatch when Path =:= <<>> ->
			lists:reverse([cowboy_http:urldecode(S) || S <- Acc]);
		nomatch ->
			lists:reverse([cowboy_http:urldecode(S) || S <- [Path|Acc]]);
		{Pos, _} ->
			<< Segment:Pos/binary, _:8, Rest/bits >> = Path,
			split_path(Rest, [Segment|Acc])
	end.

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
		{<<"">>, []},
		{<<"*">>, [<<"*">>]},
		{<<"cowboy.ninenines.eu">>,
			[<<"eu">>, <<"ninenines">>, <<"cowboy">>]},
		{<<"ninenines.eu">>,
			[<<"eu">>, <<"ninenines">>]},
		{<<"a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z">>,
			[<<"z">>, <<"y">>, <<"x">>, <<"w">>, <<"v">>, <<"u">>, <<"t">>,
			<<"s">>, <<"r">>, <<"q">>, <<"p">>, <<"o">>, <<"n">>, <<"m">>,
			<<"l">>, <<"k">>, <<"j">>, <<"i">>, <<"h">>, <<"g">>, <<"f">>,
			<<"e">>, <<"d">>, <<"c">>, <<"b">>, <<"a">>]}
	],
	[{H, fun() -> R = split_host(H) end} || {H, R} <- Tests].

split_path_test_() ->
	%% {Path, Result, QueryString}
	Tests = [
		{<<"/">>, []},
		{<<"/extend//cowboy">>, [<<"extend">>, <<>>, <<"cowboy">>]},
		{<<"/users">>, [<<"users">>]},
		{<<"/users/42/friends">>, [<<"users">>, <<"42">>, <<"friends">>]},
		{<<"/users/a+b/c%21d">>, [<<"users">>, <<"a b">>, <<"c!d">>]}
	],
	[{P, fun() -> R = split_path(P) end} || {P, R} <- Tests].

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
		{<<"any">>, <<"/">>, {ok, match_any, [], []}},
		{<<"www.any.ninenines.eu">>, <<"/users/42/mails">>,
			{ok, match_any_subdomain_users, [], []}},
		{<<"www.ninenines.eu">>, <<"/users/42/mails">>,
			{ok, match_any, [], []}},
		{<<"www.ninenines.eu">>, <<"/">>,
			{ok, match_any, [], []}},
		{<<"www.any.ninenines.eu">>, <<"/not_users/42/mails">>,
			{error, notfound, path}},
		{<<"ninenines.eu">>, <<"/">>,
			{ok, match_extend, [], []}},
		{<<"ninenines.eu">>, <<"/users/42/friends">>,
			{ok, match_extend_users_friends, [], [{id, <<"42">>}]}},
		{<<"erlang.fr">>, '_',
			{ok, match_erlang_ext, [], [{ext, <<"fr">>}]}},
		{<<"any">>, <<"/users/444/friends">>,
			{ok, match_users_friends, [], [{id, <<"444">>}]}},
		{<<"ninenines.fr">>, <<"/threads/987">>,
			{ok, match_duplicate_vars, [we, {expect, two}, var, here],
				[{var, <<"fr">>}, {var, <<"987">>}]}}
	],
	[{lists:flatten(io_lib:format("~p, ~p", [H, P])), fun() ->
		{ok, Handler, Opts, Binds, undefined, undefined}
			= match(Dispatch, H, P)
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
		{<<"ninenines.eu">>, <<"/">>,
			{ok, match_any, [], [], [], undefined}},
		{<<"bugs.ninenines.eu">>, <<"/">>,
			{ok, match_any, [], [], [<<"bugs">>], undefined}},
		{<<"cowboy.bugs.ninenines.eu">>, <<"/">>,
			{ok, match_any, [], [], [<<"cowboy">>, <<"bugs">>], undefined}},
		{<<"www.ninenines.eu">>, <<"/pathinfo/is/next">>,
			{ok, match_path, [], [], undefined, []}},
		{<<"www.ninenines.eu">>, <<"/pathinfo/is/next/path_info">>,
			{ok, match_path, [], [], undefined, [<<"path_info">>]}},
		{<<"www.ninenines.eu">>, <<"/pathinfo/is/next/foo/bar">>,
			{ok, match_path, [], [], undefined, [<<"foo">>, <<"bar">>]}}
	],
	[{lists:flatten(io_lib:format("~p, ~p", [H, P])), fun() ->
		R = match(Dispatch, H, P)
	end} || {H, P, R} <- Tests].

-endif.
