%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
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

-module(cowboy_dispatcher).
-export([split_host/1, split_path/1, match/3]). %% API.

-type bindings() :: list({Key::atom(), Value::binary()}).
-type path_tokens() :: list(binary()).
-type match_rule() :: '_' | '*' | list(binary() | '_' | atom()).
-type dispatch_rule() :: {Host::match_rule(), list({Path::match_rule(),
	Handler::module(), Opts::term()})}.
-type dispatch_rules() :: list(dispatch_rule()).

-export_type([bindings/0, path_tokens/0, dispatch_rules/0]).

-include_lib("kernel/include/inet.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API.

-spec split_host(Host::binary())
	-> {Tokens::path_tokens(), RawHost::binary(), Port::undefined | ip_port()}.
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

-spec split_path(Path::binary())
	-> {Tokens::path_tokens(), RawPath::binary(), Qs::binary()}.
split_path(Path) ->
	case binary:split(Path, <<"?">>) of
		[Path] -> {do_split_path(Path, <<"/">>), Path, <<>>};
		[<<>>, Qs] -> {[], <<>>, Qs};
		[Path2, Qs] -> {do_split_path(Path2, <<"/">>), Path2, Qs}
	end.

-spec do_split_path(RawPath::binary(), Separator::binary())
	-> Tokens::path_tokens().
do_split_path(RawPath, Separator) ->
	case binary:split(RawPath, Separator, [global, trim]) of
		[<<>>|Path] -> Path;
		Path -> Path
	end.

-spec match(Host::path_tokens(), Path::path_tokens(),
	Dispatch::dispatch_rules())
	-> {ok, Handler::module(), Opts::term(), Binds::bindings()}
	| {error, notfound, host} | {error, notfound, path}.
match(_Host, _Path, []) ->
	{error, notfound, host};
match(_Host, Path, [{'_', PathMatchs}|_Tail]) ->
	match_path(Path, PathMatchs, []);
match(Host, Path, [{HostMatch, PathMatchs}|Tail]) ->
	case try_match(host, Host, HostMatch) of
		false ->
			match(Host, Path, Tail);
		{true, HostBinds} ->
			match_path(Path, PathMatchs, HostBinds)
	end.

-spec match_path(Path::path_tokens(), list({Path::match_rule(),
	Handler::module(), Opts::term()}), HostBinds::bindings())
	-> {ok, Handler::module(), Opts::term(), Binds::bindings()}
	| {error, notfound, path}.
match_path(_Path, [], _HostBinds) ->
	{error, notfound, path};
match_path(_Path, [{'_', Handler, Opts}|_Tail], HostBinds) ->
	{ok, Handler, Opts, HostBinds};
match_path('*', [{'*', Handler, Opts}|_Tail], HostBinds) ->
	{ok, Handler, Opts, HostBinds};
match_path(Path, [{PathMatch, Handler, Opts}|Tail], HostBinds) ->
	case try_match(path, Path, PathMatch) of
		false ->
			match_path(Path, Tail, HostBinds);
		{true, PathBinds} ->
			{ok, Handler, Opts, HostBinds ++ PathBinds}
	end.

%% Internal.

-spec try_match(Type::host | path, List::path_tokens(), Match::match_rule())
	-> {true, Binds::bindings()} | false.
try_match(_Type, List, Match) when length(List) =/= length(Match) ->
	false;
try_match(host, List, Match) ->
	list_match(lists:reverse(List), lists:reverse(Match), []);
try_match(path, List, Match) ->
	list_match(List, Match, []).

-spec list_match(List::path_tokens(), Match::match_rule(), Binds::bindings())
	-> {true, Binds::bindings()} | false.
%% Atom '_' matches anything, continue.
list_match([_E|Tail], ['_'|TailMatch], Binds) ->
	list_match(Tail, TailMatch, Binds);
%% Both values match, continue.
list_match([E|Tail], [E|TailMatch], Binds) ->
	list_match(Tail, TailMatch, Binds);
%% Bind E to the variable name V and continue.
list_match([E|Tail], [V|TailMatch], Binds) when is_atom(V) ->
	list_match(Tail, TailMatch, [{V, E}|Binds]);
%% Values don't match, stop.
list_match([_E|_Tail], [_F|_TailMatch], _Binds) ->
	false;
%% Match complete.
list_match([], [], Binds) ->
	{true, Binds}.

%% Tests.

-ifdef(TEST).

split_host_test_() ->
	%% {Host, Result}
	Tests = [
		{<<"">>, {[], <<"">>, undefined}},
		{<<".........">>, {[], <<".........">>, undefined}},
		{<<"*">>, {[<<"*">>], <<"*">>, undefined}},
		{<<"cowboy.dev-extend.eu">>,
			{[<<"cowboy">>, <<"dev-extend">>, <<"eu">>],
			 <<"cowboy.dev-extend.eu">>, undefined}},
		{<<"dev-extend..eu">>,
			{[<<"dev-extend">>, <<>>, <<"eu">>],
			 <<"dev-extend..eu">>, undefined}},
		{<<"dev-extend.eu">>,
			{[<<"dev-extend">>, <<"eu">>], <<"dev-extend.eu">>, undefined}},
		{<<"dev-extend.eu:8080">>,
			{[<<"dev-extend">>, <<"eu">>], <<"dev-extend.eu">>, 8080}},
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
		<<"dev-extend.eu:owns">>,
		<<"dev-extend.eu: owns">>,
		<<"dev-extend.eu:42fun">>,
		<<"dev-extend.eu: 42fun">>,
		<<"dev-extend.eu:42 fun">>,
		<<"dev-extend.eu:fun 42">>,
		<<"dev-extend.eu: 42">>,
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
		{<<"/users">>, [<<"users">>], <<"/users">>, <<"">>},
		{<<"/users?">>, [<<"users">>], <<"/users">>, <<"">>},
		{<<"/users?a">>, [<<"users">>], <<"/users">>, <<"a">>},
		{<<"/users/42/friends?a=b&c=d&e=notsure?whatever">>,
			[<<"users">>, <<"42">>, <<"friends">>],
			<<"/users/42/friends">>, <<"a=b&c=d&e=notsure?whatever">>}
	],
	[{P, fun() -> {R, RawP, Qs} = split_path(P) end}
		|| {P, R, RawP, Qs} <- Tests].

match_test_() ->
	Dispatch = [
		{[<<"www">>, '_', <<"dev-extend">>, <<"eu">>], [
			{[<<"users">>, '_', <<"mails">>], match_any_subdomain_users, []}
		]},
		{[<<"dev-extend">>, <<"eu">>], [
			{[<<"users">>, id, <<"friends">>], match_extend_users_friends, []},
			{'_', match_extend, []}
		]},
		{[<<"dev-extend">>, var], [
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
		{[<<"www">>, <<"any">>, <<"dev-extend">>, <<"eu">>],
			[<<"users">>, <<"42">>, <<"mails">>],
			{ok, match_any_subdomain_users, [], []}},
		{[<<"www">>, <<"dev-extend">>, <<"eu">>],
			[<<"users">>, <<"42">>, <<"mails">>], {ok, match_any, [], []}},
		{[<<"www">>, <<"dev-extend">>, <<"eu">>], [], {ok, match_any, [], []}},
		{[<<"www">>, <<"any">>, <<"dev-extend">>, <<"eu">>],
			[<<"not_users">>, <<"42">>, <<"mails">>], {error, notfound, path}},
		{[<<"dev-extend">>, <<"eu">>], [], {ok, match_extend, [], []}},
		{[<<"dev-extend">>, <<"eu">>], [<<"users">>, <<"42">>, <<"friends">>],
			{ok, match_extend_users_friends, [], [{id, <<"42">>}]}},
		{[<<"erlang">>, <<"fr">>], '_',
			{ok, match_erlang_ext, [], [{ext, <<"fr">>}]}},
		{[<<"any">>], [<<"users">>, <<"444">>, <<"friends">>],
			{ok, match_users_friends, [], [{id, <<"444">>}]}},
		{[<<"dev-extend">>, <<"fr">>], [<<"threads">>, <<"987">>],
			{ok, match_duplicate_vars, [we, {expect, two}, var, here],
			[{var, <<"fr">>}, {var, <<"987">>}]}}
	],
	[{lists:flatten(io_lib:format("~p, ~p", [H, P])), fun() ->
		R = match(H, P, Dispatch)
	end} || {H, P, R} <- Tests].

-endif.
