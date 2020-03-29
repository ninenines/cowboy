%% Copyright (c) 2011-2017, Loïc Hoguin <essen@ninenines.eu>
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

%% Routing middleware.
%%
%% Resolve the handler to be used for the request based on the
%% routing information found in the <em>dispatch</em> environment value.
%% When found, the handler module and associated data are added to
%% the environment as the <em>handler</em> and <em>handler_opts</em> values
%% respectively.
%%
%% If the route cannot be found, processing stops with either
%% a 400 or a 404 reply.
-module(cowboy_router).
-behaviour(cowboy_middleware).

-export([compile/1]).
-export([execute/2]).

-type bindings() :: #{atom() => any()}.
-type tokens() :: [binary()].
-export_type([bindings/0]).
-export_type([tokens/0]).

-type route_match() :: '_' | iodata().
-type route_path() :: {Path::route_match(), Handler::module(), Opts::any()}
	| {Path::route_match(), cowboy:fields(), Handler::module(), Opts::any()}.
-type route_rule() :: {Host::route_match(), Paths::[route_path()]}
	| {Host::route_match(), cowboy:fields(), Paths::[route_path()]}.
-type routes() :: [route_rule()].
-export_type([routes/0]).

-type dispatch_match() :: '_' | <<_:8>> | [binary() | '_' | '...' | atom()].
-type dispatch_path() :: {dispatch_match(), cowboy:fields(), module(), any()}.
-type dispatch_rule() :: {Host::dispatch_match(), cowboy:fields(), Paths::[dispatch_path()]}.
-opaque dispatch_rules() :: [dispatch_rule()].
-export_type([dispatch_rules/0]).

-spec compile(routes()) -> dispatch_rules().
compile(Routes) ->
	compile(Routes, []).

compile([], Acc) ->
	lists:reverse(Acc);
compile([{Host, Paths}|Tail], Acc) ->
	compile([{Host, [], Paths}|Tail], Acc);
compile([{HostMatch, Fields, Paths}|Tail], Acc) ->
	HostRules = case HostMatch of
		'_' -> '_';
		_ -> compile_host(HostMatch)
	end,
	PathRules = compile_paths(Paths, []),
	Hosts = case HostRules of
		'_' -> [{'_', Fields, PathRules}];
		_ -> [{R, Fields, PathRules} || R <- HostRules]
	end,
	compile(Tail, Hosts ++ Acc).

compile_host(HostMatch) when is_list(HostMatch) ->
	compile_host(list_to_binary(HostMatch));
compile_host(HostMatch) when is_binary(HostMatch) ->
	compile_rules(HostMatch, $., [], [], <<>>).

compile_paths([], Acc) ->
	lists:reverse(Acc);
compile_paths([{PathMatch, Handler, Opts}|Tail], Acc) ->
	compile_paths([{PathMatch, [], Handler, Opts}|Tail], Acc);
compile_paths([{PathMatch, Fields, Handler, Opts}|Tail], Acc)
		when is_list(PathMatch) ->
	compile_paths([{iolist_to_binary(PathMatch),
		Fields, Handler, Opts}|Tail], Acc);
compile_paths([{'_', Fields, Handler, Opts}|Tail], Acc) ->
	compile_paths(Tail, [{'_', Fields, Handler, Opts}] ++ Acc);
compile_paths([{<<"*">>, Fields, Handler, Opts}|Tail], Acc) ->
	compile_paths(Tail, [{<<"*">>, Fields, Handler, Opts}|Acc]);
compile_paths([{<< $/, PathMatch/bits >>, Fields, Handler, Opts}|Tail],
		Acc) ->
	PathRules = compile_rules(PathMatch, $/, [], [], <<>>),
	Paths = [{lists:reverse(R), Fields, Handler, Opts} || R <- PathRules],
	compile_paths(Tail, Paths ++ Acc);
compile_paths([{PathMatch, _, _, _}|_], _) ->
	error({badarg, "The following route MUST begin with a slash: "
		++ binary_to_list(PathMatch)}).

compile_rules(<<>>, _, Segments, Rules, <<>>) ->
	[Segments|Rules];
compile_rules(<<>>, _, Segments, Rules, Acc) ->
	[[Acc|Segments]|Rules];
compile_rules(<< S, Rest/bits >>, S, Segments, Rules, <<>>) ->
	compile_rules(Rest, S, Segments, Rules, <<>>);
compile_rules(<< S, Rest/bits >>, S, Segments, Rules, Acc) ->
	compile_rules(Rest, S, [Acc|Segments], Rules, <<>>);
%% Colon on path segment start is special, otherwise allow.
compile_rules(<< $:, Rest/bits >>, S, Segments, Rules, <<>>) ->
	{NameBin, Rest2} = compile_binding(Rest, S, <<>>),
	Name = binary_to_atom(NameBin, utf8),
	compile_rules(Rest2, S, Segments, Rules, Name);
compile_rules(<< $[, $., $., $., $], Rest/bits >>, S, Segments, Rules, Acc)
		when Acc =:= <<>> ->
	compile_rules(Rest, S, ['...'|Segments], Rules, Acc);
compile_rules(<< $[, $., $., $., $], Rest/bits >>, S, Segments, Rules, Acc) ->
	compile_rules(Rest, S, ['...', Acc|Segments], Rules, Acc);
compile_rules(<< $[, S, Rest/bits >>, S, Segments, Rules, Acc) ->
	compile_brackets(Rest, S, [Acc|Segments], Rules);
compile_rules(<< $[, Rest/bits >>, S, Segments, Rules, <<>>) ->
	compile_brackets(Rest, S, Segments, Rules);
%% Open bracket in the middle of a segment.
compile_rules(<< $[, _/bits >>, _, _, _, _) ->
	error(badarg);
%% Missing an open bracket.
compile_rules(<< $], _/bits >>, _, _, _, _) ->
	error(badarg);
compile_rules(<< C, Rest/bits >>, S, Segments, Rules, Acc) ->
	compile_rules(Rest, S, Segments, Rules, << Acc/binary, C >>).

%% Everything past $: until the segment separator ($. for hosts,
%% $/ for paths) or $[ or $] or end of binary is the binding name.
compile_binding(<<>>, _, <<>>) ->
	error(badarg);
compile_binding(Rest = <<>>, _, Acc) ->
	{Acc, Rest};
compile_binding(Rest = << C, _/bits >>, S, Acc)
		when C =:= S; C =:= $[; C =:= $] ->
	{Acc, Rest};
compile_binding(<< C, Rest/bits >>, S, Acc) ->
	compile_binding(Rest, S, << Acc/binary, C >>).

compile_brackets(Rest, S, Segments, Rules) ->
	{Bracket, Rest2} = compile_brackets_split(Rest, <<>>, 0),
	Rules1 = compile_rules(Rest2, S, Segments, [], <<>>),
	Rules2 = compile_rules(<< Bracket/binary, Rest2/binary >>,
		S, Segments, [], <<>>),
	Rules ++ Rules2 ++ Rules1.

%% Missing a close bracket.
compile_brackets_split(<<>>, _, _) ->
	error(badarg);
%% Make sure we don't confuse the closing bracket we're looking for.
compile_brackets_split(<< C, Rest/bits >>, Acc, N) when C =:= $[ ->
	compile_brackets_split(Rest, << Acc/binary, C >>, N + 1);
compile_brackets_split(<< C, Rest/bits >>, Acc, N) when C =:= $], N > 0 ->
	compile_brackets_split(Rest, << Acc/binary, C >>, N - 1);
%% That's the right one.
compile_brackets_split(<< $], Rest/bits >>, Acc, 0) ->
	{Acc, Rest};
compile_brackets_split(<< C, Rest/bits >>, Acc, N) ->
	compile_brackets_split(Rest, << Acc/binary, C >>, N).

-spec execute(Req, Env)
	-> {ok, Req, Env} | {stop, Req}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req=#{host := Host, path := Path}, Env=#{dispatch := Dispatch0}) ->
	Dispatch = case Dispatch0 of
		{persistent_term, Key} -> persistent_term:get(Key);
		_ -> Dispatch0
	end,
	case match(Dispatch, Host, Path) of
		{ok, Handler, HandlerOpts, Bindings, HostInfo, PathInfo} ->
			{ok, Req#{
				host_info => HostInfo,
				path_info => PathInfo,
				bindings => Bindings
			}, Env#{
				handler => Handler,
				handler_opts => HandlerOpts
			}};
		{error, notfound, host} ->
			{stop, cowboy_req:reply(400, Req)};
		{error, badrequest, path} ->
			{stop, cowboy_req:reply(400, Req)};
		{error, notfound, path} ->
			{stop, cowboy_req:reply(404, Req)}
	end.

%% Internal.

%% Match hostname tokens and path tokens against dispatch rules.
%%
%% It is typically used for matching tokens for the hostname and path of
%% the request against a global dispatch rule for your listener.
%%
%% Dispatch rules are a list of <em>{Hostname, PathRules}</em> tuples, with
%% <em>PathRules</em> being a list of <em>{Path, HandlerMod, HandlerOpts}</em>.
%%
%% <em>Hostname</em> and <em>Path</em> are match rules and can be either the
%% atom <em>'_'</em>, which matches everything, `<<"*">>', which match the
%% wildcard path, or a list of tokens.
%%
%% Each token can be either a binary, the atom <em>'_'</em>,
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
	| {error, notfound, host} | {error, notfound, path}
	| {error, badrequest, path}.
match([], _, _) ->
	{error, notfound, host};
%% If the host is '_' then there can be no constraints.
match([{'_', [], PathMatchs}|_Tail], _, Path) ->
	match_path(PathMatchs, undefined, Path, #{});
match([{HostMatch, Fields, PathMatchs}|Tail], Tokens, Path)
		when is_list(Tokens) ->
	case list_match(Tokens, HostMatch, #{}) of
		false ->
			match(Tail, Tokens, Path);
		{true, Bindings, HostInfo} ->
			HostInfo2 = case HostInfo of
				undefined -> undefined;
				_ -> lists:reverse(HostInfo)
			end,
			case check_constraints(Fields, Bindings) of
				{ok, Bindings2} ->
					match_path(PathMatchs, HostInfo2, Path, Bindings2);
				nomatch ->
					match(Tail, Tokens, Path)
			end
	end;
match(Dispatch, Host, Path) ->
	match(Dispatch, split_host(Host), Path).

-spec match_path([dispatch_path()],
	HostInfo::undefined | tokens(), binary() | tokens(), bindings())
	-> {ok, module(), any(), bindings(),
		HostInfo::undefined | tokens(),
		PathInfo::undefined | tokens()}
	| {error, notfound, path} | {error, badrequest, path}.
match_path([], _, _, _) ->
	{error, notfound, path};
%% If the path is '_' then there can be no constraints.
match_path([{'_', [], Handler, Opts}|_Tail], HostInfo, _, Bindings) ->
	{ok, Handler, Opts, Bindings, HostInfo, undefined};
match_path([{<<"*">>, _, Handler, Opts}|_Tail], HostInfo, <<"*">>, Bindings) ->
	{ok, Handler, Opts, Bindings, HostInfo, undefined};
match_path([_|Tail], HostInfo, <<"*">>, Bindings) ->
	match_path(Tail, HostInfo, <<"*">>, Bindings);
match_path([{PathMatch, Fields, Handler, Opts}|Tail], HostInfo, Tokens,
		Bindings) when is_list(Tokens) ->
	case list_match(Tokens, PathMatch, Bindings) of
		false ->
			match_path(Tail, HostInfo, Tokens, Bindings);
		{true, PathBinds, PathInfo} ->
			case check_constraints(Fields, PathBinds) of
				{ok, PathBinds2} ->
					{ok, Handler, Opts, PathBinds2, HostInfo, PathInfo};
				nomatch ->
					match_path(Tail, HostInfo, Tokens, Bindings)
			end
	end;
match_path(_Dispatch, _HostInfo, badrequest, _Bindings) ->
	{error, badrequest, path};
match_path(Dispatch, HostInfo, Path, Bindings) ->
	match_path(Dispatch, HostInfo, split_path(Path), Bindings).

check_constraints([], Bindings) ->
	{ok, Bindings};
check_constraints([Field|Tail], Bindings) when is_atom(Field) ->
	check_constraints(Tail, Bindings);
check_constraints([Field|Tail], Bindings) ->
	Name = element(1, Field),
	case Bindings of
		#{Name := Value0} ->
			Constraints = element(2, Field),
			case cowboy_constraints:validate(Value0, Constraints) of
				{ok, Value} ->
					check_constraints(Tail, Bindings#{Name => Value});
				{error, _} ->
					nomatch
			end;
		_ ->
			check_constraints(Tail, Bindings)
	end.

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

%% Following RFC2396, this function may return path segments containing any
%% character, including <em>/</em> if, and only if, a <em>/</em> was escaped
%% and part of a path segment.
-spec split_path(binary()) -> tokens() | badrequest.
split_path(<< $/, Path/bits >>) ->
	split_path(Path, []);
split_path(_) ->
	badrequest.

split_path(Path, Acc) ->
	try
		case binary:match(Path, <<"/">>) of
			nomatch when Path =:= <<>> ->
				remove_dot_segments(lists:reverse([cow_uri:urldecode(S) || S <- Acc]), []);
			nomatch ->
				remove_dot_segments(lists:reverse([cow_uri:urldecode(S) || S <- [Path|Acc]]), []);
			{Pos, _} ->
				<< Segment:Pos/binary, _:8, Rest/bits >> = Path,
				split_path(Rest, [Segment|Acc])
		end
	catch error:_ ->
		badrequest
	end.

remove_dot_segments([], Acc) ->
	lists:reverse(Acc);
remove_dot_segments([<<".">>|Segments], Acc) ->
	remove_dot_segments(Segments, Acc);
remove_dot_segments([<<"..">>|Segments], Acc=[]) ->
	remove_dot_segments(Segments, Acc);
remove_dot_segments([<<"..">>|Segments], [_|Acc]) ->
	remove_dot_segments(Segments, Acc);
remove_dot_segments([S|Segments], Acc) ->
	remove_dot_segments(Segments, [S|Acc]).

-ifdef(TEST).
remove_dot_segments_test_() ->
	Tests = [
		{[<<"a">>, <<"b">>, <<"c">>, <<".">>, <<"..">>, <<"..">>, <<"g">>], [<<"a">>, <<"g">>]},
		{[<<"mid">>, <<"content=5">>, <<"..">>, <<"6">>], [<<"mid">>, <<"6">>]},
		{[<<"..">>, <<"a">>], [<<"a">>]}
	],
	[fun() -> R = remove_dot_segments(S, []) end || {S, R} <- Tests].
-endif.

-spec list_match(tokens(), dispatch_match(), bindings())
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
%% Bind E to the variable name V and continue,
%% unless V was already defined and E isn't identical to the previous value.
list_match([E|Tail], [V|TailMatch], Binds) when is_atom(V) ->
	case Binds of
		%% @todo This isn't right, the constraint must be applied FIRST
		%% otherwise we can't check for example ints in both host/path.
		#{V := E} ->
			list_match(Tail, TailMatch, Binds);
		#{V := _} ->
			false;
		_ ->
			list_match(Tail, TailMatch, Binds#{V => E})
	end;
%% Match complete.
list_match([], [], Binds) ->
	{true, Binds, undefined};
%% Values don't match, stop.
list_match(_List, _Match, _Binds) ->
	false.

%% Tests.

-ifdef(TEST).
compile_test_() ->
	Tests = [
		%% Match any host and path.
		{[{'_', [{'_', h, o}]}],
			[{'_', [], [{'_', [], h, o}]}]},
		{[{"cowboy.example.org",
				[{"/", ha, oa}, {"/path/to/resource", hb, ob}]}],
			[{[<<"org">>, <<"example">>, <<"cowboy">>], [], [
				{[], [], ha, oa},
				{[<<"path">>, <<"to">>, <<"resource">>], [], hb, ob}]}]},
		{[{'_', [{"/path/to/resource/", h, o}]}],
			[{'_', [], [{[<<"path">>, <<"to">>, <<"resource">>], [], h, o}]}]},
		% Cyrillic from a latin1 encoded file.
		{[{'_', [{[47,208,191,209,131,209,130,209,140,47,208,186,47,209,128,
				208,181,209,129,209,131,209,128,209,129,209,131,47], h, o}]}],
			[{'_', [], [{[<<208,191,209,131,209,130,209,140>>, <<208,186>>,
				<<209,128,208,181,209,129,209,131,209,128,209,129,209,131>>],
				[], h, o}]}]},
		{[{"cowboy.example.org.", [{'_', h, o}]}],
			[{[<<"org">>, <<"example">>, <<"cowboy">>], [], [{'_', [], h, o}]}]},
		{[{".cowboy.example.org", [{'_', h, o}]}],
			[{[<<"org">>, <<"example">>, <<"cowboy">>], [], [{'_', [], h, o}]}]},
		% Cyrillic from a latin1 encoded file.
		{[{[208,189,208,181,208,186,208,184,208,185,46,209,129,208,176,
				208,185,209,130,46,209,128,209,132,46], [{'_', h, o}]}],
			[{[<<209,128,209,132>>, <<209,129,208,176,208,185,209,130>>,
				<<208,189,208,181,208,186,208,184,208,185>>],
				[], [{'_', [], h, o}]}]},
		{[{":subdomain.example.org", [{"/hats/:name/prices", h, o}]}],
			[{[<<"org">>, <<"example">>, subdomain], [], [
				{[<<"hats">>, name, <<"prices">>], [], h, o}]}]},
		{[{"ninenines.:_", [{"/hats/:_", h, o}]}],
			[{['_', <<"ninenines">>], [], [{[<<"hats">>, '_'], [], h, o}]}]},
		{[{"[www.]ninenines.eu",
			[{"/horses", h, o}, {"/hats/[page/:number]", h, o}]}], [
				{[<<"eu">>, <<"ninenines">>], [], [
					{[<<"horses">>], [], h, o},
					{[<<"hats">>], [], h, o},
					{[<<"hats">>, <<"page">>, number], [], h, o}]},
				{[<<"eu">>, <<"ninenines">>, <<"www">>], [], [
					{[<<"horses">>], [], h, o},
					{[<<"hats">>], [], h, o},
					{[<<"hats">>, <<"page">>, number], [], h, o}]}]},
		{[{'_', [{"/hats/:page/:number", h, o}]}], [{'_', [], [
			{[<<"hats">>, page, number], [], h, o}]}]},
		{[{'_', [{"/hats/[page/[:number]]", h, o}]}], [{'_', [], [
			{[<<"hats">>], [], h, o},
			{[<<"hats">>, <<"page">>], [], h, o},
			{[<<"hats">>, <<"page">>, number], [], h, o}]}]},
		{[{"[...]ninenines.eu", [{"/hats/[...]", h, o}]}],
			[{[<<"eu">>, <<"ninenines">>, '...'], [], [
				{[<<"hats">>, '...'], [], h, o}]}]},
		%% Path segment containing a colon.
		{[{'_', [{"/foo/bar:blah", h, o}]}], [{'_', [], [
			{[<<"foo">>, <<"bar:blah">>], [], h, o}]}]}
	],
	[{lists:flatten(io_lib:format("~p", [Rt])),
		fun() -> Rs = compile(Rt) end} || {Rt, Rs} <- Tests].

split_host_test_() ->
	Tests = [
		{<<"">>, []},
		{<<"*">>, [<<"*">>]},
		{<<"cowboy.ninenines.eu">>,
			[<<"eu">>, <<"ninenines">>, <<"cowboy">>]},
		{<<"ninenines.eu">>,
			[<<"eu">>, <<"ninenines">>]},
		{<<"ninenines.eu.">>,
			[<<"eu">>, <<"ninenines">>]},
		{<<"a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z">>,
			[<<"z">>, <<"y">>, <<"x">>, <<"w">>, <<"v">>, <<"u">>, <<"t">>,
			<<"s">>, <<"r">>, <<"q">>, <<"p">>, <<"o">>, <<"n">>, <<"m">>,
			<<"l">>, <<"k">>, <<"j">>, <<"i">>, <<"h">>, <<"g">>, <<"f">>,
			<<"e">>, <<"d">>, <<"c">>, <<"b">>, <<"a">>]}
	],
	[{H, fun() -> R = split_host(H) end} || {H, R} <- Tests].

split_path_test_() ->
	Tests = [
		{<<"/">>, []},
		{<<"/extend//cowboy">>, [<<"extend">>, <<>>, <<"cowboy">>]},
		{<<"/users">>, [<<"users">>]},
		{<<"/users/42/friends">>, [<<"users">>, <<"42">>, <<"friends">>]},
		{<<"/users/a%20b/c%21d">>, [<<"users">>, <<"a b">>, <<"c!d">>]}
	],
	[{P, fun() -> R = split_path(P) end} || {P, R} <- Tests].

match_test_() ->
	Dispatch = [
		{[<<"eu">>, <<"ninenines">>, '_', <<"www">>], [], [
			{[<<"users">>, '_', <<"mails">>], [], match_any_subdomain_users, []}
		]},
		{[<<"eu">>, <<"ninenines">>], [], [
			{[<<"users">>, id, <<"friends">>], [], match_extend_users_friends, []},
			{'_', [], match_extend, []}
		]},
		{[var, <<"ninenines">>], [], [
			{[<<"threads">>, var], [], match_duplicate_vars,
				[we, {expect, two}, var, here]}
		]},
		{[ext, <<"erlang">>], [], [
			{'_', [], match_erlang_ext, []}
		]},
		{'_', [], [
			{[<<"users">>, id, <<"friends">>], [], match_users_friends, []},
			{'_', [], match_any, []}
		]}
	],
	Tests = [
		{<<"any">>, <<"/">>, {ok, match_any, [], #{}}},
		{<<"www.any.ninenines.eu">>, <<"/users/42/mails">>,
			{ok, match_any_subdomain_users, [], #{}}},
		{<<"www.ninenines.eu">>, <<"/users/42/mails">>,
			{ok, match_any, [], #{}}},
		{<<"www.ninenines.eu">>, <<"/">>,
			{ok, match_any, [], #{}}},
		{<<"www.any.ninenines.eu">>, <<"/not_users/42/mails">>,
			{error, notfound, path}},
		{<<"ninenines.eu">>, <<"/">>,
			{ok, match_extend, [], #{}}},
		{<<"ninenines.eu">>, <<"/users/42/friends">>,
			{ok, match_extend_users_friends, [], #{id => <<"42">>}}},
		{<<"erlang.fr">>, '_',
			{ok, match_erlang_ext, [], #{ext => <<"fr">>}}},
		{<<"any">>, <<"/users/444/friends">>,
			{ok, match_users_friends, [], #{id => <<"444">>}}},
		{<<"any">>, <<"/users//friends">>,
			{ok, match_users_friends, [], #{id => <<>>}}}
	],
	[{lists:flatten(io_lib:format("~p, ~p", [H, P])), fun() ->
		{ok, Handler, Opts, Binds, undefined, undefined}
			= match(Dispatch, H, P)
	end} || {H, P, {ok, Handler, Opts, Binds}} <- Tests].

match_info_test_() ->
	Dispatch = [
		{[<<"eu">>, <<"ninenines">>, <<"www">>], [], [
			{[<<"pathinfo">>, <<"is">>, <<"next">>, '...'], [], match_path, []}
		]},
		{[<<"eu">>, <<"ninenines">>, '...'], [], [
			{'_', [], match_any, []}
		]}
	],
	Tests = [
		{<<"ninenines.eu">>, <<"/">>,
			{ok, match_any, [], #{}, [], undefined}},
		{<<"bugs.ninenines.eu">>, <<"/">>,
			{ok, match_any, [], #{}, [<<"bugs">>], undefined}},
		{<<"cowboy.bugs.ninenines.eu">>, <<"/">>,
			{ok, match_any, [], #{}, [<<"cowboy">>, <<"bugs">>], undefined}},
		{<<"www.ninenines.eu">>, <<"/pathinfo/is/next">>,
			{ok, match_path, [], #{}, undefined, []}},
		{<<"www.ninenines.eu">>, <<"/pathinfo/is/next/path_info">>,
			{ok, match_path, [], #{}, undefined, [<<"path_info">>]}},
		{<<"www.ninenines.eu">>, <<"/pathinfo/is/next/foo/bar">>,
			{ok, match_path, [], #{}, undefined, [<<"foo">>, <<"bar">>]}}
	],
	[{lists:flatten(io_lib:format("~p, ~p", [H, P])), fun() ->
		R = match(Dispatch, H, P)
	end} || {H, P, R} <- Tests].

match_constraints_test() ->
	Dispatch0 = [{'_', [],
		[{[<<"path">>, value], [{value, int}], match, []}]}],
	{ok, _, [], #{value := 123}, _, _} = match(Dispatch0,
		<<"ninenines.eu">>, <<"/path/123">>),
	{ok, _, [], #{value := 123}, _, _} = match(Dispatch0,
		<<"ninenines.eu">>, <<"/path/123/">>),
	{error, notfound, path} = match(Dispatch0,
		<<"ninenines.eu">>, <<"/path/NaN/">>),
	Dispatch1 = [{'_', [],
		[{[<<"path">>, value, <<"more">>], [{value, nonempty}], match, []}]}],
	{ok, _, [], #{value := <<"something">>}, _, _} = match(Dispatch1,
		<<"ninenines.eu">>, <<"/path/something/more">>),
	{error, notfound, path} = match(Dispatch1,
		<<"ninenines.eu">>, <<"/path//more">>),
	Dispatch2 = [{'_', [], [{[<<"path">>, username],
		[{username, fun(_, Value) ->
			case cowboy_bstr:to_lower(Value) of
				Value -> {ok, Value};
				_ -> {error, not_lowercase}
			end end}],
		match, []}]}],
	{ok, _, [], #{username := <<"essen">>}, _, _} = match(Dispatch2,
		<<"ninenines.eu">>, <<"/path/essen">>),
	{error, notfound, path} = match(Dispatch2,
		<<"ninenines.eu">>, <<"/path/ESSEN">>),
	ok.

match_same_bindings_test() ->
	Dispatch = [{[same, same], [], [{'_', [], match, []}]}],
	{ok, _, [], #{same := <<"eu">>}, _, _} = match(Dispatch,
		<<"eu.eu">>, <<"/">>),
	{error, notfound, host} = match(Dispatch,
		<<"ninenines.eu">>, <<"/">>),
	Dispatch2 = [{[<<"eu">>, <<"ninenines">>, user], [],
		[{[<<"path">>, user], [], match, []}]}],
	{ok, _, [], #{user := <<"essen">>}, _, _} = match(Dispatch2,
		<<"essen.ninenines.eu">>, <<"/path/essen">>),
	{ok, _, [], #{user := <<"essen">>}, _, _} = match(Dispatch2,
		<<"essen.ninenines.eu">>, <<"/path/essen/">>),
	{error, notfound, path} = match(Dispatch2,
		<<"essen.ninenines.eu">>, <<"/path/notessen">>),
	Dispatch3 = [{'_', [], [{[same, same], [], match, []}]}],
	{ok, _, [], #{same := <<"path">>}, _, _} = match(Dispatch3,
		<<"ninenines.eu">>, <<"/path/path">>),
	{error, notfound, path} = match(Dispatch3,
		<<"ninenines.eu">>, <<"/path/to">>),
	ok.
-endif.
