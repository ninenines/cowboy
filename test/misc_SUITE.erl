%% Copyright (c) 2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(misc_SUITE).
-compile(export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

all() ->
	[{group, no_env}|cowboy_test:common_all()].

groups() ->
	Common = ct_helper:all(?MODULE) -- [set_env_missing],
	[{no_env, [], [set_env_missing]}|cowboy_test:common_groups(Common)].

init_per_group(Name=no_env, Config) ->
	cowboy_test:init_http(Name, #{}, Config);
init_per_group(Name, Config) ->
	cowboy_test:init_common_groups(Name, Config, ?MODULE).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

init_dispatch(_) ->
	cowboy_router:compile([{"localhost", [
		{"/", hello_h, []}
	]}]).

router_invalid_path(Config) ->
	doc("Ensure a path with invalid percent-encoded characters results in a 400."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/version/path/%\\u0016\\u0016/path"),
	{response, _, 400, _} = gun:await(ConnPid, Ref),
	ok.

set_env(Config) ->
	doc("Live replace a middleware environment value."),
	ConnPid1 = gun_open(Config),
	Ref1 = gun:get(ConnPid1, "/"),
	{response, _, 200, _} = gun:await(ConnPid1, Ref1),
	Listener = proplists:get_value(name, config(tc_group_properties, Config)),
	cowboy:set_env(Listener, dispatch, []),
	%% Only new connections get the updated environment.
	ConnPid2 = gun_open(Config),
	Ref2 = gun:get(ConnPid2, "/"),
	{response, _, 400, _} = gun:await(ConnPid2, Ref2),
	ok.

set_env_missing(Config) ->
	doc("Live replace a middleware environment value when env was not provided."),
	ConnPid1 = gun_open(Config),
	Ref1 = gun:get(ConnPid1, "/"),
	{response, _, 500, _} = gun:await(ConnPid1, Ref1),
	Listener = proplists:get_value(name, config(tc_group_properties, Config)),
	cowboy:set_env(Listener, dispatch, []),
	%% Only new connections get the updated environment.
	ConnPid2 = gun_open(Config),
	Ref2 = gun:get(ConnPid2, "/"),
	{response, _, 400, _} = gun:await(ConnPid2, Ref2),
	ok.
