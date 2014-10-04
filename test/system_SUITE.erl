%% Copyright (c) 2014, James Fish <james@fishcakez.com>
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

-module(system_SUITE).
-compile(export_all).

-import(cowboy_test, [config/2]).
-import(cowboy_test, [doc/1]).
-import(cowboy_test, [gun_open/1]).

%% ct.

all() ->
	cowboy_test:common_all().

groups() ->
	cowboy_test:common_groups(cowboy_test:all(?MODULE)).

init_per_group(Name, Config) ->
	cowboy_test:init_common_groups(Name, Config, ?MODULE).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

%% Dispatch configuration.

init_dispatch(_) ->
	cowboy_router:compile([{'_', [
		{"/default", system_default_h, system_default_sp},
		{"/default/handler", system_default_handler_h, system_full_sp},
		{"/full", system_full_h, system_full_sp}
	]}]).

%% Tests.

default(Config) ->
	doc("Ensure that a minimal sub_protocol or middleware handles system messages"),
	ConnPid = gun_open(Config),
	register(system_default_tester, self()),
	Ref = gun:get(ConnPid, "/default"),
	Pid = receive {system_default_h, P} -> P after 500 -> exit(timeout) end,
	unregister(system_default_tester),
	ok = sys:suspend(Pid),
	{system_default_sp, _Req, undefined} = sys:get_state(Pid),
	%% code_change will do nothing to sub_protocol
	ok = sys:change_code(Pid, system_default_sp, undefined, code_change),
	ok = sys:resume(Pid),
	Replace = fun({Mod, Req2, undefined}) -> {Mod, Req2, new_state} end,
	{system_default_sp, _Req3, new_state} = sys:replace_state(Pid, Replace),
	{system_default_sp, _Req5, new_state} = sys:get_state(Pid),
	%% Not allowed to change the middleware module
	BadReplace = fun({_Mod2, Req4, State}) -> {undefined, Req4, State} end,
	{'EXIT', {{callback_failed, _, _}, _}} = (catch sys:replace_state(Pid, BadReplace)),
	{system_default_sp, _Req5, new_state} = sys:get_state(Pid),
	{status, Pid, {module, _Module}, Status} = sys:get_status(Pid),
	[_PDict, running, _Parent, [], Misc] = Status,
	[{header, "Cowboy middleware"}, {data, Data}] = Misc,
	{_, system_default_sp} = lists:keyfind("Middleware", 1, Data),
	{_, new_state} = lists:keyfind("Middleware state", 1, Data),
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	ok.

default_handler(Config) ->
	doc("Ensure that a sub_protocol with default handler can handle system messages"),
	ConnPid = gun_open(Config),
	register(system_default_handler_tester, self()),
	Ref = gun:get(ConnPid, "/default/handler"),
	Pid = receive {system_default_handler_h, P} -> P after 500 -> exit(timeout) end,
	unregister(system_default_handler_tester),
	ok = sys:suspend(Pid),
	{system_default_handler_h, _Req, undefined} = sys:get_state(Pid),
	%% code_change will do nothing to sub_protocol (or handler)
	ok = sys:change_code(Pid, system_default_handler_h, undefined, code_change),
	ok = sys:resume(Pid),
	Replace = fun({system_default_handler_h, Req2, undefined}) ->
			{system_default_handler_h, Req2, new_state}
		end,
	{system_default_handler_h, _Req3, new_state} = sys:replace_state(Pid, Replace),
	{system_default_handler_h, _Req4, new_state} = sys:get_state(Pid),
	{status, Pid, {module, _Module}, Status} = sys:get_status(Pid),
	[_PDict, running, _Parent, [], Misc] = Status,
	[{header, "Cowboy system test"}, {data, Data},
		{data, Data2}] = Misc,
	{_, system_default_handler_h} = lists:keyfind("Handler", 1, Data),
	%% Default handler state format
	{_, new_state} = lists:keyfind("Handler state", 1, Data2),
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	ok.

full(Config) ->
	doc("Ensure that a sub_protocol with fully implemented handler can handle system messages"),
	ConnPid = gun_open(Config),
	register(system_full_tester, self()),
	Ref = gun:get(ConnPid, "/full"),
	Pid = receive {system_full_h, P} -> P after 500 -> exit(timeout) end,
	unregister(system_full_tester),
	ok = sys:suspend(Pid),
	{system_full_h, _Req, undefined} = sys:get_state(Pid),
	%% code_change will change handler state to extra (code_change) if
	%% handler module.
	ok = sys:change_code(Pid, system_full_h, undefined, code_change),
	{system_full_h, _Req, code_change} = sys:get_state(Pid),
	%% But not if the module is not the handler.
	ok = sys:change_code(Pid, system_full_sp, undefined, code_change2),
	{system_full_h, _Req, code_change} = sys:get_state(Pid),
	ok = sys:resume(Pid),
	Replace = fun({system_full_h, Req2, code_change}) ->
			{system_full_h, Req2, new_state}
		end,
	{system_full_h, _Req3, new_state} = sys:replace_state(Pid, Replace),
	{system_full_h, _Req4, new_state} = sys:get_state(Pid),
	{status, Pid, {module, _Module}, Status} = sys:get_status(Pid),
	[_PDict, running, _Parent, [], Misc] = Status,
	[{header, "Cowboy system test"}, {data, Data},
		{data, Data2}] = Misc,
	{_, system_full_h} = lists:keyfind("Handler", 1, Data),
	%% Handler state is formatted
	{_, {formatted, new_state}} = lists:keyfind("Handler state", 1, Data2),
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	ok.
