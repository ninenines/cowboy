%% Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_middleware).

%% API.
-export([execute/4]).
-export([resume/5]).

%% System.
-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).
-export([system_get_state/1]).
-export([system_replace_state/2]).
-export([format_status/2]).

-type env() :: [{atom(), any()}].
-export_type([env/0]).

-callback execute(Req, Env)
	-> {ok, Req, Env}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), term()}, any(), module(), Req, Env, any()}
	| {halt, Req, Env}
	when Req::cowboy_req:req(), Env::env().

-record(misc, {
	req :: cowboy_req:req(),
	env :: env(),
	halt :: {module(), atom(), list()},
	tail :: [module()],
	module :: module(),
	module_misc :: any()
}).

%%API.

-spec execute(cowboy_req:req(), env(), {module(), atom(), list()}, [module()])
	-> ok.
execute(Req, Env, {Module, Function, Args}, []) ->
	apply(Module, Function, [Req, Env | Args]);
execute(Req, Env, Halt, [Middleware | Tail]) ->
	case Middleware:execute(Req, Env) of
		{ok, Req2, Env2} ->
			execute(Req2, Env2, Halt, Tail);
		{suspend, Module, Function, Args} ->
			proc_lib:hibernate(?MODULE, resume,
				[Halt, Tail, Module, Function, Args]);
		{system, From, Msg, Module, Req2, Env2, ModMisc} ->
			{_, Parent} = lists:keyfind(parent, 1, Env2),
			{_, Dbg} = lists:keyfind(dbg, 1, Env2),
			Misc = #misc{req=Req2, env=Env2, halt=Halt, tail=Tail,
				module=Module, module_misc=ModMisc},
			sys:handle_system_msg(Msg, From, Parent, ?MODULE, Dbg, Misc);
		{halt, Req2, Env2} ->
			execute(Req2, Env2, Halt, [])
	end.

-spec resume({module(), atom(), list()}, [module()], module(), atom(), list())
	-> ok.
resume(Halt, Tail, Module, Function, Args) ->
	case apply(Module, Function, Args) of
		{ok, Req2, Env2} ->
			execute(Req2, Env2, Halt, Tail);
		{suspend, Module, Function, Args2} ->
			proc_lib:hibernate(?MODULE, resume,
				[Halt, Tail, Module, Function, Args2]);
		{system, From, Msg, Module, Req2, Env2, ModMisc} ->
			{_, Parent} = lists:keyfind(parent, 1, Env2),
			{_, Dbg} = lists:keyfind(dbg, 1, Env2),
			Misc = #misc{req=Req2, env=Env2, halt=Halt, tail=Tail,
				module=Module, module_misc=ModMisc},
			sys:handle_system_msg(Msg, From, Parent, ?MODULE, Dbg, Misc);
		{halt, Req2, Env2} ->
			execute(Req2, Env2, Halt, [])
	end.

%% System.

-spec system_continue(pid(), [sys:dbg_opt()], #misc{}) -> ok.
system_continue(_Parent, Dbg, #misc{req=Req, env=Env, halt=Halt,
		tail=Tail, module=Module, module_misc=ModMisc}) ->
	Env2 = lists:keystore(dbg, 1, Env, {dbg, Dbg}),
	resume(Halt, Tail, Module, continue, [Req, Env2, ModMisc]).

-spec system_terminate(any(), pid(), [sys:dbg_opt()], #misc{}) -> no_return().
system_terminate(Reason, _Parent, Dbg,
		#misc{req=Req, env=Env, module=Module, module_misc=ModMisc}) ->
	Env2 = lists:keystore(dbg, 1, Env, {dbg, Dbg}),
	Module:terminate(Reason, Req, Env2, ModMisc).

-spec system_code_change(#misc{}, module(), any(), any())
	-> {ok, #misc{}} | {error | exit | throw, any()}.
system_code_change(Misc=#misc{req=Req, module=Module, module_misc=ModMisc},
		Module2, OldVsn, Extra) ->
	try Module:code_change(Req, ModMisc, Module2, OldVsn, Extra) of
		{ok, Req2, ModMisc2} ->
			{ok, Misc#misc{req=Req2, module_misc=ModMisc2}}
	catch
		Class:Reason ->
			{Class, Reason}
	end.

-spec system_get_state(#misc{}) -> {ok, {module(), cowboy_req:req(), any()}}.
system_get_state(#misc{req=Req, module=Module, module_misc=ModMisc}) ->
	case erlang:function_exported(Module, get_state, 2) of
		true ->
			{ok, {_, _, _}} = Module:get_state(Req, ModMisc);
		false ->
			{ok, {Module, Req, ModMisc}}
	end.

-spec system_replace_state(cowboy_system:replace_state(), #misc{})
	-> {ok, {module(), cowboy_req:req(), any()}, #misc{}}.
system_replace_state(Replace, Misc=#misc{req=Req, module=Module, module_misc=ModMisc}) ->
	case erlang:function_exported(Module, replace_state, 3) of
		true ->
			{ok, Result, Req2, ModMisc2} = Module:replace_state(Replace, Req, ModMisc),
			{ok, Result, Misc#misc{req=Req2, module_misc=ModMisc2}};
		false ->
			{Module, Req2, ModMisc2} = Result = Replace({Module, Req, ModMisc}),
			{ok, Result, Misc#misc{req=Req2, module_misc=ModMisc2}}
	end.

-spec format_status(normal, [[{term(), term()}] | running | suspended |
		pid() | [sys:dbg_opt()] | #misc{}]) -> any().
format_status(Opt, [PDict, SysState, Parent, Dbg,
		#misc{req=Req, env=Env, module=Module, module_misc=ModMisc}]) ->
	Env2 = lists:keystore(dbg, 1, Env, {dbg, Dbg}),
	case erlang:function_exported(Module, format_status, 2) of
		true ->
			format_status(Opt, PDict, SysState, Parent, Dbg, Req, Env2, ModMisc, Module);
		false ->
			default_format_status(SysState, Parent, Dbg, Req, Env, Module, ModMisc)
	end.

format_status(Opt, PDict, SysState, Parent, Dbg, Req, Env, ModMisc, Module) ->
	Req2 = cowboy_req:lock(Req),
	try Module:format_status(Opt, [PDict, SysState, Req2, Env, ModMisc]) of
		Status ->
			Status
	catch
		_:_ ->
			default_format_status(SysState, Parent, Dbg, Req, Env, Module, ModMisc)
	end.

default_format_status(SysState, Parent, Dbg, Req, Env, Module, ModMisc) ->
	Log = sys:get_debug(log, Dbg, []),
	Data = [
		{"Status", SysState},
		{"Parent", Parent},
		{"Logged events", Log},
		{"Request", cowboy_req:to_list(Req)},
		{"Environment", Env},
		{"Middleware", Module},
		{"Middleware state", ModMisc}
	],
	[{header, "Cowboy middleware"}, {data, Data}].
