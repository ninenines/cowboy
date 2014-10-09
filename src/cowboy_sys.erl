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
-module(cowboy_sys).

%% API.
-export([handle_msg/6]).

% System.
-export([system_continue/3]).
-export([system_code_change/4]).
-export([system_terminate/4]).

-type state() :: {module(), cowboy_req:req() | undefined, any()}.

-callback sys_continue(cowboy_req:req() | undefined, any())
	-> {ok, cowboy_req:req(), cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), any()}, any(), module(), cowboy_req:req(), any()}
	| {halt, cowboy_req:req()}.
-callback sys_terminate(pid(), cowboy_req:req(), any()) -> no_return().

%% API.

-spec handle_msg(any(), {pid(), any()}, pid(), module(),
        cowboy_req:req() | undefined, any())
    -> no_return().
handle_msg(Msg, From, Parent, Mod, Req, ModState) ->
	Dbg = case get('$dbg') of
		undefined -> [];
		Other -> Other
	end,
	sys:handle_system_msg(Msg, From, Parent, ?MODULE, Dbg,
		{Mod, Req, ModState}).

%% System.

-spec system_continue(pid(), [sys:dbg_opt()], state()) -> no_return().
system_continue(_Parent, Dbg, {Mod, Req, ModState}) ->
	_ = put('$dbg', Dbg),
	continue(Mod, sys_continue, [Req, ModState]).

-spec system_code_change(state(), module(), any(), any()) -> {ok, state()}.
system_code_change(State, _Module, _OldVsn, _Extra) ->
	{ok, State}.

-spec system_terminate(any(), pid(), [sys:dbg_opt()], state()) -> no_return().
system_terminate(Reason, _Parent, Dbg, {Mod, Req, ModState}) ->
	_ = put('$dbg', Dbg),
	continue(Mod, sys_terminate, [Reason, Req, ModState]).

%% Internal.

continue(Module, Fun, Args) ->
	case process_info(self(), catchlevel) of
		{catchlevel, 1} ->
			% Process was hibernated while handling system messages and so not
			% inside cowboy_proc try..catch.
			cowboy_proc:continue(Module, Fun, Args);
		{catchlevel, 2} ->
			apply(Module, Fun, Args)
	end.
