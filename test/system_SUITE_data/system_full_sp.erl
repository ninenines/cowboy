-module(system_full_sp).

-export([upgrade/6]).
-export([continue/3]).
-export([terminate/4]).
-export([code_change/5]).
-export([get_state/2]).
-export([replace_state/3]).
-export([format_status/2]).

upgrade(Req, Env, Handler, State, _, _) ->
	loop(Req, Env, {Handler, State}).

loop(Req, Env, {Handler, State}) ->
	receive
		{system, From, Msg} ->
			{system, From, Msg, ?MODULE, Req, Env, {Handler, State}}
	after 500 ->
		 	  Result = cowboy_handler:terminate(timeout, Req, State, Handler),
			  {ok, Req, [{result, Result} | Env]}
	end.

continue(Req, Env, Misc) ->
	loop(Req, Env, Misc).

terminate(Reason, Req, _Env, {Handler, State}) ->
	_ = cowboy_handler:terminate({shutdown, Reason}, Req, State, Handler),
	exit(Reason).

code_change(Req, {Handler, State}, Module, OldVsn, Extra) ->
	{ok, Req2, State2} = cowboy_handler:code_change(OldVsn, Req, State,
		Module, Extra, Handler),
	{ok, Req2, {Handler, State2}}.

get_state(Req, {Handler, State}) ->
	{ok, {Handler, Req, State}}.

replace_state(Replace, Req, {Handler, State}) ->
	{Handler, Req2, State2} = Result = Replace({Handler, Req, State}),
	{ok, Result, Req2, {Handler, State2}}.

format_status(Opt, [PDict, SysState, Req, Env, {Handler, HandlerState}]) ->
	Parent = lists:keyfind(parent, 1, Env),
	{dbg, Dbg} = lists:keyfind(dbg, 1, Env),
	Log = sys:get_debug(log, Dbg, []),
	Data = [
		{"Status", SysState},
		{"Parent", Parent},
		{"Logged events", Log},
		{"Request", cowboy_req:to_list(Req)},
		{"Environment", Env},
		{"Handler", Handler}
	],
	HandlerStatus = cowboy_handler:format_status(Opt, PDict, Req, HandlerState, Handler),
	[{header, "Cowboy system test"}, {data, Data} | HandlerStatus].
