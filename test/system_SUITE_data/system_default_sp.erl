-module(system_default_sp).

-export([upgrade/6]).
-export([continue/3]).
-export([terminate/4]).
-export([code_change/5]).

upgrade(Req, Env, _, State, _, _) ->
	loop(Req, Env, State).

loop(Req, Env, State) ->
	receive
		{system, From, Msg} ->
			{system, From, Msg, ?MODULE, Req, Env, State}
	after 500 ->
			  {ok, Req, [{result, ok} | Env]}
	end.

continue(Req, Env, State) ->
	loop(Req, Env, State).

terminate(Reason, _Req, _Env, _state) ->
	exit(Reason).

code_change(Req, State, _Module, _OldVsn, _Extra) ->
	{ok, Req, State}.
