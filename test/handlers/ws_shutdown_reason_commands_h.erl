%% This module sends the process pid to the test pid
%% found in the x-test-pid header, then changes the
%% shutdown reason and closes the connection normally.

-module(ws_shutdown_reason_commands_h).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, RunOrHibernate) ->
	TestPid = list_to_pid(binary_to_list(cowboy_req:header(<<"x-test-pid">>, Req))),
	{cowboy_websocket, Req, {TestPid, RunOrHibernate}}.

websocket_init(State={TestPid, RunOrHibernate}) ->
	TestPid ! {ws_pid, self()},
	ShutdownReason = receive
		{TestPid, SR} ->
			SR
	after 1000 ->
		error(timeout)
	end,
	Commands = [
		{shutdown_reason, ShutdownReason},
		close
	],
	case RunOrHibernate of
		run -> {Commands, State};
		hibernate -> {Commands, State, hibernate}
	end.

websocket_handle(_, State) ->
	{[], State}.

websocket_info(_, State) ->
	{[], State}.
