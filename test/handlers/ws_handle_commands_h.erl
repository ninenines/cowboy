%% This module takes commands from the x-commands header
%% and returns them in the websocket_handle/2 callback.

-module(ws_handle_commands_h).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	Commands0 = cowboy_req:header(<<"x-commands">>, Req),
	Commands = binary_to_term(base64:decode(Commands0)),
	case Commands of
		bad ->
			Pid = case Req of
				#{version := 'HTTP/2'} -> self();
				#{pid := Pid0} -> Pid0
			end,
			ct_helper_error_h:ignore(Pid, cowboy_websocket, handler_call, 6);
		_ ->
			ok
	end,
	{cowboy_websocket, Req, {Commands, maps:get(run_or_hibernate, Opts)}, Opts}.

websocket_init(State) ->
	{[], State}.

websocket_handle(_, State={Commands, run}) ->
	{Commands, State};
websocket_handle(_, State={Commands, hibernate}) ->
	{Commands, State, hibernate}.

websocket_info(_, State) ->
	{[], State}.

