%% This module takes commands from the x-commands header
%% and returns them in the websocket_info/2 callback.
%% This callback is triggered via a message.

-module(ws_info_commands_h).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req=#{pid := Pid}, RunOrHibernate) ->
	Commands0 = cowboy_req:header(<<"x-commands">>, Req),
	Commands = binary_to_term(base64:decode(Commands0)),
	case Commands of
		bad -> ct_helper_error_h:ignore(Pid, cowboy_websocket, handler_call, 6);
		_ -> ok
	end,
	{cowboy_websocket, Req, {Commands, RunOrHibernate}}.

websocket_init(State) ->
	self() ! shoot,
	{[], State}.

websocket_handle(_, State) ->
	{[], State}.

websocket_info(_, State={Commands, run}) ->
	{Commands, State};
websocket_info(_, State={Commands, hibernate}) ->
	{Commands, State, hibernate}.
