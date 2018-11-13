%% This module starts with active mode disabled
%% and enables it again once a timeout is triggered.

-module(ws_active_commands_h).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, RunOrHibernate) ->
	{cowboy_websocket, Req, RunOrHibernate}.

websocket_init(State=run) ->
	erlang:send_after(1500, self(), active_true),
	{[{active, false}], State};
websocket_init(State=hibernate) ->
	erlang:send_after(1500, self(), active_true),
	{[{active, false}], State, hibernate}.

websocket_handle(Frame, State=run) ->
	{[Frame], State};
websocket_handle(Frame, State=hibernate) ->
	{[Frame], State, hibernate}.

websocket_info(active_true, State=run) ->
	{[{active, true}], State};
websocket_info(active_true, State=hibernate) ->
	{[{active, true}], State, hibernate}.
