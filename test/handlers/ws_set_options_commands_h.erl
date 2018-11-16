%% This module sets options based on the frame received.

-module(ws_set_options_commands_h).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, RunOrHibernate) ->
	{cowboy_websocket, Req, RunOrHibernate,
		#{idle_timeout => infinity}}.

websocket_handle(Frame={text, <<"idle_timeout_short">>}, State=run) ->
	{[{set_options, #{idle_timeout => 500}}, Frame], State};
websocket_handle(Frame={text, <<"idle_timeout_short">>}, State=hibernate) ->
	{[{set_options, #{idle_timeout => 500}}, Frame], State, hibernate}.

websocket_info(_Info, State) ->
	{[], State}.
