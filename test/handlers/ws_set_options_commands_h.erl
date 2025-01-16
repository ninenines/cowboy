%% This module sets options based on the frame received.

-module(ws_set_options_commands_h).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, RunOrHibernate) ->
	{cowboy_websocket, Req, RunOrHibernate,
		#{idle_timeout => infinity}}.

%% Set the idle_timeout option dynamically.
websocket_handle({text, <<"idle_timeout_short">>}, State=run) ->
	{[{set_options, #{idle_timeout => 500}}], State};
websocket_handle({text, <<"idle_timeout_short">>}, State=hibernate) ->
	{[{set_options, #{idle_timeout => 500}}], State, hibernate};
%% Set the max_frame_size option dynamically.
websocket_handle({text, <<"max_frame_size_small">>}, State=run) ->
	{[{set_options, #{max_frame_size => 1000}}], State};
websocket_handle({text, <<"max_frame_size_small">>}, State=hibernate) ->
	{[{set_options, #{max_frame_size => 1000}}], State, hibernate};
%% We just echo binary frames.
websocket_handle(Frame={binary, _}, State=run) ->
	{[Frame], State};
websocket_handle(Frame={binary, _}, State=hibernate) ->
	{[Frame], State, hibernate}.

websocket_info(_Info, State) ->
	{[], State}.
