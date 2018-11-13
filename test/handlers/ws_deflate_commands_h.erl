%% This module enables/disables compression
%% every time it echoes a frame.

-module(ws_deflate_commands_h).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, RunOrHibernate) ->
	{cowboy_websocket, Req,
		#{deflate => true, hibernate => RunOrHibernate},
		#{compress => true}}.

websocket_handle(Frame, State=#{deflate := Deflate0, hibernate := run}) ->
	Deflate = not Deflate0,
	{[Frame, {deflate, Deflate}], State#{deflate => Deflate}};
websocket_handle(Frame, State=#{deflate := Deflate0, hibernate := hibernate}) ->
	Deflate = not Deflate0,
	{[Frame, {deflate, Deflate}], State#{deflate => Deflate}, hibernate}.

websocket_info(_Info, State) ->
	{[], State}.
