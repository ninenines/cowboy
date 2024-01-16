%% This module sends an empty ping to the client and
%% waits for a pong before sending a text frame. It
%% is used to confirm server-initiated pings work.

-module(ws_ping_h).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, _) ->
	{cowboy_websocket, Req, undefined}.

websocket_init(State) ->
	{[{ping, <<>>}], State}.

websocket_handle(pong, State) ->
	{[{text, <<"OK!!">>}], State}.

websocket_info(_, State) ->
	{[], State}.
