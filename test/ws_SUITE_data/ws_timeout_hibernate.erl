%% Feel free to use, reuse and abuse the code in this file.

-module(ws_timeout_hibernate).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, _) ->
	{cowboy_websocket, Req, undefined, 1000, hibernate}.

websocket_handle(_Frame, State) ->
	{ok, State, hibernate}.

websocket_info(_Info, State) ->
	{ok, State, hibernate}.
