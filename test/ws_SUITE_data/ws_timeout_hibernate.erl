%% Feel free to use, reuse and abuse the code in this file.

-module(ws_timeout_hibernate).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, _) ->
	{cowboy_websocket, Req, undefined, 1000, hibernate}.

websocket_handle(_Frame, Req, State) ->
	{ok, Req, State, hibernate}.

websocket_info(_Info, Req, State) ->
	{ok, Req, State, hibernate}.
