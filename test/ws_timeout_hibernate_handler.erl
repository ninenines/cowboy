%% Feel free to use, reuse and abuse the code in this file.

-module(ws_timeout_hibernate_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
	websocket_info/3, websocket_terminate/3]).

init(_Any, _Req, _Opts) ->
	{upgrade, protocol, cowboy_http_websocket}.

handle(_Req, _State) ->
	exit(badarg).

terminate(_Req, _State) ->
	exit(badarg).

websocket_init(_TransportName, Req, _Opts) ->
	{ok, Req, undefined, 1000, hibernate}.

websocket_handle(_Frame, Req, State) ->
	{ok, Req, State, hibernate}.

websocket_info(_Info, Req, State) ->
	{ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
