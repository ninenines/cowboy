%% Feel free to use, reuse and abuse the code in this file.

-module(websocket_handler_init_shutdown).
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
	{ok, Req2} = cowboy_http_req:reply(403, Req),
	{shutdown, Req2}.

websocket_handle(_Frame, _Req, _State) ->
	exit(badarg).

websocket_info(_Info, _Req, _State) ->
	exit(badarg).

websocket_terminate(_Reason, _Req, _State) ->
	exit(badarg).
