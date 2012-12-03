%% Feel free to use, reuse and abuse the code in this file.

-module(ws_upgrade_with_opts_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init(_Any, Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket, Req, <<"success">>}.

websocket_init(_TransportName, Req, Response) ->
	Req2 = cowboy_req:compact(Req),
	erlang:send_after(10, self(), send_response),
	{ok, Req2, Response}.

websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info(send_response, Req, State = Response)
		when is_binary(Response) ->
	{reply, {text, Response}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
