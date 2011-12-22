%% Feel free to use, reuse and abuse the code in this file.

-module(websocket_handler).
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
	erlang:start_timer(1000, self(), <<"websocket_init">>),
	Req2 = cowboy_http_req:compact(Req),
	{ok, Req2, undefined}.

websocket_handle({text, Data}, Req, State) ->
	{reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
	{reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	erlang:start_timer(1000, self(), <<"websocket_handle">>),
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
