%% Feel free to use, reuse and abuse the code in this file.

-module(ws_echo).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, _) ->
	{cowboy_websocket, Req, undefined, #{
		compress => true
	}}.

websocket_handle({text, Data}, State) ->
	{reply, {text, Data}, State};
websocket_handle({binary, Data}, State) ->
	{reply, {binary, Data}, State};
websocket_handle(_Frame, State) ->
	{ok, State}.

websocket_info(_Info, State) ->
	{ok, State}.
