%% Feel free to use, reuse and abuse the code in this file.

-module(ws_echo).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) when not is_map(Opts) ->
	init(Req, #{});
init(Req, Opts) ->
	{cowboy_websocket, Req, undefined, Opts#{
		data_delivery => relay,
		compress => true
	}}.

websocket_handle({text, Data}, State) ->
	{[{text, Data}], State};
websocket_handle({binary, Data}, State) ->
	{[{binary, Data}], State};
websocket_handle(_Frame, State) ->
	{[], State}.

websocket_info(_Info, State) ->
	{[], State}.
