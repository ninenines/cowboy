%% Feel free to use, reuse and abuse the code in this file.

-module(ws_ignore).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, _) ->
	{cowboy_websocket, Req, undefined, #{
		compress => true
	}}.

websocket_handle({text, <<"CHECK">>}, State) ->
	{[{text, <<"CHECK">>}], State};
websocket_handle(_Frame, State) ->
	{[], State}.

websocket_info(_Info, State) ->
	{[], State}.
