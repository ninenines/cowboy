-module(ws_max_frame_size).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
	{cowboy_websocket, Req, State, #{max_frame_size => 8, compress => true}}.

websocket_handle({text, Data}, State) ->
	{[{text, Data}], State};
websocket_handle({binary, Data}, State) ->
	{[{binary, Data}], State};
websocket_handle(_Frame, State) ->
	{[], State}.

websocket_info(_Info, State) ->
	{[], State}.
