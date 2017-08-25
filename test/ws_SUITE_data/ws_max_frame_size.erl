-module(ws_max_frame_size).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
	{cowboy_websocket, Req, State, #{max_frame_size => 8}}.

websocket_init(State) ->
	{ok, State}.

websocket_handle({text, Data}, State) ->
	{reply, {text, Data}, State};
websocket_handle({binary, Data}, State) ->
	{reply, {binary, Data}, State};
websocket_handle(_Frame, State) ->
	{ok, State}.

websocket_info(_Info, State) ->
	{ok, State}.
