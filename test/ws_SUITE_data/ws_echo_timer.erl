%% Feel free to use, reuse and abuse the code in this file.

-module(ws_echo_timer).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, _) ->
	{cowboy_websocket, Req, undefined}.

websocket_init(State) ->
	erlang:start_timer(1000, self(), <<"websocket_init">>),
	{[], State}.

websocket_handle({text, Data}, State) ->
	{[{text, Data}], State};
websocket_handle({binary, Data}, State) ->
	{[{binary, Data}], State};
websocket_handle(_Frame, State) ->
	{[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(1000, self(), <<"websocket_handle">>),
	{[{text, Msg}], State};
websocket_info(_Info, State) ->
	{[], State}.
