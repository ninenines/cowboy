%% Feel free to use, reuse and abuse the code in this file.

-module(ws_send_many).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	erlang:send_after(10, self(), send_many),
	{ok, State}.

websocket_handle(_Frame, State) ->
	{ok, State}.

websocket_info(send_many, State = [{sequence, Sequence}]) ->
	{reply, Sequence, State}.
