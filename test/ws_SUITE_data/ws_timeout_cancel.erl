%% Feel free to use, reuse and abuse the code in this file.

-module(ws_timeout_cancel).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, _) ->
	erlang:start_timer(500, self(), should_not_cancel_timer),
	{cowboy_websocket, Req, undefined, 1000}.

websocket_handle({text, Data}, Req, State) ->
	{reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
	{reply, {binary, Data}, Req, State}.

websocket_info(_Info, Req, State) ->
	erlang:start_timer(500, self(), should_not_cancel_timer),
	{ok, Req, State}.
