%% Feel free to use, reuse and abuse the code in this file.

-module(ws_timeout_cancel).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, _) ->
	erlang:start_timer(500, self(), should_not_cancel_timer),
	{cowboy_websocket, Req, undefined, #{
		idle_timeout => 1000
	}}.

websocket_handle({text, Data}, State) ->
	{reply, {text, Data}, State};
websocket_handle({binary, Data}, State) ->
	{reply, {binary, Data}, State}.

websocket_info(_Info, State) ->
	erlang:start_timer(500, self(), should_not_cancel_timer),
	{ok, State}.
