%% Feel free to use, reuse and abuse the code in this file.

-module(ws_echo_timer).

-export([init/2]).
-export([websocket_init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, _) ->
	{cowboy_websocket, Req, undefined}.

websocket_init(Req, State) ->
	erlang:start_timer(1000, self(), <<"websocket_init">>),
	{ok, Req, State}.

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
