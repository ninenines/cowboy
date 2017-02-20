%% This module returns a different value in websocket_init/1 depending on the query string.

-module(ws_init_h).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, _) ->
	State = binary_to_atom(cowboy_req:qs(Req), latin1),
	{cowboy_websocket, Req, State}.

%% Sleep to make sure the HTTP response was sent.
websocket_init(State) ->
	timer:sleep(100),
	do_websocket_init(State).

do_websocket_init(State=ok) ->
	{ok, State};
do_websocket_init(State=ok_hibernate) ->
	{ok, State, hibernate};
do_websocket_init(State=reply) ->
	{reply, {text, "Hello"}, State};
do_websocket_init(State=reply_hibernate) ->
	{reply, {text, "Hello"}, State, hibernate};
do_websocket_init(State=reply_close) ->
	{reply, close, State};
do_websocket_init(State=reply_close_hibernate) ->
	{reply, close, State, hibernate};
do_websocket_init(State=reply_many) ->
	{reply, [{text, "Hello"}, {binary, "World"}], State};
do_websocket_init(State=reply_many_hibernate) ->
	{reply, [{text, "Hello"}, {binary, "World"}], State, hibernate};
do_websocket_init(State=reply_many_close) ->
	{reply, [{text, "Hello"}, close], State};
do_websocket_init(State=reply_many_close_hibernate) ->
	{reply, [{text, "Hello"}, close], State, hibernate};
do_websocket_init(State=stop) ->
	{stop, State}.

websocket_handle(_, State) ->
	{ok, State}.

websocket_info(_, State) ->
	{ok, State}.
