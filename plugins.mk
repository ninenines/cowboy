# See LICENSE for licensing information.

# Plain HTTP handlers.
define tpl_cowboy.http
-module($(n)).
-behavior(cowboy_handler).

-export([init/2]).

init(Req, State) ->
	{ok, Req, State}.
endef

# Loop handlers.
define tpl_cowboy.loop
-module($(n)).
-behavior(cowboy_loop).

-export([init/2]).
-export([info/3]).

init(Req, State) ->
	{cowboy_loop, Req, State, hibernate}.

info(_Info, Req, State) ->
	{ok, Req, State, hibernate}.
endef

# REST handlers.
define tpl_cowboy.rest
-module($(n)).
-behavior(cowboy_rest).

-export([init/2]).
-export([content_types_provided/2]).
-export([to_html/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"html">>, '*'}, to_html}
	], Req, State}.

to_html(Req, State) ->
	{<<"<html><body>This is REST!</body></html>">>, Req, State}.
endef

# Websocket handlers.
define tpl_cowboy.ws
-module($(n)).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
	{cowboy_websocket, Req, State}.

websocket_init(State) ->
	{[], State}.

websocket_handle({text, Data}, State) ->
	{[{text, Data}], State};
websocket_handle({binary, Data}, State) ->
	{[{binary, Data}], State};
websocket_handle(_Frame, State) ->
	{[], State}.

websocket_info(_Info, State) ->
	{[], State}.
endef
