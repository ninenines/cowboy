%% This module sends an invalid response status in early_error.

-module(stream_handler_invalid_status_h).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

init(_StreamID, _Req, _Opts) ->
	{[], undefined}.

data(_StreamID, _IsFin, _Data, State) ->
	{[], State}.

info(_StreamID, _Info, State) ->
	{[], State}.

terminate(_StreamID, _Reason, _State) ->
	ok.

early_error(_StreamID, _Reason, _PartialReq, {response, _, Headers, Body}, _Opts) ->
	{response, <<"400 Bad\r\nX-Injected: 1">>, Headers, Body}.
