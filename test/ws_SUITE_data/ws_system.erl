%% Feel free to use, reuse and abuse the code in this file.

-module(ws_system).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([terminate/3]).

init(Req, _) ->
	[{<<"from">>, From}] = cowboy_req:parse_qs(Req),
	{Pid, Tag} = binary_to_term(From),
	Pid ! {Tag, self()},
	{cowboy_websocket, Req, state, 500}.

websocket_handle({text, Data}, Req, State) ->
	{reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
	{reply, {binary, Data}, Req, State}.

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

terminate(timeout, _, _) ->
	ok.
