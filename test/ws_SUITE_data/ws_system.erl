%% Feel free to use, reuse and abuse the code in this file.

-module(ws_system).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([code_change/4]).
-export([format_status/2]).

init(Req, _) ->
	ws_system_tester ! {ws_system, self()},
	{cowboy_websocket, Req, 0, 1000}.

websocket_handle({text, Data}, Req, State) ->
	{reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
	{reply, {binary, Data}, Req, State}.

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

code_change(_OldVsn, Req, _State, State2) ->
	{ok, Req, State2}.

format_status(normal, [_PDict, _Req, State]) ->
	[{data, [{"Handler state", {formatted, State}}]}];
format_status(terminate, [_PDict, _Req, State]) ->
	{formatted, State}.
