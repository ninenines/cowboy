%% This module sends a message with terminate arguments to the test case process.

-module(ws_terminate_h).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-record(state, {
	pid
}).

init(Req, _) ->
	Pid = list_to_pid(binary_to_list(cowboy_req:header(<<"x-test-pid">>, Req))),
	Opts = case cowboy_req:qs(Req) of
		<<"req_filter">> -> #{req_filter => fun(_) -> filtered end};
		_ -> #{}
	end,
	{cowboy_websocket, Req, #state{pid=Pid}, Opts}.

websocket_init(State) ->
	{ok, State}.

websocket_handle(_, State) ->
	{ok, State}.

websocket_info(_, State) ->
	{ok, State}.

terminate(Reason, Req, #state{pid=Pid}) ->
	Pid ! {terminate, Reason, Req},
	ok.
