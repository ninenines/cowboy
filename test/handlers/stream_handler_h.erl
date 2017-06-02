%% This module behaves differently depending on a specific header.

-module(stream_handler_h).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

init(StreamID, Req, Opts) ->
	%% @todo Vary behavior depending on x-test-case.
	Pid = list_to_pid(binary_to_list(cowboy_req:header(<<"x-test-pid">>, Req))),
	Pid ! {Pid, self(), init, StreamID, Req, Opts},
	{[{headers, 200, #{}}], Pid}.

data(StreamID, IsFin, Data, State=Pid) ->
	Pid ! {Pid, self(), data, StreamID, IsFin, Data, State},
	{[], State}.

info(StreamID, Info, State=Pid) ->
	Pid ! {Pid, self(), info, StreamID, Info, State},
	{[], State}.

terminate(StreamID, Reason, State=Pid) ->
	Pid ! {Pid, self(), terminate, StreamID, Reason, State},
	ok.

%% This clause can only test for early errors that reached the required header.
early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
	Pid = list_to_pid(binary_to_list(cowboy_req:header(<<"x-test-pid">>, PartialReq))),
	Pid ! {Pid, self(), early_error, StreamID, Reason, PartialReq, Resp, Opts},
	Resp.
