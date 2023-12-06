%% This module implements a loop handler that streams endless data

-module(loop_handler_endless_h).

-export([init/2]).
-export([info/3]).

init(Req0, #{delay := Delay} = Opts) ->
	case cowboy_req:header(<<"x-test-pid">>, Req0) of
		BinPid when is_binary(BinPid) ->
			Pid = list_to_pid(binary_to_list(BinPid)),
			Pid ! {stream, self()},
			ok;
		_ ->
			ok
	end,
	erlang:send_after(Delay, self(), timeout),
	Req = cowboy_req:stream_reply(200, Req0),
	{cowboy_loop, Req, Opts}.

info(timeout, Req, State) ->
	cowboy_req:stream_body(<<0:1000/unit:8>>, nofin, Req),
	erlang:send_after(10, self(), timeout),
	{ok, Req, State}.
