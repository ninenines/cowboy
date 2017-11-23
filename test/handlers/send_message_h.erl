%% This module sends a message to the pid passed in a header.

-module(send_message_h).
-export([init/2]).

init(Req, State) ->
	Pid = list_to_pid(binary_to_list(cowboy_req:header(<<"x-test-pid">>, Req))),
	Pid ! {Pid, self(), init, Req, State},
	{ok, cowboy_req:reply(200, Req), State}.
