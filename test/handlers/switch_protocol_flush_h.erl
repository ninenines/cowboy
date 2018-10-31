%% This module is used to test the flushing of messages when
%% switch_protocol is executed by cowboy_http.

-module(switch_protocol_flush_h).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).
-export([takeover/7]).
-export([validate/1]).

init(StreamID, Req, _) ->
	Pid = list_to_pid(binary_to_list(cowboy_req:header(<<"x-test-pid">>, Req))),
	%% Send ourselves a few messages that may or may not be flushed.
	self() ! good,
	self() ! {'EXIT', Pid, normal},
	self() ! {system, a, b},
	self() ! {{self(), StreamID}, hello},
	self() ! {'$gen_call', a, b},
	self() ! {timeout, make_ref(), ?MODULE},
	self() ! {ranch_tcp, socket, <<"123">>},
	{[{switch_protocol, #{}, ?MODULE, Pid}], undefined}.

info(_, _, State) ->
	{[], State}.

terminate(_, _, _) ->
	ok.

%% @todo It would be good if we could allow this function to return normally.
-spec takeover(_, _, _, _, _, _, _) -> no_return().
takeover(_, _, _, _, _, _, Pid) ->
	Msgs = receive_all([]),
	Pid ! {Pid, Msgs},
	exit(normal).

receive_all(Acc) ->
	receive
		Msg ->
			receive_all([Msg|Acc])
	after 0 ->
		Acc
	end.

validate(Msgs) ->
	[
		{ranch_tcp, socket, <<"123">>},
		{'$gen_call', a, b},
		{system, a, b},
		good
	] = Msgs,
	ok.
