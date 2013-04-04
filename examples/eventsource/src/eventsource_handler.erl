%% Feel free to use, reuse and abuse the code in this file.

%% @doc EventSource emitter.
-module(eventsource_handler).
-behaviour(cowboy_loop_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{Peer, Req2} = cowboy_req:peer(Req),
	{LastEventId, Req3} = cowboy_eventsource:last_event_id(Req2),
	error_logger:info_report({client_connection, Peer, LastEventId}),
	{ok, Req4} = cowboy_eventsource:init(Req3),
	Events = [{retry, 5000}, {comment, <<"EventSource demo">>}],
	ok = cowboy_eventsource:send(Events, Req4),
	erlang:send_after(1000, self(), {message, "Tick"}),
	erlang:send_after(3000, self(), {special, "I think you are pretty cool."}),
	{loop, Req4, undefined, 5000}.

info({message, Msg}, Req, State) ->
	Event = {event, [{id, id()}, {data, Msg}]},
	ok = cowboy_eventsource:send(Event, Req),
	erlang:send_after(1000, self(), {message, "Tick"}),
	{loop, Req, State};
info({special, Msg}, Req, State) ->
	Event = {event, [{id, id()}, {type, <<"special">>}, {data, Msg}]},
	ok = cowboy_eventsource:send(Event, Req),
	{loop, Req, State}.

terminate(_Reason, _Req, _State) ->
	ok.

id() ->
	{Mega, Sec, Micro} = erlang:now(),
	Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
	integer_to_list(Id, 16).
