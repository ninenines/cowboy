%% This module sets options dynamically and performs
%% some related relevant operation for testing the change.

-module(set_options_h).

-export([init/2]).

init(Req, State) ->
	set_options(cowboy_req:binding(key, Req), Req, State).

set_options(<<"chunked_false">>, Req0, State) ->
	%% @todo This should be replaced by a cowboy_req:cast/cowboy_stream:cast.
	#{pid := Pid, streamid := StreamID} = Req0,
	Pid ! {{Pid, StreamID}, {set_options, #{chunked => false}}},
	Req = cowboy_req:stream_reply(200, Req0),
	cowboy_req:stream_body(<<0:8000000>>, fin, Req),
	{ok, Req, State};
set_options(<<"chunked_false_ignored">>, Req0, State) ->
	%% @todo This should be replaced by a cowboy_req:cast/cowboy_stream:cast.
	#{pid := Pid, streamid := StreamID} = Req0,
	Pid ! {{Pid, StreamID}, {set_options, #{chunked => false}}},
	Req = cowboy_req:reply(200, #{}, <<"Hello world!">>, Req0),
	{ok, Req, State};
set_options(<<"idle_timeout_short">>, Req0, State) ->
	%% @todo This should be replaced by a cowboy_req:cast/cowboy_stream:cast.
	#{pid := Pid, streamid := StreamID} = Req0,
	Pid ! {{Pid, StreamID}, {set_options, #{idle_timeout => 500}}},
	{_, Body, Req} = cowboy_req:read_body(Req0),
	{ok, cowboy_req:reply(200, #{}, Body, Req), State};
set_options(<<"idle_timeout_long">>, Req0, State) ->
	%% @todo This should be replaced by a cowboy_req:cast/cowboy_stream:cast.
	#{pid := Pid, streamid := StreamID} = Req0,
	Pid ! {{Pid, StreamID}, {set_options, #{idle_timeout => 60000}}},
	{_, Body, Req} = cowboy_req:read_body(Req0),
	{ok, cowboy_req:reply(200, #{}, Body, Req), State}.
