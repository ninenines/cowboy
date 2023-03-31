%% This module echoes a request body of to test
%% the cowboy_decompress_h stream handler.

-module(decompress_h).

-export([init/2]).

init(Req0, State=[]) ->
	case cowboy_req:binding(what, Req0) of
		<<"decompress_ignore">> ->
			cowboy_req:cast({set_options, #{decompress_ignore => true}}, Req0);
		<<"decompress_ratio_limit">> ->
			cowboy_req:cast({set_options, #{decompress_ratio_limit => 0.5}}, Req0);
		<<"normal">> -> ok
	end,
	{ok, Body, Req1} = read_body(Req0),
	Req = cowboy_req:reply(200, #{}, Body, Req1),
	{ok, Req, State};

init(Req0, State=header_command) ->
	{ok, Body, Req1} = read_body(Req0),
	Req2 = cowboy_req:stream_reply(200, #{}, Req1),
	Req = cowboy_req:stream_body(Body, fin, Req2),
	{ok, Req, State};

init(Req0, State=accept_identity) ->
	{ok, Body, Req1} = read_body(Req0),
	Req = cowboy_req:reply(200, #{<<"accept-encoding">> => <<"identity">>}, Body, Req1),
	{ok, Req, State};

init(Req0, State=invalid_header) ->
	{ok, Body, Req1} = read_body(Req0),
	Req = cowboy_req:reply(200, #{<<"accept-encoding">> => <<";">>}, Body, Req1),
	{ok, Req, State};

init(Req0, State=reject_explicit_header) ->
	{ok, Body, Req1} = read_body(Req0),
	Req = cowboy_req:reply(200, #{<<"accept-encoding">> => <<"identity, gzip;q=0">>},
		Body, Req1),
	{ok, Req, State};

init(Req0, State=reject_implicit_header) ->
	{ok, Body, Req1} = read_body(Req0),
	Req = cowboy_req:reply(200, #{<<"accept-encoding">> => <<"identity, *;q=0">>},
		Body, Req1),
	{ok, Req, State};

init(Req0, State=accept_explicit_header) ->
	{ok, Body, Req1} = read_body(Req0),
	Req = cowboy_req:reply(200, #{<<"accept-encoding">> => <<"identity, gzip;q=0.5">>},
		Body, Req1),
	{ok, Req, State};

init(Req0, State=accept_implicit_header) ->
	{ok, Body, Req1} = read_body(Req0),
	Req = cowboy_req:reply(200, #{<<"accept-encoding">> => <<"identity, *;q=0.5">>},
		Body, Req1),
	{ok, Req, State}.

read_body(Req0) ->
	{Status, Data, Req} = cowboy_req:read_body(Req0, #{length => 1000}),
	do_read_body(Status, Req, Data).

do_read_body(more, Req0, Acc) ->
	{Status, Data, Req} = cowboy_req:read_body(Req0),
	do_read_body(Status, Req, << Acc/binary, Data/binary >>);
do_read_body(ok, Req, Acc) ->
	{ok, Acc, Req}.
