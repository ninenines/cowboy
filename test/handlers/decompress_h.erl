%% This module echoes a request body of to test
%% the cowboy_decompress_h stream handler.

-module(decompress_h).

-export([init/2]).

init(Req0, State=echo) ->
	case cowboy_req:binding(what, Req0) of
		<<"decompress_disable">> ->
			cowboy_req:cast({set_options, #{decompress_enabled => false}}, Req0);
		<<"decompress_ratio_limit">> ->
			cowboy_req:cast({set_options, #{decompress_ratio_limit => 0.5}}, Req0);
		<<"normal">> -> ok
	end,
	{ok, Body, Req1} = read_body(Req0),
	Req = cowboy_req:reply(200, #{}, Body, Req1),
	{ok, Req, State};
init(Req0, State=test) ->
	Req = test(Req0, cowboy_req:binding(what, Req0)),
	{ok, Req, State}.

test(Req, <<"content-encoding">>) ->
	cowboy_req:reply(200, #{},
		cowboy_req:header(<<"content-encoding">>, Req, <<"undefined">>),
		Req);
test(Req, <<"content-decoded">>) ->
	cowboy_req:reply(200, #{},
		io_lib:format("~0p", [maps:get(content_decoded, Req, undefined)]),
		Req);
test(Req0, <<"disable-in-the-middle">>) ->
	{Status, Data, Req1} = cowboy_req:read_body(Req0, #{length => 1000}),
	cowboy_req:cast({set_options, #{decompress_enabled => false}}, Req1),
	{ok, Body, Req} = do_read_body(Status, Req1, Data),
	cowboy_req:reply(200, #{}, Body, Req);
test(Req0, <<"enable-in-the-middle">>) ->
	{Status, Data, Req1} = cowboy_req:read_body(Req0, #{length => 1000}),
	cowboy_req:cast({set_options, #{decompress_enabled => true}}, Req1),
	{ok, Body, Req} = do_read_body(Status, Req1, Data),
	cowboy_req:reply(200, #{}, Body, Req);
test(Req0, <<"header-command">>) ->
	{ok, Body, Req1} = read_body(Req0),
	Req = cowboy_req:stream_reply(200, #{}, Req1),
	cowboy_req:stream_body(Body, fin, Req);
test(Req0, <<"accept-identity">>) ->
	{ok, Body, Req} = read_body(Req0),
	cowboy_req:reply(200,
		#{<<"accept-encoding">> => <<"identity">>},
		Body, Req);
test(Req0, <<"invalid-header">>) ->
	{ok, Body, Req} = read_body(Req0),
	cowboy_req:reply(200,
		#{<<"accept-encoding">> => <<";">>},
		Body, Req);
test(Req0, <<"reject-explicit-header">>) ->
	{ok, Body, Req} = read_body(Req0),
	cowboy_req:reply(200,
		#{<<"accept-encoding">> => <<"identity, gzip;q=0">>},
		Body, Req);
test(Req0, <<"reject-implicit-header">>) ->
	{ok, Body, Req} = read_body(Req0),
	cowboy_req:reply(200,
		#{<<"accept-encoding">> => <<"identity, *;q=0">>},
		Body, Req);
test(Req0, <<"accept-explicit-header">>) ->
	{ok, Body, Req} = read_body(Req0),
	cowboy_req:reply(200,
		#{<<"accept-encoding">> => <<"identity, gzip;q=0.5">>},
		Body, Req);
test(Req0, <<"accept-implicit-header">>) ->
	{ok, Body, Req} = read_body(Req0),
	cowboy_req:reply(200,
		#{<<"accept-encoding">> => <<"identity, *;q=0.5">>},
		Body, Req).

read_body(Req0) ->
	{Status, Data, Req} = cowboy_req:read_body(Req0, #{length => 1000}),
	do_read_body(Status, Req, Data).

do_read_body(more, Req0, Acc) ->
	{Status, Data, Req} = cowboy_req:read_body(Req0),
	do_read_body(Status, Req, << Acc/binary, Data/binary >>);
do_read_body(ok, Req, Acc) ->
	{ok, Acc, Req}.
