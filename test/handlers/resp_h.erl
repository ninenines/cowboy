%% This module echoes back the value the test is interested in.

-module(resp_h).

-export([init/2]).

init(Req, Opts) ->
	do(cowboy_req:binding(key, Req), Req, Opts).

do(<<"set_resp_cookie3">>, Req0, Opts) ->
	Req = case cowboy_req:binding(arg, Req0) of
		undefined ->
			cowboy_req:set_resp_cookie(<<"mycookie">>, "myvalue", Req0);
		<<"multiple">> ->
			Req1 = cowboy_req:set_resp_cookie(<<"mycookie">>, "myvalue", Req0),
			cowboy_req:set_resp_cookie(<<"yourcookie">>, <<"yourvalue">>, Req1);
		<<"overwrite">> ->
			Req1 = cowboy_req:set_resp_cookie(<<"mycookie">>, "myvalue", Req0),
			cowboy_req:set_resp_cookie(<<"mycookie">>, <<"overwrite">>, Req1)
	end,
	{ok, cowboy_req:reply(200, #{}, "OK", Req), Opts};
do(<<"set_resp_cookie4">>, Req0, Opts) ->
	Req = cowboy_req:set_resp_cookie(<<"mycookie">>, "myvalue", Req0,
		#{path => cowboy_req:path(Req0)}),
	{ok, cowboy_req:reply(200, #{}, "OK", Req), Opts};
do(<<"set_resp_header">>, Req0, Opts) ->
	Req = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, Req0),
	{ok, cowboy_req:reply(200, #{}, "OK", Req), Opts};
do(<<"set_resp_headers">>, Req0, Opts) ->
	Req = cowboy_req:set_resp_headers(#{
		<<"content-type">> => <<"text/plain">>,
		<<"content-encoding">> => <<"compress">>
	}, Req0),
	{ok, cowboy_req:reply(200, #{}, "OK", Req), Opts};
do(<<"resp_header_defined">>, Req0, Opts) ->
	Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, Req0),
	<<"text/plain">> = cowboy_req:resp_header(<<"content-type">>, Req1),
	<<"text/plain">> = cowboy_req:resp_header(<<"content-type">>, Req1, default),
	{ok, cowboy_req:reply(200, #{}, "OK", Req0), Opts};
do(<<"resp_header_default">>, Req, Opts) ->
	undefined = cowboy_req:resp_header(<<"content-type">>, Req),
	default = cowboy_req:resp_header(<<"content-type">>, Req, default),
	{ok, cowboy_req:reply(200, #{}, "OK", Req), Opts};
do(<<"resp_headers">>, Req0, Opts) ->
	Req1 = cowboy_req:set_resp_header(<<"server">>, <<"nginx">>, Req0),
	Req = cowboy_req:set_resp_headers(#{
		<<"content-type">> => <<"text/plain">>,
		<<"content-encoding">> => <<"compress">>
	}, Req1),
	Headers = cowboy_req:resp_headers(Req),
	true = maps:is_key(<<"server">>, Headers),
	true = maps:is_key(<<"content-type">>, Headers),
	true = maps:is_key(<<"content-encoding">>, Headers),
	{ok, cowboy_req:reply(200, #{}, "OK", Req), Opts};
do(<<"resp_headers_empty">>, Req, Opts) ->
	#{} = cowboy_req:resp_headers(Req),
	{ok, cowboy_req:reply(200, #{}, "OK", Req), Opts};
do(<<"set_resp_body">>, Req0, Opts) ->
	Arg = cowboy_req:binding(arg, Req0),
	Req1 = case Arg of
		<<"sendfile">> ->
			AppFile = code:where_is_file("cowboy.app"),
			cowboy_req:set_resp_body({sendfile, 0, filelib:file_size(AppFile), AppFile}, Req0);
		_ ->
			cowboy_req:set_resp_body(<<"OK">>, Req0)
	end,
	Req = case Arg of
		<<"override">> ->
			cowboy_req:reply(200, #{}, <<"OVERRIDE">>, Req1);
		_ ->
			cowboy_req:reply(200, Req1)
	end,
	{ok, Req, Opts};
do(<<"has_resp_header">>, Req0, Opts) ->
	false = cowboy_req:has_resp_header(<<"content-type">>, Req0),
	Req = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, Req0),
	true = cowboy_req:has_resp_header(<<"content-type">>, Req),
	{ok, cowboy_req:reply(200, #{}, "OK", Req), Opts};
do(<<"has_resp_body">>, Req0, Opts) ->
	case cowboy_req:binding(arg, Req0) of
		<<"sendfile">> ->
			%% @todo Cases for sendfile. Note that sendfile 0 is unallowed.
			false = cowboy_req:has_resp_body(Req0),
			Req = cowboy_req:set_resp_body({sendfile, 0, 10, code:where_is_file("cowboy.app")}, Req0),
			true = cowboy_req:has_resp_body(Req),
			{ok, cowboy_req:reply(200, #{}, <<"OK">>, Req), Opts};
		undefined ->
			false = cowboy_req:has_resp_body(Req0),
			Req = cowboy_req:set_resp_body(<<"OK">>, Req0),
			true = cowboy_req:has_resp_body(Req),
			{ok, cowboy_req:reply(200, #{}, Req), Opts}
	end;
do(<<"delete_resp_header">>, Req0, Opts) ->
	false = cowboy_req:has_resp_header(<<"content-type">>, Req0),
	Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, Req0),
	true = cowboy_req:has_resp_header(<<"content-type">>, Req1),
	Req = cowboy_req:delete_resp_header(<<"content-type">>, Req1),
	false = cowboy_req:has_resp_header(<<"content-type">>, Req),
	{ok, cowboy_req:reply(200, #{}, "OK", Req), Opts};
do(<<"reply2">>, Req0, Opts) ->
	Req = case cowboy_req:binding(arg, Req0) of
		<<"binary">> ->
			cowboy_req:reply(<<"200 GOOD">>, Req0);
		<<"error">> ->
			ct_helper:ignore(cowboy_req, reply, 4),
			cowboy_req:reply(ok, Req0);
		<<"twice">> ->
			ct_helper:ignore(cowboy_req, reply, 4),
			Req1 = cowboy_req:reply(200, Req0),
			cowboy_req:reply(200, Req1);
		Status ->
			cowboy_req:reply(binary_to_integer(Status), Req0)
	end,
	{ok, Req, Opts};
do(<<"reply3">>, Req0, Opts) ->
	Req = case cowboy_req:binding(arg, Req0) of
		<<"error">> ->
			ct_helper:ignore(cowboy_req, reply, 4),
			cowboy_req:reply(200, ok, Req0);
		Status ->
			cowboy_req:reply(binary_to_integer(Status),
				#{<<"content-type">> => <<"text/plain">>}, Req0)
	end,
	{ok, Req, Opts};
do(<<"reply4">>, Req0, Opts) ->
	Req = case cowboy_req:binding(arg, Req0) of
		<<"error">> ->
			ct_helper:ignore(erlang, iolist_size, 1),
			cowboy_req:reply(200, #{}, ok, Req0);
		Status ->
			cowboy_req:reply(binary_to_integer(Status), #{}, <<"OK">>, Req0)
	end,
	{ok, Req, Opts};
do(<<"stream_reply2">>, Req0, Opts) ->
	Req = case cowboy_req:binding(arg, Req0) of
		<<"binary">> ->
			cowboy_req:stream_reply(<<"200 GOOD">>, Req0);
		<<"error">> ->
			ct_helper:ignore(cowboy_req, stream_reply, 3),
			cowboy_req:stream_reply(ok, Req0);
		Status ->
			cowboy_req:stream_reply(binary_to_integer(Status), Req0)
	end,
	stream_body(Req),
	{ok, Req, Opts};
do(<<"stream_reply3">>, Req0, Opts) ->
	Req = case cowboy_req:binding(arg, Req0) of
		<<"error">> ->
			ct_helper:ignore(cowboy_req, stream_reply, 3),
			cowboy_req:stream_reply(200, ok, Req0);
		Status ->
			cowboy_req:stream_reply(binary_to_integer(Status),
				#{<<"content-type">> => <<"text/plain">>}, Req0)
	end,
	stream_body(Req),
	{ok, Req, Opts};
do(<<"stream_body">>, Req, Opts) ->
	%% Call stream_body without initiating streaming.
	cowboy_req:stream_body(<<0:800000>>, fin, Req),
	{ok, Req, Opts};
do(<<"push">>, Req, Opts) ->
	case cowboy_req:binding(arg, Req) of
		<<"method">> ->
			cowboy_req:push("/static/style.css", #{<<"accept">> => <<"text/css">>}, Req,
				#{method => <<"HEAD">>});
		<<"origin">> ->
			cowboy_req:push("/static/style.css", #{<<"accept">> => <<"text/css">>}, Req,
				#{scheme => <<"ftp">>, host => <<"127.0.0.1">>, port => 21});
		<<"qs">> ->
			cowboy_req:push("/static/style.css", #{<<"accept">> => <<"text/css">>}, Req,
				#{qs => <<"server=cowboy&version=2.0">>});
		_ ->
			cowboy_req:push("/static/style.css", #{<<"accept">> => <<"text/css">>}, Req),
			%% The text/plain mime is not defined by default, so a 406 will be returned.
			cowboy_req:push("/static/plain.txt", #{<<"accept">> => <<"text/plain">>}, Req)
	end,
	{ok, cowboy_req:reply(200, Req), Opts}.

stream_body(Req) ->
	_ = [cowboy_req:stream_body(<<0:800000>>, nofin, Req) || _ <- lists:seq(1,9)],
	cowboy_req:stream_body(<<0:800000>>, fin, Req).
