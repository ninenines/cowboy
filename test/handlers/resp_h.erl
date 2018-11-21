%% This module echoes back the value the test is interested in.

-module(resp_h).

%% @todo Probably should have a separate handler for errors,
%% so that we can dialyze all the other correct calls.
-dialyzer({nowarn_function, do/3}).

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
do(<<"set_resp_header_server">>, Req0, Opts) ->
	Req = cowboy_req:set_resp_header(<<"server">>, <<"nginx">>, Req0),
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
		<<"sendfile0">> ->
			AppFile = code:where_is_file("cowboy.app"),
			cowboy_req:set_resp_body({sendfile, 0, 0, AppFile}, Req0);
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
	%% We try to delete first even though it hasn't been set to
	%% make sure this noop is possible.
	Req1 = cowboy_req:delete_resp_header(<<"content-type">>, Req0),
	false = cowboy_req:has_resp_header(<<"content-type">>, Req1),
	Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, Req1),
	true = cowboy_req:has_resp_header(<<"content-type">>, Req2),
	Req = cowboy_req:delete_resp_header(<<"content-type">>, Req2),
	false = cowboy_req:has_resp_header(<<"content-type">>, Req),
	{ok, cowboy_req:reply(200, #{}, "OK", Req), Opts};
do(<<"inform2">>, Req0, Opts) ->
	case cowboy_req:binding(arg, Req0) of
		<<"binary">> ->
			cowboy_req:inform(<<"102 On my way">>, Req0);
		<<"error">> ->
			ct_helper:ignore(cowboy_req, inform, 3),
			cowboy_req:inform(ok, Req0);
		<<"twice">> ->
			cowboy_req:inform(102, Req0),
			cowboy_req:inform(102, Req0);
		Status ->
			cowboy_req:inform(binary_to_integer(Status), Req0)
	end,
	Req = cowboy_req:reply(200, Req0),
	{ok, Req, Opts};
do(<<"inform3">>, Req0, Opts) ->
	Headers = #{<<"ext-header">> => <<"ext-value">>},
	case cowboy_req:binding(arg, Req0) of
		<<"binary">> ->
			cowboy_req:inform(<<"102 On my way">>, Headers, Req0);
		<<"error">> ->
			ct_helper:ignore(cowboy_req, inform, 3),
			cowboy_req:inform(ok, Headers, Req0);
		<<"twice">> ->
			cowboy_req:inform(102, Headers, Req0),
			cowboy_req:inform(102, Headers, Req0);
		Status ->
			cowboy_req:inform(binary_to_integer(Status), Headers, Req0)
	end,
	Req = cowboy_req:reply(200, Req0),
	{ok, Req, Opts};
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
	case cowboy_req:binding(arg, Req0) of
		<<"binary">> ->
			Req = cowboy_req:stream_reply(<<"200 GOOD">>, Req0),
			stream_body(Req),
			{ok, Req, Opts};
		<<"error">> ->
			ct_helper:ignore(cowboy_req, stream_reply, 3),
			Req = cowboy_req:stream_reply(ok, Req0),
			stream_body(Req),
			{ok, Req, Opts};
		<<"204">> ->
			Req = cowboy_req:stream_reply(204, Req0),
			{ok, Req, Opts};
		<<"304">> ->
			Req = cowboy_req:stream_reply(304, Req0),
			{ok, Req, Opts};
		Status ->
			Req = cowboy_req:stream_reply(binary_to_integer(Status), Req0),
			stream_body(Req),
			{ok, Req, Opts}
	end;
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
do(<<"stream_body">>, Req0, Opts) ->
	case cowboy_req:binding(arg, Req0) of
		<<"fin0">> ->
			Req = cowboy_req:stream_reply(200, Req0),
			cowboy_req:stream_body(<<"Hello world!">>, nofin, Req),
			cowboy_req:stream_body(<<>>, fin, Req),
			{ok, Req, Opts};
		<<"multiple">> ->
			Req = cowboy_req:stream_reply(200, Req0),
			cowboy_req:stream_body(<<"Hello ">>, nofin, Req),
			cowboy_req:stream_body(<<"world">>, nofin, Req),
			cowboy_req:stream_body(<<"!">>, fin, Req),
			{ok, Req, Opts};
		<<"nofin">> ->
			Req = cowboy_req:stream_reply(200, Req0),
			cowboy_req:stream_body(<<"Hello world!">>, nofin, Req),
			{ok, Req, Opts};
		<<"sendfile">> ->
			AppFile = code:where_is_file("cowboy.app"),
			AppSize = filelib:file_size(AppFile),
			Req = cowboy_req:stream_reply(200, Req0),
			cowboy_req:stream_body(<<"Hello ">>, nofin, Req),
			cowboy_req:stream_body({sendfile, 0, AppSize, AppFile}, nofin, Req),
			cowboy_req:stream_body(<<" interspersed ">>, nofin, Req),
			cowboy_req:stream_body({sendfile, 0, AppSize, AppFile}, nofin, Req),
			cowboy_req:stream_body(<<" world!">>, fin, Req),
			{ok, Req, Opts};
		<<"sendfile_fin">> ->
			AppFile = code:where_is_file("cowboy.app"),
			AppSize = filelib:file_size(AppFile),
			Req = cowboy_req:stream_reply(200, Req0),
			cowboy_req:stream_body(<<"Hello! ">>, nofin, Req),
			cowboy_req:stream_body({sendfile, 0, AppSize, AppFile}, fin, Req),
			{ok, Req, Opts};
		_ ->
			%% Call stream_body without initiating streaming.
			cowboy_req:stream_body(<<0:800000>>, fin, Req0),
			{ok, Req0, Opts}
	end;
do(<<"stream_body_content_length">>, Req0, Opts) ->
	case cowboy_req:binding(arg, Req0) of
		<<"fin0">> ->
			Req1 = cowboy_req:set_resp_header(<<"content-length">>, <<"12">>, Req0),
			Req = cowboy_req:stream_reply(200, Req1),
			cowboy_req:stream_body(<<"Hello world!">>, nofin, Req),
			cowboy_req:stream_body(<<>>, fin, Req),
			{ok, Req, Opts};
		<<"multiple">> ->
			Req1 = cowboy_req:set_resp_header(<<"content-length">>, <<"12">>, Req0),
			Req = cowboy_req:stream_reply(200, Req1),
			cowboy_req:stream_body(<<"Hello ">>, nofin, Req),
			cowboy_req:stream_body(<<"world">>, nofin, Req),
			cowboy_req:stream_body(<<"!">>, fin, Req),
			{ok, Req, Opts};
		<<"nofin">> ->
			Req1 = cowboy_req:set_resp_header(<<"content-length">>, <<"12">>, Req0),
			Req = cowboy_req:stream_reply(200, Req1),
			cowboy_req:stream_body(<<"Hello world!">>, nofin, Req),
			{ok, Req, Opts};
		<<"nofin-error">> ->
			Req1 = cowboy_req:set_resp_header(<<"content-length">>, <<"12">>, Req0),
			Req = cowboy_req:stream_reply(200, Req1),
			cowboy_req:stream_body(<<"Hello">>, nofin, Req),
			{ok, Req, Opts}
	end;
do(<<"stream_events">>, Req0, Opts) ->
	case cowboy_req:binding(arg, Req0) of
		%%<<"single">>
		%%<<"list">>
		<<"single">> ->
			Req = cowboy_req:stream_reply(200,
				#{<<"content-type">> => <<"text/event-stream">>},
				Req0),
			cowboy_req:stream_events(#{
				event => <<"add_comment">>,
				data => <<"Comment text.\nWith many lines.">>
			}, fin, Req),
			{ok, Req, Opts};
		<<"list">> ->
			Req = cowboy_req:stream_reply(200,
				#{<<"content-type">> => <<"text/event-stream">>},
				Req0),
			cowboy_req:stream_events([
				#{
					event => <<"add_comment">>,
					data => <<"Comment text.\nWith many lines.">>
				},
				#{
					comment => <<"Set retry higher\nwith many lines also.">>,
					retry => 10000
				},
				#{
					id => <<"123">>,
					event => <<"add_comment">>,
					data => <<"Closing!">>
				}
			], fin, Req),
			{ok, Req, Opts};
		<<"multiple">> ->
			Req = cowboy_req:stream_reply(200,
				#{<<"content-type">> => <<"text/event-stream">>},
				Req0),
			cowboy_req:stream_events(#{
				event => <<"add_comment">>,
				data => <<"Comment text.\nWith many lines.">>
			}, nofin, Req),
			cowboy_req:stream_events(#{
				comment => <<"Set retry higher\nwith many lines also.">>,
				retry => 10000
			}, nofin, Req),
			cowboy_req:stream_events(#{
				id => <<"123">>,
				event => <<"add_comment">>,
				data => <<"Closing!">>
			}, fin, Req),
			{ok, Req, Opts}
	end;
do(<<"stream_trailers">>, Req0, Opts) ->
	case cowboy_req:binding(arg, Req0) of
		<<"large">> ->
			Req = cowboy_req:stream_reply(200, #{
				<<"trailer">> => <<"grpc-status">>
			}, Req0),
			cowboy_req:stream_body(<<0:800000>>, nofin, Req),
			cowboy_req:stream_trailers(#{
				<<"grpc-status">> => <<"0">>
			}, Req),
			{ok, Req, Opts};
		_ ->
			Req = cowboy_req:stream_reply(200, #{
				<<"trailer">> => <<"grpc-status">>
			}, Req0),
			cowboy_req:stream_body(<<"Hello world!">>, nofin, Req),
			cowboy_req:stream_trailers(#{
				<<"grpc-status">> => <<"0">>
			}, Req),
			{ok, Req, Opts}
	end;
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
