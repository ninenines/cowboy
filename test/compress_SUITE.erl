%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(compress_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

%% ct.

all() ->
	All = [
		{group, http_compress},
		{group, https_compress},
		{group, h2_compress},
		{group, h2c_compress}
%		{group, h3_compress}
	],
	%% Don't run HTTP/3 tests on Windows for now.
	case os:type() of
		{win32, _} ->
			All -- [{group, h3_compress}];
		_ ->
			All
	end.

groups() ->
	cowboy_test:common_groups(ct_helper:all(?MODULE)).

init_per_group(Name, Config) ->
	cowboy_test:init_common_groups(Name, Config, ?MODULE).

end_per_group(Name, _) ->
	cowboy_test:stop_group(Name).

%% Routes.

init_dispatch(_Config) ->
	cowboy_router:compile([{"[...]", [
		{"/reply/:what", compress_h, reply},
		{"/stream_reply/:what", compress_h, stream_reply}
	]}]).

%% Logger function silencing the expected crashes.

error("Unhandled exception " ++ _, [error, bad_compression_level|_]) ->
	ok;
error(Format, Args) ->
	error_logger:error_msg(Format, Args).

warning("Received message " ++ _, _) ->
	ok;
warning(Format, Args) ->
	error_logger:warning_msg(Format, Args).

%% Internal.

do_get(Path, ReqHeaders, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, ReqHeaders),
	{response, IsFin, Status, RespHeaders} = gun:await(ConnPid, Ref),
	{ok, Body} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref);
		fin -> {ok, <<>>}
	end,
	gun:close(ConnPid),
	{Status, RespHeaders, Body}.

do_get_opts(Name, Path, ProtoOpts, Config) ->
	do_with_listener(Name, ProtoOpts, Config, fun(ListenerConfig) ->
		do_get(Path, [{<<"accept-encoding">>, <<"gzip">>}], ListenerConfig)
	end).

do_with_listener(Name, ProtoOpts, Config0, TestFun) ->
	Fun = case config(ref, Config0) of
		https_compress -> init_https;
		h2_compress -> init_http2;
		_ -> init_http
	end,
	Config = cowboy_test:Fun(Name, ProtoOpts#{
		env => #{dispatch => init_dispatch(Config0)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h]
	}, Config0),
	try
		TestFun(Config)
	after
		cowboy:stop_listener(Name)
	end.

%% The stream handler crashes on invalid levels: HTTP/1.1
%% replies with a 500 and HTTP/2 resets the stream.
do_get_invalid_level(Path, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, [{<<"accept-encoding">>, <<"gzip">>}]),
	case {config(protocol, Config), gun:await(ConnPid, Ref)} of
		{http, {response, _, 500, _}} -> ok;
		{http2, {error, {stream_error, {stream_error, internal_error, _}}}} -> ok
	end,
	gun:close(ConnPid).

do_gzip(Level, Chunks) ->
	do_gzip(Level, sync, Chunks).

%% Chunks may contain {level, Level} to change the level
%% between chunks like the set_options command does.
do_gzip(Level, Flush, Chunks) ->
	Z = zlib:open(),
	try
		zlib:deflateInit(Z, Level, deflated, 31, 8, default),
		{Nofin, [Last]} = lists:split(length(Chunks) - 1, Chunks),
		Gz = [case C of
			{level, NewLevel} -> zlib:deflateParams(Z, NewLevel, default), [];
			_ -> zlib:deflate(Z, C, Flush)
		end || C <- Nofin],
		GzLast = zlib:deflate(Z, Last, finish),
		zlib:deflateEnd(Z),
		iolist_to_binary([Gz, GzLast])
	after
		zlib:close(Z)
	end.

do_set_level(Chunks, Level) ->
	{Chunks1, Chunks2} = lists:split(5, Chunks),
	Chunks1 ++ [{level, Level}|Chunks2].

%% Tests.

gzip_accept_encoding_malformed(Config) ->
	doc("Send malformed accept-encoding; get an uncompressed response."),
	{200, Headers, _} = do_get("/reply/large",
		[{<<"accept-encoding">>, <<";">>}], Config),
	false = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	{_, <<"100000">>} = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

gzip_accept_encoding_missing(Config) ->
	doc("Don't send accept-encoding; get an uncompressed response."),
	{200, Headers, _} = do_get("/reply/large",
		[], Config),
	false = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	{_, <<"100000">>} = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

gzip_accept_encoding_no_gzip(Config) ->
	doc("Send accept-encoding: compress (unsupported by Cowboy); get an uncompressed response."),
	{200, Headers, _} = do_get("/reply/large",
		[{<<"accept-encoding">>, <<"compress">>}], Config),
	false = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	{_, <<"100000">>} = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

gzip_accept_encoding_not_supported(Config) ->
	doc("Send unsupported accept-encoding; get an uncompressed response."),
	{200, Headers, _} = do_get("/reply/large",
		[{<<"accept-encoding">>, <<"application/gzip">>}], Config),
	false = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	{_, <<"100000">>} = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

gzip_reply_content_encoding(Config) ->
	doc("Reply with content-encoding header; get an uncompressed response."),
	{200, Headers, _} = do_get("/reply/content-encoding",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	%% We set the content-encoding to compress; without actually compressing.
	{_, <<"compress">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	%% The reply didn't include a vary header.
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	{_, <<"100000">>} = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

gzip_reply_etag(Config) ->
	doc("Reply with etag header; get an uncompressed response."),
	{200, Headers, _} = do_get("/reply/etag",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	%% We set a strong etag.
	{_, <<"\"STRONK\"">>} = lists:keyfind(<<"etag">>, 1, Headers),
	%% The reply didn't include a vary header.
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	{_, <<"100000">>} = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

gzip_reply_large_body(Config) ->
	doc("Reply a large body; get a gzipped response."),
	{200, Headers, GzBody} = do_get("/reply/large",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	{_, Length} = lists:keyfind(<<"content-length">>, 1, Headers),
	ct:log("Original length: 100000; compressed: ~s.", [Length]),
	_ = zlib:gunzip(GzBody),
	ok.

gzip_reply_sendfile(Config) ->
	doc("Reply using sendfile; get an uncompressed response."),
	{200, Headers, Body} = do_get("/reply/sendfile",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	false = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	ct:log("Body received:~n~p~n", [Body]),
	ok.

gzip_reply_small_body(Config) ->
	doc("Reply a small body; get an uncompressed response."),
	{200, Headers, _} = do_get("/reply/small",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	false = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	{_, <<"100">>} = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

gzip_stream_reply(Config) ->
	doc("Stream reply; get a gzipped response."),
	{200, Headers, GzBody} = do_get("/stream_reply/large",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	_ = zlib:gunzip(GzBody),
	ok.

gzip_stream_reply_sendfile(Config) ->
	doc("Stream reply using sendfile for some chunks; get a gzipped response."),
	{200, Headers, GzBody} = do_get("/stream_reply/sendfile",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	_ = zlib:gunzip(GzBody),
	ok.

gzip_stream_reply_sendfile_fin(Config) ->
	doc("Stream reply using sendfile for some chunks; get a gzipped response."),
	{200, Headers, GzBody} = do_get("/stream_reply/sendfile_fin",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	_ = zlib:gunzip(GzBody),
	ok.

gzip_stream_reply_content_encoding(Config) ->
	doc("Stream reply with content-encoding header; get an uncompressed response."),
	{200, Headers, Body} = do_get("/stream_reply/content-encoding",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	{_, <<"compress">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	100000 = iolist_size(Body),
	ok.

gzip_stream_reply_etag(Config) ->
	doc("Stream reply with etag header; get an uncompressed response."),
	{200, Headers, Body} = do_get("/stream_reply/etag",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	{_, <<"\"STRONK\"">>} = lists:keyfind(<<"etag">>, 1, Headers),
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	100000 = iolist_size(Body),
	ok.

opts_compress_buffering_false(Config0) ->
	doc("Confirm that the compress_buffering option can be set to false, "
		"which is the default."),
	Fun = case config(ref, Config0) of
		https_compress -> init_https;
		h2_compress -> init_http2;
		_ -> init_http
	end,
	Config = cowboy_test:Fun(?FUNCTION_NAME, #{
		env => #{dispatch => init_dispatch(Config0)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h],
		compress_buffering => false
	}, Config0),
	try
		ConnPid = gun_open(Config),
		Ref = gun:get(ConnPid, "/stream_reply/delayed",
			[{<<"accept-encoding">>, <<"gzip">>}]),
		{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
		{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
		{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
		Z = zlib:open(),
		zlib:inflateInit(Z, 31),
		{data, nofin, Data1} = gun:await(ConnPid, Ref, 500),
		<<"data: Hello!\r\n\r\n">> = iolist_to_binary(zlib:inflate(Z, Data1)),
		timer:sleep(1000),
		{data, nofin, Data2} = gun:await(ConnPid, Ref, 500),
		<<"data: World!\r\n\r\n">> = iolist_to_binary(zlib:inflate(Z, Data2)),
		gun:close(ConnPid)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

opts_compress_buffering_true(Config0) ->
	doc("Confirm that the compress_buffering option can be set to true, "
		"and that the data received is buffered."),
	Fun = case config(ref, Config0) of
		https_compress -> init_https;
		h2_compress -> init_http2;
		_ -> init_http
	end,
	Config = cowboy_test:Fun(?FUNCTION_NAME, #{
		env => #{dispatch => init_dispatch(Config0)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h],
		compress_buffering => true
	}, Config0),
	try
		ConnPid = gun_open(Config),
		Ref = gun:get(ConnPid, "/stream_reply/delayed",
			[{<<"accept-encoding">>, <<"gzip">>}]),
		{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
		{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
		{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
		Z = zlib:open(),
		zlib:inflateInit(Z, 31),
		%% The data gets buffered because it is too small.
		%% In zlib versions before OTP 20.1 the gzip header was also buffered.
		<<>> = case gun:await(ConnPid, Ref, 500) of
			{data, nofin, Data1} ->
				iolist_to_binary(zlib:inflate(Z, Data1));
			{error, timeout} ->
				<<>>
		end,
		gun:close(ConnPid)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

set_options_compress_buffering_false(Config0) ->
	doc("Confirm that the compress_buffering option can be dynamically "
		"set to false by a handler and that the data received is not buffered."),
	Fun = case config(ref, Config0) of
		https_compress -> init_https;
		h2_compress -> init_http2;
		_ -> init_http
	end,
	Config = cowboy_test:Fun(?FUNCTION_NAME, #{
		env => #{dispatch => init_dispatch(Config0)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h],
		compress_buffering => true
	}, Config0),
	try
		ConnPid = gun_open(Config),
		Ref = gun:get(ConnPid, "/stream_reply/set_options_buffering_false",
			[{<<"accept-encoding">>, <<"gzip">>}]),
		{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
		{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
		{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
		Z = zlib:open(),
		zlib:inflateInit(Z, 31),
		{data, nofin, Data1} = gun:await(ConnPid, Ref, 500),
		<<"data: Hello!\r\n\r\n">> = iolist_to_binary(zlib:inflate(Z, Data1)),
		timer:sleep(1000),
		{data, nofin, Data2} = gun:await(ConnPid, Ref, 500),
		<<"data: World!\r\n\r\n">> = iolist_to_binary(zlib:inflate(Z, Data2)),
		gun:close(ConnPid)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

set_options_compress_buffering_true(Config0) ->
	doc("Confirm that the compress_buffering option can be dynamically "
		"set to true by a handler and that the data received is buffered."),
	Fun = case config(ref, Config0) of
		https_compress -> init_https;
		h2_compress -> init_http2;
		_ -> init_http
	end,
	Config = cowboy_test:Fun(?FUNCTION_NAME, #{
		env => #{dispatch => init_dispatch(Config0)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h],
		compress_buffering => false
	}, Config0),
	try
		ConnPid = gun_open(Config),
		Ref = gun:get(ConnPid, "/stream_reply/set_options_buffering_true",
			[{<<"accept-encoding">>, <<"gzip">>}]),
		{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
		{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
		{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
		Z = zlib:open(),
		zlib:inflateInit(Z, 31),
		%% The data gets buffered because it is too small.
		%% In zlib versions before OTP 20.1 the gzip header was also buffered.
		<<>> = case gun:await(ConnPid, Ref, 500) of
			{data, nofin, Data1} ->
				iolist_to_binary(zlib:inflate(Z, Data1));
			{error, timeout} ->
				<<>>
		end,
		gun:close(ConnPid)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

set_options_compress_threshold_0(Config) ->
	doc("Confirm that the compress_threshold option can be dynamically "
		"set to change how large response bodies must be to be compressed."),
	{200, Headers, GzBody} = do_get("/reply/set_options_threshold0",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	_ = zlib:gunzip(GzBody),
	ok.

opts_compress_level_default(Config) ->
	doc("Confirm that responses are compressed using the default "
		"compression level when the compress_level option is not set."),
	Data = iolist_to_binary(compress_h:level_chunks()),
	{200, Headers, GzBody} = do_get("/reply/level",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	GzBody = do_gzip(default, [Data]),
	ok.

opts_compress_level_reply(Config) ->
	doc("Confirm that the compress_level option sets the compression "
		"level of normal responses."),
	Data = iolist_to_binary(compress_h:level_chunks()),
	Bodies = [begin
		{200, Headers, GzBody} = do_get_opts(?FUNCTION_NAME, "/reply/level",
			#{compress_level => Level}, Config),
		{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
		{Level, GzBody} = {Level, do_gzip(Level, [Data])},
		GzBody
	end || Level <- lists:seq(0, 9)],
	10 = length(lists:usort(Bodies)),
	_ = [begin
		{200, _, GzBody} = do_get_opts(?FUNCTION_NAME, "/reply/level",
			#{compress_level => Level}, Config),
		{Level, GzBody} = {Level, do_gzip(Level, [Data])}
	end || Level <- [none, best_speed, default, best_compression]],
	ok.

opts_compress_level_invalid(Config) ->
	doc("Confirm that an out of range compress_level option "
		"results in an error instead of a compressed response."),
	_ = [do_with_listener(?FUNCTION_NAME, #{compress_level => Level, logger => ?MODULE}, Config,
		fun(ListenerConfig) ->
			do_get_invalid_level("/reply/level?small", ListenerConfig),
			do_get_invalid_level("/stream_reply/level?small", ListenerConfig)
		end)
	|| Level <- [-1, 10]],
	ok.

opts_compress_level_stream_reply(Config) ->
	doc("Confirm that the compress_level option sets the compression "
		"level of streamed responses."),
	Chunks = compress_h:level_chunks(),
	_ = [begin
		{200, Headers, GzBody} = do_get_opts(?FUNCTION_NAME, "/stream_reply/level",
			#{compress_level => Level}, Config),
		{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
		{Level, GzBody} = {Level, do_gzip(Level, Chunks)}
	end || Level <- [0, 1, 9]],
	ok.

set_options_compress_level_reply(Config) ->
	doc("Confirm that the compress_level option can be dynamically "
		"set by a handler before the response is sent."),
	Data = iolist_to_binary(compress_h:level_chunks()),
	_ = [begin
		{200, Headers, GzBody} = do_get("/reply/set_options_level?level=" ++ QsLevel,
			[{<<"accept-encoding">>, <<"gzip">>}], Config),
		{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
		{Level, GzBody} = {Level, do_gzip(Level, [Data])}
	end || {Level, QsLevel} <- [{0, "0"}, {9, "9"}, {best_speed, "best_speed"}]],
	ok.

set_options_compress_level_reply_invalid(Config) ->
	doc("Confirm that dynamically setting an out of range compress_level "
		"option before the response is sent results in an error."),
	do_with_listener(?FUNCTION_NAME, #{logger => ?MODULE}, Config, fun(ListenerConfig) ->
		do_get_invalid_level("/reply/set_options_level?small&level=-1", ListenerConfig),
		do_get_invalid_level("/reply/set_options_level?small&level=10", ListenerConfig)
	end).

set_options_compress_level_stream_reply(Config) ->
	doc("Confirm that the compress_level option can be dynamically "
		"set by a handler while the response is streamed and that "
		"only the data sent afterwards uses the new level."),
	Chunks = compress_h:level_chunks(),
	_ = [begin
		{200, Headers, GzBody} = do_get("/stream_reply/set_options_level?level=" ++ QsLevel,
			[{<<"accept-encoding">>, <<"gzip">>}], Config),
		{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
		{Level, GzBody} = {Level, do_gzip(default, do_set_level(Chunks, Level))},
		false = GzBody =:= do_gzip(default, Chunks),
		false = GzBody =:= do_gzip(Level, Chunks)
	end || {Level, QsLevel} <- [{0, "0"}, {9, "9"}, {best_speed, "best_speed"}]],
	ok.

set_options_compress_level_stream_reply_invalid(Config0) ->
	doc("Confirm that dynamically setting an out of range compress_level "
		"option while the response is streamed aborts the response "
		"after the data sent before the option was set."),
	{Chunks, _} = lists:split(5, compress_h:small_chunks()),
	DataBefore = iolist_to_binary(Chunks),
	do_with_listener(?FUNCTION_NAME, #{logger => ?MODULE}, Config0, fun(Config) -> [begin
		ConnPid = gun_open(Config),
		Ref = gun:get(ConnPid, "/stream_reply/set_options_level?small&level=" ++ QsLevel,
			[{<<"accept-encoding">>, <<"gzip">>}]),
		{response, nofin, 200, _} = gun:await(ConnPid, Ref),
		case {config(protocol, Config), gun:await_body(ConnPid, Ref)} of
			{http, {ok, GzBody}} ->
				DataBefore = zlib:gunzip(GzBody);
			{http2, {error, {stream_error, {stream_error, internal_error, _}}}} ->
				ok
		end,
		gun:close(ConnPid)
	end || QsLevel <- ["-1", "10"]] end),
	ok.

set_options_compress_level_stream_reply_same(Config) ->
	doc("Confirm that dynamically setting the compress_level option "
		"to the current or an equivalent level does not flush buffered data."),
	Chunks = compress_h:level_chunks(),
	_ = [begin
		{200, _, GzBody} = do_get_opts(?FUNCTION_NAME,
			"/stream_reply/set_options_level?level=" ++ QsLevel,
			#{compress_buffering => true, compress_level => Level}, Config),
		{Level, QsLevel, GzBody} = {Level, QsLevel, do_gzip(Level, none, Chunks)},
		false = GzBody =:= do_gzip(Level, none, do_set_level(Chunks, Level))
	end || {Level, QsLevel} <- [
		{1, "1"},
		{1, "best_speed"},
		{best_speed, "1"},
		{0, "none"},
		{none, "0"},
		{9, "best_compression"},
		{best_compression, "9"},
		{6, "default"},
		{default, "6"}
	]],
	ok.

vary_accept(Config) ->
	doc("Add accept-encoding to vary when the response has a 'vary: accept' header."),
	{200, Headers, _} = do_get("/reply/vary", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-vary">>, <<"accept">>}
	], Config),
	{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept, accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	ok.

vary_accept_accept_encoding(Config) ->
	doc("Don't change the vary value when the response has a 'vary: accept, accept-encoding' header."),
	{200, Headers, _} = do_get("/reply/vary", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-vary">>, <<"accept, accept-encoding">>}
	], Config),
	{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept, accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	ok.

vary_empty(Config) ->
	doc("Add accept-encoding to vary when the response has an empty vary header."),
	{200, Headers, _} = do_get("/reply/vary", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-vary">>, <<>>}
	], Config),
	{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"accept-encoding">>} = lists:keyfind(<<"vary">>, 1, Headers),
	ok.

vary_wildcard(Config) ->
	doc("Don't change the vary value when the response has a 'vary: *' header."),
	{200, Headers, _} = do_get("/reply/vary", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-vary">>, <<"*">>}
	], Config),
	{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"*">>} = lists:keyfind(<<"vary">>, 1, Headers),
	ok.
