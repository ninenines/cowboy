-module(decompress_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

%% ct.

all() ->
	cowboy_test:common_all().

groups() ->
	cowboy_test:common_groups(ct_helper:all(?MODULE)).

init_per_group(Name = http, Config) ->
	cowboy_test:init_http(Name, init_plain_opts(Config), Config);
init_per_group(Name = https, Config) ->
	cowboy_test:init_http(Name, init_plain_opts(Config), Config);
init_per_group(Name = h2, Config) ->
	cowboy_test:init_http2(Name, init_plain_opts(Config), Config);
init_per_group(Name = h2c, Config) ->
	Config1 = cowboy_test:init_http(Name, init_plain_opts(Config), Config),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2});
init_per_group(Name = http_compress, Config) ->
	cowboy_test:init_http(Name, init_compress_opts(Config), Config);
init_per_group(Name = https_compress, Config) ->
	cowboy_test:init_http(Name, init_compress_opts(Config), Config);
init_per_group(Name = h2_compress, Config) ->
	cowboy_test:init_http2(Name, init_compress_opts(Config), Config);
init_per_group(Name = h2c_compress, Config) ->
	Config1 = cowboy_test:init_http(Name, init_compress_opts(Config), Config),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2}).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

init_plain_opts(Config) ->
	#{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))},
		stream_handlers => [cowboy_decompress_h, cowboy_stream_h]
	}.

init_compress_opts(Config) ->
	#{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))},
		stream_handlers => [cowboy_decompress_h, cowboy_compress_h, cowboy_stream_h]
	}.

init_routes(_) ->
	[{'_', [
		{"/echo/:what", decompress_h, []},
		{"/header", decompress_h, header_command},
		{"/invalid-header", decompress_h, invalid_header},
		{"/accept-identity", decompress_h, accept_identity},
		{"/reject-explicit-header", decompress_h, reject_explicit_header},
		{"/reject-implicit-header", decompress_h, reject_implicit_header},
		{"/accept-explicit-header", decompress_h, accept_explicit_header},
		{"/accept-implicit-header", decompress_h, accept_implicit_header}
	]}].

%% Internal.

do_post(Path, ReqHeaders, Body, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, Path, ReqHeaders, Body),
	{response, IsFin, Status, RespHeaders} = gun:await(ConnPid, Ref),
	{ok, ResponseBody} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref);
		fin -> {ok, <<>>}
	end,
	gun:close(ConnPid),
	{Status, RespHeaders, ResponseBody}.

create_gzip_bomb() ->
	Z = zlib:open(),
	zlib:deflateInit(Z, 9, deflated, 31, 8, default),
	%% 1000 chunks of 100000 zeroes (100MB).
	Bomb = do_create_gzip_bomb(Z, 1000),
	zlib:deflateEnd(Z),
	zlib:close(Z),
	iolist_to_binary(Bomb).

do_create_gzip_bomb(Z, 0) ->
	zlib:deflate(Z, << >>, finish);
do_create_gzip_bomb(Z, N) ->
	Data = <<0:800000>>,
	Deflate = zlib:deflate(Z, Data),
	[Deflate | do_create_gzip_bomb(Z, N - 1)].

%% Tests.

content_encoding_none(Config) ->
	doc("Send no content-encoding; get echo."),
	Body = <<"test">>,
	{200, _, Body} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<";">>}], Body, Config),
	ok.

content_encoding_malformed(Config) ->
	doc("Send malformed content-encoding; get echo."),
	Body = <<"test">>,
	{200, _, Body} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<";">>}], Body, Config),
	ok.

content_encoding_not_supported(Config) ->
	doc("Send content-encoding: compress (unsupported by Cowboy); get echo."),
	Body = <<"test">>,
	{200, _, Body} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<"compress">>}], Body, Config),
	ok.

content_encoding_wrong(Config) ->
	doc("Send content-encoding and unencoded body; get 400."),
	Body = <<"test">>,
	{400, _, _} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	ok.

decompress(Config) ->
	doc("Send content-encoding and encoded body; get decompressed response."),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, _, Data} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	ok.

decompress_stream(Config) ->
	doc("Stream encoded body; get decompressed response."),
	%% Handler read length 1KB. Compressing 3KB should be enough to trigger more.
	Data = crypto:strong_rand_bytes(3000),
	Body = zlib:gzip(Data),
	Size = byte_size(Body),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/normal", [{<<"content-encoding">>, <<"gzip">>}]),
	gun:data(ConnPid, Ref, nofin, binary:part(Body, 0, Size div 2)),
	timer:sleep(1000),
	gun:data(ConnPid, Ref, fin, binary:part(Body, Size div 2, Size div 2 + Size rem 2)),
	{response, IsFin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Data} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref);
		fin -> {ok, <<>>}
	end,
	gun:close(ConnPid),
	{200, _, Data} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	ok.

opts_decompress_ignore(Config0) ->
	doc("Confirm that the decompress_ignore option can be set."),
	Fun = case config(ref, Config0) of
		HTTPS when HTTPS =:= https_compress; HTTPS =:= https -> init_https;
		H2 when H2 =:= h2_compress; H2 =:= h2 -> init_http2;
		_ -> init_http
	end,
	Config = cowboy_test:Fun(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		stream_handlers => [cowboy_decompress_h, cowboy_stream_h],
		decompress_ignore => true
	}, Config0),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	try
		{200, _, Body} = do_post("/echo/normal",
			[{<<"content-encoding">>, <<"gzip">>}], Body, Config)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

set_options_decompress_ignore(Config) ->
	doc("Confirm that the decompress_ignore option can be dynamically
		set to true and the data received is not decompressed."),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, _, Body} = do_post("/echo/decompress_ignore",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	ok.

opts_decompress_ratio_limit(Config0) ->
	doc("Confirm that the decompress_ignore option can be set"),
	Fun = case config(ref, Config0) of
		HTTPS when HTTPS =:= https_compress; HTTPS =:= https -> init_https;
		H2 when H2 =:= h2_compress; H2 =:= h2 -> init_http2;
		_ -> init_http
	end,
	Config = cowboy_test:Fun(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		stream_handlers => [cowboy_decompress_h, cowboy_stream_h],
		decompress_ratio_limit => 1
	}, Config0),
	%% Data must be big enough for compression to be effective, so that ratio_limit=1 will fail.
	Data = <<0:800>>,
	Body = zlib:gzip(Data),
	try
		{413, _, _} = do_post("/echo/normal",
			[{<<"content-encoding">>, <<"gzip">>}], Body, Config)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

set_options_decompress_ratio_limit(Config) ->
	doc("Confirm that the decompress_ratio_limit option can be dynamically set."),
	%% Data must be big enough for compression to be effective, so that ratio_limit=1 will fail.
	Data = <<0:800>>,
	Body = zlib:gzip(Data),
	{413, _, _} = do_post("/echo/decompress_ratio_limit",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	ok.

gzip_bomb(Config) ->
	doc("Send body compressed with suspiciously large ratio; get 413."),
	Body = create_gzip_bomb(),
	{413, _, _} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	ok.

%% RFC 9110. Section 12.5.3. 3. When sent by a server in a response,
%% Accept-Encoding provides information about which content codings are
%% preferred in the content of a subsequent request to the same resource.
%%
%% Set or add gzip
set_accept_encoding_response(Config) ->
	doc("Header accept-encoding must be set on valid response command."),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"gzip">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

set_accept_encoding_header(Config) ->
	doc("Header accept-encoding must be set on valid header command."),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/header",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"gzip">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

add_accept_encoding_header_valid(Config) ->
	doc("Header accept-encoding must be added on valid accept-encoding."),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/accept-identity",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"identity, gzip">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

override_accept_encoding_header_invalid(Config) ->
	doc("Header accept-encoding must override invalid accept-encoding."),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/invalid-header",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"gzip">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

%% RFC 9110. Section 12.5.3. 10.3. If the representation's content coding is
%% one of the content codings listed in the Accept-Encoding field value, then
%% it is acceptable unless it is accompanied by a qvalue of 0.
%%
%% gzip must not have a qvalue of 0 when the handler is used. Set to 1.
override_accept_encoding_excluded(Config) ->
	doc("Header accept-encoding must override when explicitly excluded."),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/reject-explicit-header",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"identity;q=1, gzip;q=1">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

%% RFC 9110. Section 12.5.3. 10.2. If the representation has no content coding,
%% then it is acceptable by default unless specifically excluded by the
%% Accept-Encoding field stating either "identity;q=0" or "*;q=0" wihout a more
%% specific entry for "identity".
%%
%% *;q=0 will reject codings that are not listed. Specific entry gzip must
%% always be listed when the handler is used. Add gzip.
add_accept_encoding_excluded(Config) ->
	doc("Header accept-encoding must added when implicitly excluded."),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/reject-implicit-header",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"gzip;q=1, identity;q=1, *;q=0">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

no_override_accept_coding_set_explicit(Config) ->
	doc("Confirm that accept-encoding is not overridden when explicitly set."),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/accept-explicit-header",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"identity, gzip;q=0.5">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

no_override_accept_coding_set_implicit(Config) ->
	doc("Confirm that accept-encoding is not overridden when implicitly set."),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/accept-implicit-header",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"identity, *;q=0.5">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

%% RFC 9110. Section 12.5.3. 10.1. If no Accept-Encoding header field is in the
%% request, any content coding is considered acceptable by the user agent.
%%
%% Don't add anything on error or when that handler is not used.
no_set_accept_encoding(Config) ->
	doc("No header accept-encoding on invalid responses."),
	Body = <<"test">>,
	{400, Headers, _} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	false = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

no_set_accept_encoding_ignore(Config) ->
	doc("Confirm that no accept-encoding is set when stream is ignored."),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Body} = do_post("/echo/decompress_ignore",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	false = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.
