%% Copyright (c) 2024, jdamanalo <joshuadavid.agustin@manalo.ph>
%% Copyright (c) 2024, Loïc Hoguin <essen@ninenines.eu>
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
		{"/echo/:what", decompress_h, echo},
		{"/test/:what", decompress_h, test}
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
	doc("Requests without content-encoding are processed normally."),
	Body = <<"test">>,
	{200, _, Body} = do_post("/echo/normal", [], Body, Config),
	%% The content-encoding header would be propagated,
	%% but there was no content-encoding header to propagate.
	{200, _, <<"undefined">>} = do_post("/test/content-encoding", [], Body, Config),
	%% The content_decoded list is empty.
	{200, _, <<"[]">>} = do_post("/test/content-decoded", [], Body, Config),
	ok.

content_encoding_malformed(Config) ->
	doc("Requests with a malformed content-encoding are processed "
		"as if no content-encoding was sent."),
	Body = <<"test">>,
	{200, _, Body} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<";">>}], Body, Config),
	%% The content-encoding header is propagated.
	{200, _, <<";">>} = do_post("/test/content-encoding",
		[{<<"content-encoding">>, <<";">>}], Body, Config),
	%% The content_decoded list is empty.
	{200, _, <<"[]">>} = do_post("/test/content-decoded",
		[{<<"content-encoding">>, <<";">>}], Body, Config),
	ok.

content_encoding_not_supported(Config) ->
	doc("Requests with an unsupported content-encoding are processed "
		"as if no content-encoding was sent."),
	Body = <<"test">>,
	{200, _, Body} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<"compress">>}], Body, Config),
	%% The content-encoding header is propagated.
	{200, _, <<"compress">>} = do_post("/test/content-encoding",
		[{<<"content-encoding">>, <<"compress">>}], Body, Config),
	%% The content_decoded list is empty.
	{200, _, <<"[]">>} = do_post("/test/content-decoded",
		[{<<"content-encoding">>, <<"compress">>}], Body, Config),
	ok.

content_encoding_multiple(Config) ->
	doc("Requests with multiple content-encoding values are processed "
		"as if no content-encoding was sent."),
	Body = <<"test">>,
	{200, _, Body} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<"gzip, compress">>}], Body, Config),
	%% The content-encoding header is propagated.
	{200, _, <<"gzip, compress">>} = do_post("/test/content-encoding",
		[{<<"content-encoding">>, <<"gzip, compress">>}], Body, Config),
	%% The content_decoded list is empty.
	{200, _, <<"[]">>} = do_post("/test/content-decoded",
		[{<<"content-encoding">>, <<"gzip, compress">>}], Body, Config),
	ok.

decompress(Config) ->
	doc("Requests with content-encoding set to gzip and gzipped data "
		"are transparently decompressed."),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, _, Data} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	%% The content-encoding header is NOT propagated.
	{200, _, <<"undefined">>} = do_post("/test/content-encoding",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	%% The content_decoded list contains <<"gzip">>.
	{200, _, <<"[<<\"gzip\">>]">>} = do_post("/test/content-decoded",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	ok.

decompress_error(Config) ->
	doc("Requests with content-encoding set to gzip but the data "
		"cannot be decoded are rejected with a 400 Bad Request error."),
	Body = <<"test">>,
	{400, _, _} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	ok.

decompress_stream(Config) ->
	doc("Requests with content-encoding set to gzip and gzipped data "
		"are transparently decompressed, even when the data is streamed."),
	%% Handler read length 1KB. Compressing 3KB should be enough to trigger more.
	Data = crypto:strong_rand_bytes(3000),
	Body = zlib:gzip(Data),
	Size = byte_size(Body),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/normal",
		[{<<"content-encoding">>, <<"gzip">>}]),
	gun:data(ConnPid, Ref, nofin, binary:part(Body, 0, Size div 2)),
	timer:sleep(1000),
	gun:data(ConnPid, Ref, fin, binary:part(Body, Size div 2, Size div 2 + Size rem 2)),
	{response, IsFin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Data} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref);
		fin -> {ok, <<>>}
	end,
	gun:close(ConnPid),
	%% The content-encoding header is NOT propagated.
	ConnPid2 = gun_open(Config),
	Ref2 = gun:post(ConnPid2, "/test/content-encoding",
		[{<<"content-encoding">>, <<"gzip">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid2, Ref2),
	{ok, <<"undefined">>} = gun:await_body(ConnPid2, Ref2),
	gun:close(ConnPid2),
	%% The content_decoded list contains <<"gzip">>.
	ConnPid3 = gun_open(Config),
	Ref3 = gun:post(ConnPid3, "/test/content-decoded",
		[{<<"content-encoding">>, <<"gzip">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid3, Ref3),
	{ok, <<"[<<\"gzip\">>]">>} = gun:await_body(ConnPid3, Ref3),
	gun:close(ConnPid3).

opts_decompress_enabled_false(Config0) ->
	doc("Confirm that the decompress_enabled option can be set."),
	Fun = case config(ref, Config0) of
		HTTPS when HTTPS =:= https_compress; HTTPS =:= https -> init_https;
		H2 when H2 =:= h2_compress; H2 =:= h2 -> init_http2;
		_ -> init_http
	end,
	Config = cowboy_test:Fun(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		stream_handlers => [cowboy_decompress_h, cowboy_stream_h],
		decompress_enabled => false
	}, Config0),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	try
		{200, Headers, Body} = do_post("/echo/normal",
			[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
		%% We do not set accept-encoding when we are disabled.
		false = lists:keyfind(<<"accept-encoding">>, 1, Headers)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

set_options_decompress_enabled_false(Config) ->
	doc("Confirm that the decompress_enabled option can be dynamically "
		"set to false and the data received is not decompressed."),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Body} = do_post("/echo/decompress_disable",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	%% We do not set accept-encoding when we are disabled.
	false = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

set_options_decompress_disable_in_the_middle(Config) ->
	doc("Confirm that setting the decompress_enabled option dynamically "
		"to false after starting to read the body does not disable decompression "
		"and the data received is decompressed."),
	Data = rand:bytes(1000000),
	Body = zlib:gzip(Data),
	%% Since we were not ignoring before starting to read,
	%% we receive the entire body decompressed.
	{200, Headers, Data} = do_post("/test/disable-in-the-middle",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	%% We do set accept-encoding when we are enabled,
	%% even if an attempt to disable in the middle is ignored.
	{_, _} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

set_options_decompress_enable_in_the_middle(Config0) ->
	doc("Confirm that setting the decompress_enabled option dynamically "
		"to true after starting to read the body does not enable decompression "
		"and the data received is not decompressed."),
	Fun = case config(ref, Config0) of
		HTTPS when HTTPS =:= https_compress; HTTPS =:= https -> init_https;
		H2 when H2 =:= h2_compress; H2 =:= h2 -> init_http2;
		_ -> init_http
	end,
	Config = cowboy_test:Fun(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config0))},
		stream_handlers => [cowboy_decompress_h, cowboy_stream_h],
		decompress_enabled => false
	}, Config0),
	Data = rand:bytes(1000000),
	Body = zlib:gzip(Data),
	try
		%% Since we were ignoring before starting to read,
		%% we receive the entire body compressed.
		{200, Headers, Body} = do_post("/test/enable-in-the-middle",
			[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
		%% We do not set accept-encoding when we are disabled,
		%% even if an attempt to enable in the middle is ignored.
		false = lists:keyfind(<<"accept-encoding">>, 1, Headers)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

opts_decompress_ratio_limit(Config0) ->
	doc("Confirm that the decompress_ratio_limit option can be set."),
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
	%% Data must be big enough for compression to be effective,
	%% so that ratio_limit=1 will fail.
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
	%% Data must be big enough for compression to be effective,
	%% so that ratio_limit=1 will fail.
	Data = <<0:800>>,
	Body = zlib:gzip(Data),
	{413, _, _} = do_post("/echo/decompress_ratio_limit",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	ok.

gzip_bomb(Config) ->
	doc("Confirm that requests are rejected with a 413 Payload Too Large "
		"error when the ratio limit is exceeded."),
	Body = create_gzip_bomb(),
	{413, _, _} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	ok.

set_accept_encoding_response(Config) ->
	doc("Header accept-encoding must be set on valid response command. "
		"(RFC9110 12.5.3)"),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/echo/normal",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"gzip">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

set_accept_encoding_header(Config) ->
	doc("Header accept-encoding must be set on valid header command. "
		"(RFC9110 12.5.3)"),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/test/header-command",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"gzip">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

add_accept_encoding_header_valid(Config) ->
	doc("Supported content codings must be added to the accept-encoding "
		"header if it already exists. (RFC9110 12.5.3)"),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/test/accept-identity",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"identity, gzip">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

override_accept_encoding_header_invalid(Config) ->
	doc("When the stream handler cannot parse the accept-encoding header "
		"found in the response, it overrides it."),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/test/invalid-header",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"gzip">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

override_accept_encoding_excluded(Config) ->
	doc("The stream handler must ensure that the content encodings "
		"it supports are not marked as unsupported in response headers. "
		"The stream handler enables gzip when explicitly excluded. "
		"(RFC9110 12.5.3)"),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/test/reject-explicit-header",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"identity;q=1, gzip;q=1">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

%% *;q=0 will reject codings that are not listed. Supported codings
%% must always be enabled when the handler is used.
add_accept_encoding_excluded(Config) ->
	doc("The stream handler must ensure that the content encodings "
		"it supports are not marked as unsupported in response headers. "
		"The stream handler enables gzip when implicitly excluded (*;q=0). "
		"(RFC9110 12.5.3)"),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/test/reject-implicit-header",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"gzip;q=1, identity;q=1, *;q=0">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

no_override_accept_coding_set_explicit(Config) ->
	doc("Confirm that accept-encoding is not overridden when the "
		"content encodings it supports are explicitly set. "
		"(RFC9110 12.5.3)"),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/test/accept-explicit-header",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"identity, gzip;q=0.5">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.

no_override_accept_coding_set_implicit(Config) ->
	doc("Confirm that accept-encoding is not overridden when the "
		"content encodings it supports are implicitly set. "
		"(RFC9110 12.5.3)"),
	Data = <<"test">>,
	Body = zlib:gzip(Data),
	{200, Headers, Data} = do_post("/test/accept-implicit-header",
		[{<<"content-encoding">>, <<"gzip">>}], Body, Config),
	{_, <<"identity, *;q=0.5">>} = lists:keyfind(<<"accept-encoding">>, 1, Headers),
	ok.
