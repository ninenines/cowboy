%% Copyright (c) 2016-2024, Loïc Hoguin <essen@ninenines.eu>
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

-module(req_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

%% ct.

suite() ->
	Timeout = case os:type() of
		{win32, _} -> 120000;
		_ -> 30000
	end,
	[{timetrap, Timeout}].

all() ->
	cowboy_test:common_all().

groups() ->
	cowboy_test:common_groups(ct_helper:all(?MODULE)).

init_per_suite(Config) ->
	ct_helper:create_static_dir(config(priv_dir, Config) ++ "/static"),
	Config.

end_per_suite(Config) ->
	ct_helper:delete_static_dir(config(priv_dir, Config) ++ "/static").

init_per_group(Name, Config) ->
	cowboy_test:init_common_groups(Name, Config, ?MODULE).

end_per_group(Name, _) ->
	cowboy_test:stop_group(Name).

%% Routes.

init_dispatch(Config) ->
	cowboy_router:compile([{"[...]", [
		{"/static/[...]", cowboy_static, {dir, config(priv_dir, Config) ++ "/static"}},
		%% @todo Seriously InitialState should be optional.
		{"/resp/:key[/:arg]", resp_h, []},
		{"/multipart[/:key]", multipart_h, []},
		{"/args/:key/:arg[/:default]", echo_h, []},
		{"/crash/:key/period", echo_h,
			#{length => 999999999, period => 1000, timeout => 5000, crash => true}},
		{"/no-opts/:key", echo_h, #{crash => true}},
		{"/opts/:key/length", echo_h, #{length => 1000}},
		{"/opts/:key/period", echo_h, #{length => 999999999, period => 2000}},
		{"/opts/:key/timeout", echo_h, #{timeout => 1000, crash => true}},
		{"/100-continue/:key", echo_h, []},
		{"/full/:key", echo_h, []},
		{"/auto-sync/:key", echo_h, []},
		{"/auto-async/:key", echo_h, []},
		{"/spawn/:key", echo_h, []},
		{"/no/:key", echo_h, []},
		{"/direct/:key/[...]", echo_h, []},
		{"/:key/[...]", echo_h, []}
	]}]).

%% Internal.

do_body(Method, Path, Config) ->
	do_body(Method, Path, [], Config).

do_body(Method, Path, Headers, Config) ->
	do_body(Method, Path, Headers, <<>>, Config).

do_body(Method, Path, Headers0, Body, Config) ->
	ConnPid = gun_open(Config),
	Headers = [{<<"accept-encoding">>, <<"gzip">>}|Headers0],
	Ref = gun:request(ConnPid, Method, Path, Headers, Body),
	{response, IsFin, 200, RespHeaders} = gun:await(ConnPid, Ref, infinity),
	{ok, RespBody} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref, infinity);
		fin -> {ok, <<>>}
	end,
	gun:close(ConnPid),
	do_decode(RespHeaders, RespBody).

do_body_error(Method, Path, Headers0, Body, Config) ->
	ConnPid = gun_open(Config),
	Headers = [{<<"accept-encoding">>, <<"gzip">>}|Headers0],
	Ref = gun:request(ConnPid, Method, Path, Headers, Body),
	{response, _, Status, RespHeaders} = gun:await(ConnPid, Ref, infinity),
	gun:close(ConnPid),
	{Status, RespHeaders}.

do_get(Path, Config) ->
	do_get(Path, [], Config).

do_get(Path, Headers, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, [{<<"accept-encoding">>, <<"gzip">>}|Headers]),
	case gun:await(ConnPid, Ref, infinity) of
		{response, IsFin, Status, RespHeaders} ->
			{ok, RespBody} = case IsFin of
				nofin -> gun:await_body(ConnPid, Ref, infinity);
				fin -> {ok, <<>>}
			end,
			gun:close(ConnPid),
			{Status, RespHeaders, do_decode(RespHeaders, RespBody)};
		{error, {stream_error, Error}} ->
			Error
	end.

do_get_body(Path, Config) ->
	do_get_body(Path, [], Config).

do_get_body(Path, Headers, Config) ->
	do_body("GET", Path, Headers, Config).

do_get_inform(Path, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, [{<<"accept-encoding">>, <<"gzip">>}]),
	case gun:await(ConnPid, Ref, infinity) of
		{response, _, RespStatus, RespHeaders} ->
			%% We don't care about the body.
			gun:close(ConnPid),
			{RespStatus, RespHeaders};
		{inform, InfoStatus, InfoHeaders} ->
			{response, IsFin, RespStatus, RespHeaders}
				= case gun:await(ConnPid, Ref, infinity) of
					{inform, InfoStatus, InfoHeaders} ->
						gun:await(ConnPid, Ref, infinity);
					Response ->
						Response
			end,
			{ok, RespBody} = case IsFin of
				nofin -> gun:await_body(ConnPid, Ref, infinity);
				fin -> {ok, <<>>}
			end,
			gun:close(ConnPid),
			{InfoStatus, InfoHeaders, RespStatus, RespHeaders, do_decode(RespHeaders, RespBody)};
		{error, {stream_error, Error}} ->
			Error
	end.

do_decode(Headers, Body) ->
	case lists:keyfind(<<"content-encoding">>, 1, Headers) of
		{_, <<"gzip">>} -> zlib:gunzip(Body);
		_ -> Body
	end.

do_get_error(Path, Config) ->
	do_get_error(Path, [], Config).

do_get_error(Path, Headers, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, [{<<"accept-encoding">>, <<"gzip">>}|Headers]),
	{response, IsFin, Status, RespHeaders} = gun:await(ConnPid, Ref, infinity),
	Result = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref, infinity);
		fin -> {ok, <<>>}
	end,
	case Result of
		{ok, RespBody} -> {Status, RespHeaders, do_decode(RespHeaders, RespBody)};
		_ -> Result
	end.

%% Tests: Request.

binding(Config) ->
	doc("Value bound from request URI path with/without default."),
	<<"binding">> = do_get_body("/args/binding/key", Config),
	<<"binding">> = do_get_body("/args/binding/key/default", Config),
	<<"default">> = do_get_body("/args/binding/undefined/default", Config),
	ok.

bindings(Config) ->
	doc("Values bound from request URI path."),
	<<"#{key => <<\"bindings\">>}">> = do_get_body("/bindings", Config),
	ok.

cert(Config) ->
	case config(type, Config) of
		tcp -> doc("TLS certificates can only be provided over TLS.");
		ssl -> do_cert(Config);
		quic -> do_cert(Config)
	end.

do_cert(Config) ->
	doc("A client TLS certificate was provided."),
	Cert = do_get_body("/cert", Config),
	Cert = do_get_body("/direct/cert", Config),
	ok.

cert_undefined(Config) ->
	doc("No client TLS certificate was provided."),
	<<"undefined">> = do_get_body("/cert", [{no_cert, true}|Config]),
	<<"undefined">> = do_get_body("/direct/cert", [{no_cert, true}|Config]),
	ok.

header(Config) ->
	doc("Request header with/without default."),
	<<"value">> = do_get_body("/args/header/defined", [{<<"defined">>, "value"}], Config),
	<<"value">> = do_get_body("/args/header/defined/default", [{<<"defined">>, "value"}], Config),
	<<"default">> = do_get_body("/args/header/undefined/default", [{<<"defined">>, "value"}], Config),
	ok.

headers(Config) ->
	doc("Request headers."),
	do_headers("/headers", Config),
	do_headers("/direct/headers", Config).

do_headers(Path, Config) ->
	%% We always send accept-encoding with this test suite's requests.
	<<"#{<<\"accept-encoding\">> => <<\"gzip\">>,"
		"<<\"content-length\">> => <<\"0\">>,"
		"<<\"header\">> => <<\"value\">>", _/bits>>
		= do_get_body(Path, [{<<"header">>, "value"}], Config),
	ok.

host(Config) ->
	doc("Request URI host."),
	<<"localhost">> = do_get_body("/host", Config),
	<<"localhost">> = do_get_body("/direct/host", Config),
	ok.

host_info(Config) ->
	doc("Request host_info."),
	<<"[<<\"localhost\">>]">> = do_get_body("/host_info", Config),
	ok.

%% @todo Actually write the related unit tests.
match_cookies(Config) ->
	doc("Matched request cookies."),
	<<"#{}">> = do_get_body("/match/cookies", [{<<"cookie">>, "a=b; c=d"}], Config),
	<<"#{a => <<\"b\">>}">> = do_get_body("/match/cookies/a", [{<<"cookie">>, "a=b; c=d"}], Config),
	<<"#{c => <<\"d\">>}">> = do_get_body("/match/cookies/c", [{<<"cookie">>, "a=b; c=d"}], Config),
	case do_get_body("/match/cookies/a/c", [{<<"cookie">>, "a=b; c=d"}], Config) of
		<<"#{a => <<\"b\">>,c => <<\"d\">>}">> -> ok;
		<<"#{c => <<\"d\">>,a => <<\"b\">>}">> -> ok
	end,
	%% Ensure match errors result in a 400 response.
	{400, _, _} = do_get("/match/cookies/a/c",
		[{<<"cookie">>, "a=b"}], Config),
	%% This function is tested more extensively through unit tests.
	ok.

%% @todo Actually write the related unit tests.
match_qs(Config) ->
	doc("Matched request URI query string."),
	<<"#{}">> = do_get_body("/match/qs?a=b&c=d", Config),
	<<"#{a => <<\"b\">>}">> = do_get_body("/match/qs/a?a=b&c=d", Config),
	<<"#{c => <<\"d\">>}">> = do_get_body("/match/qs/c?a=b&c=d", Config),
	case do_get_body("/match/qs/a/c?a=b&c=d", Config) of
		<<"#{a => <<\"b\">>,c => <<\"d\">>}">> -> ok;
		<<"#{c => <<\"d\">>,a => <<\"b\">>}">> -> ok
	end,
	case do_get_body("/match/qs/a/c?a=b&c", Config) of
		<<"#{a => <<\"b\">>,c => true}">> -> ok;
		<<"#{c => true,a => <<\"b\">>}">> -> ok
	end,
	case do_get_body("/match/qs/a/c?a&c=d", Config) of
		<<"#{a => true,c => <<\"d\">>}">> -> ok;
		<<"#{c => <<\"d\">>,a => true}">> -> ok
	end,
	%% Ensure match errors result in a 400 response.
	{400, _, _} = do_get("/match/qs/a/c?a=b", [], Config),
	{400, _, _} = do_get("/match/qs_with_constraints", [], Config),
	%% This function is tested more extensively through unit tests.
	ok.

method(Config) ->
	doc("Request method."),
	do_method("/method", Config),
	do_method("/direct/method", Config).

do_method(Path, Config) ->
	<<"GET">> = do_body("GET", Path, Config),
	<<>> = do_body("HEAD", Path, Config),
	<<"OPTIONS">> = do_body("OPTIONS", Path, Config),
	<<"PATCH">> = do_body("PATCH", Path, Config),
	<<"POST">> = do_body("POST", Path, Config),
	<<"PUT">> = do_body("PUT", Path, Config),
	<<"ZZZZZZZZ">> = do_body("ZZZZZZZZ", Path, Config),
	ok.

parse_cookies(Config) ->
	doc("Request cookies."),
	<<"[]">> = do_get_body("/parse_cookies", Config),
	<<"[{<<\"cake\">>,<<\"strawberry\">>}]">>
		= do_get_body("/parse_cookies", [{<<"cookie">>, "cake=strawberry"}], Config),
	<<"[{<<\"cake\">>,<<\"strawberry\">>},{<<\"color\">>,<<\"blue\">>}]">>
		= do_get_body("/parse_cookies", [{<<"cookie">>, "cake=strawberry; color=blue"}], Config),
	<<"[{<<\"cake\">>,<<\"strawberry\">>},{<<\"color\">>,<<\"blue\">>}]">>
		= do_get_body("/parse_cookies",
			[{<<"cookie">>, "cake=strawberry"}, {<<"cookie">>, "color=blue"}], Config),
	%% Ensure parse errors result in a 400 response.
	{400, _, _} = do_get("/parse_cookies",
		[{<<"cookie">>, "bad\tname=strawberry"}], Config),
	{400, _, _} = do_get("/parse_cookies",
		[{<<"cookie">>, "goodname=strawberry\tmilkshake"}], Config),
	ok.

filter_then_parse_cookies(Config) ->
	doc("Filter cookies then parse them."),
	<<"[]">> = do_get_body("/filter_then_parse_cookies", Config),
	<<"[{<<\"cake\">>,<<\"strawberry\">>}]">>
		= do_get_body("/filter_then_parse_cookies", [{<<"cookie">>, "cake=strawberry"}], Config),
	<<"[{<<\"cake\">>,<<\"strawberry\">>},{<<\"color\">>,<<\"blue\">>}]">>
		= do_get_body("/filter_then_parse_cookies", [{<<"cookie">>, "cake=strawberry; color=blue"}], Config),
	<<"[{<<\"cake\">>,<<\"strawberry\">>},{<<\"color\">>,<<\"blue\">>}]">>
		= do_get_body("/filter_then_parse_cookies",
			[{<<"cookie">>, "cake=strawberry"}, {<<"cookie">>, "color=blue"}], Config),
	<<"[]">>
		= do_get_body("/filter_then_parse_cookies",
			[{<<"cookie">>, "bad name=strawberry"}], Config),
	<<"[{<<\"cake\">>,<<\"strawberry\">>}]">>
		= do_get_body("/filter_then_parse_cookies",
			[{<<"cookie">>, "bad name=strawberry; cake=strawberry"}], Config),
	<<"[]">>
		= do_get_body("/filter_then_parse_cookies",
			[{<<"cookie">>, "Blocked by http://www.example.com/upgrade-to-remove"}], Config),
	ok.

parse_header(Config) ->
	doc("Parsed request header with/without default."),
	<<"[{{<<\"text\">>,<<\"html\">>,[]},1000,[]}]">>
		= do_get_body("/args/parse_header/accept", [{<<"accept">>, "text/html"}], Config),
	<<"[{{<<\"text\">>,<<\"html\">>,[]},1000,[]}]">>
		= do_get_body("/args/parse_header/accept/default", [{<<"accept">>, "text/html"}], Config),
	%% Header not in request but with default defined by Cowboy.
	<<"0">> = do_get_body("/args/parse_header/content-length", Config),
	%% Header not in request and no default from Cowboy.
	<<"undefined">> = do_get_body("/args/parse_header/upgrade", Config),
	%% Header in request and with default provided.
	<<"100-continue">> = do_get_body("/args/parse_header/expect/100-continue", Config),
	%% Ensure parse errors result in a 400 response.
	{400, _, _} = do_get("/args/parse_header/accept",
		[{<<"accept">>, "bad media type"}], Config),
	ok.

parse_qs(Config) ->
	doc("Parsed request URI query string."),
	<<"[]">> = do_get_body("/parse_qs", Config),
	<<"[{<<\"abc\">>,true}]">> = do_get_body("/parse_qs?abc", Config),
	<<"[{<<\"a\">>,<<\"b\">>},{<<\"c\">>,<<\"d e\">>}]">> = do_get_body("/parse_qs?a=b&c=d+e", Config),
	%% Ensure parse errors result in a 400 response.
	{400, _, _} = do_get("/parse_qs?%%%%%%%", Config),
	ok.

path(Config) ->
	doc("Request URI path."),
	do_path("/path", Config),
	do_path("/direct/path", Config).

do_path(Path0, Config) ->
	Path = list_to_binary(Path0 ++ "/to/the/resource"),
	Path = do_get_body(Path, Config),
	Path = do_get_body([Path, "?query"], Config),
	Path = do_get_body([Path, "?query#fragment"], Config),
	Path = do_get_body([Path, "#fragment"], Config),
	ok.

path_info(Config) ->
	doc("Request path_info."),
	<<"undefined">> = do_get_body("/no/path_info", Config),
	<<"[]">> = do_get_body("/path_info", Config),
	<<"[]">> = do_get_body("/path_info/", Config),
	<<"[<<\"to\">>,<<\"the\">>,<<\"resource\">>]">> = do_get_body("/path_info/to/the/resource", Config),
	<<"[<<\"to\">>,<<\"the\">>,<<\"resource\">>]">> = do_get_body("/path_info/to/the/resource?query", Config),
	<<"[<<\"to\">>,<<\"the\">>,<<\"resource\">>]">> = do_get_body("/path_info/to/the/resource?query#fragment", Config),
	<<"[<<\"to\">>,<<\"the\">>,<<\"resource\">>]">> = do_get_body("/path_info/to/the/resource#fragment", Config),
	ok.

peer(Config) ->
	doc("Remote socket address."),
	<<"{{127,0,0,1},", _/bits >> = do_get_body("/peer", Config),
	<<"{{127,0,0,1},", _/bits >> = do_get_body("/direct/peer", Config),
	ok.

port(Config) ->
	doc("Request URI port."),
	Port = integer_to_binary(config(port, Config)),
	Port = do_get_body("/port", Config),
	Port = do_get_body("/direct/port", Config),
	ExpectedPort = case config(type, Config) of
		tcp -> <<"80">>;
		ssl -> <<"443">>;
		quic -> <<"443">>
	end,
	ExpectedPort = do_get_body("/port", [{<<"host">>, <<"localhost">>}], Config),
	ExpectedPort = do_get_body("/direct/port", [{<<"host">>, <<"localhost">>}], Config),
	ok.

qs(Config) ->
	doc("Request URI query string."),
	do_qs("/qs", Config),
	do_qs("/direct/qs", Config).

do_qs(Path, Config) ->
	<<>> = do_get_body(Path, Config),
	<<"abc">> = do_get_body(Path ++ "?abc", Config),
	<<"a=b&c=d+e">> = do_get_body(Path ++ "?a=b&c=d+e", Config),
	ok.

scheme(Config) ->
	doc("Request URI scheme."),
	do_scheme("/scheme", Config),
	do_scheme("/direct/scheme", Config).

do_scheme(Path, Config) ->
	Transport = config(type, Config),
	case do_get_body(Path, Config) of
		<<"http">> when Transport =:= tcp -> ok;
		<<"https">> when Transport =:= ssl -> ok;
		<<"https">> when Transport =:= quic -> ok
	end.

sock(Config) ->
	doc("Local socket address."),
	<<"{{127,0,0,1},", _/bits >> = do_get_body("/sock", Config),
	<<"{{127,0,0,1},", _/bits >> = do_get_body("/direct/sock", Config),
	ok.

uri(Config) ->
	doc("Request URI building/modification."),
	Scheme = case config(type, Config) of
		tcp -> <<"http">>;
		ssl -> <<"https">>;
		quic -> <<"https">>
	end,
	SLen = byte_size(Scheme),
	Port = integer_to_binary(config(port, Config)),
	PLen = byte_size(Port),
	%% Absolute form.
	<< Scheme:SLen/binary, "://localhost:", Port:PLen/binary, "/uri?qs" >>
		= do_get_body("/uri?qs", Config),
	%% Origin form.
	<< "/uri/origin?qs" >> = do_get_body("/uri/origin?qs", Config),
	%% Protocol relative.
	<< "//localhost:", Port:PLen/binary, "/uri/protocol-relative?qs" >>
		= do_get_body("/uri/protocol-relative?qs", Config),
	%% No query string.
	<< Scheme:SLen/binary, "://localhost:", Port:PLen/binary, "/uri/no-qs" >>
		= do_get_body("/uri/no-qs?qs", Config),
	%% No path or query string.
	<< Scheme:SLen/binary, "://localhost:", Port:PLen/binary >>
		= do_get_body("/uri/no-path?qs", Config),
	%% Changed port.
	<< Scheme:SLen/binary, "://localhost:123/uri/set-port?qs" >>
		= do_get_body("/uri/set-port?qs", Config),
	%% This function is tested more extensively through unit tests.
	ok.

version(Config) ->
	doc("Request HTTP version."),
	do_version("/version", Config),
	do_version("/direct/version", Config).

do_version(Path, Config) ->
	Protocol = config(protocol, Config),
	case do_get_body(Path, Config) of
		<<"HTTP/1.1">> when Protocol =:= http -> ok;
		<<"HTTP/2">> when Protocol =:= http2 -> ok;
		<<"HTTP/3">> when Protocol =:= http3 -> ok
	end.

%% Tests: Request body.

body_length(Config) ->
	doc("Request body length."),
	<<"0">> = do_get_body("/body_length", Config),
	<<"12">> = do_body("POST", "/body_length", [], "hello world!", Config),
	ok.

has_body(Config) ->
	doc("Has a request body?"),
	<<"false">> = do_get_body("/has_body", Config),
	<<"true">> = do_body("POST", "/has_body", [], "hello world!", Config),
	ok.

read_body(Config) ->
	doc("Request body."),
	<<>> = do_get_body("/read_body", Config),
	<<"hello world!">> = do_body("POST", "/read_body", [], "hello world!", Config),
	%% We expect to have read *at least* 1000 bytes.
	<<0:8000, _/bits>> = do_body("POST", "/opts/read_body/length", [], <<0:8000000>>, Config),
	%% The timeout value is set too low on purpose to ensure a crash occurs.
	ok = do_read_body_timeout("/opts/read_body/timeout", <<0:8000000>>, Config),
	%% 10MB body larger than default length.
	<<0:80000000>> = do_body("POST", "/full/read_body", [], <<0:80000000>>, Config),
	ok.

read_body_mtu(Config) ->
	case os:type() of
		{win32, _} ->
			{skip, "Loopback MTU size is 0xFFFFFFFF on Windows."};
		{unix, _} ->
			doc("Request body whose sizes are around the MTU."),
			MTU = ct_helper:get_loopback_mtu(),
			_ = [begin
				Body = <<0:Size/unit:8>>,
				Body = do_body("POST", "/full/read_body", [], Body, Config)
			end || Size <- lists:seq(MTU - 10, MTU + 10)],
			ok
	end.

read_body_period(Config) ->
	doc("Read the request body for at most 2 seconds."),
	ConnPid = gun_open(Config),
	Body = <<0:8000000>>,
	Ref = gun:headers(ConnPid, "POST", "/opts/read_body/period", [
		{<<"content-length">>, integer_to_binary(byte_size(Body) * 2)}
	]),
	%% The body is sent without fin. The server will read what it can
	%% for 2 seconds. The test succeeds if we get some of the data back
	%% (meaning the function will have returned after the period ends).
	gun:data(ConnPid, Ref, nofin, Body),
	Response = gun:await(ConnPid, Ref, infinity),
	case Response of
		{response, nofin, 200, _} ->
			{data, _, Data} = gun:await(ConnPid, Ref, infinity),
			%% We expect to read at least some data.
			true = Data =/= <<>>,
			gun:close(ConnPid);
		%% We got a crash, likely because the environment
		%% was overloaded and the timeout triggered. Try again.
		{response, _, 500, _} ->
			gun:close(ConnPid),
			read_body_period(Config)
	end.

%% We expect a crash.
do_read_body_timeout(Path, Body, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:headers(ConnPid, "POST", Path, [
		{<<"content-length">>, integer_to_binary(byte_size(Body))}
	]),
	case gun:await(ConnPid, Ref, infinity) of
		{response, _, 500, _} ->
			ok;
		%% See do_maybe_h3_error comment for details.
		{error, {stream_error, {stream_error, h3_internal_error, _}}} ->
			ok
	end,
	gun:close(ConnPid).

read_body_auto(Config) ->
	doc("Read the request body using auto mode."),
	<<0:80000000>> = do_body("POST", "/auto-sync/read_body", [], <<0:80000000>>, Config),
	<<0:80000000>> = do_body("POST", "/auto-async/read_body", [], <<0:80000000>>, Config),
	ok.

read_body_spawn(Config) ->
	doc("Confirm we can use cowboy_req:read_body/1,2 from another process."),
	<<"hello world!">> = do_body("POST", "/spawn/read_body", [], "hello world!", Config),
	ok.

read_body_expect_100_continue(Config) ->
	doc("Request body with a 100-continue expect header."),
	do_read_body_expect_100_continue("/read_body", Config).

read_body_expect_100_continue_user_sent(Config) ->
	doc("Request body with a 100-continue expect header, 100 response sent by handler."),
	do_read_body_expect_100_continue("/100-continue/read_body", Config).

do_read_body_expect_100_continue(Path, Config) ->
	ConnPid = gun_open(Config),
	Body = <<0:8000000>>,
	Headers = [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"expect">>, <<"100-continue">>},
		{<<"content-length">>, integer_to_binary(byte_size(Body))}
	],
	Ref = gun:post(ConnPid, Path, Headers),
	{inform, 100, []} = gun:await(ConnPid, Ref, infinity),
	gun:data(ConnPid, Ref, fin, Body),
	{response, IsFin, 200, RespHeaders} = gun:await(ConnPid, Ref, infinity),
	{ok, RespBody} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref, infinity);
		fin -> {ok, <<>>}
	end,
	gun:close(ConnPid),
	do_decode(RespHeaders, RespBody),
	ok.

read_urlencoded_body(Config) ->
	doc("application/x-www-form-urlencoded request body."),
	<<"[]">> = do_body("POST", "/read_urlencoded_body", [], <<>>, Config),
	<<"[{<<\"abc\">>,true}]">> = do_body("POST", "/read_urlencoded_body", [], "abc", Config),
	<<"[{<<\"a\">>,<<\"b\">>},{<<\"c\">>,<<\"d e\">>}]">>
		= do_body("POST", "/read_urlencoded_body", [], "a=b&c=d+e", Config),
	%% The timeout value is set too low on purpose to ensure a crash occurs.
	ok = do_read_body_timeout("/opts/read_urlencoded_body/timeout", <<"abc">>, Config),
	%% Ensure parse errors result in a 400 response.
	{400, _} = do_body_error("POST", "/read_urlencoded_body", [], "%%%%%", Config),
	ok.

read_urlencoded_body_too_large(Config) ->
	doc("application/x-www-form-urlencoded request body too large. "
		"Send a 10MB body, larger than the default length, to ensure a crash occurs."),
	do_read_urlencoded_body_too_large("/no-opts/read_urlencoded_body",
		string:chars($a, 10000000), Config).

%% We expect a crash.
do_read_urlencoded_body_too_large(Path, Body, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:headers(ConnPid, "POST", Path, [
		{<<"content-length">>, integer_to_binary(iolist_size(Body))}
	]),
	gun:data(ConnPid, Ref, fin, Body),
	Response = gun:await(ConnPid, Ref, infinity),
	gun:close(ConnPid),
	case Response of
		{response, _, 413, _} ->
			ok;
		%% We got the wrong crash, likely because the environment
		%% was overloaded and the timeout triggered. Try again.
		{response, _, 408, _} ->
			do_read_urlencoded_body_too_large(Path, Body, Config);
		%% Timing issues make it possible for the connection to be
		%% closed before the data went through. We retry.
		{error, {stream_error, {closed, {error,closed}}}} ->
			do_read_urlencoded_body_too_large(Path, Body, Config)
	end.

read_urlencoded_body_too_long(Config) ->
	doc("application/x-www-form-urlencoded request body sent too slow. "
		"The body is simply not being sent fully. It is read by the handler "
		"for at most 1 second. A crash occurs because we don't have the full body."),
	do_read_urlencoded_body_too_long("/crash/read_urlencoded_body/period", <<"abc">>, Config).

%% We expect a crash.
do_read_urlencoded_body_too_long(Path, Body, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:headers(ConnPid, "POST", Path, [
		{<<"content-length">>, integer_to_binary(byte_size(Body) * 2)}
	]),
	gun:data(ConnPid, Ref, nofin, Body),
	Protocol = config(protocol, Config),
	case gun:await(ConnPid, Ref, infinity) of
		{response, _, 408, RespHeaders} when Protocol =:= http ->
			%% 408 error responses should close HTTP/1.1 connections.
			{_, <<"close">>} = lists:keyfind(<<"connection">>, 1, RespHeaders),
			gun:close(ConnPid);
		{response, _, 408, _} when Protocol =:= http2; Protocol =:= http3 ->
			gun:close(ConnPid);
		%% We must have hit the timeout due to busy CI environment. Retry.
		{response, _, 500, _} ->
			gun:close(ConnPid),
			do_read_urlencoded_body_too_long(Path, Body, Config)
	end.

read_and_match_urlencoded_body(Config) ->
	doc("Read and match an application/x-www-form-urlencoded request body."),
	<<"#{}">> = do_body("POST", "/match/body_qs", [], "a=b&c=d", Config),
	<<"#{a => <<\"b\">>}">> = do_body("POST", "/match/body_qs/a", [], "a=b&c=d", Config),
	<<"#{c => <<\"d\">>}">> = do_body("POST", "/match/body_qs/c", [], "a=b&c=d", Config),
	case do_body("POST", "/match/body_qs/a/c", [], "a=b&c=d", Config) of
		<<"#{a => <<\"b\">>,c => <<\"d\">>}">> -> ok;
		<<"#{c => <<\"d\">>,a => <<\"b\">>}">> -> ok
	end,
	case do_body("POST", "/match/body_qs/a/c", [], "a=b&c", Config) of
		<<"#{a => <<\"b\">>,c => true}">> -> ok;
		<<"#{c => true,a => <<\"b\">>}">> -> ok
	end,
	case do_body("POST", "/match/body_qs/a/c", [], "a&c=d", Config) of
		<<"#{a => true,c => <<\"d\">>}">> -> ok;
		<<"#{c => <<\"d\">>,a => true}">> -> ok
	end,
	%% Ensure match errors result in a 400 response.
	{400, _} = do_body_error("POST", "/match/body_qs/a/c", [], "a=b", Config),
	%% Ensure parse errors result in a 400 response.
	{400, _} = do_body_error("POST", "/match/body_qs", [], "%%%%%", Config),
	%% The timeout value is set too low on purpose to ensure a crash occurs.
	ok = do_read_body_timeout("/opts/read_and_match_urlencoded_body/timeout", <<"abc">>, Config),
	ok.

read_and_match_urlencoded_body_too_large(Config) ->
	doc("Read and match an application/x-www-form-urlencoded request body too large. "
		"Send a 10MB body, larger than the default length, to ensure a crash occurs."),
	do_read_urlencoded_body_too_large(
		"/no-opts/read_and_match_urlencoded_body",
		string:chars($a, 10000000), Config).

read_and_match_urlencoded_body_too_long(Config) ->
	doc("Read and match an application/x-www-form-urlencoded request body sent too slow. "
		"The body is simply not being sent fully. It is read by the handler "
		"for at most 1 second. A crash occurs because we don't have the full body."),
	do_read_urlencoded_body_too_long(
		"/crash/read_and_match_urlencoded_body/period", <<"abc">>, Config).

multipart(Config) ->
	doc("Multipart request body."),
	do_multipart("/multipart", Config).

do_multipart(Path, Config) ->
	LargeBody = iolist_to_binary(string:chars($a, 10000000)),
	ReqBody = [
		"--deadbeef\r\nContent-Type: text/plain\r\n\r\nCowboy is an HTTP server.\r\n"
		"--deadbeef\r\nContent-Type: application/octet-stream\r\nX-Custom: value\r\n\r\n", LargeBody, "\r\n"
		"--deadbeef--"
	],
	RespBody = do_body("POST", Path, [
		{<<"content-type">>, <<"multipart/mixed; boundary=deadbeef">>}
	], ReqBody, Config),
	[
		{#{<<"content-type">> := <<"text/plain">>}, <<"Cowboy is an HTTP server.">>},
		{LargeHeaders, LargeBody}
	] = binary_to_term(RespBody),
	#{
		<<"content-type">> := <<"application/octet-stream">>,
		<<"x-custom">> := <<"value">>
	} = LargeHeaders,
	ok.

multipart_error_empty(Config) ->
	doc("Multipart request body is empty."),
	%% We use an empty list as a body to make sure Gun knows
	%% we want to send an empty body.
	%% @todo This is a terrible hack. Improve Gun!
	Body = [],
	%% Ensure an empty body results in a 400 error.
	{400, _} = do_body_error("POST", "/multipart", [
		{<<"content-type">>, <<"multipart/mixed; boundary=deadbeef">>}
	], Body, Config),
	ok.

multipart_error_preamble_only(Config) ->
	doc("Multipart request body only contains a preamble."),
	%% Ensure an empty body results in a 400 error.
	{400, _} = do_body_error("POST", "/multipart", [
		{<<"content-type">>, <<"multipart/mixed; boundary=deadbeef">>}
	], <<"Preamble.">>, Config),
	ok.

multipart_error_headers(Config) ->
	doc("Multipart request body with invalid part headers."),
	ReqBody = [
		"--deadbeef\r\nbad-header text/plain\r\n\r\nCowboy is an HTTP server.\r\n"
		"--deadbeef--"
	],
	%% Ensure parse errors result in a 400 response.
	{400, _} = do_body_error("POST", "/multipart", [
		{<<"content-type">>, <<"multipart/mixed; boundary=deadbeef">>}
	], ReqBody, Config),
	ok.

%% The function to parse the multipart body currently does not crash,
%% as far as I can tell. There is therefore no test for it.

multipart_error_no_final_boundary(Config) ->
	doc("Multipart request body with no final boundary."),
	ReqBody = [
		"--deadbeef\r\nContent-Type: text/plain\r\n\r\nCowboy is an HTTP server.\r\n"
	],
	%% Ensure parse errors result in a 400 response.
	{400, _} = do_body_error("POST", "/multipart", [
		{<<"content-type">>, <<"multipart/mixed; boundary=deadbeef">>}
	], ReqBody, Config),
	ok.

multipart_missing_boundary(Config) ->
	doc("Multipart request body without a boundary in the media type."),
	ReqBody = [
		"--deadbeef\r\nContent-Type: text/plain\r\n\r\nCowboy is an HTTP server.\r\n"
		"--deadbeef--"
	],
	%% Ensure parse errors result in a 400 response.
	{400, _} = do_body_error("POST", "/multipart", [
		{<<"content-type">>, <<"multipart/mixed">>}
	], ReqBody, Config),
	ok.

read_part_skip_body(Config) ->
	doc("Multipart request body skipping part bodies."),
	LargeBody = iolist_to_binary(string:chars($a, 10000000)),
	ReqBody = [
		"--deadbeef\r\nContent-Type: text/plain\r\n\r\nCowboy is an HTTP server.\r\n"
		"--deadbeef\r\nContent-Type: application/octet-stream\r\nX-Custom: value\r\n\r\n", LargeBody, "\r\n"
		"--deadbeef--"
	],
	RespBody = do_body("POST", "/multipart/skip_body", [
		{<<"content-type">>, <<"multipart/mixed; boundary=deadbeef">>}
	], ReqBody, Config),
	[
		#{<<"content-type">> := <<"text/plain">>},
		LargeHeaders
	] = binary_to_term(RespBody),
	#{
		<<"content-type">> := <<"application/octet-stream">>,
		<<"x-custom">> := <<"value">>
	} = LargeHeaders,
	ok.

%% @todo When reading a multipart body, length and period
%% only apply to a single read_body call. We may want a
%% separate option to know how many reads we want to do
%% before we give up.

read_part2(Config) ->
	doc("Multipart request body using read_part/2."),
	%% Override the length and period values only, making
	%% the request process use more read_body calls.
	%%
	%% We do not try a custom timeout value since this would
	%% be the same test as read_body/2.
	do_multipart("/multipart/read_part2", Config).

read_part_body2(Config) ->
	doc("Multipart request body using read_part_body/2."),
	%% Override the length and period values only, making
	%% the request process use more read_body calls.
	%%
	%% We do not try a custom timeout value since this would
	%% be the same test as read_body/2.
	do_multipart("/multipart/read_part_body2", Config).

%% Tests: Response.

%% @todo We want to crash when calling set_resp_* or related
%% functions after the reply has been sent.

set_resp_cookie(Config) ->
	doc("Response using set_resp_cookie."),
	%% Single cookie, no options.
	{200, Headers1, _} = do_get("/resp/set_resp_cookie3", Config),
	{_, <<"mycookie=myvalue">>}
		= lists:keyfind(<<"set-cookie">>, 1, Headers1),
	%% Single cookie, with options.
	{200, Headers2, _} = do_get("/resp/set_resp_cookie4", Config),
	{_, <<"mycookie=myvalue; Path=/resp/set_resp_cookie4">>}
		= lists:keyfind(<<"set-cookie">>, 1, Headers2),
	%% Multiple cookies.
	{200, Headers3, _} = do_get("/resp/set_resp_cookie3/multiple", Config),
	[_, _] = [H || H={<<"set-cookie">>, _} <- Headers3],
	%% Overwrite previously set cookie.
	{200, Headers4, _} = do_get("/resp/set_resp_cookie3/overwrite", Config),
	{_, <<"mycookie=overwrite">>}
		= lists:keyfind(<<"set-cookie">>, 1, Headers4),
	ok.

set_resp_header(Config) ->
	doc("Response using set_resp_header."),
	{200, Headers, <<"OK">>} = do_get("/resp/set_resp_header", Config),
	true = lists:keymember(<<"content-type">>, 1, Headers),
	%% The set-cookie header is special. set_resp_cookie must be used.
	{500, _, _} = do_maybe_h3_error3(do_get("/resp/set_resp_header_cookie", Config)),
	ok.

set_resp_headers(Config) ->
	doc("Response using set_resp_headers."),
	{200, Headers, <<"OK">>} = do_get("/resp/set_resp_headers", Config),
	true = lists:keymember(<<"content-type">>, 1, Headers),
	true = lists:keymember(<<"content-encoding">>, 1, Headers),
	%% The set-cookie header is special. set_resp_cookie must be used.
	{500, _, _} = do_maybe_h3_error3(do_get("/resp/set_resp_headers_cookie", Config)),
	ok.

resp_header(Config) ->
	doc("Response header with/without default."),
	{200, _, <<"OK">>} = do_get("/resp/resp_header_defined", Config),
	{200, _, <<"OK">>} = do_get("/resp/resp_header_default", Config),
	ok.

resp_headers(Config) ->
	doc("Get all response headers."),
	{200, _, <<"OK">>} = do_get("/resp/resp_headers", Config),
	{200, _, <<"OK">>} = do_get("/resp/resp_headers_empty", Config),
	ok.

set_resp_body(Config) ->
	doc("Response using set_resp_body."),
	{200, _, <<"OK">>} = do_get("/resp/set_resp_body", Config),
	{200, _, <<"OVERRIDE">>} = do_get("/resp/set_resp_body/override", Config),
	{ok, AppFile} = file:read_file(code:where_is_file("cowboy.app")),
	{200, _, AppFile} = do_get("/resp/set_resp_body/sendfile", Config),
	ok.

set_resp_body_sendfile0(Config) ->
	doc("Response using set_resp_body with a sendfile of length 0."),
	Path = "/resp/set_resp_body/sendfile0",
	ConnPid = gun_open(Config),
	%% First request.
	Ref1 = gun:get(ConnPid, Path, [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, IsFin, 200, _} = gun:await(ConnPid, Ref1, infinity),
	{ok, <<>>} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref1, infinity);
		fin -> {ok, <<>>}
	end,
	%% Second request will confirm everything works as intended.
	Ref2 = gun:get(ConnPid, Path, [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, IsFin, 200, _} = gun:await(ConnPid, Ref2, infinity),
	{ok, <<>>} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref2, infinity);
		fin -> {ok, <<>>}
	end,
	gun:close(ConnPid),
	ok.

has_resp_header(Config) ->
	doc("Has response header?"),
	{200, Headers, <<"OK">>} = do_get("/resp/has_resp_header", Config),
	true = lists:keymember(<<"content-type">>, 1, Headers),
	ok.

has_resp_body(Config) ->
	doc("Has response body?"),
	{200, _, <<"OK">>} = do_get("/resp/has_resp_body", Config),
	{200, _, <<"OK">>} = do_get("/resp/has_resp_body/sendfile", Config),
	ok.

delete_resp_header(Config) ->
	doc("Delete response header."),
	{200, Headers, <<"OK">>} = do_get("/resp/delete_resp_header", Config),
	false = lists:keymember(<<"content-type">>, 1, Headers),
	ok.

%% Data may be lost due to how RESET_STREAM QUIC frame works.
%% Because there is ongoing work for a better way to reset streams
%% (https://www.ietf.org/archive/id/draft-ietf-quic-reliable-stream-reset-03.html)
%% we convert the error to a 500 to keep the tests more explicit
%% at what we expect.
%% @todo When RESET_STREAM_AT gets added we can remove this function.
do_maybe_h3_error2({stream_error, h3_internal_error, _}) -> {500, []};
do_maybe_h3_error2(Result) -> Result.

do_maybe_h3_error3({stream_error, h3_internal_error, _}) -> {500, [], <<>>};
do_maybe_h3_error3(Result) -> Result.

inform2(Config) ->
	doc("Informational response(s) without headers, followed by the real response."),
	{102, [], 200, _, _} = do_get_inform("/resp/inform2/102", Config),
	{102, [], 200, _, _} = do_get_inform("/resp/inform2/binary", Config),
	{500, _} = do_maybe_h3_error2(do_get_inform("/resp/inform2/error", Config)),
	{102, [], 200, _, _} = do_get_inform("/resp/inform2/twice", Config),
	%% With HTTP/1.1 and HTTP/2 we will not get an error.
	%% With HTTP/3 however the stream will occasionally
	%% be reset before Gun receives the response.
	case do_get_inform("/resp/inform2/after_reply", Config) of
		{200, _} ->
			ok;
		{stream_error, h3_internal_error, _} ->
			ok
	end.

inform3(Config) ->
	doc("Informational response(s) with headers, followed by the real response."),
	Headers = [{<<"ext-header">>, <<"ext-value">>}],
	{102, Headers, 200, _, _} = do_get_inform("/resp/inform3/102", Config),
	{102, Headers, 200, _, _} = do_get_inform("/resp/inform3/binary", Config),
	{500, _} = do_maybe_h3_error2(do_get_inform("/resp/inform3/error", Config)),
	%% The set-cookie header is special. set_resp_cookie must be used.
	{500, _} = do_maybe_h3_error2(do_get_inform("/resp/inform3/set_cookie", Config)),
	{102, Headers, 200, _, _} = do_get_inform("/resp/inform3/twice", Config),
	%% With HTTP/1.1 and HTTP/2 we will not get an error.
	%% With HTTP/3 however the stream will occasionally
	%% be reset before Gun receives the response.
	case do_get_inform("/resp/inform3/after_reply", Config) of
		{200, _} ->
			ok;
		{stream_error, h3_internal_error, _} ->
			ok
	end.

reply2(Config) ->
	doc("Response with default headers and no body."),
	{200, _, _} = do_get("/resp/reply2/200", Config),
	{201, _, _} = do_get("/resp/reply2/201", Config),
	{404, _, _} = do_get("/resp/reply2/404", Config),
	{200, _, _} = do_get("/resp/reply2/binary", Config),
	{500, _, _} = do_maybe_h3_error3(do_get("/resp/reply2/error", Config)),
	%% @todo How to test this properly? This isn't enough.
	{200, _, _} = do_get("/resp/reply2/twice", Config),
	ok.

reply3(Config) ->
	doc("Response with additional headers and no body."),
	{200, Headers1, _} = do_get("/resp/reply3/200", Config),
	true = lists:keymember(<<"content-type">>, 1, Headers1),
	{201, Headers2, _} = do_get("/resp/reply3/201", Config),
	true = lists:keymember(<<"content-type">>, 1, Headers2),
	{404, Headers3, _} = do_get("/resp/reply3/404", Config),
	true = lists:keymember(<<"content-type">>, 1, Headers3),
	{500, _, _} = do_maybe_h3_error3(do_get("/resp/reply3/error", Config)),
	%% The set-cookie header is special. set_resp_cookie must be used.
	{500, _, _} = do_maybe_h3_error3(do_get("/resp/reply3/set_cookie", Config)),
	ok.

reply4(Config) ->
	doc("Response with additional headers and body."),
	{200, _, <<"OK">>} = do_get("/resp/reply4/200", Config),
	{201, _, <<"OK">>} = do_get("/resp/reply4/201", Config),
	{404, _, <<"OK">>} = do_get("/resp/reply4/404", Config),
	{500, _, _} = do_maybe_h3_error3(do_get("/resp/reply4/error", Config)),
	%% The set-cookie header is special. set_resp_cookie must be used.
	{500, _, _} = do_maybe_h3_error3(do_get("/resp/reply4/set_cookie", Config)),
	ok.

stream_reply2(Config) ->
	doc("Response with default headers and streamed body."),
	Body = <<0:8000000>>,
	{200, _, Body} = do_get("/resp/stream_reply2/200", Config),
	{201, _, Body} = do_get("/resp/stream_reply2/201", Config),
	{404, _, Body} = do_get("/resp/stream_reply2/404", Config),
	{200, _, Body} = do_get("/resp/stream_reply2/binary", Config),
	{500, _, _} = do_maybe_h3_error3(do_get("/resp/stream_reply2/error", Config)),
	ok.

stream_reply2_twice(Config) ->
	doc("Attempting to stream a response twice results in a crash."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/stream_reply2/twice",
		[{<<"accept-encoding">>, <<"gzip">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref, infinity),
	Protocol = config(protocol, Config),
	Flavor = config(flavor, Config),
	case {Protocol, Flavor, gun:await_body(ConnPid, Ref, infinity)} of
		%% In HTTP/1.1 we cannot propagate an error at that point.
		%% The response will simply not have a body.
		{http, vanilla, {ok, <<>>}} ->
			ok;
		%% When compression was used we do get gzip headers. But
		%% we do not have any data in the zlib stream.
		{http, compress, {ok, Data}} ->
			Z = zlib:open(),
			zlib:inflateInit(Z, 31),
			0 = iolist_size(zlib:inflate(Z, Data)),
			ok;
		%% In HTTP/2 and HTTP/3 the stream gets reset with an appropriate error.
		{http2, _, {error, {stream_error, {stream_error, internal_error, _}}}} ->
			ok;
		{http3, _, {error, {stream_error, {stream_error, h3_internal_error, _}}}} ->
			ok
	end,
	gun:close(ConnPid).

stream_reply3(Config) ->
	doc("Response with additional headers and streamed body."),
	Body = <<0:8000000>>,
	{200, Headers1, Body} = do_get("/resp/stream_reply3/200", Config),
	true = lists:keymember(<<"content-type">>, 1, Headers1),
	{201, Headers2, Body} = do_get("/resp/stream_reply3/201", Config),
	true = lists:keymember(<<"content-type">>, 1, Headers2),
	{404, Headers3, Body} = do_get("/resp/stream_reply3/404", Config),
	true = lists:keymember(<<"content-type">>, 1, Headers3),
	{500, _, _} = do_maybe_h3_error3(do_get("/resp/stream_reply3/error", Config)),
	%% The set-cookie header is special. set_resp_cookie must be used.
	{500, _, _} = do_maybe_h3_error3(do_get("/resp/stream_reply3/set_cookie", Config)),
	ok.

stream_body_fin0(Config) ->
	doc("Streamed body with last chunk of size 0."),
	{200, _, <<"Hello world!">>} = do_get("/resp/stream_body/fin0", Config),
	ok.

stream_body_multiple(Config) ->
	doc("Streamed body via multiple calls."),
	{200, _, <<"Hello world!">>} = do_get("/resp/stream_body/multiple", Config),
	ok.

stream_body_loop(Config) ->
	doc("Streamed body via a fast loop."),
	{200, _, <<0:32000000/unit:8>>} = do_get("/resp/stream_body/loop", Config),
	ok.

stream_body_nofin(Config) ->
	doc("Unfinished streamed body."),
	{200, _, <<"Hello world!">>} = do_get("/resp/stream_body/nofin", Config),
	ok.

stream_body_sendfile(Config) ->
	doc("Streamed body via multiple calls, including sendfile calls."),
	{ok, AppFile} = file:read_file(code:where_is_file("cowboy.app")),
	ExpectedBody = iolist_to_binary([
		<<"Hello ">>,
		AppFile,
		<<" interspersed ">>,
		AppFile,
		<<" world!">>
	]),
	{200, _, ExpectedBody} = do_get("/resp/stream_body/sendfile", Config),
	ok.

stream_body_sendfile_fin(Config) ->
	doc("Streamed body via multiple calls, including a sendfile final call."),
	{ok, AppFile} = file:read_file(code:where_is_file("cowboy.app")),
	ExpectedBody = iolist_to_binary([
		<<"Hello! ">>,
		AppFile
	]),
	{200, _, ExpectedBody} = do_get("/resp/stream_body/sendfile_fin", Config),
	ok.

stream_body_spawn(Config) ->
	doc("Confirm we can use cowboy_req:stream_body/3 from another process."),
	{200, _, <<"Hello world!">>} = do_get("/resp/stream_body/spawn", Config),
	ok.

stream_body_content_length_multiple(Config) ->
	doc("Streamed body via multiple calls."),
	{200, _, <<"Hello world!">>} = do_get("/resp/stream_body_content_length/multiple", Config),
	ok.

stream_body_content_length_fin0(Config) ->
	doc("Streamed body with last chunk of size 0."),
	{200, _, <<"Hello world!">>} = do_get("/resp/stream_body_content_length/fin0", Config),
	ok.

stream_body_content_length_nofin(Config) ->
	doc("Unfinished streamed body."),
	{200, _, <<"Hello world!">>} = do_get("/resp/stream_body_content_length/nofin", Config),
	ok.

stream_body_content_length_nofin_error(Config) ->
	doc("Not all of the response body sent."),
	case config(protocol, Config) of
		http ->
			case do_get_error("/resp/stream_body_content_length/nofin-error", Config) of
				%% When compression is used content-length is not sent.
				{200, Headers, <<"Hello">>} ->
					{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers);
				%% The server closes the connection when the body couldn't be sent fully.
				{error, {stream_error, closed}} ->
					receive
						{gun_down, ConnPid, _, _, _} ->
							gun:close(ConnPid)
					after 1000 ->
						error(timeout)
					end
			end;
		http2 ->
			%% @todo HTTP/2 should have the same content-length checks.
			{skip, "Implement the test for HTTP/2."};
		http3 ->
			%% @todo HTTP/3 should have the same content-length checks.
			{skip, "Implement the test for HTTP/3."}
	end.

stream_body_concurrent(Config) ->
	ConnPid = gun_open(Config),
	Ref1 = gun:get(ConnPid, "/resp/stream_body/loop", [{<<"accept-encoding">>, <<"gzip">>}]),
	Ref2 = gun:get(ConnPid, "/resp/stream_body/loop", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref1, infinity),
	{ok, _} = gun:await_body(ConnPid, Ref1, infinity),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref2, infinity),
	{ok, _} = gun:await_body(ConnPid, Ref2, infinity),
	gun:close(ConnPid).

%% @todo Crash when calling stream_body after the fin flag has been set.
%% @todo Crash when calling stream_body after calling reply.
%% @todo Crash when calling stream_body before calling stream_reply.

stream_events_single(Config) ->
	doc("Streamed event."),
	{200, Headers, <<
		"event: add_comment\n"
		"data: Comment text.\n"
		"data: With many lines.\n"
		"\n"
	>>} = do_get("/resp/stream_events/single", Config),
	{_, <<"text/event-stream">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

stream_events_list(Config) ->
	doc("Streamed list of events."),
	{200, Headers, <<
		"event: add_comment\n"
		"data: Comment text.\n"
		"data: With many lines.\n"
		"\n"
		": Set retry higher\n"
		": with many lines also.\n"
		"retry: 10000\n"
		"\n"
		"id: 123\n"
		"event: add_comment\n"
		"data: Closing!\n"
		"\n"
	>>} = do_get("/resp/stream_events/list", Config),
	{_, <<"text/event-stream">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

stream_events_multiple(Config) ->
	doc("Streamed events via multiple calls."),
	{200, Headers, <<
		"event: add_comment\n"
		"data: Comment text.\n"
		"data: With many lines.\n"
		"\n"
		": Set retry higher\n"
		": with many lines also.\n"
		"retry: 10000\n"
		"\n"
		"id: 123\n"
		"event: add_comment\n"
		"data: Closing!\n"
		"\n"
	>>} = do_get("/resp/stream_events/multiple", Config),
	{_, <<"text/event-stream">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

stream_trailers(Config) ->
	doc("Stream body followed by trailer headers."),
	{200, RespHeaders, <<"Hello world!">>, [
		{<<"grpc-status">>, <<"0">>}
	]} = do_trailers("/resp/stream_trailers", Config),
	{_, <<"grpc-status">>} = lists:keyfind(<<"trailer">>, 1, RespHeaders),
	ok.

stream_trailers_large(Config) ->
	doc("Stream large body followed by trailer headers."),
	{200, RespHeaders, <<0:80000000>>, [
		{<<"grpc-status">>, <<"0">>}
	]} = do_trailers("/resp/stream_trailers/large", Config),
	{_, <<"grpc-status">>} = lists:keyfind(<<"trailer">>, 1, RespHeaders),
	ok.

stream_trailers_no_te(Config) ->
	doc("Stream body followed by trailer headers without a te header in the request."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/stream_trailers", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, nofin, 200, RespHeaders} = gun:await(ConnPid, Ref, infinity),
	%% @todo Do we want to remove the trailer header automatically?
%	false = lists:keyfind(<<"trailer">>, 1, RespHeaders),
	{ok, RespBody} = gun:await_body(ConnPid, Ref, infinity),
	<<"Hello world!">> = do_decode(RespHeaders, RespBody),
	gun:close(ConnPid).

stream_trailers_set_cookie(Config) ->
	doc("Trying to send set-cookie in trailers should result in a crash."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/stream_trailers/set_cookie", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"te">>, <<"trailers">>}
	]),
	Protocol = config(protocol, Config),
	case gun:await(ConnPid, Ref, infinity) of
		{response, nofin, 200, _} when Protocol =:= http ->
			%% Trailers are not sent because of the stream error.
			{ok, _Body} = gun:await_body(ConnPid, Ref, infinity),
			{error, timeout} = gun:await_body(ConnPid, Ref, 1000),
			ok;
		{response, nofin, 200, _} when Protocol =:= http2 ->
			{error, {stream_error, {stream_error, internal_error, _}}}
				= gun:await_body(ConnPid, Ref, infinity),
			ok;
		{response, nofin, 200, _} when Protocol =:= http3 ->
			{error, {stream_error, {stream_error, h3_internal_error, _}}}
				= gun:await_body(ConnPid, Ref, infinity),
			ok;
		%% The RST_STREAM arrived before the start of the response.
		%% See maybe_h3_error comment for details.
		{error, {stream_error, {stream_error, h3_internal_error, _}}} when Protocol =:= http3 ->
			ok
	end,
	gun:close(ConnPid).

do_trailers(Path, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"te">>, <<"trailers">>}
	]),
	{response, nofin, Status, RespHeaders} = gun:await(ConnPid, Ref, infinity),
	{ok, RespBody, Trailers} = gun:await_body(ConnPid, Ref, infinity),
	gun:close(ConnPid),
	{Status, RespHeaders, do_decode(RespHeaders, RespBody), Trailers}.

%% @todo Crash when calling stream_trailers twice.
%% @todo Crash when calling stream_trailers after the fin flag has been set.
%% @todo Crash when calling stream_trailers after calling reply.
%% @todo Crash when calling stream_trailers before calling stream_reply.

%% Tests: Push.

%% @todo We want to crash when push is called after reply has been initiated.

push(Config) ->
	case config(protocol, Config) of
		http -> do_push_http("/resp/push", Config);
		http2 -> do_push_http2(Config);
		http3 -> {skip, "Implement server push for HTTP/3."}
	end.

push_after_reply(Config) ->
	doc("Trying to push a response after the final response results in a crash."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/push/after_reply", []),
	%% With HTTP/1.1 and HTTP/2 we will not get an error.
	%% With HTTP/3 however the stream will occasionally
	%% be reset before Gun receives the response.
	case gun:await(ConnPid, Ref, infinity) of
		{response, fin, 200, _} ->
			ok;
		{error, {stream_error, {stream_error, h3_internal_error, _}}} ->
			ok
	end,
	gun:close(ConnPid).

push_method(Config) ->
	case config(protocol, Config) of
		http -> do_push_http("/resp/push/method", Config);
		http2 -> do_push_http2_method(Config);
		http3 -> {skip, "Implement server push for HTTP/3."}
	end.


push_origin(Config) ->
	case config(protocol, Config) of
		http -> do_push_http("/resp/push/origin", Config);
		http2 -> do_push_http2_origin(Config);
		http3 -> {skip, "Implement server push for HTTP/3."}
	end.

push_qs(Config) ->
	case config(protocol, Config) of
		http -> do_push_http("/resp/push/qs", Config);
		http2 -> do_push_http2_qs(Config);
		http3 -> {skip, "Implement server push for HTTP/3."}
	end.

do_push_http(Path, Config) ->
	doc("Ignore pushed responses when protocol is HTTP/1.1."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, []),
	{response, fin, 200, _} = gun:await(ConnPid, Ref, infinity),
	gun:close(ConnPid).

do_push_http2(Config) ->
	doc("Pushed responses."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/push", []),
	%% We expect two pushed resources.
	Origin = iolist_to_binary([
		case config(type, Config) of
			tcp -> "http";
			ssl -> "https"
		end,
		"://localhost:",
		integer_to_binary(config(port, Config))
	]),
	OriginLen = byte_size(Origin),
	{push, PushCSS, <<"GET">>, <<Origin:OriginLen/binary, "/static/style.css">>,
		[{<<"accept">>,<<"text/css">>}]} = gun:await(ConnPid, Ref, infinity),
	{push, PushTXT, <<"GET">>, <<Origin:OriginLen/binary, "/static/plain.txt">>,
		[{<<"accept">>,<<"text/plain">>}]} = gun:await(ConnPid, Ref, infinity),
	%% Pushed CSS.
	{response, nofin, 200, HeadersCSS} = gun:await(ConnPid, PushCSS, infinity),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, HeadersCSS),
	{ok, <<"body{color:red}\n">>} = gun:await_body(ConnPid, PushCSS, infinity),
	%% Pushed TXT is 406 because the pushed accept header uses an undefined type.
	{response, fin, 406, _} = gun:await(ConnPid, PushTXT, infinity),
	%% Let's not forget about the response to the client's request.
	{response, fin, 200, _} = gun:await(ConnPid, Ref, infinity),
	gun:close(ConnPid).

do_push_http2_method(Config) ->
	doc("Pushed response with non-GET method."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/push/method", []),
	%% Pushed CSS.
	{push, PushCSS, <<"HEAD">>, _, [{<<"accept">>,<<"text/css">>}]} = gun:await(ConnPid, Ref, infinity),
	{response, fin, 200, HeadersCSS} = gun:await(ConnPid, PushCSS, infinity),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, HeadersCSS),
	%% Let's not forget about the response to the client's request.
	{response, fin, 200, _} = gun:await(ConnPid, Ref, infinity),
	gun:close(ConnPid).

do_push_http2_origin(Config) ->
	doc("Pushed response with custom scheme/host/port."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/push/origin", []),
	%% Pushed CSS.
	{push, PushCSS, <<"GET">>, <<"ftp://127.0.0.1:21/static/style.css">>,
		[{<<"accept">>,<<"text/css">>}]} = gun:await(ConnPid, Ref, infinity),
	{response, nofin, 200, HeadersCSS} = gun:await(ConnPid, PushCSS, infinity),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, HeadersCSS),
	{ok, <<"body{color:red}\n">>} = gun:await_body(ConnPid, PushCSS, infinity),
	%% Let's not forget about the response to the client's request.
	{response, fin, 200, _} = gun:await(ConnPid, Ref, infinity),
	gun:close(ConnPid).

do_push_http2_qs(Config) ->
	doc("Pushed response with query string."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/push/qs", []),
	%% Pushed CSS.
	Origin = iolist_to_binary([
		case config(type, Config) of
			tcp -> "http";
			ssl -> "https"
		end,
		"://localhost:",
		integer_to_binary(config(port, Config))
	]),
	OriginLen = byte_size(Origin),
	{push, PushCSS, <<"GET">>, <<Origin:OriginLen/binary, "/static/style.css?server=cowboy&version=2.0">>,
		[{<<"accept">>,<<"text/css">>}]} = gun:await(ConnPid, Ref, infinity),
	{response, nofin, 200, HeadersCSS} = gun:await(ConnPid, PushCSS, infinity),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, HeadersCSS),
	{ok, <<"body{color:red}\n">>} = gun:await_body(ConnPid, PushCSS, infinity),
	%% Let's not forget about the response to the client's request.
	{response, fin, 200, _} = gun:await(ConnPid, Ref, infinity),
	gun:close(ConnPid).
