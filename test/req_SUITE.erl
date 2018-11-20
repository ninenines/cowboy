%% Copyright (c) 2016-2017, Loïc Hoguin <essen@ninenines.eu>
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
	cowboy:stop_listener(Name).

%% Routes.

init_dispatch(Config) ->
	cowboy_router:compile([{"[...]", [
		{"/static/[...]", cowboy_static, {dir, config(priv_dir, Config) ++ "/static"}},
		%% @todo Seriously InitialState should be optional.
		{"/resp/:key[/:arg]", resp_h, []},
		{"/multipart[/:key]", multipart_h, []},
		{"/args/:key/:arg[/:default]", echo_h, []},
		{"/crash/:key/period", echo_h, #{length => 999999999, period => 1000, crash => true}},
		{"/no-opts/:key", echo_h, #{crash => true}},
		{"/opts/:key/length", echo_h, #{length => 1000}},
		{"/opts/:key/period", echo_h, #{length => 999999999, period => 1000}},
		{"/opts/:key/timeout", echo_h, #{timeout => 1000, crash => true}},
		{"/100-continue/:key", echo_h, []},
		{"/full/:key", echo_h, []},
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
	Ref = case Body of
		<<>> -> gun:request(ConnPid, Method, Path, Headers);
		_ -> gun:request(ConnPid, Method, Path, Headers, Body)
	end,
	{response, IsFin, 200, RespHeaders} = gun:await(ConnPid, Ref, 10000),
	{ok, RespBody} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref);
		fin -> {ok, <<>>}
	end,
	gun:close(ConnPid),
	do_decode(RespHeaders, RespBody).

do_body_error(Method, Path, Headers0, Body, Config) ->
	ConnPid = gun_open(Config),
	Headers = [{<<"accept-encoding">>, <<"gzip">>}|Headers0],
	Ref = case Body of
		<<>> -> gun:request(ConnPid, Method, Path, Headers);
		_ -> gun:request(ConnPid, Method, Path, Headers, Body)
	end,
	{response, _, Status, RespHeaders} = gun:await(ConnPid, Ref),
	gun:close(ConnPid),
	{Status, RespHeaders}.

do_get(Path, Config) ->
	do_get(Path, [], Config).

do_get(Path, Headers, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, [{<<"accept-encoding">>, <<"gzip">>}|Headers]),
	{response, IsFin, Status, RespHeaders} = gun:await(ConnPid, Ref),
	{ok, RespBody} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref);
		fin -> {ok, <<>>}
	end,
	gun:close(ConnPid),
	{Status, RespHeaders, do_decode(RespHeaders, RespBody)}.

do_get_body(Path, Config) ->
	do_get_body(Path, [], Config).

do_get_body(Path, Headers, Config) ->
	do_body("GET", Path, Headers, Config).

do_get_inform(Path, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, [{<<"accept-encoding">>, <<"gzip">>}]),
	case gun:await(ConnPid, Ref) of
		{response, _, RespStatus, RespHeaders} ->
			%% We don't care about the body.
			gun:close(ConnPid),
			{RespStatus, RespHeaders};
		{inform, InfoStatus, InfoHeaders} ->
			{response, IsFin, RespStatus, RespHeaders}
				= case gun:await(ConnPid, Ref) of
					{inform, InfoStatus, InfoHeaders} ->
						gun:await(ConnPid, Ref);
					Response ->
						Response
			end,
			{ok, RespBody} = case IsFin of
				nofin -> gun:await_body(ConnPid, Ref);
				fin -> {ok, <<>>}
			end,
			gun:close(ConnPid),
			{InfoStatus, InfoHeaders, RespStatus, RespHeaders, do_decode(RespHeaders, RespBody)}
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
	{response, IsFin, Status, RespHeaders} = gun:await(ConnPid, Ref),
	Result = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref);
		fin -> {ok, <<>>}
	end,
	gun:close(ConnPid),
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
		ssl -> do_cert(Config)
	end.

do_cert(Config0) ->
	doc("A client TLS certificate was provided."),
	{CaCert, Cert, Key} = ct_helper:make_certs(),
	Config = [{transport_opts, [
		{cert, Cert},
		{key, Key},
		{cacerts, [CaCert]}
	]}|Config0],
	Cert = do_get_body("/cert", Config),
	Cert = do_get_body("/direct/cert", Config),
	ok.

cert_undefined(Config) ->
	doc("No client TLS certificate was provided."),
	<<"undefined">> = do_get_body("/cert", Config),
	<<"undefined">> = do_get_body("/direct/cert", Config),
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
	<<"#{<<\"accept-encoding\">> => <<\"gzip\">>,<<\"header\">> => <<\"value\">>", _/bits>>
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
	<<"#{a => <<\"b\">>,c => <<\"d\">>}">> = do_get_body("/match/cookies/a/c",
		[{<<"cookie">>, "a=b; c=d"}], Config),
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
	<<"#{a => <<\"b\">>,c => <<\"d\">>}">> = do_get_body("/match/qs/a/c?a=b&c=d", Config),
	<<"#{a => <<\"b\">>,c => true}">> = do_get_body("/match/qs/a/c?a=b&c", Config),
	<<"#{a => true,c => <<\"d\">>}">> = do_get_body("/match/qs/a/c?a&c=d", Config),
	%% Ensure match errors result in a 400 response.
	{400, _, _} = do_get("/match/qs/a/c?a=b", [], Config),
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
		[{<<"cookie">>, "bad name=strawberry"}], Config),
	{400, _, _} = do_get("/parse_cookies",
		[{<<"cookie">>, "goodname=strawberry\tmilkshake"}], Config),
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
		ssl -> <<"443">>
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
		<<"https">> when Transport =:= ssl -> ok
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
		ssl -> <<"https">>
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
		<<"HTTP/2">> when Protocol =:= http2 -> ok
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
	doc("Request body whose sizes are around the MTU."),
	MTU = ct_helper:get_loopback_mtu(),
	_ = [begin
		Body = <<0:Size/unit:8>>,
		Body = do_body("POST", "/full/read_body", [], Body, Config)
	end || Size <- lists:seq(MTU - 10, MTU + 10)],
	ok.

read_body_period(Config) ->
	doc("Read the request body for at most 1 second."),
	ConnPid = gun_open(Config),
	Body = <<0:8000000>>,
	Ref = gun:request(ConnPid, "POST", "/opts/read_body/period", [
		{<<"content-length">>, integer_to_binary(byte_size(Body) * 2)}
	]),
	%% The body is sent twice, first with nofin, then wait 2 seconds, then again with fin.
	gun:data(ConnPid, Ref, nofin, Body),
	timer:sleep(2000),
	gun:data(ConnPid, Ref, fin, Body),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	gun:close(ConnPid).

%% We expect a crash.
do_read_body_timeout(Path, Body, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:request(ConnPid, "POST", Path, [
		{<<"content-length">>, integer_to_binary(byte_size(Body))}
	]),
	{response, _, 500, _} = gun:await(ConnPid, Ref),
	gun:close(ConnPid).

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
	{inform, 100, []} = gun:await(ConnPid, Ref),
	gun:data(ConnPid, Ref, fin, Body),
	{response, IsFin, 200, RespHeaders} = gun:await(ConnPid, Ref),
	{ok, RespBody} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref);
		fin -> {ok, <<>>}
	end,
	gun:close(ConnPid),
	do_decode(RespHeaders, RespBody).

read_urlencoded_body(Config) ->
	doc("application/x-www-form-urlencoded request body."),
	<<"[]">> = do_body("POST", "/read_urlencoded_body", [], <<>>, Config),
	<<"[{<<\"abc\">>,true}]">> = do_body("POST", "/read_urlencoded_body", [], "abc", Config),
	<<"[{<<\"a\">>,<<\"b\">>},{<<\"c\">>,<<\"d e\">>}]">>
		= do_body("POST", "/read_urlencoded_body", [], "a=b&c=d+e", Config),
	%% Send a 10MB body, larger than the default length, to ensure a crash occurs.
	ok = do_read_urlencoded_body_too_large("/no-opts/read_urlencoded_body",
		string:chars($a, 10000000), Config),
	%% We read any length for at most 1 second.
	%%
	%% The body is sent twice, first with nofin, then wait 1.1 second, then again with fin.
	%% We expect the handler to crash because read_urlencoded_body expects the full body.
	ok = do_read_urlencoded_body_too_long("/crash/read_urlencoded_body/period", <<"abc">>, Config),
	%% The timeout value is set too low on purpose to ensure a crash occurs.
	ok = do_read_body_timeout("/opts/read_urlencoded_body/timeout", <<"abc">>, Config),
	%% Ensure parse errors result in a 400 response.
	{400, _} = do_body_error("POST", "/read_urlencoded_body", [], "%%%%%", Config),
	ok.

%% We expect a crash.
do_read_urlencoded_body_too_large(Path, Body, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:request(ConnPid, "POST", Path, [
		{<<"content-length">>, integer_to_binary(iolist_size(Body))}
	]),
	gun:data(ConnPid, Ref, fin, Body),
	{response, _, 413, _} = gun:await(ConnPid, Ref),
	gun:close(ConnPid).

%% We expect a crash.
do_read_urlencoded_body_too_long(Path, Body, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:request(ConnPid, "POST", Path, [
		{<<"content-length">>, integer_to_binary(byte_size(Body) * 2)}
	]),
	gun:data(ConnPid, Ref, nofin, Body),
	timer:sleep(2000),
	gun:data(ConnPid, Ref, fin, Body),
	{response, _, 408, RespHeaders} = gun:await(ConnPid, Ref),
	_ = case config(protocol, Config) of
		http ->
			%% 408 error responses should close HTTP/1.1 connections.
			{_, <<"close">>} = lists:keyfind(<<"connection">>, 1, RespHeaders);
		http2 ->
			ok
	end,
	gun:close(ConnPid).

read_and_match_urlencoded_body(Config) ->
	doc("Read and match an application/x-www-form-urlencoded request body."),
	<<"#{}">> = do_body("POST", "/match/body_qs", [], "a=b&c=d", Config),
	<<"#{a => <<\"b\">>}">> = do_body("POST", "/match/body_qs/a", [], "a=b&c=d", Config),
	<<"#{c => <<\"d\">>}">> = do_body("POST", "/match/body_qs/c", [], "a=b&c=d", Config),
	<<"#{a => <<\"b\">>,c => <<\"d\">>}">>
		= do_body("POST", "/match/body_qs/a/c", [], "a=b&c=d", Config),
	<<"#{a => <<\"b\">>,c => true}">> = do_body("POST", "/match/body_qs/a/c", [], "a=b&c", Config),
	<<"#{a => true,c => <<\"d\">>}">> = do_body("POST", "/match/body_qs/a/c", [], "a&c=d", Config),
	%% Ensure match errors result in a 400 response.
	{400, _} = do_body_error("POST", "/match/body_qs/a/c", [], "a=b", Config),
	%% Ensure parse errors result in a 400 response.
	{400, _} = do_body_error("POST", "/match/body_qs", [], "%%%%%", Config),
	%% Send a 10MB body, larger than the default length, to ensure a crash occurs.
	ok = do_read_urlencoded_body_too_large(
		"/no-opts/read_and_match_urlencoded_body",
		string:chars($a, 10000000), Config),
	%% We read any length for at most 1 second.
	%%
	%% The body is sent twice, first with nofin, then wait 1.1 second, then again with fin.
	%% We expect the handler to crash because read_and_match_urlencoded_body expects the full body.
	ok = do_read_urlencoded_body_too_long(
		"/crash/read_and_match_urlencoded_body/period", <<"abc">>, Config),
	%% The timeout value is set too low on purpose to ensure a crash occurs.
	ok = do_read_body_timeout("/opts/read_and_match_urlencoded_body/timeout", <<"abc">>, Config),
	ok.

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
	{_, <<"mycookie=myvalue; Version=1">>}
		= lists:keyfind(<<"set-cookie">>, 1, Headers1),
	%% Single cookie, with options.
	{200, Headers2, _} = do_get("/resp/set_resp_cookie4", Config),
	{_, <<"mycookie=myvalue; Version=1; Path=/resp/set_resp_cookie4">>}
		= lists:keyfind(<<"set-cookie">>, 1, Headers2),
	%% Multiple cookies.
	{200, Headers3, _} = do_get("/resp/set_resp_cookie3/multiple", Config),
	[_, _] = [H || H={<<"set-cookie">>, _} <- Headers3],
	%% Overwrite previously set cookie.
	{200, Headers4, _} = do_get("/resp/set_resp_cookie3/overwrite", Config),
	{_, <<"mycookie=overwrite; Version=1">>}
		= lists:keyfind(<<"set-cookie">>, 1, Headers4),
	ok.

set_resp_header(Config) ->
	doc("Response using set_resp_header."),
	{200, Headers, <<"OK">>} = do_get("/resp/set_resp_header", Config),
	true = lists:keymember(<<"content-type">>, 1, Headers),
	ok.

set_resp_headers(Config) ->
	doc("Response using set_resp_headers."),
	{200, Headers, <<"OK">>} = do_get("/resp/set_resp_headers", Config),
	true = lists:keymember(<<"content-type">>, 1, Headers),
	true = lists:keymember(<<"content-encoding">>, 1, Headers),
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
	{response, IsFin, 200, _} = gun:await(ConnPid, Ref1),
	{ok, <<>>} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref1);
		fin -> {ok, <<>>}
	end,
	%% Second request will confirm everything works as intended.
	Ref2 = gun:get(ConnPid, Path, [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, IsFin, 200, _} = gun:await(ConnPid, Ref2),
	{ok, <<>>} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref2);
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

inform2(Config) ->
	doc("Informational response(s) without headers, followed by the real response."),
	{102, [], 200, _, _} = do_get_inform("/resp/inform2/102", Config),
	{102, [], 200, _, _} = do_get_inform("/resp/inform2/binary", Config),
	{500, _} = do_get_inform("/resp/inform2/error", Config),
	{102, [], 200, _, _} = do_get_inform("/resp/inform2/twice", Config),
	ok.

inform3(Config) ->
	doc("Informational response(s) with headers, followed by the real response."),
	Headers = [{<<"ext-header">>, <<"ext-value">>}],
	{102, Headers, 200, _, _} = do_get_inform("/resp/inform3/102", Config),
	{102, Headers, 200, _, _} = do_get_inform("/resp/inform3/binary", Config),
	{500, _} = do_get_inform("/resp/inform3/error", Config),
	{102, Headers, 200, _, _} = do_get_inform("/resp/inform3/twice", Config),
	ok.

reply2(Config) ->
	doc("Response with default headers and no body."),
	{200, _, _} = do_get("/resp/reply2/200", Config),
	{201, _, _} = do_get("/resp/reply2/201", Config),
	{404, _, _} = do_get("/resp/reply2/404", Config),
	{200, _, _} = do_get("/resp/reply2/binary", Config),
	{500, _, _} = do_get("/resp/reply2/error", Config),
	%% @todo We want to crash when reply or stream_reply is called twice.
	%% How to test this properly? This isn't enough.
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
	{500, _, _} = do_get("/resp/reply3/error", Config),
	ok.

reply4(Config) ->
	doc("Response with additional headers and body."),
	{200, _, <<"OK">>} = do_get("/resp/reply4/200", Config),
	{201, _, <<"OK">>} = do_get("/resp/reply4/201", Config),
	{404, _, <<"OK">>} = do_get("/resp/reply4/404", Config),
	{500, _, _} = do_get("/resp/reply4/error", Config),
	ok.

%% @todo Crash when stream_reply is called twice.

stream_reply2(Config) ->
	doc("Response with default headers and streamed body."),
	Body = <<0:8000000>>,
	{200, _, Body} = do_get("/resp/stream_reply2/200", Config),
	{201, _, Body} = do_get("/resp/stream_reply2/201", Config),
	{404, _, Body} = do_get("/resp/stream_reply2/404", Config),
	{200, _, Body} = do_get("/resp/stream_reply2/binary", Config),
	{500, _, _} = do_get("/resp/stream_reply2/error", Config),
	ok.

stream_reply3(Config) ->
	doc("Response with additional headers and streamed body."),
	Body = <<0:8000000>>,
	{200, Headers1, Body} = do_get("/resp/stream_reply3/200", Config),
	true = lists:keymember(<<"content-type">>, 1, Headers1),
	{201, Headers2, Body} = do_get("/resp/stream_reply3/201", Config),
	true = lists:keymember(<<"content-type">>, 1, Headers2),
	{404, Headers3, Body} = do_get("/resp/stream_reply3/404", Config),
	true = lists:keymember(<<"content-type">>, 1, Headers3),
	{500, _, _} = do_get("/resp/stream_reply3/error", Config),
	ok.

stream_body_fin0(Config) ->
	doc("Streamed body with last chunk of size 0."),
	{200, _, <<"Hello world!">>} = do_get("/resp/stream_body/fin0", Config),
	ok.

stream_body_multiple(Config) ->
	doc("Streamed body via multiple calls."),
	{200, _, <<"Hello world!">>} = do_get("/resp/stream_body/multiple", Config),
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
	doc("Not all of body sent."),
	case config(protocol, Config) of
		http ->
			case do_get_error("/resp/stream_body_content_length/nofin-error", Config) of
				{200, Headers, <<"Hello">>} ->
					{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers);
				{error, {closed, "The connection was lost."}} ->
					ok;
				{error, timeout} ->
					ok
			end;
		http2 ->
			%% @todo HTTP2 should have the same content-length checks
			ok
	end.

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
	{200, RespHeaders, <<0:800000>>, [
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
	{response, nofin, 200, RespHeaders} = gun:await(ConnPid, Ref),
	%% @todo Do we want to remove the trailer header automatically?
%	false = lists:keyfind(<<"trailer">>, 1, RespHeaders),
	{ok, RespBody} = gun:await_body(ConnPid, Ref),
	<<"Hello world!">> = do_decode(RespHeaders, RespBody),
	gun:close(ConnPid).

do_trailers(Path, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"te">>, <<"trailers">>}
	]),
	{response, nofin, Status, RespHeaders} = gun:await(ConnPid, Ref),
	{ok, RespBody, Trailers} = gun:await_body(ConnPid, Ref),
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
		http2 -> do_push_http2(Config)
	end.

push_method(Config) ->
	case config(protocol, Config) of
		http -> do_push_http("/resp/push/method", Config);
		http2 -> do_push_http2_method(Config)
	end.


push_origin(Config) ->
	case config(protocol, Config) of
		http -> do_push_http("/resp/push/origin", Config);
		http2 -> do_push_http2_origin(Config)
	end.

push_qs(Config) ->
	case config(protocol, Config) of
		http -> do_push_http("/resp/push/qs", Config);
		http2 -> do_push_http2_qs(Config)
	end.

do_push_http(Path, Config) ->
	doc("Ignore pushed responses when protocol is HTTP/1.1."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, []),
	{response, fin, 200, _} = gun:await(ConnPid, Ref),
	ok.

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
		[{<<"accept">>,<<"text/css">>}]} = gun:await(ConnPid, Ref),
	{push, PushTXT, <<"GET">>, <<Origin:OriginLen/binary, "/static/plain.txt">>,
		[{<<"accept">>,<<"text/plain">>}]} = gun:await(ConnPid, Ref),
	%% Pushed CSS.
	{response, nofin, 200, HeadersCSS} = gun:await(ConnPid, PushCSS),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, HeadersCSS),
	{ok, <<"body{color:red}\n">>} = gun:await_body(ConnPid, PushCSS),
	%% Pushed TXT is 406 because the pushed accept header uses an undefined type.
	{response, fin, 406, _} = gun:await(ConnPid, PushTXT),
	%% Let's not forget about the response to the client's request.
	{response, fin, 200, _} = gun:await(ConnPid, Ref),
	gun:close(ConnPid).

do_push_http2_method(Config) ->
	doc("Pushed response with non-GET method."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/push/method", []),
	%% Pushed CSS.
	{push, PushCSS, <<"HEAD">>, _, [{<<"accept">>,<<"text/css">>}]} = gun:await(ConnPid, Ref),
	{response, fin, 200, HeadersCSS} = gun:await(ConnPid, PushCSS),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, HeadersCSS),
	%% Let's not forget about the response to the client's request.
	{response, fin, 200, _} = gun:await(ConnPid, Ref),
	gun:close(ConnPid).

do_push_http2_origin(Config) ->
	doc("Pushed response with custom scheme/host/port."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/push/origin", []),
	%% Pushed CSS.
	{push, PushCSS, <<"GET">>, <<"ftp://127.0.0.1:21/static/style.css">>,
		[{<<"accept">>,<<"text/css">>}]} = gun:await(ConnPid, Ref),
	{response, nofin, 200, HeadersCSS} = gun:await(ConnPid, PushCSS),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, HeadersCSS),
	{ok, <<"body{color:red}\n">>} = gun:await_body(ConnPid, PushCSS),
	%% Let's not forget about the response to the client's request.
	{response, fin, 200, _} = gun:await(ConnPid, Ref),
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
		[{<<"accept">>,<<"text/css">>}]} = gun:await(ConnPid, Ref),
	{response, nofin, 200, HeadersCSS} = gun:await(ConnPid, PushCSS),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, HeadersCSS),
	{ok, <<"body{color:red}\n">>} = gun:await_body(ConnPid, PushCSS),
	%% Let's not forget about the response to the client's request.
	{response, fin, 200, _} = gun:await(ConnPid, Ref),
	gun:close(ConnPid).
