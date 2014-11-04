%% Copyright (c) 2011-2014, Loïc Hoguin <essen@ninenines.eu>
%% Copyright (c) 2011, Anthony Ramine <nox@dev-extend.eu>
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

-module(http_SUITE).
-compile(export_all).

-import(cowboy_test, [config/2]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [gun_monitor_open/1]).
-import(cowboy_test, [gun_monitor_open/2]).
-import(cowboy_test, [gun_is_gone/2]).
-import(cowboy_test, [raw_open/1]).
-import(cowboy_test, [raw_send/2]).
-import(cowboy_test, [raw_recv_head/1]).
-import(cowboy_test, [raw_expect_recv/2]).

%% ct.

all() ->
	[
		{group, http},
		{group, https},
		{group, http_compress},
		{group, https_compress},
		{group, onresponse},
		{group, onresponse_capitalize},
		{group, parse_host},
		{group, set_env}
	].

groups() ->
	Tests = cowboy_test:all(?MODULE) -- [
		onresponse_crash, onresponse_reply, onresponse_capitalize,
		parse_host, set_env_dispatch
	],
	[
		{http, [parallel], Tests},
		{https, [parallel], Tests},
		{http_compress, [parallel], Tests},
		{https_compress, [parallel], Tests},
		{onresponse, [parallel], [
			onresponse_crash,
			onresponse_reply
		]},
		{onresponse_capitalize, [parallel], [
			onresponse_capitalize
		]},
		{parse_host, [], [
			parse_host
		]},
		{set_env, [], [
			set_env_dispatch
		]}
	].

init_per_suite(Config) ->
	Dir = config(priv_dir, Config) ++ "/static",
	ct_helper:create_static_dir(Dir),
	[{static_dir, Dir}|Config].

end_per_suite(Config) ->
	ct_helper:delete_static_dir(config(static_dir, Config)).

init_per_group(Name = http, Config) ->
	cowboy_test:init_http(Name, [
		{env, [{dispatch, init_dispatch(Config)}]}
	], Config);
init_per_group(Name = https, Config) ->
	cowboy_test:init_https(Name, [
		{env, [{dispatch, init_dispatch(Config)}]}
	], Config);
init_per_group(Name = http_compress, Config) ->
	cowboy_test:init_http(Name, [
		{env, [{dispatch, init_dispatch(Config)}]},
		{compress, true}
	], Config);
init_per_group(Name = https_compress, Config) ->
	cowboy_test:init_https(Name, [
		{env, [{dispatch, init_dispatch(Config)}]},
		{compress, true}
	], Config);
%% Most, if not all of these, should be in separate test suites.
init_per_group(onresponse, Config) ->
	{ok, _} = cowboy:start_http(onresponse, 100, [{port, 0}], [
		{env, [{dispatch, init_dispatch(Config)}]},
		{max_keepalive, 50},
		{onresponse, fun do_onresponse_hook/4},
		{timeout, 500}
	]),
	Port = ranch:get_port(onresponse),
	[{type, tcp}, {port, Port}, {opts, []}|Config];
init_per_group(onresponse_capitalize, Config) ->
	{ok, _} = cowboy:start_http(onresponse_capitalize, 100, [{port, 0}], [
		{env, [{dispatch, init_dispatch(Config)}]},
		{max_keepalive, 50},
		{onresponse, fun do_onresponse_capitalize_hook/4},
		{timeout, 500}
	]),
	Port = ranch:get_port(onresponse_capitalize),
	[{type, tcp}, {port, Port}, {opts, []}|Config];
init_per_group(parse_host, Config) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/req_attr", http_req_attr, []}
		]}
	]),
	{ok, _} = cowboy:start_http(parse_host, 100, [{port, 0}], [
		{env, [{dispatch, Dispatch}]},
		{max_keepalive, 50},
		{timeout, 500}
	]),
	Port = ranch:get_port(parse_host),
	[{type, tcp}, {port, Port}, {opts, []}|Config];
init_per_group(set_env, Config) ->
	{ok, _} = cowboy:start_http(set_env, 100, [{port, 0}], [
		{env, [{dispatch, []}]},
		{max_keepalive, 50},
		{timeout, 500}
	]),
	Port = ranch:get_port(set_env),
	[{type, tcp}, {port, Port}, {opts, []}|Config].

end_per_group(Name, _) ->
	ok = cowboy:stop_listener(Name).

%% Dispatch configuration.

init_dispatch(Config) ->
	cowboy_router:compile([
		{"localhost", [
			{"/chunked_response", http_chunked, []},
			{"/streamed_response", http_streamed, []},
			{"/headers/dupe", http_handler,
				[{headers, [{<<"connection">>, <<"close">>}]}]},
			{"/set_resp/header", http_set_resp,
				[{headers, [{<<"vary">>, <<"Accept">>}]}]},
			{"/set_resp/overwrite", http_set_resp,
				[{headers, [{<<"server">>, <<"DesireDrive/1.0">>}]}]},
			{"/set_resp/body", http_set_resp,
				[{body, <<"A flameless dance does not equal a cycle">>}]},
			{"/stream_body/set_resp", http_stream_body,
				[{reply, set_resp}, {body, <<"stream_body_set_resp">>}]},
			{"/stream_body/set_resp_close",
				http_stream_body, [
					{reply, set_resp_close},
					{body, <<"stream_body_set_resp_close">>}]},
			{"/stream_body/set_resp_chunked",
				http_stream_body, [
					{reply, set_resp_chunked},
					{body, [<<"stream_body">>, <<"_set_resp_chunked">>]}]},
			{"/static/[...]", cowboy_static,
				{dir, config(static_dir, Config)}},
			{"/static_mimetypes_function/[...]", cowboy_static,
				{dir, config(static_dir, Config),
					[{mimetypes, ?MODULE, do_mimetypes_text_html}]}},
			{"/handler_errors", http_errors, []},
			{"/static_attribute_etag/[...]", cowboy_static,
				{dir, config(static_dir, Config)}},
			{"/static_function_etag/[...]", cowboy_static,
				{dir, config(static_dir, Config),
					[{etag, ?MODULE, do_etag_gen}]}},
			{"/static_specify_file/[...]", cowboy_static,
				{file, config(static_dir, Config) ++ "/style.css"}},
			{"/multipart", http_multipart, []},
			{"/multipart/large", http_multipart_stream, []},
			{"/echo/body", http_echo_body, []},
			{"/echo/body_qs", http_body_qs, []},
			{"/crash/content-length", input_crash_h, content_length},
			{"/param_all", rest_param_all, []},
			{"/bad_accept", rest_simple_resource, []},
			{"/bad_content_type", rest_patch_resource, []},
			{"/simple", rest_simple_resource, []},
			{"/forbidden_post", rest_forbidden_resource, [true]},
			{"/simple_post", rest_forbidden_resource, [false]},
			{"/missing_get_callbacks", rest_missing_callbacks, []},
			{"/missing_put_callbacks", rest_missing_callbacks, []},
			{"/nodelete", rest_nodelete_resource, []},
			{"/post_charset", rest_post_charset_resource, []},
			{"/postonly", rest_postonly_resource, []},
			{"/patch", rest_patch_resource, []},
			{"/resetags", rest_resource_etags, []},
			{"/rest_expires", rest_expires, []},
			{"/rest_expires_binary", rest_expires_binary, []},
			{"/rest_empty_resource", rest_empty_resource, []},
			{"/loop_stream_recv", http_loop_stream_recv, []},
			{"/system", http_system_h, []},
			{"/", http_handler, []}
		]}
	]).

%% Callbacks.

do_etag_gen(_, _, _) ->
	{strong, <<"etag">>}.

do_mimetypes_text_html(_) ->
	<<"text/html">>.

%% Convenience functions.

do_raw(Data, Config) ->
	Client = raw_open(Config),
	ok = raw_send(Client, Data),
	case catch raw_recv_head(Client) of
		{'EXIT', _} -> closed;
		Resp -> element(2, cow_http:parse_status_line(Resp))
	end.

do_get(Path, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path),
	{response, _, Status, _} = gun:await(ConnPid, Ref),
	gun:close(ConnPid),
	Status.

%% Tests.

check_raw_status(Config) ->
	Huge = [$0 || _ <- lists:seq(1, 5000)],
	HugeCookie = lists:flatten(["whatever_man_biiiiiiiiiiiig_cookie_me_want_77="
		"Wed Apr 06 2011 10:38:52 GMT-0500 (CDT)" || _ <- lists:seq(1, 40)]),
	ResponsePacket =
"HTTP/1.0 302 Found\r
Location: http://www.google.co.il/\r
Cache-Control: private\r
Content-Type: text/html; charset=UTF-8\r
Set-Cookie: PREF=ID=568f67013d4a7afa:FF=0:TM=1323014101:LM=1323014101:S=XqctDWC65MzKT0zC; expires=Tue, 03-Dec-2013 15:55:01 GMT; path=/; domain=.google.com\r
Date: Sun, 04 Dec 2011 15:55:01 GMT\r
Server: gws\r
Content-Length: 221\r
X-XSS-Protection: 1; mode=block\r
X-Frame-Options: SAMEORIGIN\r
\r
<HTML><HEAD><meta http-equiv=\"content-type\" content=\"text/html;charset=utf-8\">
<TITLE>302 Moved</TITLE></HEAD><BODY>
<H1>302 Moved</H1>
The document has moved
<A HREF=\"http://www.google.co.il/\">here</A>.
</BODY></HTML>",
	Tests = [
		{200, ["GET / HTTP/1.0\r\nHost: localhost\r\n"
			"Set-Cookie: ", HugeCookie, "\r\n\r\n"]},
		{200, "\r\n\r\n\r\n\r\n\r\nGET / HTTP/1.1\r\nHost: localhost\r\n\r\n"},
		{200, "GET http://proxy/ HTTP/1.1\r\nHost: localhost\r\n\r\n"},
		{400, "\n"},
		{400, "Garbage\r\n\r\n"},
		{400, "\r\n\r\n\r\n\r\n\r\n\r\n"},
		{400, " / HTTP/1.1\r\nHost: localhost\r\n\r\n"},
		{400, "GET  HTTP/1.1\r\nHost: localhost\r\n\r\n"},
		{400, "GET / HTTP/1.1\r\nHost: ninenines.eu\r\n\r\n"},
		{400, "GET http://proxy/ HTTP/1.1\r\n\r\n"},
		{400, "GET / HTTP/1.1\r\nHost: localhost:bad_port\r\n\r\n"},
		{400, ["POST /crash/content-length HTTP/1.1\r\nHost: localhost\r\nContent-Length: 5000,5000\r\n\r\n", Huge]},
		{505, ResponsePacket},
		{408, "GET / HTTP/1.1\r\n"},
		{408, "GET / HTTP/1.1\r\nHost: localhost"},
		{408, "GET / HTTP/1.1\r\nHost: localhost\r\n"},
		{408, "GET / HTTP/1.1\r\nHost: localhost\r\n\r"},
		{414, Huge},
		{400, "GET / HTTP/1.1\r\n" ++ Huge},
		{505, "GET / HTTP/1.2\r\nHost: localhost\r\n\r\n"},
		{closed, ""},
		{closed, "\r\n"},
		{closed, "\r\n\r\n"},
		{closed, "GET / HTTP/1.1"}
	],
	_ = [{Status, Packet} = begin
		Ret = do_raw(Packet, Config),
		{Ret, Packet}
	end || {Status, Packet} <- Tests],
	ok.

check_status(Config) ->
	Tests = [
		{200, "/"},
		{200, "/simple"},
		{400, "/static/%2f"},
		{400, "/static/%2e"},
		{400, "/static/%2e%2e"},
		{403, "/static/directory"},
		{403, "/static/directory/"},
		{403, "/static/unreadable"},
		{404, "/not/found"},
		{404, "/static/not_found"},
		{500, "/handler_errors?case=init_before_reply"}
	],
	_ = [{Status, URL} = begin
		Ret = do_get(URL, Config),
		{Ret, URL}
	end || {Status, URL} <- Tests].

chunked_response(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/chunked_response"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	true = lists:keymember(<<"transfer-encoding">>, 1, Headers),
	{ok, <<"chunked_handler\r\nworks fine!">>} = gun:await_body(ConnPid, Ref),
	ok.

%% Check if sending requests whose size is around the MTU breaks something.
echo_body(Config) ->
	MTU = ct_helper:get_loopback_mtu(),
	_ = [begin
		Body = list_to_binary(lists:duplicate(Size, $a)),
		ConnPid = gun_open(Config),
		Ref = gun:post(ConnPid, "/echo/body", [], Body),
		{response, nofin, 200, _} = gun:await(ConnPid, Ref),
		{ok, Body} = gun:await_body(ConnPid, Ref)
	end || Size <- lists:seq(MTU - 500, MTU)],
	ok.

%% Check if sending request whose size is bigger than 1000000 bytes causes 413
echo_body_max_length(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body", [], << 0:2000000/unit:8 >>),
	{response, nofin, 413, _} = gun:await(ConnPid, Ref),
	ok.

% check if body_qs echo's back results
echo_body_qs(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body_qs", [], <<"echo=67890">>),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"67890">>} = gun:await_body(ConnPid, Ref),
	ok.

echo_body_qs_max_length(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body_qs", [], << "echo=", 0:2000000/unit:8 >>),
	{response, nofin, 413, _} = gun:await(ConnPid, Ref),
	ok.

error_init_after_reply(Config) ->
	{ConnPid, MRef} = gun_monitor_open(Config),
	Ref = gun:get(ConnPid, "/handler_errors?case=init_after_reply"),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref, MRef),
	gun_is_gone(ConnPid, MRef).

headers_dupe(Config) ->
	{ConnPid, MRef} = gun_monitor_open(Config),
	Ref = gun:get(ConnPid, "/headers/dupe"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref, MRef),
	%% Ensure that only one connection header was received.
	[<<"close">>] = [V || {Name, V} <- Headers, Name =:= <<"connection">>],
	gun_is_gone(ConnPid, MRef).

http10_chunkless(Config) ->
	{ConnPid, MRef} = gun_monitor_open(Config, [{http, [{version, 'HTTP/1.0'}]}]),
	Ref = gun:get(ConnPid, "/chunked_response"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref, MRef),
	false = lists:keyfind(<<"transfer-encoding">>, 1, Headers),
	{ok, <<"chunked_handler\r\nworks fine!">>} = gun:await_body(ConnPid, Ref, MRef),
	gun_is_gone(ConnPid, MRef).

http10_hostless(Config) ->
	Name = http10_hostless,
	Port10 = config(port, Config) + 10,
	Transport = case config(type, Config) of
		tcp -> ranch_tcp;
		ssl -> ranch_ssl
	end,
	ranch:start_listener(Name, 5, Transport,
		config(opts, Config) ++ [{port, Port10}],
		cowboy_protocol, [
			{env, [{dispatch, cowboy_router:compile([
				{'_', [{"/http1.0/hostless", http_handler, []}]}])}]},
			{max_keepalive, 50},
			{timeout, 500}]
	),
	200 = do_raw("GET /http1.0/hostless HTTP/1.0\r\n\r\n",
		[{port, Port10}|Config]),
	cowboy:stop_listener(http10_hostless).

http10_keepalive_default(Config) ->
	Normal = "GET / HTTP/1.0\r\nhost: localhost\r\n\r\n",
	Client = raw_open(Config),
	ok = raw_send(Client, Normal),
	case catch raw_recv_head(Client) of
		{'EXIT', _} -> error(closed);
		_ -> ok
	end,
	ok = raw_send(Client, Normal),
	case catch raw_recv_head(Client) of
		{'EXIT', _} -> closed;
		_ -> error(not_closed)
	end.

http10_keepalive_forced(Config) ->
	Keepalive = "GET / HTTP/1.0\r\nhost: localhost\r\nConnection: keep-alive\r\n\r\n",
	Client = raw_open(Config),
	ok = raw_send(Client, Keepalive),
	case catch raw_recv_head(Client) of
		{'EXIT', _} -> error(closed);
		_ -> ok
	end,
	ok = raw_send(Client, Keepalive),
	case catch raw_recv_head(Client) of
		{'EXIT', Err} -> error({closed, Err});
		_ -> ok
	end.

keepalive_max(Config) ->
	{ConnPid, MRef} = gun_monitor_open(Config),
	Refs = [gun:get(ConnPid, "/", [{<<"connection">>, <<"keep-alive">>}])
		|| _ <- lists:seq(1, 49)],
	CloseRef = gun:get(ConnPid, "/", [{<<"connection">>, <<"keep-alive">>}]),
	_ = [begin
		{response, nofin, 200, Headers} = gun:await(ConnPid, Ref, MRef),
		{_, <<"keep-alive">>} = lists:keyfind(<<"connection">>, 1, Headers)
	end || Ref <- Refs],
	{response, nofin, 200, Headers} = gun:await(ConnPid, CloseRef, MRef),
	{_, <<"close">>} = lists:keyfind(<<"connection">>, 1, Headers),
	gun_is_gone(ConnPid, MRef).

keepalive_nl(Config) ->
	ConnPid = gun_open(Config),
	Refs = [begin
		Ref = gun:get(ConnPid, "/", [{<<"connection">>, <<"keep-alive">>}]),
		gun:dbg_send_raw(ConnPid, <<"\r\n">>),
		Ref
	end || _ <- lists:seq(1, 10)],
	_ = [begin
		{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
		{_, <<"keep-alive">>} = lists:keyfind(<<"connection">>, 1, Headers)
	end || Ref <- Refs],
	ok.

keepalive_stream_loop(Config) ->
	ConnPid = gun_open(Config),
	Refs = [begin
		Ref = gun:post(ConnPid, "/loop_stream_recv",
			[{<<"transfer-encoding">>, <<"chunked">>}]),
		_ = [gun:data(ConnPid, Ref, nofin, << ID:32 >>)
			|| ID <- lists:seq(1, 250)],
		gun:data(ConnPid, Ref, fin, <<>>),
		Ref
	end || _ <- lists:seq(1, 10)],
	_ = [begin
		{response, fin, 200, _} = gun:await(ConnPid, Ref)
	end || Ref <- Refs],
	ok.

multipart(Config) ->
	ConnPid = gun_open(Config),
	Body = <<
		"This is a preamble."
		"\r\n--OHai\r\nX-Name:answer\r\n\r\n42"
		"\r\n--OHai\r\nServer:Cowboy\r\n\r\nIt rocks!\r\n"
		"\r\n--OHai--\r\n"
		"This is an epilogue."
	>>,
	Ref = gun:post(ConnPid, "/multipart",
		[{<<"content-type">>, <<"multipart/x-makes-no-sense; boundary=OHai">>}],
		Body),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, RespBody} = gun:await_body(ConnPid, Ref),
	Parts = binary_to_term(RespBody),
	Parts = [
		{[{<<"x-name">>, <<"answer">>}], <<"42">>},
		{[{<<"server">>, <<"Cowboy">>}], <<"It rocks!\r\n">>}
	],
	ok.

multipart_chunked(Config) ->
	ConnPid = gun_open(Config),
	Body = <<
		"This is a preamble."
		"\r\n--OHai\r\nX-Name:answer\r\n\r\n42"
		"\r\n--OHai\r\nServer:Cowboy\r\n\r\nIt rocks!\r\n"
		"\r\n--OHai--\r\n"
		"This is an epilogue."
	>>,
	Ref = gun:post(ConnPid, "/multipart", [
		{<<"content-type">>, <<"multipart/x-makes-no-sense; boundary=OHai">>},
		{<<"transfer-encoding">>, <<"chunked">>}]),
	gun:data(ConnPid, Ref, fin, Body),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, RespBody} = gun:await_body(ConnPid, Ref),
	Parts = binary_to_term(RespBody),
	Parts = [
		{[{<<"x-name">>, <<"answer">>}], <<"42">>},
		{[{<<"server">>, <<"Cowboy">>}], <<"It rocks!\r\n">>}
	],
	ok.

multipart_large(Config) ->
	ConnPid = gun_open(Config),
	Boundary = "----------",
	Big = << 0:9000000/unit:8 >>,
	Bigger = << 0:9999999/unit:8 >>,
	Body = ["--", Boundary, "\r\ncontent-length: 9000000\r\n\r\n", Big, "\r\n",
		"--", Boundary, "\r\ncontent-length: 9999999\r\n\r\n", Bigger, "\r\n",
		"--", Boundary, "--\r\n"],
	Ref = gun:post(ConnPid, "/multipart/large",
		[{<<"content-type">>, ["multipart/x-large; boundary=", Boundary]}],
		Body),
	{response, fin, 200, _} = gun:await(ConnPid, Ref),
	ok.

do_nc(Config, Input) ->
	Cat = os:find_executable("cat"),
	Nc = os:find_executable("nc"),
	case {Cat, Nc} of
		{false, _} ->
			{skip, {notfound, cat}};
		{_, false} ->
			{skip, {notfound, nc}};
		_Good ->
			%% Throw garbage at the server then check if it's still up.
			StrPort = integer_to_list(config(port, Config)),
			[os:cmd("cat " ++ Input ++ " | nc localhost " ++ StrPort)
				|| _ <- lists:seq(1, 100)],
			200 = do_get("/", Config)
	end.

nc_rand(Config) ->
	do_nc(Config, "/dev/urandom").

nc_zero(Config) ->
	do_nc(Config, "/dev/zero").

onresponse_capitalize(Config) ->
	Client = raw_open(Config),
	ok = raw_send(Client, "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"),
	Data = raw_recv_head(Client),
	false = nomatch =:= binary:match(Data, <<"Content-Length">>),
	ok.

%% Hook for the above onresponse_capitalize test.
do_onresponse_capitalize_hook(Status, Headers, Body, Req) ->
	Headers2 = [{cowboy_bstr:capitalize_token(N), V}
		|| {N, V} <- Headers],
	cowboy_req:reply(Status, Headers2, Body, Req).

onresponse_crash(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/handler_errors?case=init_before_reply"),
	{response, fin, 777, Headers} = gun:await(ConnPid, Ref),
	{<<"x-hook">>, <<"onresponse">>} = lists:keyfind(<<"x-hook">>, 1, Headers).

onresponse_reply(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/"),
	{response, nofin, 777, Headers} = gun:await(ConnPid, Ref),
	{<<"x-hook">>, <<"onresponse">>} = lists:keyfind(<<"x-hook">>, 1, Headers),
	ok.

%% Hook for the above onresponse tests.
do_onresponse_hook(_, Headers, _, Req) ->
	cowboy_req:reply(<<"777 Lucky">>, [{<<"x-hook">>, <<"onresponse">>}|Headers], Req).

parse_host(Config) ->
	ConnPid = gun_open(Config),
	Tests = [
		{<<"example.org:8080">>, <<"example.org\n8080">>},
		{<<"example.org">>, <<"example.org\n80">>},
		{<<"192.0.2.1:8080">>, <<"192.0.2.1\n8080">>},
		{<<"192.0.2.1">>, <<"192.0.2.1\n80">>},
		{<<"[2001:db8::1]:8080">>, <<"[2001:db8::1]\n8080">>},
		{<<"[2001:db8::1]">>, <<"[2001:db8::1]\n80">>},
		{<<"[::ffff:192.0.2.1]:8080">>, <<"[::ffff:192.0.2.1]\n8080">>},
		{<<"[::ffff:192.0.2.1]">>, <<"[::ffff:192.0.2.1]\n80">>}
	],
	[begin
		Ref = gun:get(ConnPid, "/req_attr?attr=host_and_port",
			[{<<"host">>, Host}]),
		{response, nofin, 200, _} = gun:await(ConnPid, Ref),
		{ok, Body} = gun:await_body(ConnPid, Ref)
	end || {Host, Body} <- Tests],
	ok.

pipeline(Config) ->
	ConnPid = gun_open(Config),
	Refs = [gun:get(ConnPid, "/") || _ <- lists:seq(1, 5)],
	_ = [{response, nofin, 200, _} = gun:await(ConnPid, Ref) || Ref <- Refs],
	ok.

rest_param_all(Config) ->
	ConnPid = gun_open(Config),
	%% Accept without param.
	Ref1 = gun:get(ConnPid, "/param_all",
		[{<<"accept">>, <<"text/plain">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref1),
	{ok, <<"[]">>} = gun:await_body(ConnPid, Ref1),
	%% Accept with param.
	Ref2 = gun:get(ConnPid, "/param_all",
		[{<<"accept">>, <<"text/plain;level=1">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref2),
	{ok, <<"level=1">>} = gun:await_body(ConnPid, Ref2),
	%% Accept with param and quality.
	Ref3 = gun:get(ConnPid, "/param_all",
		[{<<"accept">>, <<"text/plain;level=1;q=0.8, text/plain;level=2;q=0.5">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref3),
	{ok, <<"level=1">>} = gun:await_body(ConnPid, Ref3),
	Ref4 = gun:get(ConnPid, "/param_all",
		[{<<"accept">>, <<"text/plain;level=1;q=0.5, text/plain;level=2;q=0.8">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref4),
	{ok, <<"level=2">>} = gun:await_body(ConnPid, Ref4),
	%% Without Accept.
	Ref5 = gun:get(ConnPid, "/param_all"),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref5),
	{ok, <<"'*'">>} = gun:await_body(ConnPid, Ref5),
	%% Content-Type without param.
	Ref6 = gun:put(ConnPid, "/param_all",
		[{<<"content-type">>, <<"text/plain">>}]),
	{response, fin, 204, _} = gun:await(ConnPid, Ref6),
	%% Content-Type with param.
	Ref7 = gun:put(ConnPid, "/param_all",
		[{<<"content-type">>, <<"text/plain; charset=utf-8">>}]),
	{response, fin, 204, _} = gun:await(ConnPid, Ref7),
	ok.

rest_bad_accept(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/bad_accept",
		[{<<"accept">>, <<"1">>}]),
	{response, fin, 400, _} = gun:await(ConnPid, Ref),
	ok.

rest_bad_content_type(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:patch(ConnPid, "/bad_content_type",
		[{<<"content-type">>, <<"text/plain, text/html">>}], <<"Whatever">>),
	{response, fin, 415, _} = gun:await(ConnPid, Ref),
	ok.

rest_expires(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/rest_expires"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, Expires} = lists:keyfind(<<"expires">>, 1, Headers),
	{_, LastModified} = lists:keyfind(<<"last-modified">>, 1, Headers),
	Expires = LastModified = <<"Fri, 21 Sep 2012 22:36:14 GMT">>,
	ok.

rest_expires_binary(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/rest_expires_binary"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"0">>} = lists:keyfind(<<"expires">>, 1, Headers),
	ok.

rest_keepalive(Config) ->
	ConnPid = gun_open(Config),
	Refs = [gun:get(ConnPid, "/simple") || _ <- lists:seq(1, 10)],
	_ = [begin
		{response, nofin, 200, Headers} =  gun:await(ConnPid, Ref),
		{_, <<"keep-alive">>} = lists:keyfind(<<"connection">>, 1, Headers)
	end || Ref <- Refs],
	ok.

rest_keepalive_post(Config) ->
	ConnPid = gun_open(Config),
	Refs = [{
		gun:post(ConnPid, "/forbidden_post",
			[{<<"content-type">>, <<"text/plain">>}]),
		gun:post(ConnPid, "/simple_post",
			[{<<"content-type">>, <<"text/plain">>}])
	} || _ <- lists:seq(1, 5)],
	_ = [begin
		{response, fin, 403, Headers1} = gun:await(ConnPid, Ref1),
		{_, <<"keep-alive">>} = lists:keyfind(<<"connection">>, 1, Headers1),
		{response, fin, 303, Headers2} = gun:await(ConnPid, Ref2),
		{_, <<"keep-alive">>} = lists:keyfind(<<"connection">>, 1, Headers2)
	end || {Ref1, Ref2} <- Refs],
	ok.

rest_missing_get_callbacks(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/missing_get_callbacks"),
	{response, fin, 500, _} = gun:await(ConnPid, Ref),
	ok.

rest_missing_put_callbacks(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:put(ConnPid, "/missing_put_callbacks",
		[{<<"content-type">>, <<"application/json">>}], <<"{}">>),
	{response, fin, 500, _} = gun:await(ConnPid, Ref),
	ok.

rest_nodelete(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:delete(ConnPid, "/nodelete"),
	{response, fin, 500, _} = gun:await(ConnPid, Ref),
	ok.

rest_options_default(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:options(ConnPid, "/rest_empty_resource"),
	{response, fin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"HEAD, GET, OPTIONS">>} = lists:keyfind(<<"allow">>, 1, Headers),
	ok.

rest_patch(Config) ->
	Tests = [
		{204, [{<<"content-type">>, <<"text/plain">>}], <<"whatever">>},
		{400, [{<<"content-type">>, <<"text/plain">>}], <<"false">>},
		{400, [{<<"content-type">>, <<"text/plain">>}], <<"halt">>},
		{415, [{<<"content-type">>, <<"application/json">>}], <<"bad_content_type">>}
	],
	ConnPid = gun_open(Config),
	_ = [begin
		Ref = gun:patch(ConnPid, "/patch", Headers, Body),
		{response, fin, Status, _} = gun:await(ConnPid, Ref)
	end || {Status, Headers, Body} <- Tests],
	ok.

rest_post_charset(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/post_charset",
		[{<<"content-type">>, <<"text/plain;charset=UTF-8">>}], "12345"),
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	ok.

rest_postonly(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/postonly",
		[{<<"content-type">>, <<"text/plain">>}], "12345"),
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	ok.

rest_resource_get_etag(Config, Type) ->
	rest_resource_get_etag(Config, Type, []).

rest_resource_get_etag(Config, Type, Headers) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resetags?type=" ++ Type, Headers),
	{response, _, Status, RespHeaders} = gun:await(ConnPid, Ref),
	case lists:keyfind(<<"etag">>, 1, RespHeaders) of
		false -> {Status, false};
		{<<"etag">>, ETag} -> {Status, ETag}
	end.

rest_resource_etags(Config) ->
	Tests = [
		{200, <<"W/\"etag-header-value\"">>, "tuple-weak"},
		{200, <<"\"etag-header-value\"">>, "tuple-strong"},
		{200, <<"W/\"etag-header-value\"">>, "binary-weak-quoted"},
		{200, <<"\"etag-header-value\"">>, "binary-strong-quoted"},
		{500, false, "binary-strong-unquoted"},
		{500, false, "binary-weak-unquoted"}
	],
	_ = [{Status, ETag, Type} = begin
		{Ret, RespETag} = rest_resource_get_etag(Config, Type),
		{Ret, RespETag, Type}
	end || {Status, ETag, Type} <- Tests].

rest_resource_etags_if_none_match(Config) ->
	Tests = [
		{304, <<"W/\"etag-header-value\"">>, "tuple-weak"},
		{304, <<"\"etag-header-value\"">>, "tuple-strong"},
		{304, <<"W/\"etag-header-value\"">>, "binary-weak-quoted"},
		{304, <<"\"etag-header-value\"">>, "binary-strong-quoted"}
	],
	_ = [{Status, Type} = begin
		{Ret, _} = rest_resource_get_etag(Config, Type,
			[{<<"if-none-match">>, ETag}]),
		{Ret, Type}
	end || {Status, ETag, Type} <- Tests].

set_env_dispatch(Config) ->
	ConnPid1 = gun_open(Config),
	Ref1 = gun:get(ConnPid1, "/"),
	{response, fin, 400, _} = gun:await(ConnPid1, Ref1),
	ok = cowboy:set_env(set_env, dispatch,
		cowboy_router:compile([{'_', [{"/", http_handler, []}]}])),
	ConnPid2 = gun_open(Config),
	Ref2 = gun:get(ConnPid2, "/"),
	{response, nofin, 200, _} = gun:await(ConnPid2, Ref2),
	ok.

set_resp_body(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/set_resp/body"),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"A flameless dance does not equal a cycle">>}
		= gun:await_body(ConnPid, Ref),
	ok.

set_resp_header(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/set_resp/header"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"Accept">>} = lists:keyfind(<<"vary">>, 1, Headers),
	{_, _} = lists:keyfind(<<"set-cookie">>, 1, Headers),
	ok.

set_resp_overwrite(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/set_resp/overwrite"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"DesireDrive/1.0">>} = lists:keyfind(<<"server">>, 1, Headers),
	ok.

slowloris(Config) ->
	Client = raw_open(Config),
	try
		[begin
			ok = raw_send(Client, [C]),
			receive after 25 -> ok end
		end || C <- "GET / HTTP/1.1\r\nHost: localhost\r\n"
			"User-Agent: Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US)\r\n"
			"Cookie: name=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n\r\n"],
		error(failure)
	catch error:{badmatch, _} ->
		ok
	end.

slowloris2(Config) ->
	Client = raw_open(Config),
	ok = raw_send(Client, "GET / HTTP/1.1\r\n"),
	receive after 300 -> ok end,
	ok = raw_send(Client, "Host: localhost\r\n"),
	receive after 300 -> ok end,
	Data = raw_recv_head(Client),
	{_, 408, _, _} = cow_http:parse_status_line(Data),
	ok.

static_attribute_etag(Config) ->
	ConnPid = gun_open(Config),
	Ref1 = gun:get(ConnPid, "/static_attribute_etag/index.html"),
	Ref2 = gun:get(ConnPid, "/static_attribute_etag/index.html"),
	{response, nofin, 200, Headers1} = gun:await(ConnPid, Ref1),
	{response, nofin, 200, Headers2} = gun:await(ConnPid, Ref2),
	{_, ETag} = lists:keyfind(<<"etag">>, 1, Headers1),
	{_, ETag} = lists:keyfind(<<"etag">>, 1, Headers2),
	true = ETag =/= undefined,
	ok.

static_function_etag(Config) ->
	ConnPid = gun_open(Config),
	Ref1 = gun:get(ConnPid, "/static_function_etag/index.html"),
	Ref2 = gun:get(ConnPid, "/static_function_etag/index.html"),
	{response, nofin, 200, Headers1} = gun:await(ConnPid, Ref1),
	{response, nofin, 200, Headers2} = gun:await(ConnPid, Ref2),
	{_, ETag} = lists:keyfind(<<"etag">>, 1, Headers1),
	{_, ETag} = lists:keyfind(<<"etag">>, 1, Headers2),
	true = ETag =/= undefined,
	ok.

static_mimetypes_function(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/static_mimetypes_function/index.html"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"text/html">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

static_specify_file(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/static_specify_file"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	{ok, <<"body{color:red}\n">>} = gun:await_body(ConnPid, Ref),
	ok.

static_specify_file_catchall(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/static_specify_file/none"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	{ok, <<"body{color:red}\n">>} = gun:await_body(ConnPid, Ref),
	ok.

static_test_file(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/static/unknown"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"application/octet-stream">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

static_test_file_css(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/static/style.css"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

stream_body_set_resp(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/stream_body/set_resp"),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"stream_body_set_resp">>} = gun:await_body(ConnPid, Ref),
	ok.

stream_body_set_resp_close(Config) ->
	{ConnPid, MRef} = gun_monitor_open(Config),
	Ref = gun:get(ConnPid, "/stream_body/set_resp_close"),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref, MRef),
	{ok, <<"stream_body_set_resp_close">>} = gun:await_body(ConnPid, Ref, MRef),
	gun_is_gone(ConnPid, MRef).

stream_body_set_resp_chunked(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/stream_body/set_resp_chunked"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"chunked">>} = lists:keyfind(<<"transfer-encoding">>, 1, Headers),
	{ok, <<"stream_body_set_resp_chunked">>} = gun:await_body(ConnPid, Ref),
	ok.

stream_body_set_resp_chunked10(Config) ->
	{ConnPid, MRef} = gun_monitor_open(Config, [{http, [{version, 'HTTP/1.0'}]}]),
	Ref = gun:get(ConnPid, "/stream_body/set_resp_chunked"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref, MRef),
	false = lists:keyfind(<<"transfer-encoding">>, 1, Headers),
	{ok, <<"stream_body_set_resp_chunked">>} = gun:await_body(ConnPid, Ref, MRef),
	gun_is_gone(ConnPid, MRef).

%% Undocumented hack: force chunked response to be streamed as HTTP/1.1.
streamed_response(Config) ->
	Client = raw_open(Config),
	ok = raw_send(Client, "GET /streamed_response HTTP/1.1\r\nHost: localhost\r\n\r\n"),
	Data = raw_recv_head(Client),
	{'HTTP/1.1', 200, _, Rest} = cow_http:parse_status_line(Data),
	{Headers, Rest2} = cow_http:parse_headers(Rest),
	false = lists:keymember(<<"transfer-encoding">>, 1, Headers),
	Rest2Size = byte_size(Rest2),
	ok = case <<"streamed_handler\r\nworks fine!">> of
		Rest2 -> ok;
		<< Rest2:Rest2Size/binary, Expect/bits >> -> raw_expect_recv(Client, Expect)
	end.

te_chunked(Config) ->
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body",
		[{<<"transfer-encoding">>, <<"chunked">>}], Body),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.

do_body_to_chunks(_, <<>>, Acc) ->
	lists:reverse([<<"0\r\n\r\n">>|Acc]);
do_body_to_chunks(ChunkSize, Body, Acc) ->
	BodySize = byte_size(Body),
	ChunkSize2 = case BodySize < ChunkSize of
		true -> BodySize;
		false -> ChunkSize
	end,
	<< Chunk:ChunkSize2/binary, Rest/binary >> = Body,
	ChunkSizeBin = list_to_binary(integer_to_list(ChunkSize2, 16)),
	do_body_to_chunks(ChunkSize, Rest,
		[<< ChunkSizeBin/binary, "\r\n", Chunk/binary, "\r\n" >>|Acc]).

te_chunked_chopped(Config) ->
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Body2 = iolist_to_binary(do_body_to_chunks(50, Body, [])),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body",
		[{<<"transfer-encoding">>, <<"chunked">>}]),
	_ = [begin
		ok = gun:dbg_send_raw(ConnPid, << C >>),
		receive after 10 -> ok end
	end || << C >> <= Body2],
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.

te_chunked_delayed(Config) ->
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = do_body_to_chunks(50, Body, []),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body",
		[{<<"transfer-encoding">>, <<"chunked">>}]),
	_ = [begin
		ok = gun:dbg_send_raw(ConnPid, Chunk),
		receive after 10 -> ok end
	end || Chunk <- Chunks],
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.

te_chunked_split_body(Config) ->
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = do_body_to_chunks(50, Body, []),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body",
		[{<<"transfer-encoding">>, <<"chunked">>}]),
	_ = [begin
		case Chunk of
			<<"0\r\n\r\n">> ->
				ok = gun:dbg_send_raw(ConnPid, Chunk);
			_ ->
				[Size, ChunkBody, <<>>] =
					binary:split(Chunk, [<<"\r\n">>], [global]),
				PartASize = random:uniform(byte_size(ChunkBody)),
				<<PartA:PartASize/binary, PartB/binary>> = ChunkBody,
				ok = gun:dbg_send_raw(ConnPid, [Size, <<"\r\n">>, PartA]),
				receive after 10 -> ok end,
				ok = gun:dbg_send_raw(ConnPid, [PartB, <<"\r\n">>])
		end
	end || Chunk <- Chunks],
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.

te_chunked_split_crlf(Config) ->
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = do_body_to_chunks(50, Body, []),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body",
		[{<<"transfer-encoding">>, <<"chunked">>}]),
	_ = [begin
		%% Split in the newline just before the end of the chunk.
		Len = byte_size(Chunk) - (random:uniform(2) - 1),
		<< Chunk2:Len/binary, End/binary >> = Chunk,
		ok = gun:dbg_send_raw(ConnPid, Chunk2),
		receive after 10 -> ok end,
		ok = gun:dbg_send_raw(ConnPid, End)
	end || Chunk <- Chunks],
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.

te_identity(Config) ->
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body", [], Body),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.

sys_suspend_resume(Config) ->
	%% Ensure that cowboy_protocol can handle sys:suspend/1 and sys:resume/1.
	ConnPid = gun_open(Config),
	{Pid, Ref} = system_gun_get(ConnPid, "/system"),
	ok = sys:suspend(Pid),
	ok = sys:resume(Pid),
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	ok.

sys_change_code(Config) ->
	%% Ensure that cowboy_protocol can handle sys:change_code/4.
	ConnPid = gun_open(Config),
	{Pid, Ref} = system_gun_get(ConnPid, "/system"),
	ok = sys:suspend(Pid),
	ok = sys:change_code(Pid, ?MODULE, undefined, undefined),
	ok = sys:resume(Pid),
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	ok.

sys_statistics(Config) ->
	%% Ensure that cowboy_protocol can handle sys:statistics/2
	ConnPid = gun_open(Config),
	{Pid, Ref} = system_gun_get(ConnPid, "/system"),
	ok = sys:statistics(Pid, true),
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	{Pid, Ref2} = system_gun_get(ConnPid, "/system"),
	{ok, [{_,_} | _]} = sys:statistics(Pid, get),
	{response, fin, 204, _} = gun:await(ConnPid, Ref2),
	{Pid, Ref3} = system_gun_get(ConnPid, "/system"),
	ok = sys:statistics(Pid, false),
	{response, fin, 204, _} = gun:await(ConnPid, Ref3),
	{Pid, Ref4} = system_gun_get(ConnPid, "/system"),
	{ok, no_statistics} = sys:statistics(Pid, get),
	{response, fin, 204, _} = gun:await(ConnPid, Ref4),
	ok.

%% Internal

system_gun_get(ConnPid, Path) ->
	Tag = make_ref(),
	QS = cow_qs:qs([{<<"from">>, term_to_binary({self(), Tag})}]),
	Ref = gun:get(ConnPid, [Path, $? | QS]),
	Pid = receive {Tag, P} -> P after 500 -> exit(timeout) end,
	{Pid, Ref}.
