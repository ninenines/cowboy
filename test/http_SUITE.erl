%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
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

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
	init_per_group/2, end_per_group/2]). %% ct.
-export([chunked_response/1, headers_dupe/1, headers_huge/1,
	keepalive_nl/1, max_keepalive/1, nc_rand/1, nc_zero/1,
	pipeline/1, raw/1, set_resp_header/1, set_resp_overwrite/1,
	set_resp_body/1, stream_body_set_resp/1, response_as_req/1,
	static_mimetypes_function/1, static_attribute_etag/1,
	static_function_etag/1, multipart/1, te_identity/1,
	te_chunked/1, te_chunked_delayed/1]). %% http.
-export([http_200/1, http_404/1, handler_errors/1,
	file_200/1, file_403/1, dir_403/1, file_404/1,
	file_400/1]). %% http and https.
-export([http_10_hostless/1, http_10_chunkless/1]). %% misc.
-export([rest_simple/1, rest_keepalive/1, rest_keepalive_post/1,
	rest_nodelete/1, rest_resource_etags/1]). %% rest.
-export([onrequest/1, onrequest_reply/1]). %% hooks.

%% ct.

all() ->
	[{group, http}, {group, https}, {group, misc}, {group, rest},
		{group, hooks}].

groups() ->
	BaseTests = [http_200, http_404, handler_errors,
		file_200, file_403, dir_403, file_404, file_400],
	[{http, [], [chunked_response, headers_dupe, headers_huge,
		keepalive_nl, max_keepalive, nc_rand, nc_zero, pipeline, raw,
		set_resp_header, set_resp_overwrite,
		set_resp_body, response_as_req, stream_body_set_resp,
		static_mimetypes_function, static_attribute_etag,
		static_function_etag, multipart, te_identity, te_chunked,
		te_chunked_delayed] ++ BaseTests},
	{https, [], BaseTests},
	{misc, [], [http_10_hostless, http_10_chunkless]},
	{rest, [], [rest_simple, rest_keepalive, rest_keepalive_post,
		rest_nodelete, rest_resource_etags]},
	{hooks, [], [onrequest, onrequest_reply]}].

init_per_suite(Config) ->
	application:start(inets),
	application:start(cowboy),
	Config.

end_per_suite(_Config) ->
	application:stop(cowboy),
	application:stop(inets),
	ok.

init_per_group(http, Config) ->
	Port = 33080,
	Config1 = init_static_dir(Config),
	cowboy:start_listener(http, 100,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [{max_keepalive, 50},
			{dispatch, init_http_dispatch(Config1)}]
	),
	[{scheme, "http"}, {port, Port}|Config1];
init_per_group(https, Config) ->
	Port = 33081,
	Config1 = init_static_dir(Config),
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	DataDir = ?config(data_dir, Config),
	{ok,_} = cowboy:start_listener(https, 100,
		cowboy_ssl_transport, [
			{port, Port}, {certfile, DataDir ++ "cert.pem"},
			{keyfile, DataDir ++ "key.pem"}, {password, "cowboy"}],
		cowboy_http_protocol, [{dispatch, init_https_dispatch(Config1)}]
	),
	[{scheme, "https"}, {port, Port}|Config1];
init_per_group(misc, Config) ->
	Port = 33082,
	{ok,_} = cowboy:start_listener(misc, 100,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [{dispatch, [{'_', [
			{[<<"chunked_response">>], chunked_handler, []},
			{[], http_handler, []}
	]}]}]),
	[{port, Port}|Config];
init_per_group(rest, Config) ->
	Port = 33083,
	{ok,_} = cowboy:start_listener(rest, 100,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [{dispatch, [{'_', [
			{[<<"simple">>], rest_simple_resource, []},
			{[<<"forbidden_post">>], rest_forbidden_resource, [true]},
			{[<<"simple_post">>], rest_forbidden_resource, [false]},
			{[<<"nodelete">>], rest_nodelete_resource, []},
			{[<<"resetags">>], rest_resource_etags, []}
	]}]}]),
	[{scheme, "http"},{port, Port}|Config];
init_per_group(hooks, Config) ->
	Port = 33084,
	{ok, _} = cowboy:start_listener(hooks, 100,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [
			{dispatch, init_http_dispatch(Config)},
			{onrequest, fun onrequest_hook/1}
		]),
	[{scheme, "http"}, {port, Port}|Config].

end_per_group(https, Config) ->
	cowboy:stop_listener(https),
	application:stop(ssl),
	application:stop(public_key),
	application:stop(crypto),
	end_static_dir(Config),
	ok;
end_per_group(http, Config) ->
	cowboy:stop_listener(http),
	end_static_dir(Config);
end_per_group(Listener, _Config) ->
	cowboy:stop_listener(Listener),
	ok.

%% Dispatch configuration.

init_http_dispatch(Config) ->
	[
		{[<<"localhost">>], [
			{[<<"chunked_response">>], chunked_handler, []},
			{[<<"init_shutdown">>], http_handler_init_shutdown, []},
			{[<<"long_polling">>], http_handler_long_polling, []},
			{[<<"headers">>, <<"dupe">>], http_handler,
				[{headers, [{<<"Connection">>, <<"close">>}]}]},
			{[<<"set_resp">>, <<"header">>], http_handler_set_resp,
				[{headers, [{<<"Vary">>, <<"Accept">>}]}]},
			{[<<"set_resp">>, <<"overwrite">>], http_handler_set_resp,
				[{headers, [{<<"Server">>, <<"DesireDrive/1.0">>}]}]},
			{[<<"set_resp">>, <<"body">>], http_handler_set_resp,
				[{body, <<"A flameless dance does not equal a cycle">>}]},
			{[<<"stream_body">>, <<"set_resp">>], http_handler_stream_body,
				[{reply, set_resp}, {body, <<"stream_body_set_resp">>}]},
			{[<<"static">>, '...'], cowboy_http_static,
				[{directory, ?config(static_dir, Config)},
				 {mimetypes, [{<<".css">>, [<<"text/css">>]}]}]},
			{[<<"static_mimetypes_function">>, '...'], cowboy_http_static,
				[{directory, ?config(static_dir, Config)},
				 {mimetypes, {fun(Path, data) when is_binary(Path) ->
					[<<"text/html">>] end, data}}]},
			{[<<"handler_errors">>], http_handler_errors, []},
			{[<<"static_attribute_etag">>, '...'], cowboy_http_static,
				[{directory, ?config(static_dir, Config)},
				 {etag, {attributes, [filepath, filesize, inode, mtime]}}]},
			{[<<"static_function_etag">>, '...'], cowboy_http_static,
				[{directory, ?config(static_dir, Config)},
				 {etag, {fun static_function_etag/2, etag_data}}]},
			{[<<"multipart">>], http_handler_multipart, []},
			{[<<"echo">>, <<"body">>], http_handler_echo_body, []},
			{[], http_handler, []}
		]}
	].

init_https_dispatch(Config) ->
	init_http_dispatch(Config).


init_static_dir(Config) ->
	Dir = filename:join(?config(priv_dir, Config), "static"),
	Level1 = fun(Name) -> filename:join(Dir, Name) end,
	ok = file:make_dir(Dir),
	ok = file:write_file(Level1("test_file"), "test_file\n"),
	ok = file:write_file(Level1("test_file.css"), "test_file.css\n"),
	ok = file:write_file(Level1("test_noread"), "test_noread\n"),
	ok = file:change_mode(Level1("test_noread"), 8#0333),
	ok = file:write_file(Level1("test.html"), "test.html\n"),
	ok = file:make_dir(Level1("test_dir")),
	[{static_dir, Dir}|Config].

end_static_dir(Config) ->
	Dir = ?config(static_dir, Config),
	Level1 = fun(Name) -> filename:join(Dir, Name) end,
	ok = file:delete(Level1("test_file")),
	ok = file:delete(Level1("test_file.css")),
	ok = file:delete(Level1("test_noread")),
	ok = file:delete(Level1("test.html")),
	ok = file:del_dir(Level1("test_dir")),
	ok = file:del_dir(Dir),
	Config.

%% http.

chunked_response(Config) ->
	{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, "chunked_handler\r\nworks fine!"}} =
		httpc:request(build_url("/chunked_response", Config)).

headers_dupe(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, "GET /headers/dupe HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: keep-alive\r\n\r\n"),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{_Start, _Length} = binary:match(Data, <<"Connection: close">>),
	nomatch = binary:match(Data, <<"Connection: keep-alive">>),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000).

headers_huge(Config) ->
	Cookie = lists:flatten(["whatever_man_biiiiiiiiiiiig_cookie_me_want_77="
		"Wed Apr 06 2011 10:38:52 GMT-0500 (CDT)" || _N <- lists:seq(1, 40)]),
	{_Packet, 200} = raw_req(["GET / HTTP/1.0\r\nHost: localhost\r\n"
		"Set-Cookie: ", Cookie, "\r\n\r\n"], Config).

keepalive_nl(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = keepalive_nl_loop(Socket, 10),
	ok = gen_tcp:close(Socket).

keepalive_nl_loop(_Socket, 0) ->
	ok;
keepalive_nl_loop(Socket, N) ->
	ok = gen_tcp:send(Socket, "GET / HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: keep-alive\r\n\r\n"),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{0, 12} = binary:match(Data, <<"HTTP/1.1 200">>),
	nomatch = binary:match(Data, <<"Connection: close">>),
	ok = gen_tcp:send(Socket, "\r\n"), %% extra nl
	keepalive_nl_loop(Socket, N - 1).

max_keepalive(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = max_keepalive_loop(Socket, 50),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000).

max_keepalive_loop(_Socket, 0) ->
	ok;
max_keepalive_loop(Socket, N) ->
	ok = gen_tcp:send(Socket, "GET / HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: keep-alive\r\n\r\n"),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{0, 12} = binary:match(Data, <<"HTTP/1.1 200">>),
	case N of
		1 -> {_, _} = binary:match(Data, <<"Connection: close">>);
		N -> nomatch = binary:match(Data, <<"Connection: close">>)
	end,
	keepalive_nl_loop(Socket, N - 1).

multipart(Config) ->
	Url = build_url("/multipart", Config),
	Body = <<
		"This is a preamble."
		"\r\n--OHai\r\nX-Name:answer\r\n\r\n42"
		"\r\n--OHai\r\nServer:Cowboy\r\n\r\nIt rocks!\r\n"
		"\r\n--OHai--"
		"This is an epiloque."
	>>,
	Request = {Url, [], "multipart/x-makes-no-sense; boundary=OHai", Body},
	{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Response}} =
		httpc:request(post, Request, [], [{body_format, binary}]),
	Parts = binary_to_term(Response),
	Parts = [
		{[{<<"X-Name">>, <<"answer">>}], <<"42">>},
		{[{'Server', <<"Cowboy">>}], <<"It rocks!\r\n">>}
	].

nc_rand(Config) ->
	nc_reqs(Config, "/dev/urandom").

nc_zero(Config) ->
	nc_reqs(Config, "/dev/zero").

nc_reqs(Config, Input) ->
	Cat = os:find_executable("cat"),
	Nc = os:find_executable("nc"),
	case {Cat, Nc} of
		{false, _} ->
			{skip, {notfound, cat}};
		{_, false} ->
			{skip, {notfound, nc}};
		_Good ->
			%% Throw garbage at the server then check if it's still up.
			{port, Port} = lists:keyfind(port, 1, Config),
			[nc_run_req(Port, Input) || _N <- lists:seq(1, 100)],
			Packet = "GET / HTTP/1.0\r\nHost: localhost\r\n\r\n",
			{Packet, 200} = raw_req(Packet, Config)
	end.

nc_run_req(Port, Input) ->
	os:cmd("cat " ++ Input ++ " | nc localhost " ++ integer_to_list(Port)).

pipeline(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket,
		"GET / HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n"
		"GET / HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n"
		"GET / HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n"
		"GET / HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n"
		"GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"),
	Data = pipeline_recv(Socket, <<>>),
	Reqs = binary:split(Data, << "\r\n\r\nhttp_handler" >>, [global, trim]),
	5 = length(Reqs),
	pipeline_check(Reqs).

pipeline_check([]) ->
	ok;
pipeline_check([Req|Tail]) ->
	<< "HTTP/1.1 200", _Rest/bits >> = Req,
	pipeline_check(Tail).

pipeline_recv(Socket, SoFar) ->
	case gen_tcp:recv(Socket, 0, 6000) of
		{ok, Data} ->
			pipeline_recv(Socket, << SoFar/binary, Data/binary >>);
		{error, closed} ->
			ok = gen_tcp:close(Socket),
			SoFar
	end.

raw_req(Packet, Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, Packet),
	Res = case gen_tcp:recv(Socket, 0, 6000) of
		{ok, << "HTTP/1.1 ", Str:24/bits, _Rest/bits >>} ->
			list_to_integer(binary_to_list(Str));
		{error, Reason} ->
			Reason
	end,
	gen_tcp:close(Socket),
	{Packet, Res}.

%% Send a raw request. Return the response code and the full response.
raw_resp(Request, Config) ->
   	{port, Port} = lists:keyfind(port, 1, Config),
	Transport = case ?config(scheme, Config) of
		"http" -> gen_tcp;
		"https" -> ssl
	end,
	{ok, Socket} = Transport:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = Transport:send(Socket, Request),
	{StatusCode,  Response} = case recv_loop(Transport, Socket, <<>>) of
		{ok, << "HTTP/1.1 ", Str:24/bits, _Rest/bits >> = Bin} ->
			{list_to_integer(binary_to_list(Str)), Bin};
		{ok, Bin} ->
			{badresp, Bin};
		{error, Reason} ->
			{Reason, <<>>}
	end,
	Transport:close(Socket),
	{Response, StatusCode}.

recv_loop(Transport, Socket, Acc) ->
	case Transport:recv(Socket, 0, 6000) of
		{ok, Data} ->
			recv_loop(Transport, Socket, <<Acc/binary, Data/binary>>);
		{error, closed} ->
			ok = Transport:close(Socket),
			{ok, Acc};
		{error, Reason} ->
			{error, Reason}
	end.



raw(Config) ->
	Huge = [$0 || _N <- lists:seq(1, 5000)],
	Tests = [
		{"\r\n\r\n\r\n\r\n\r\nGET / HTTP/1.1\r\nHost: localhost\r\n\r\n", 200},
		{"\n", 400},
		{"Garbage\r\n\r\n", 400},
		{"\r\n\r\n\r\n\r\n\r\n\r\n", 400},
		{"GET / HTTP/1.1\r\nHost: dev-extend.eu\r\n\r\n", 400},
		{"", closed},
		{"\r\n", closed},
		{"\r\n\r\n", closed},
		{"GET / HTTP/1.1", closed},
		{"GET / HTTP/1.1\r\n", 408},
		{"GET / HTTP/1.1\r\nHost: localhost", 408},
		{"GET / HTTP/1.1\r\nHost: localhost\r\n", 408},
		{"GET / HTTP/1.1\r\nHost: localhost\r\n\r", 408},
		{"GET http://proxy/ HTTP/1.1\r\n\r\n", 400},
		{"GET http://proxy/ HTTP/1.1\r\nHost: localhost\r\n\r\n", 200},
		{"GET / HTTP/1.2\r\nHost: localhost\r\n\r\n", 505},
		{"GET /init_shutdown HTTP/1.1\r\nHost: localhost\r\n\r\n", 666},
		{"GET /long_polling HTTP/1.1\r\nHost: localhost\r\n\r\n", 102},
		{Huge, 413},
		{"GET / HTTP/1.1\r\n" ++ Huge, 413}
	],
	[{Packet, StatusCode} = raw_req(Packet, Config)
		|| {Packet, StatusCode} <- Tests].

set_resp_header(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, "GET /set_resp/header HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: close\r\n\r\n"),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{_, _} = binary:match(Data, <<"Vary: Accept">>),
	{_, _} = binary:match(Data, <<"Set-Cookie: ">>).

set_resp_overwrite(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, "GET /set_resp/overwrite HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: close\r\n\r\n"),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{_Start, _Length} = binary:match(Data, <<"Server: DesireDrive/1.0">>).

set_resp_body(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, "GET /set_resp/body HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: close\r\n\r\n"),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{_Start, _Length} = binary:match(Data, <<"\r\n\r\n"
		"A flameless dance does not equal a cycle">>).

response_as_req(Config) ->
	Packet =
"HTTP/1.0 302 Found
Location: http://www.google.co.il/
Cache-Control: private
Content-Type: text/html; charset=UTF-8
Set-Cookie: PREF=ID=568f67013d4a7afa:FF=0:TM=1323014101:LM=1323014101:S=XqctDWC65MzKT0zC; expires=Tue, 03-Dec-2013 15:55:01 GMT; path=/; domain=.google.com
Date: Sun, 04 Dec 2011 15:55:01 GMT
Server: gws
Content-Length: 221
X-XSS-Protection: 1; mode=block
X-Frame-Options: SAMEORIGIN

<HTML><HEAD><meta http-equiv=\"content-type\" content=\"text/html;charset=utf-8\">
<TITLE>302 Moved</TITLE></HEAD><BODY>
<H1>302 Moved</H1>
The document has moved
<A HREF=\"http://www.google.co.il/\">here</A>.
</BODY></HTML>",
	{Packet, 400} = raw_req(Packet, Config).

stream_body_set_resp(Config) ->
	{Packet, 200} = raw_resp(
		"GET /stream_body/set_resp HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: close\r\n\r\n", Config),
	{_Start, _Length} = binary:match(Packet, <<"stream_body_set_resp">>).

static_mimetypes_function(Config) ->
	TestURL = build_url("/static_mimetypes_function/test.html", Config),
	{ok, {{"HTTP/1.1", 200, "OK"}, Headers1, "test.html\n"}} =
		httpc:request(TestURL),
	"text/html" = ?config("content-type", Headers1).

handler_errors(Config) ->
	Request = fun(Case) ->
		raw_resp(["GET /handler_errors?case=", Case, " HTTP/1.1\r\n",
		 "Host: localhost\r\n\r\n"], Config) end,

	{_Packet1, 500} = Request("init_before_reply"),

	{Packet2, 200} = Request("init_after_reply"),
	nomatch = binary:match(Packet2, <<"HTTP/1.1 500">>),

	{Packet3, 200} = Request("init_reply_handle_error"),
	nomatch = binary:match(Packet3, <<"HTTP/1.1 500">>),

	{_Packet4, 500} = Request("handle_before_reply"),

	{Packet5, 200} = Request("handle_after_reply"),
	nomatch = binary:match(Packet5, <<"HTTP/1.1 500">>),

	{Packet6, 200} = raw_resp([
		"GET / HTTP/1.1\r\n",
		"Host: localhost\r\n",
		"Connection: keep-alive\r\n\r\n",
		"GET /handler_errors?case=handle_after_reply\r\n",
		"Host: localhost\r\n\r\n"], Config),
	nomatch = binary:match(Packet6, <<"HTTP/1.1 500">>),

	{Packet7, 200} = raw_resp([
		"GET / HTTP/1.1\r\n",
		"Host: localhost\r\n",
		"Connection: keep-alive\r\n\r\n",
		"GET /handler_errors?case=handle_before_reply HTTP/1.1\r\n",
		"Host: localhost\r\n\r\n"], Config),
	{{_, _}, _} = {binary:match(Packet7, <<"HTTP/1.1 500">>), Packet7},

	done.

static_attribute_etag(Config) ->
	TestURL = build_url("/static_attribute_etag/test.html", Config),
	{ok, {{"HTTP/1.1", 200, "OK"}, Headers1, "test.html\n"}} =
		httpc:request(TestURL),
	false = ?config("etag", Headers1) =:= undefined,
	{ok, {{"HTTP/1.1", 200, "OK"}, Headers2, "test.html\n"}} =
		httpc:request(TestURL),
	true = ?config("etag", Headers1) =:= ?config("etag", Headers2).

static_function_etag(Config) ->
	TestURL = build_url("/static_function_etag/test.html", Config),
	{ok, {{"HTTP/1.1", 200, "OK"}, Headers1, "test.html\n"}} =
		httpc:request(TestURL),
	false = ?config("etag", Headers1) =:= undefined,
	{ok, {{"HTTP/1.1", 200, "OK"}, Headers2, "test.html\n"}} =
		httpc:request(TestURL),
	true = ?config("etag", Headers1) =:= ?config("etag", Headers2).

static_function_etag(Arguments, etag_data) ->
	{_, Filepath} = lists:keyfind(filepath, 1, Arguments),
	{_, _Filesize} = lists:keyfind(filesize, 1, Arguments),
	{_, _INode} = lists:keyfind(inode, 1, Arguments),
	{_, _Modified} = lists:keyfind(mtime, 1, Arguments),
	ChecksumCommand = lists:flatten(io_lib:format("sha1sum ~s", [Filepath])),
	[Checksum|_] = string:tokens(os:cmd(ChecksumCommand), " "),
	{strong, iolist_to_binary(Checksum)}.

te_identity(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	StrLen = integer_to_list(byte_size(Body)),
	ok = gen_tcp:send(Socket, ["GET /echo/body HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: close\r\n"
		"Content-Length: ", StrLen, "\r\n\r\n", Body]),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{_, _} = binary:match(Data, Body).

te_chunked(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = body_to_chunks(50, Body, []),
	ok = gen_tcp:send(Socket, ["GET /echo/body HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: close\r\n"
		"Transfer-Encoding: chunked\r\n\r\n", Chunks]),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{_, _} = binary:match(Data, Body).

te_chunked_delayed(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = body_to_chunks(50, Body, []),
	ok = gen_tcp:send(Socket, ["GET /echo/body HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: close\r\n"
		"Transfer-Encoding: chunked\r\n\r\n"]),
	_ = [begin ok = gen_tcp:send(Socket, Chunk), ok = timer:sleep(10) end
		|| Chunk <- Chunks],
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{_, _} = binary:match(Data, Body).

body_to_chunks(_, <<>>, Acc) ->
	lists:reverse([<<"0\r\n\r\n">>|Acc]);
body_to_chunks(ChunkSize, Body, Acc) ->
	BodySize = byte_size(Body),
	ChunkSize2 = case BodySize < ChunkSize of
		true -> BodySize;
		false -> ChunkSize
	end,
	<< Chunk:ChunkSize2/binary, Rest/binary >> = Body,
	ChunkSizeBin = list_to_binary(integer_to_list(ChunkSize2, 16)),
	body_to_chunks(ChunkSize, Rest,
		[<< ChunkSizeBin/binary, "\r\n", Chunk/binary, "\r\n" >>|Acc]).

%% http and https.

build_url(Path, Config) ->
	{scheme, Scheme} = lists:keyfind(scheme, 1, Config),
	{port, Port} = lists:keyfind(port, 1, Config),
	Scheme ++ "://localhost:" ++ integer_to_list(Port) ++ Path.

http_200(Config) ->
	{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, "http_handler"}} =
		httpc:request(build_url("/", Config)).

http_404(Config) ->
	{ok, {{"HTTP/1.1", 404, "Not Found"}, _Headers, _Body}} =
		httpc:request(build_url("/not/found", Config)).

file_200(Config) ->
	{ok, {{"HTTP/1.1", 200, "OK"}, Headers, "test_file\n"}} =
		httpc:request(build_url("/static/test_file", Config)),
	"application/octet-stream" = ?config("content-type", Headers),

	{ok, {{"HTTP/1.1", 200, "OK"}, Headers1, "test_file.css\n"}} =
		httpc:request(build_url("/static/test_file.css", Config)),
	"text/css" = ?config("content-type", Headers1).

file_403(Config) ->
	{ok, {{"HTTP/1.1", 403, "Forbidden"}, _Headers, _Body}} =
		httpc:request(build_url("/static/test_noread", Config)).

dir_403(Config) ->
	{ok, {{"HTTP/1.1", 403, "Forbidden"}, _Headers, _Body}} =
		httpc:request(build_url("/static/test_dir", Config)),
	{ok, {{"HTTP/1.1", 403, "Forbidden"}, _Headers, _Body}} =
		httpc:request(build_url("/static/test_dir/", Config)).

file_404(Config) ->
	{ok, {{"HTTP/1.1", 404, "Not Found"}, _Headers, _Body}} =
		httpc:request(build_url("/static/not_found", Config)).

file_400(Config) ->
	{ok, {{"HTTP/1.1", 400, "Bad Request"}, _Headers, _Body}} =
		httpc:request(build_url("/static/%2f", Config)),
	{ok, {{"HTTP/1.1", 400, "Bad Request"}, _Headers1, _Body1}} =
		httpc:request(build_url("/static/%2e", Config)),
	{ok, {{"HTTP/1.1", 400, "Bad Request"}, _Headers2, _Body2}} =
		httpc:request(build_url("/static/%2e%2e", Config)).

%% misc.

http_10_hostless(Config) ->
	Packet = "GET / HTTP/1.0\r\n\r\n",
	{Packet, 200} = raw_req(Packet, Config).

http_10_chunkless(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	Packet = "GET /chunked_response HTTP/1.0\r\nContent-Length: 0\r\n\r\n",
	ok = gen_tcp:send(Socket, Packet),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	nomatch = binary:match(Data, <<"Transfer-Encoding">>),
	{_, _} = binary:match(Data, <<"chunked_handler\r\nworks fine!">>),
	ok = gen_tcp:close(Socket).

%% rest.

rest_simple(Config) ->
	Packet = "GET /simple HTTP/1.1\r\nHost: localhost\r\n\r\n",
	{Packet, 200} = raw_req(Packet, Config).

rest_keepalive(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = rest_keepalive_loop(Socket, 100),
	ok = gen_tcp:close(Socket).

rest_keepalive_loop(_Socket, 0) ->
	ok;
rest_keepalive_loop(Socket, N) ->
	ok = gen_tcp:send(Socket, "GET /simple HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: keep-alive\r\n\r\n"),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{0, 12} = binary:match(Data, <<"HTTP/1.1 200">>),
	nomatch = binary:match(Data, <<"Connection: close">>),
	rest_keepalive_loop(Socket, N - 1).

rest_keepalive_post(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = rest_keepalive_post_loop(Socket, 10, forbidden_post),
	ok = gen_tcp:close(Socket).

rest_keepalive_post_loop(_Socket, 0, _) ->
	ok;
rest_keepalive_post_loop(Socket, N, simple_post) ->
	ok = gen_tcp:send(Socket, "POST /simple_post HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: keep-alive\r\n"
		"Content-Length: 5\r\nContent-Type: text/plain\r\n\r\n12345"),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{0, 12} = binary:match(Data, <<"HTTP/1.1 303">>),
	nomatch = binary:match(Data, <<"Connection: close">>),
	rest_keepalive_post_loop(Socket, N - 1, forbidden_post);
rest_keepalive_post_loop(Socket, N, forbidden_post) ->
	ok = gen_tcp:send(Socket, "POST /forbidden_post HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: keep-alive\r\n"
		"Content-Length: 5\r\nContent-Type: text/plain\r\n\r\n12345"),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{0, 12} = binary:match(Data, <<"HTTP/1.1 403">>),
	nomatch = binary:match(Data, <<"Connection: close">>),
	rest_keepalive_post_loop(Socket, N - 1, simple_post).

rest_nodelete(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	Request = "DELETE /nodelete HTTP/1.1\r\nHost: localhost\r\n\r\n",
	ok = gen_tcp:send(Socket, Request),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{0, 12} = binary:match(Data, <<"HTTP/1.1 500">>),
	ok = gen_tcp:close(Socket).

rest_resource_etags(Config) ->
	%% The Etag header should be set to the return value of generate_etag/2.
	fun() ->
	%% Correct return values from generate_etag/2.
	{Packet1, 200} = raw_resp([
		"GET /resetags?type=tuple-weak HTTP/1.1\r\n",
		"Host: localhost\r\n", "Connection: close\r\n", "\r\n"], Config),
	{_,_} = binary:match(Packet1, <<"ETag: W/\"etag-header-value\"\r\n">>),
	{Packet2, 200} = raw_resp([
		"GET /resetags?type=tuple-strong HTTP/1.1\r\n",
		"Host: localhost\r\n", "Connection: close\r\n", "\r\n"], Config),
	{_,_} = binary:match(Packet2, <<"ETag: \"etag-header-value\"\r\n">>),
	%% Backwards compatible return values from generate_etag/2.
	{Packet3, 200} = raw_resp([
		"GET /resetags?type=binary-weak-quoted HTTP/1.1\r\n",
		"Host: localhost\r\n", "Connection: close\r\n", "\r\n"], Config),
	{_,_} = binary:match(Packet3, <<"ETag: W/\"etag-header-value\"\r\n">>),
	{Packet4, 200} = raw_resp([
		"GET /resetags?type=binary-strong-quoted HTTP/1.1\r\n",
		"Host: localhost\r\n", "Connection: close\r\n", "\r\n"], Config),
	{_,_} = binary:match(Packet4, <<"ETag: \"etag-header-value\"\r\n">>),
	%% Invalid return values from generate_etag/2.
	{_Packet5, 500} = raw_resp([
		"GET /resetags?type=binary-strong-unquoted HTTP/1.1\r\n",
		"Host: localhost\r\n", "Connection: close\r\n", "\r\n"], Config),
	{_Packet6, 500} = raw_resp([
		"GET /resetags?type=binary-weak-unquoted HTTP/1.1\r\n",
		"Host: localhost\r\n", "Connection: close\r\n", "\r\n"], Config)
	end(),

	%% The return value of generate_etag/2 should match the request header.
	fun() ->
	%% Correct return values from generate_etag/2.
	{_Packet1, 304} = raw_resp([
		"GET /resetags?type=tuple-weak HTTP/1.1\r\n",
		"Host: localhost\r\n", "Connection: close\r\n",
		"If-None-Match: W/\"etag-header-value\"\r\n", "\r\n"], Config),
	{_Packet2, 304} = raw_resp([
		"GET /resetags?type=tuple-strong HTTP/1.1\r\n",
		"Host: localhost\r\n", "Connection: close\r\n",
		"If-None-Match: \"etag-header-value\"\r\n", "\r\n"], Config),
	%% Backwards compatible return values from generate_etag/2.
	{_Packet3, 304} = raw_resp([
		"GET /resetags?type=binary-weak-quoted HTTP/1.1\r\n",
		"Host: localhost\r\n", "Connection: close\r\n",
		"If-None-Match: W/\"etag-header-value\"\r\n", "\r\n"], Config),
	{_Packet4, 304} = raw_resp([
		"GET /resetags?type=binary-strong-quoted HTTP/1.1\r\n",
		"Host: localhost\r\n", "Connection: close\r\n",
		"If-None-Match: \"etag-header-value\"\r\n", "\r\n"], Config)
	end().

onrequest(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{_, _} = binary:match(Data, <<"Server: Serenity">>),
	{_, _} = binary:match(Data, <<"http_handler">>),
	gen_tcp:close(Socket).

onrequest_reply(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, "GET /?reply=1 HTTP/1.1\r\nHost: localhost\r\n\r\n"),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{_, _} = binary:match(Data, <<"Server: Cowboy">>),
	nomatch = binary:match(Data, <<"http_handler">>),
	{_, _} = binary:match(Data, <<"replied!">>),
	gen_tcp:close(Socket).

onrequest_hook(Req) ->
	case cowboy_http_req:qs_val(<<"reply">>, Req) of
		{undefined, Req2} ->
			{ok, Req3} = cowboy_http_req:set_resp_header(
				'Server', <<"Serenity">>, Req2),
			Req3;
		{_, Req2} ->
			{ok, Req3} = cowboy_http_req:reply(
				200, [], <<"replied!">>, Req2),
			Req3
	end.
