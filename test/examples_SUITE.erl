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

-module(examples_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

%% ct.

all() ->
	ct_helper:all(?MODULE).

init_per_suite(Config) ->
	%% Remove environment variables inherited from Erlang.mk.
	os:unsetenv("ERLANG_MK_TMP"),
	os:unsetenv("APPS_DIR"),
	os:unsetenv("DEPS_DIR"),
	os:unsetenv("ERL_LIBS"),
	%% Clone and build Cowboy, Cowlib and Ranch only once and
	%% reuse the same build across all tests.
	Make = do_find_make_cmd(),
	CommonDir = config(priv_dir, Config),
	ct:log("~s~n", [os:cmd("git clone --depth 1 https://github.com/ninenines/cowboy "
		++ CommonDir ++ "cowboy")]),
	ct:log("~s~n", [os:cmd(Make ++ " -C " ++ CommonDir ++ "cowboy distclean")]),
	ct:log("~s~n", [os:cmd(Make ++ " -C " ++ CommonDir ++ "cowboy DEPS_DIR=" ++ CommonDir)]),
	Config.

end_per_suite(_) ->
	ok.

%% Find GNU Make.

do_find_make_cmd() ->
	case os:getenv("MAKE") of
		false ->
			case os:find_executable("gmake") of
				false -> "make";
				Cmd   -> Cmd
			end;
		Cmd ->
			Cmd
	end.

%% Compile, start and stop releases.

do_get_paths(Example0) ->
	Example = atom_to_list(Example0),
	{ok, CWD} = file:get_cwd(),
	Dir = CWD ++ "/../../examples/" ++ Example,
	Rel = Dir ++ "/_rel/" ++ Example ++ "_example/bin/" ++ Example ++ "_example",
	Log = Dir ++ "/_rel/" ++ Example ++ "_example/log/erlang.log.1",
	{Dir, Rel, Log}.

do_compile_and_start(Example, Config) ->
	Make = do_find_make_cmd(),
	{Dir, Rel, _} = do_get_paths(Example),
	ct:log("~s~n", [os:cmd(Make ++ " -C " ++ Dir ++ " distclean")]),
	%% We use a common build for Cowboy, Cowlib and Ranch to speed things up.
	CommonDir = config(priv_dir, Config),
	ct:log("~s~n", [os:cmd("mkdir " ++ Dir ++ "/deps")]),
	ct:log("~s~n", [os:cmd("ln -s " ++ CommonDir ++ "cowboy " ++ Dir ++ "/deps/cowboy")]),
	ct:log("~s~n", [os:cmd("ln -s " ++ CommonDir ++ "cowlib " ++ Dir ++ "/deps/cowlib")]),
	ct:log("~s~n", [os:cmd("ln -s " ++ CommonDir ++ "ranch " ++ Dir ++ "/deps/ranch")]),
	%% TERM=dumb disables relx coloring.
	ct:log("~s~n", [os:cmd(Make ++ " -C " ++ Dir ++ " TERM=dumb")]),
	ct:log("~s~n", [os:cmd(Rel ++ " stop")]),
	ct:log("~s~n", [os:cmd(Rel ++ " start")]),
	timer:sleep(2000),
	ok.

do_stop(Example) ->
	{_, Rel, Log} = do_get_paths(Example),
	ct:log("~s~n", [os:cmd(Rel ++ " stop")]),
	ct:log("~s~n", [element(2, file:read_file(Log))]),
	ok.

%% Fetch a response.

do_get(Transport, Protocol, Path, Config) ->
	do_get(Transport, Protocol, Path, [], Config).

do_get(Transport, Protocol, Path, ReqHeaders, Config) ->
	Port = case Transport of
		tcp -> 8080;
		ssl -> 8443
	end,
	ConnPid = gun_open([{port, Port}, {type, Transport}, {protocol, Protocol}|Config]),
	Ref = gun:get(ConnPid, Path, ReqHeaders),
	case gun:await(ConnPid, Ref) of
		{response, nofin, Status, RespHeaders} ->
			{ok, Body} = gun:await_body(ConnPid, Ref),
			{Status, RespHeaders, Body};
		{response, fin, Status, RespHeaders} ->
			{Status, RespHeaders, <<>>}
	end.

%% TCP and SSL Hello World.

hello_world(Config) ->
	doc("Hello World example."),
	try
		do_compile_and_start(hello_world, Config),
		do_hello_world(tcp, http, Config),
		do_hello_world(tcp, http2, Config)
	after
		do_stop(hello_world)
	end.

ssl_hello_world(Config) ->
	doc("SSL Hello World example."),
	try
		do_compile_and_start(ssl_hello_world, Config),
		do_hello_world(ssl, http, Config),
		do_hello_world(ssl, http2, Config)
	after
		do_stop(ssl_hello_world)
	end.

do_hello_world(Transport, Protocol, Config) ->
	{200, _, <<"Hello world!">>} = do_get(Transport, Protocol, "/", Config),
	ok.

%% Chunked Hello World.

chunked_hello_world(Config) ->
	doc("Chunked Hello World example."),
	try
		do_compile_and_start(chunked_hello_world, Config),
		do_chunked_hello_world(tcp, http, Config),
		do_chunked_hello_world(tcp, http2, Config)
	after
		do_stop(chunked_hello_world)
	end.

do_chunked_hello_world(Transport, Protocol, Config) ->
	ConnPid = gun_open([{port, 8080}, {type, Transport}, {protocol, Protocol}|Config]),
	Ref = gun:get(ConnPid, "/"),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	%% We expect to receive a chunk every second, three total.
	{data, nofin, <<"Hello\r\n">>} = gun:await(ConnPid, Ref, 2000),
	{data, nofin, <<"World\r\n">>} = gun:await(ConnPid, Ref, 2000),
	{data, IsFin, <<"Chunked!\r\n">>} = gun:await(ConnPid, Ref, 2000),
	%% We may get an extra empty chunk (last chunk for HTTP/1.1,
	%% empty DATA frame with the FIN bit set for HTTP/2).
	case IsFin of
		fin -> ok;
		nofin ->
			{data, fin, <<>>} = gun:await(ConnPid, Ref, 500),
			ok
	end.

%% Compressed responses.

compress_response(Config) ->
	doc("Compressed response example."),
	try
		do_compile_and_start(compress_response, Config),
		do_compress_response(tcp, http, Config),
		do_compress_response(tcp, http2, Config)
	after
		do_stop(compress_response)
	end.

do_compress_response(Transport, Protocol, Config) ->
	{200, Headers, Body} = do_get(Transport, Protocol, "/",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	_ = zlib:gunzip(Body),
	ok.

%% Cookie.

cookie(Config) ->
	doc("Cookie example."),
	try
		do_compile_and_start(cookie, Config),
		do_cookie(tcp, http, Config),
		do_cookie(tcp, http2, Config)
	after
		do_stop(cookie)
	end.

do_cookie(Transport, Protocol, Config) ->
	{200, _, One} = do_get(Transport, Protocol, "/", Config),
	{200, _, Two} = do_get(Transport, Protocol, "/", [{<<"cookie">>, <<"server=abcdef">>}], Config),
	true = One =/= Two,
	ok.

%% Echo GET.

echo_get(Config) ->
	doc("GET parameter echo example."),
	try
		do_compile_and_start(echo_get, Config),
		do_echo_get(tcp, http, Config),
		do_echo_get(tcp, http2, Config)
	after
		do_stop(echo_get)
	end.

do_echo_get(Transport, Protocol, Config) ->
	{200, _, <<"this is fun">>} = do_get(Transport, Protocol, "/?echo=this+is+fun", Config),
	{400, _, _} = do_get(Transport, Protocol, "/", Config),
	ok.

%% Echo POST.

echo_post(Config) ->
	doc("POST parameter echo example."),
	try
		do_compile_and_start(echo_post, Config),
		do_echo_post(tcp, http, Config),
		do_echo_post(tcp, http2, Config)
	after
		do_stop(echo_post)
	end.

do_echo_post(Transport, Protocol, Config) ->
	ConnPid = gun_open([{port, 8080}, {type, Transport}, {protocol, Protocol}|Config]),
	Ref = gun:post(ConnPid, "/", [
		{<<"content-type">>, <<"application/octet-stream">>}
	], <<"echo=this+is+fun">>),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"this is fun">>} = gun:await_body(ConnPid, Ref),
	ok.

%% Eventsource.

eventsource(Config) ->
	doc("Eventsource example."),
	try
		do_compile_and_start(eventsource, Config),
		do_eventsource(tcp, http, Config),
		do_eventsource(tcp, http2, Config)
	after
		do_stop(eventsource)
	end.

do_eventsource(Transport, Protocol, Config) ->
	ConnPid = gun_open([{port, 8080}, {type, Transport}, {protocol, Protocol}|Config]),
	Ref = gun:get(ConnPid, "/eventsource"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"text/event-stream">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	%% Receive a few events.
	{data, nofin, << "id: ", _/bits >>} = gun:await(ConnPid, Ref, 2000),
	{data, nofin, << "id: ", _/bits >>} = gun:await(ConnPid, Ref, 2000),
	{data, nofin, << "id: ", _/bits >>} = gun:await(ConnPid, Ref, 2000),
	gun:close(ConnPid).

%% REST Hello World.

rest_hello_world(Config) ->
	doc("REST Hello World example."),
	try
		do_compile_and_start(rest_hello_world, Config),
		do_rest_hello_world(tcp, http, Config),
		do_rest_hello_world(tcp, http2, Config)
	after
		do_stop(rest_hello_world)
	end.

do_rest_hello_world(Transport, Protocol, Config) ->
	<< "<html>", _/bits >> = do_rest_get(Transport, Protocol,
		"/", undefined, undefined, Config),
	<< "REST Hello World as text!" >> = do_rest_get(Transport, Protocol,
		"/", <<"text/plain">>, undefined, Config),
	<< "{\"rest\": \"Hello World!\"}" >> = do_rest_get(Transport, Protocol,
		"/", <<"application/json">>, undefined, Config),
	not_acceptable = do_rest_get(Transport, Protocol,
		"/", <<"text/css">>, undefined, Config),
	ok.

do_rest_get(Transport, Protocol, Path, Accept, Auth, Config) ->
	ReqHeaders0 = case Accept of
		undefined -> [];
		_ -> [{<<"accept">>, Accept}]
	end,
	ReqHeaders = case Auth of
		undefined -> ReqHeaders0;
		_ -> [{<<"authorization">>, [<<"Basic ">>, base64:encode(Auth)]}|ReqHeaders0]
	end,
	case do_get(Transport, Protocol, Path, ReqHeaders, Config) of
		{200, RespHeaders, Body} ->
			Accept = case Accept of
				undefined -> undefined;
				_ ->
					{_, ContentType} = lists:keyfind(<<"content-type">>, 1, RespHeaders),
					ContentType
			end,
			Body;
		{401, _, _} ->
			unauthorized;
		{406, _, _} ->
			not_acceptable
	end.

%% REST basic auth.

rest_basic_auth(Config) ->
	doc("REST basic authorization example."),
	try
		do_compile_and_start(rest_basic_auth, Config),
		do_rest_basic_auth(tcp, http, Config),
		do_rest_basic_auth(tcp, http2, Config)
	after
		do_stop(rest_basic_auth)
	end.

do_rest_basic_auth(Transport, Protocol, Config) ->
	unauthorized = do_rest_get(Transport, Protocol, "/", undefined, undefined, Config),
	<<"Hello, Alladin!\n">> = do_rest_get(Transport, Protocol, "/", undefined, "Alladin:open sesame", Config),
	ok.

%% REST pastebin.

rest_pastebin(Config) ->
	doc("REST pastebin example."),
	try
		do_compile_and_start(rest_pastebin, Config),
		do_rest_pastebin(tcp, http, Config),
		do_rest_pastebin(tcp, http2, Config)
	after
		do_stop(rest_pastebin)
	end.

do_rest_pastebin(Transport, Protocol, Config) ->
	%% Existing files.
	_ = do_rest_get(Transport, Protocol, "/", <<"text/html">>, undefined, Config),
	_ = do_rest_get(Transport, Protocol, "/", <<"text/plain">>, undefined, Config),
	%% Use POST to upload a new file and download it back.
	ConnPid = gun_open([{port, 8080}, {type, Transport}, {protocol, Protocol}|Config]),
	Ref = gun:post(ConnPid, "/", [
		{<<"content-type">>, <<"application/x-www-form-urlencoded">>}
	], <<"paste=this+is+fun">>),
	%% @todo Not too happy about 303 here,
	%% will need to revisit this example.
	{response, _, 303, Headers} = gun:await(ConnPid, Ref),
	{_, Location} = lists:keyfind(<<"location">>, 1, Headers),
	<<"this is fun">> = do_rest_get(Transport, Protocol, Location, <<"text/plain">>, undefined, Config),
	<< "<!DOCTYPE html><html>", _/bits >>
		= do_rest_get(Transport, Protocol, Location, <<"text/html">>, undefined, Config),
	ok.

%% File server.

file_server(Config) ->
	doc("File server example with directory listing."),
	try
		do_compile_and_start(file_server, Config),
		do_file_server(tcp, http, Config),
		do_file_server(tcp, http2, Config)
	after
		do_stop(file_server)
	end.

do_file_server(Transport, Protocol, Config) ->
	%% Directory.
	{200, DirHeaders, <<"<!DOCTYPE html><html>", _/bits >>} = do_get(Transport, Protocol, "/", Config),
	{_, <<"text/html">>} = lists:keyfind(<<"content-type">>, 1, DirHeaders),
	_ = do_rest_get(Transport, Protocol, "/", <<"application/json">>, undefined, Config),
	%% Files.
	{200, _, _} = do_get(Transport, Protocol, "/small.mp4", Config),
	{200, _, _} = do_get(Transport, Protocol, "/small.ogv", Config),
	{200, _, _} = do_get(Transport, Protocol, "/test.txt", Config),
	{200, _, _} = do_get(Transport, Protocol, "/video.html", Config),
	ok.

%% Markdown middleware.

markdown_middleware(Config) ->
	doc("Markdown middleware example."),
	try
		do_compile_and_start(markdown_middleware, Config),
		do_markdown_middleware(tcp, http, Config),
		do_markdown_middleware(tcp, http2, Config)
	after
		do_stop(markdown_middleware)
	end.

do_markdown_middleware(Transport, Protocol, Config) ->
	{200, Headers, <<"<h1>", _/bits >>} = do_get(Transport, Protocol, "/video.html", Config),
	{_, <<"text/html">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

%% Upload.

upload(Config) ->
	doc("Upload example."),
	try
		do_compile_and_start(upload, Config),
		do_upload(tcp, http, Config),
		do_upload(tcp, http2, Config)
	after
		do_stop(upload)
	end.

do_upload(Transport, Protocol, Config) ->
	{200, _, << "<html>", _/bits >>} = do_get(Transport, Protocol, "/", Config),
	%% Use POST to upload a file using multipart.
	ConnPid = gun_open([{port, 8080}, {type, Transport}, {protocol, Protocol}|Config]),
	Ref = gun:post(ConnPid, "/upload", [
		{<<"content-type">>, <<"multipart/form-data;boundary=deadbeef">>}
	], <<
		"--deadbeef\r\n"
		"Content-Disposition: form-data; name=\"inputfile\"; filename=\"test.txt\"\r\n"
		"Content-Type: text/plain\r\n"
		"\r\n"
		"Cowboy upload example!\r\n"
		"--deadbeef--">>),
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	ok.

%% Websocket.

websocket(Config) ->
	doc("Websocket example."),
	try
		do_compile_and_start(websocket, Config),
		%% We can only initiate a Websocket connection from HTTP/1.1.
		{ok, Pid} = gun:open("127.0.0.1", 8080, #{protocols => [http], retry => 0}),
		{ok, http} = gun:await_up(Pid),
		_ = monitor(process, Pid),
		StreamRef = gun:ws_upgrade(Pid, "/websocket", [], #{compress => true}),
		receive
			{gun_upgrade, Pid, StreamRef, _, _} ->
				ok;
			Msg1 ->
				exit({connection_failed, Msg1})
		end,
		%% Check that we receive the message sent on timer on init.
		receive
			{gun_ws, Pid, StreamRef, {text, <<"Hello!">>}} ->
				ok
		after 2000 ->
			exit(timeout)
		end,
		%% Check that we receive subsequent messages sent on timer.
		receive
			{gun_ws, Pid, StreamRef, {text, <<"How' you doin'?">>}} ->
				ok
		after 2000 ->
			exit(timeout)
		end,
		%% Check that we receive the echoed message.
		gun:ws_send(Pid, {text, <<"hello">>}),
		receive
			{gun_ws, Pid, StreamRef, {text, <<"That's what she said! hello">>}} ->
				ok
		after 500 ->
			exit(timeout)
		end,
		gun:ws_send(Pid, close)
	after
		do_stop(websocket)
	end.
