%% Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
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

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

%% ct.

all() ->
	ct_helper:all(?MODULE).

%% Remove environment variables inherited from Erlang.mk.
init_per_suite(Config) ->
	os:unsetenv("ERLANG_MK_TMP"),
	os:unsetenv("APPS_DIR"),
	os:unsetenv("DEPS_DIR"),
	os:unsetenv("ERL_LIBS"),
	Config.

end_per_suite(_) ->
	ok.

%% Compile, start and stop releases.

do_get_paths(Example0) ->
	Example = atom_to_list(Example0),
	{ok, CWD} = file:get_cwd(),
	Dir = CWD ++ "/../../examples/" ++ Example,
	Rel = Dir ++ "/_rel/" ++ Example ++ "_example/bin/" ++ Example ++ "_example",
	Log = Dir ++ "/_rel/" ++ Example ++ "_example/log/erlang.log.1",
	{Dir, Rel, Log}.

do_compile_and_start(Example) ->
	{Dir, Rel, _} = do_get_paths(Example),
	%% TERM=dumb disables relx coloring.
	ct:log("~s~n", [os:cmd("cd " ++ Dir ++ " && make distclean && TERM=dumb make all")]),
	ct:log("~s~n", [os:cmd(Rel ++ " stop")]),
	ct:log("~s~n", [os:cmd(Rel ++ " start")]),
	timer:sleep(2000),
	ok.

do_stop(Example) ->
	{_, Rel, Log} = do_get_paths(Example),
	ct:log("~s~n", [os:cmd(Rel ++ " stop")]),
	ct:log("~s~n", [element(2, file:read_file(Log))]),
	ok.

%% TCP and SSL Hello World.

hello_world(Config) ->
	doc("Hello World example."),
	try
		do_compile_and_start(hello_world),
		do_hello_world(tcp, http, Config),
		do_hello_world(tcp, http2, Config)
	after
		do_stop(hello_world)
	end.

ssl_hello_world(Config) ->
	doc("SSL Hello World example."),
	try
		do_compile_and_start(ssl_hello_world),
		do_hello_world(ssl, http, Config),
		do_hello_world(ssl, http2, Config)
	after
		do_stop(ssl_hello_world)
	end.

do_hello_world(Transport, Protocol, Config) ->
	Port = case Transport of
		tcp -> 8080;
		ssl -> 8443
	end,
	ConnPid = gun_open([{port, Port}, {type, Transport}, {protocol, Protocol}|Config]),
	Ref = gun:get(ConnPid, "/"),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"Hello world!">>} = gun:await_body(ConnPid, Ref),
	ok.

%% Echo GET and POST.

echo_get(Config) ->
	doc("GET parameter echo example."),
	try
		do_compile_and_start(echo_get),
		do_echo_get(tcp, http, Config),
		do_echo_get(tcp, http2, Config)
	after
		do_stop(echo_get)
	end.

do_echo_get(Transport, Protocol, Config) ->
	ConnPid = gun_open([{port, 8080}, {type, Transport}, {protocol, Protocol}|Config]),
	Ref = gun:get(ConnPid, "/?echo=this+is+fun"),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"this is fun">>} = gun:await_body(ConnPid, Ref),
	ok.
