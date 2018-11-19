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

-module(static_handler_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

%% ct.

all() ->
	cowboy_test:common_all() ++ [
		{group, http_no_sendfile},
		{group, h2c_no_sendfile}
	].

groups() ->
	AllTests = ct_helper:all(?MODULE),
	%% The directory tests are shared between dir and priv_dir options.
	DirTests = lists:usort([F || {F, 1} <- ?MODULE:module_info(exports),
		string:substr(atom_to_list(F), 1, 4) =:= "dir_"
	]),
	OtherTests = AllTests -- DirTests,
	GroupTests = OtherTests ++ [
		{dir, [parallel], DirTests},
		{priv_dir, [parallel], DirTests}
	],
	[
		{http, [parallel], GroupTests},
		{https, [parallel], GroupTests},
		{h2, [parallel], GroupTests},
		{h2c, [parallel], GroupTests},
		{http_compress, [parallel], GroupTests},
		{https_compress, [parallel], GroupTests},
		{h2_compress, [parallel], GroupTests},
		{h2c_compress, [parallel], GroupTests},
		%% No real need to test sendfile disabled against https or h2.
		{http_no_sendfile, [parallel], GroupTests},
		{h2c_no_sendfile, [parallel], GroupTests}
	].

init_per_suite(Config) ->
	%% Two static folders are created: one in ct_helper's private directory,
	%% and one in the test run private directory.
	PrivDir = code:priv_dir(ct_helper) ++ "/static",
	StaticDir = config(priv_dir, Config) ++ "/static",
	ct_helper:create_static_dir(PrivDir),
	ct_helper:create_static_dir(StaticDir),
	init_large_file(PrivDir ++ "/large.bin"),
	init_large_file(StaticDir ++ "/large.bin"),
	%% Add a simple Erlang application archive containing one file
	%% in its priv directory.
	true = code:add_pathz(filename:join(
		[config(data_dir, Config), "static_files_app", "ebin"])),
	ok = application:load(static_files_app),
	%% A special folder contains files of 1 character from 0 to 127.
	CharDir = config(priv_dir, Config) ++ "/char",
	ok = filelib:ensure_dir(CharDir ++ "/file"),
	Chars0 = lists:flatten([case file:write_file(CharDir ++ [$/, C], [C]) of
		ok -> C;
		{error, _} -> []
	end || C <- lists:seq(0, 127)]),
	%% Determine whether we are on a case insensitive filesystem and
	%% remove uppercase characters in that case. On case insensitive
	%% filesystems we end up overwriting the "A" file with the "a" contents.
	{CaseSensitive, Chars} = case file:read_file(CharDir ++ "/A") of
		{ok, <<"A">>} -> {true, Chars0};
		{ok, <<"a">>} -> {false, Chars0 -- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"}
	end,
	[{static_dir, StaticDir}, {char_dir, CharDir},
		{chars, Chars}, {case_sensitive, CaseSensitive}|Config].

end_per_suite(Config) ->
	%% Special directory.
	CharDir = config(char_dir, Config),
	_ = [file:delete(CharDir ++ [$/, C]) || C <- lists:seq(0, 127)],
	_ = file:del_dir(CharDir),
	%% Static directories.
	StaticDir = config(static_dir, Config),
	PrivDir = code:priv_dir(ct_helper) ++ "/static",
	%% This file is not created on Windows.
	_ = file:delete(StaticDir ++ "/large.bin"),
	_ = file:delete(PrivDir ++ "/large.bin"),
	ct_helper:delete_static_dir(StaticDir),
	ct_helper:delete_static_dir(PrivDir).

init_per_group(dir, Config) ->
	[{prefix, "/dir"}|Config];
init_per_group(priv_dir, Config) ->
	[{prefix, "/priv_dir"}|Config];
init_per_group(Name=http_no_sendfile, Config) ->
	cowboy_test:init_http(Name, #{
		env => #{dispatch => init_dispatch(Config)},
		middlewares => [?MODULE, cowboy_router, cowboy_handler],
		sendfile => false
	}, [{flavor, vanilla}|Config]);
init_per_group(Name=h2c_no_sendfile, Config) ->
	Config1 = cowboy_test:init_http(Name, #{
		env => #{dispatch => init_dispatch(Config)},
		middlewares => [?MODULE, cowboy_router, cowboy_handler],
		sendfile => false
	}, [{flavor, vanilla}|Config]),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2});
init_per_group(Name, Config) ->
	Config1 = cowboy_test:init_common_groups(Name, Config, ?MODULE),
	Opts = ranch:get_protocol_options(Name),
	ok = ranch:set_protocol_options(Name, Opts#{
		middlewares => [?MODULE, cowboy_router, cowboy_handler]
	}),
	Config1.

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

%% Large file.

init_large_file(Filename) ->
	case os:type() of
		{unix, _} ->
			"" = os:cmd("truncate -s 32M " ++ Filename),
			ok;
		{win32, _} ->
			ok
	end.

%% Routes.

init_dispatch(Config) ->
	cowboy_router:compile([{'_', [
		{"/priv_dir/[...]", cowboy_static, {priv_dir, ct_helper, "static"}},
		{"/dir/[...]", cowboy_static, {dir, config(static_dir, Config)}},
		{"/priv_file/style.css", cowboy_static, {priv_file, ct_helper, "static/style.css"}},
		{"/file/style.css", cowboy_static, {file, config(static_dir, Config) ++ "/style.css"}},
		{"/index", cowboy_static, {file, config(static_dir, Config) ++ "/index.html"}},
		{"/mime/all/[...]", cowboy_static, {priv_dir, ct_helper, "static",
			[{mimetypes, cow_mimetypes, all}]}},
		{"/mime/custom/[...]", cowboy_static, {priv_dir, ct_helper, "static",
			[{mimetypes, ?MODULE, do_mime_custom}]}},
		{"/mime/crash/[...]", cowboy_static, {priv_dir, ct_helper, "static",
			[{mimetypes, ?MODULE, do_mime_crash}]}},
		{"/mime/hardcode/binary-form", cowboy_static, {priv_file, ct_helper, "static/file.cowboy",
			[{mimetypes, <<"application/vnd.ninenines.cowboy+xml;v=1">>}]}},
		{"/mime/hardcode/tuple-form", cowboy_static, {priv_file, ct_helper, "static/file.cowboy",
			[{mimetypes, {<<"application">>, <<"vnd.ninenines.cowboy+xml">>, [{<<"v">>, <<"1">>}]}}]}},
		{"/charset/custom/[...]", cowboy_static, {priv_dir, ct_helper, "static",
			[{charset, ?MODULE, do_charset_custom}]}},
		{"/charset/crash/[...]", cowboy_static, {priv_dir, ct_helper, "static",
			[{charset, ?MODULE, do_charset_crash}]}},
		{"/charset/hardcode/[...]", cowboy_static, {priv_file, ct_helper, "static/index.html",
			[{charset, <<"utf-8">>}]}},
		{"/etag/custom", cowboy_static, {file, config(static_dir, Config) ++ "/style.css",
			[{etag, ?MODULE, do_etag_custom}]}},
		{"/etag/crash", cowboy_static, {file, config(static_dir, Config) ++ "/style.css",
			[{etag, ?MODULE, do_etag_crash}]}},
		{"/etag/disable", cowboy_static, {file, config(static_dir, Config) ++ "/style.css",
			[{etag, false}]}},
		{"/bad", cowboy_static, bad},
		{"/bad/priv_dir/app/[...]", cowboy_static, {priv_dir, bad_app, "static"}},
		{"/bad/priv_dir/no-priv/[...]", cowboy_static, {priv_dir, cowboy, "static"}},
		{"/bad/priv_dir/path/[...]", cowboy_static, {priv_dir, ct_helper, "bad"}},
		{"/bad/priv_dir/route", cowboy_static, {priv_dir, ct_helper, "static"}},
		{"/bad/dir/path/[...]", cowboy_static, {dir, "/bad/path"}},
		{"/bad/dir/route", cowboy_static, {dir, config(static_dir, Config)}},
		{"/bad/priv_file/app", cowboy_static, {priv_file, bad_app, "static/style.css"}},
		{"/bad/priv_file/no-priv", cowboy_static, {priv_file, cowboy, "static/style.css"}},
		{"/bad/priv_file/path", cowboy_static, {priv_file, ct_helper, "bad/style.css"}},
		{"/bad/file/path", cowboy_static, {file, "/bad/path/style.css"}},
		{"/bad/options", cowboy_static, {priv_file, ct_helper, "static/style.css", bad}},
		{"/bad/options/mime", cowboy_static, {priv_file, ct_helper, "static/style.css", [{mimetypes, bad}]}},
		{"/bad/options/charset", cowboy_static, {priv_file, ct_helper, "static/style.css", [{charset, bad}]}},
		{"/bad/options/etag", cowboy_static, {priv_file, ct_helper, "static/style.css", [{etag, true}]}},
		{"/unknown/option", cowboy_static, {priv_file, ct_helper, "static/style.css", [{bad, option}]}},
		{"/char/[...]", cowboy_static, {dir, config(char_dir, Config)}},
		{"/ez_priv_file/index.html", cowboy_static, {priv_file, static_files_app, "www/index.html"}},
		{"/bad/ez_priv_file/index.php", cowboy_static, {priv_file, static_files_app, "www/index.php"}},
		{"/ez_priv_dir/[...]", cowboy_static, {priv_dir, static_files_app, "www"}},
		{"/bad/ez_priv_dir/[...]", cowboy_static, {priv_dir, static_files_app, "cgi-bin"}}
	]}]).

%% Middleware interface to silence expected errors.

execute(Req=#{path := Path}, Env) ->
	case Path of
		<<"/bad/priv_dir/app/", _/bits>> -> ct_helper:ignore(cowboy_static, priv_path, 2);
		<<"/bad/priv_file/app">> -> ct_helper:ignore(cowboy_static, priv_path, 2);
		<<"/bad/priv_dir/route">> -> ct_helper:ignore(cowboy_static, escape_reserved, 1);
		<<"/bad/dir/route">> -> ct_helper:ignore(cowboy_static, escape_reserved, 1);
		<<"/bad">> -> ct_helper:ignore(cowboy_static, init_opts, 2);
		<<"/bad/options">> -> ct_helper:ignore(cowboy_static, content_types_provided, 2);
		<<"/bad/options/mime">> -> ct_helper:ignore(cowboy_rest, set_content_type, 2);
		<<"/bad/options/etag">> -> ct_helper:ignore(cowboy_static, generate_etag, 2);
		<<"/bad/options/charset">> -> ct_helper:ignore(cowboy_static, charsets_provided, 2);
		_ -> ok
	end,
	{ok, Req, Env}.

%% Internal functions.

-spec do_charset_crash(_) -> no_return().
do_charset_crash(_) ->
	ct_helper_error_h:ignore(?MODULE, do_charset_crash, 1),
	exit(crash).

do_charset_custom(Path) ->
	case filename:extension(Path) of
		<<".cowboy">> -> <<"utf-32">>;
		<<".html">> -> <<"utf-16">>;
		_ -> <<"utf-8">>
	end.

-spec do_etag_crash(_, _, _) -> no_return().
do_etag_crash(_, _, _) ->
	ct_helper_error_h:ignore(?MODULE, do_etag_crash, 3),
	exit(crash).

do_etag_custom(_, _, _) ->
	{strong, <<"etag">>}.

-spec do_mime_crash(_) -> no_return().
do_mime_crash(_) ->
	ct_helper_error_h:ignore(?MODULE, do_mime_crash, 1),
	exit(crash).

do_mime_custom(Path) ->
	case filename:extension(Path) of
		<<".cowboy">> -> <<"application/vnd.ninenines.cowboy+xml;v=1">>;
		<<".txt">> -> <<"text/plain">>;
		_ -> {<<"application">>, <<"octet-stream">>, []}
	end.

do_get(Path, Config) ->
	do_get(Path, [], Config).

do_get(Path, ReqHeaders, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, [{<<"accept-encoding">>, <<"gzip">>}|ReqHeaders]),
	{response, IsFin, Status, RespHeaders} = gun:await(ConnPid, Ref),
	{ok, Body} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref);
		fin -> {ok, <<>>}
	end,
	gun:close(ConnPid),
	{Status, RespHeaders, Body}.

%% Tests.

bad(Config) ->
	doc("Bad cowboy_static options: not a tuple."),
	{500, _, _} = do_get("/bad", Config),
	ok.

bad_dir_path(Config) ->
	doc("Bad cowboy_static options: wrong path."),
	{404, _, _} = do_get("/bad/dir/path/style.css", Config),
	ok.

bad_dir_route(Config) ->
	doc("Bad cowboy_static options: missing [...] in route."),
	{500, _, _} = do_get("/bad/dir/route", Config),
	ok.

bad_file_in_priv_dir_in_ez_archive(Config) ->
	doc("Get a missing file from a priv_dir stored in Erlang application .ez archive."),
	{404, _, _} = do_get("/ez_priv_dir/index.php", Config),
	ok.

bad_file_path(Config) ->
	doc("Bad cowboy_static options: wrong path."),
	{404, _, _} = do_get("/bad/file/path", Config),
	ok.

bad_options(Config) ->
	doc("Bad cowboy_static extra options: not a list."),
	{500, _, _} = do_get("/bad/options", Config),
	ok.

bad_options_charset(Config) ->
	doc("Bad cowboy_static extra options: invalid charset option."),
	{500, _, _} = do_get("/bad/options/charset", Config),
	ok.

bad_options_etag(Config) ->
	doc("Bad cowboy_static extra options: invalid etag option."),
	{500, _, _} = do_get("/bad/options/etag", Config),
	ok.

bad_options_mime(Config) ->
	doc("Bad cowboy_static extra options: invalid mimetypes option."),
	{500, _, _} = do_get("/bad/options/mime", Config),
	ok.

bad_priv_dir_app(Config) ->
	doc("Bad cowboy_static options: wrong application name."),
	{500, _, _} = do_get("/bad/priv_dir/app/style.css", Config),
	ok.

bad_priv_dir_in_ez_archive(Config) ->
	doc("Bad cowboy_static options: priv_dir path missing from Erlang application .ez archive."),
	{404, _, _} = do_get("/bad/ez_priv_dir/index.html", Config),
	ok.

bad_priv_dir_no_priv(Config) ->
	doc("Bad cowboy_static options: application has no priv directory."),
	{404, _, _} = do_get("/bad/priv_dir/no-priv/style.css", Config),
	ok.

bad_priv_dir_path(Config) ->
	doc("Bad cowboy_static options: wrong path."),
	{404, _, _} = do_get("/bad/priv_dir/path/style.css", Config),
	ok.

bad_priv_dir_route(Config) ->
	doc("Bad cowboy_static options: missing [...] in route."),
	{500, _, _} = do_get("/bad/priv_dir/route", Config),
	ok.

bad_priv_file_app(Config) ->
	doc("Bad cowboy_static options: wrong application name."),
	{500, _, _} = do_get("/bad/priv_file/app", Config),
	ok.

bad_priv_file_in_ez_archive(Config) ->
	doc("Bad cowboy_static options: priv_file path missing from Erlang application .ez archive."),
	{404, _, _} = do_get("/bad/ez_priv_file/index.php", Config),
	ok.

bad_priv_file_no_priv(Config) ->
	doc("Bad cowboy_static options: application has no priv directory."),
	{404, _, _} = do_get("/bad/priv_file/no-priv", Config),
	ok.

bad_priv_file_path(Config) ->
	doc("Bad cowboy_static options: wrong path."),
	{404, _, _} = do_get("/bad/priv_file/path", Config),
	ok.

dir_cowboy(Config) ->
	doc("Get a .cowboy file."),
	{200, Headers, <<"File with custom extension.\n">>}
		= do_get(config(prefix, Config) ++ "/file.cowboy", Config),
	{_, <<"application/octet-stream">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

dir_css(Config) ->
	doc("Get a .css file."),
	{200, Headers, <<"body{color:red}\n">>}
		= do_get(config(prefix, Config) ++ "/style.css", Config),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

dir_css_urlencoded(Config) ->
	doc("Get a .css file with the extension dot urlencoded."),
	{200, Headers, <<"body{color:red}\n">>}
		= do_get(config(prefix, Config) ++ "/style%2ecss", Config),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

dir_dot_file(Config) ->
	doc("Get a file with extra dot segments in the path."),
	%% All these are equivalent.
	{200, _, _} = do_get(config(prefix, Config) ++ "/./style.css", Config),
	{200, _, _} = do_get(config(prefix, Config) ++ "/././style.css", Config),
	{200, _, _} = do_get(config(prefix, Config) ++ "/./././style.css", Config),
	{200, _, _} = do_get("/./priv_dir/style.css", Config),
	{200, _, _} = do_get("/././priv_dir/style.css", Config),
	{200, _, _} = do_get("/./././priv_dir/style.css", Config),
	ok.

dir_dotdot_file(Config) ->
	doc("Get a file with extra dotdot segments in the path."),
	%% All these are equivalent.
	{200, _, _} = do_get("/../priv_dir/style.css", Config),
	{200, _, _} = do_get("/../../priv_dir/style.css", Config),
	{200, _, _} = do_get("/../../../priv_dir/style.css", Config),
	{200, _, _} = do_get(config(prefix, Config) ++ "/../priv_dir/style.css", Config),
	{200, _, _} = do_get(config(prefix, Config) ++ "/../../priv_dir/style.css", Config),
	{200, _, _} = do_get(config(prefix, Config) ++ "/../../../priv_dir/style.css", Config),
	{200, _, _} = do_get("/../priv_dir/../priv_dir/style.css", Config),
	{200, _, _} = do_get("/../../priv_dir/../../priv_dir/style.css", Config),
	{200, _, _} = do_get("/../../../priv_dir/../../../priv_dir/style.css", Config),
	%% Try with non-existing segments, which may correspond to real folders.
	{200, _, _} = do_get("/anything/../priv_dir/style.css", Config),
	{200, _, _} = do_get(config(prefix, Config) ++ "/anything/../style.css", Config),
	{200, _, _} = do_get(config(prefix, Config) ++ "/directory/../style.css", Config),
	{200, _, _} = do_get(config(prefix, Config) ++ "/static/../style.css", Config),
	%% Try with segments corresponding to real files. It works because
	%% URI normalization happens before looking at the filesystem.
	{200, _, _} = do_get(config(prefix, Config) ++ "/style.css/../style.css", Config),
	{200, _, _} = do_get(config(prefix, Config) ++ "/style.css/../../priv_dir/style.css", Config),
	%% Try to fool the server to accept segments corresponding to real folders.
	{404, _, _} = do_get(config(prefix, Config) ++ "/../static/style.css", Config),
	{404, _, _} = do_get(config(prefix, Config) ++ "/directory/../../static/style.css", Config),
	ok.

dir_empty_file(Config) ->
	doc("Get an empty .txt file."),
	{200, _, <<>>} = do_get(config(prefix, Config) ++ "/empty.txt", Config),
	ok.

dir_error_directory(Config) ->
	doc("Try to get a directory."),
	{403, _, _} = do_get(config(prefix, Config) ++ "/directory", Config),
	ok.

dir_error_directory_slash(Config) ->
	doc("Try to get a directory with an extra slash in the path."),
	{403, _, _} = do_get(config(prefix, Config) ++ "/directory/", Config),
	ok.

dir_error_doesnt_exist(Config) ->
	doc("Try to get a file that does not exist."),
	{404, Headers, _} = do_get(config(prefix, Config) ++ "/not.found", Config),
	false = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

dir_error_dot(Config) ->
	doc("Try to get a file named '.'."),
	{403, _, _} = do_get(config(prefix, Config) ++ "/.", Config),
	ok.

dir_error_dot_urlencoded(Config) ->
	doc("Try to get a file named '.' percent encoded."),
	{403, _, _} = do_get(config(prefix, Config) ++ "/%2e", Config),
	ok.

dir_error_dotdot(Config) ->
	doc("Try to get a file named '..'."),
	{404, _, _} = do_get(config(prefix, Config) ++ "/..", Config),
	ok.

dir_error_dotdot_urlencoded(Config) ->
	doc("Try to get a file named '..' percent encoded."),
	{404, _, _} = do_get(config(prefix, Config) ++ "/%2e%2e", Config),
	ok.

dir_error_empty(Config) ->
	doc("Try to get the configured directory."),
	{403, _, _} = do_get(config(prefix, Config) ++ "", Config),
	ok.

dir_error_slash(Config) ->
	%% I know the description isn't that good considering / has a meaning in URIs.
	doc("Try to get a file named '/'."),
	{403, _, _} = do_get(config(prefix, Config) ++ "//", Config),
	ok.

dir_error_slash_urlencoded(Config) ->
	doc("Try to get a file named '/' percent encoded."),
	{404, _, _} = do_get(config(prefix, Config) ++ "/%2f", Config),
	ok.

dir_error_slash_urlencoded_dotdot_file(Config) ->
	doc("Try to use a percent encoded slash to access an existing file."),
	{200, _, _} = do_get(config(prefix, Config) ++ "/directory/../style.css", Config),
	{404, _, _} = do_get(config(prefix, Config) ++ "/directory%2f../style.css", Config),
	ok.

dir_error_unreadable(Config) ->
	doc("Try to get a file that can't be read."),
	{403, _, _} = do_get(config(prefix, Config) ++ "/unreadable", Config),
	ok.

dir_html(Config) ->
	doc("Get a .html file."),
	{200, Headers, <<"<html><body>Hello!</body></html>\n">>}
		= do_get(config(prefix, Config) ++ "/index.html", Config),
	{_, <<"text/html">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

dir_large_file(Config) ->
	doc("Get a large file."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, config(prefix, Config) ++ "/large.bin",
		[{<<"accept-encoding">>, <<"gzip">>}]),
	{response, nofin, 200, RespHeaders} = gun:await(ConnPid, Ref),
	{_, <<"application/octet-stream">>} = lists:keyfind(<<"content-type">>, 1, RespHeaders),
	Size = 32*1024*1024,
	{ok, Size} = do_dir_large_file(ConnPid, Ref, 0),
	ok.

do_dir_large_file(ConnPid, Ref, N) ->
	receive
		{gun_data, ConnPid, Ref, nofin, Data} ->
			do_dir_large_file(ConnPid, Ref, N + byte_size(Data));
		{gun_data, ConnPid, Ref, fin, Data} ->
			{ok, N + byte_size(Data)};
		{gun_error, ConnPid, Ref, Reason} ->
			{error, Reason};
		{gun_error, ConnPid, Reason} ->
			{error, Reason}
	after 5000 ->
		{error, timeout}
	end.

dir_text(Config) ->
	doc("Get a .txt file. The extension is unknown by default."),
	{200, Headers, <<"Timeless space.\n">>}
		= do_get(config(prefix, Config) ++ "/plain.txt", Config),
	{_, <<"application/octet-stream">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

dir_unknown(Config) ->
	doc("Get a file with no extension."),
	{200, Headers, <<"File with no extension.\n">>}
		= do_get(config(prefix, Config) ++ "/unknown", Config),
	{_, <<"application/octet-stream">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

etag_crash(Config) ->
	doc("Get a file with a crashing etag function."),
	{500, _, _} = do_get("/etag/crash", Config),
	ok.

etag_custom(Config) ->
	doc("Get a file with custom Etag function and make sure it is used."),
	{200, Headers, _} = do_get("/etag/custom", Config),
	{_, <<"\"etag\"">>} = lists:keyfind(<<"etag">>, 1, Headers),
	ok.

etag_default(Config) ->
	doc("Get a file twice and make sure the Etag matches."),
	{200, Headers1, _} = do_get("/dir/style.css", Config),
	{200, Headers2, _} = do_get("/dir/style.css", Config),
	{_, Etag} = lists:keyfind(<<"etag">>, 1, Headers1),
	{_, Etag} = lists:keyfind(<<"etag">>, 1, Headers2),
	ok.

etag_default_change(Config) ->
	doc("Get a file, modify it, get it again and make sure the Etag doesn't match."),
	{200, Headers1, _} = do_get("/dir/index.html", Config),
	{_, Etag1} = lists:keyfind(<<"etag">>, 1, Headers1),
	ok = file:change_time(config(static_dir, Config) ++ "/index.html",
		{{config(port, Config), 1, 1}, {1, 1, 1}}),
	{200, Headers2, _} = do_get("/dir/index.html", Config),
	{_, Etag2} = lists:keyfind(<<"etag">>, 1, Headers2),
	true = Etag1 =/= Etag2,
	ok.

etag_disable(Config) ->
	doc("Get a file with disabled Etag and make sure no Etag is provided."),
	{200, Headers, _} = do_get("/etag/disable", Config),
	false = lists:keyfind(<<"etag">>, 1, Headers),
	ok.

file(Config) ->
	doc("Get a file with hardcoded route."),
	{200, Headers, <<"body{color:red}\n">>} = do_get("/file/style.css", Config),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

if_match(Config) ->
	doc("Get a file with If-Match matching."),
	{200, _, _} = do_get("/etag/custom", [
		{<<"if-match">>, <<"\"etag\"">>}
	], Config),
	ok.

if_match_fail(Config) ->
	doc("Get a file with If-Match not matching."),
	{412, _, _} = do_get("/etag/custom", [
		{<<"if-match">>, <<"\"invalid\"">>}
	], Config),
	ok.

if_match_invalid(Config) ->
	doc("Try to get a file with an invalid If-Match header."),
	{400, _, _} = do_get("/etag/custom", [
		{<<"if-match">>, <<"bad input">>}
	], Config),
	ok.

if_match_list(Config) ->
	doc("Get a file with If-Match matching."),
	{200, _, _} = do_get("/etag/custom", [
		{<<"if-match">>, <<"\"invalid\", \"etag\", \"cowboy\"">>}
	], Config),
	ok.

if_match_list_fail(Config) ->
	doc("Get a file with If-Match not matching."),
	{412, _, _} = do_get("/etag/custom", [
		{<<"if-match">>, <<"\"invalid\", W/\"etag\", \"cowboy\"">>}
	], Config),
	ok.

if_match_weak(Config) ->
	doc("Try to get a file with a weak If-Match header."),
	{412, _, _} = do_get("/etag/custom", [
		{<<"if-match">>, <<"W/\"etag\"">>}
	], Config),
	ok.

if_match_wildcard(Config) ->
	doc("Get a file with a wildcard If-Match."),
	{200, _, _} = do_get("/etag/custom", [
		{<<"if-match">>, <<"*">>}
	], Config),
	ok.

if_modified_since(Config) ->
	doc("Get a file with If-Modified-Since in the past."),
	{200, _, _} = do_get("/etag/custom", [
		{<<"if-modified-since">>, <<"Sat, 29 Oct 1994 19:43:31 GMT">>}
	], Config),
	ok.

if_modified_since_fail(Config) ->
	doc("Get a file with If-Modified-Since equal to file modification time."),
	LastModified = filelib:last_modified(config(static_dir, Config) ++ "/style.css"),
	{304, _, _} = do_get("/etag/custom", [
		{<<"if-modified-since">>, httpd_util:rfc1123_date(LastModified)}
	], Config),
	ok.

if_modified_since_future(Config) ->
	doc("Get a file with If-Modified-Since in the future."),
	{{Year, _, _}, {_, _, _}} = calendar:universal_time(),
	{200, _, _} = do_get("/etag/custom", [
		{<<"if-modified-since">>, [
			<<"Sat, 29 Oct ">>,
			integer_to_binary(Year + 1),
			<<" 19:43:31 GMT">>]}
	], Config),
	ok.

if_modified_since_if_none_match(Config) ->
	doc("Get a file with both If-Modified-Since and If-None-Match headers."
		"If-None-Match takes precedence and If-Modified-Since is ignored. (RFC7232 3.3)"),
	LastModified = filelib:last_modified(config(static_dir, Config) ++ "/style.css"),
	{200, _, _} = do_get("/etag/custom", [
		{<<"if-modified-since">>, httpd_util:rfc1123_date(LastModified)},
		{<<"if-none-match">>, <<"\"not-etag\"">>}
	], Config),
	ok.

if_modified_since_invalid(Config) ->
	doc("Get a file with an invalid If-Modified-Since header."),
	{200, _, _} = do_get("/etag/custom", [
		{<<"if-modified-since">>, <<"\"not a date\"">>}
	], Config),
	ok.

if_none_match(Config) ->
	doc("Get a file with If-None-Match not matching."),
	{200, _, _} = do_get("/etag/custom", [
		{<<"if-none-match">>, <<"\"not-etag\"">>}
	], Config),
	ok.

if_none_match_fail(Config) ->
	doc("Get a file with If-None-Match matching."),
	{304, _, _} = do_get("/etag/custom", [
		{<<"if-none-match">>, <<"\"etag\"">>}
	], Config),
	ok.

if_none_match_invalid(Config) ->
	doc("Try to get a file with an invalid If-None-Match header."),
	{400, _, _} = do_get("/etag/custom", [
		{<<"if-none-match">>, <<"bad input">>}
	], Config),
	ok.

if_none_match_list(Config) ->
	doc("Get a file with If-None-Match not matching."),
	{200, _, _} = do_get("/etag/custom", [
		{<<"if-none-match">>, <<"\"invalid\", W/\"not-etag\", \"cowboy\"">>}
	], Config),
	ok.

if_none_match_list_fail(Config) ->
	doc("Get a file with If-None-Match matching."),
	{304, _, _} = do_get("/etag/custom", [
		{<<"if-none-match">>, <<"\"invalid\", \"etag\", \"cowboy\"">>}
	], Config),
	ok.

if_none_match_weak(Config) ->
	doc("Try to get a file with a weak If-None-Match header matching."),
	{304, _, _} = do_get("/etag/custom", [
		{<<"if-none-match">>, <<"W/\"etag\"">>}
	], Config),
	ok.

if_none_match_wildcard(Config) ->
	doc("Try to get a file with a wildcard If-None-Match."),
	{304, _, _} = do_get("/etag/custom", [
		{<<"if-none-match">>, <<"*">>}
	], Config),
	ok.

if_unmodified_since(Config) ->
	doc("Get a file with If-Unmodified-Since equal to file modification time."),
	LastModified = filelib:last_modified(config(static_dir, Config) ++ "/style.css"),
	{200, _, _} = do_get("/etag/custom", [
		{<<"if-unmodified-since">>, httpd_util:rfc1123_date(LastModified)}
	], Config),
	ok.

if_unmodified_since_fail(Config) ->
	doc("Get a file with If-Unmodified-Since in the past."),
	{412, _, _} = do_get("/etag/custom", [
		{<<"if-unmodified-since">>, <<"Sat, 29 Oct 1994 19:43:31 GMT">>}
	], Config),
	ok.

if_unmodified_since_future(Config) ->
	doc("Get a file with If-Unmodified-Since in the future."),
	{{Year, _, _}, {_, _, _}} = calendar:universal_time(),
	{200, _, _} = do_get("/etag/custom", [
		{<<"if-unmodified-since">>, [
			<<"Sat, 29 Oct ">>,
			integer_to_binary(Year + 1),
			<<" 19:43:31 GMT">>]}
	], Config),
	ok.

if_unmodified_since_if_match(Config) ->
	doc("Get a file with both If-Unmodified-Since and If-Match headers."
		"If-Match takes precedence and If-Unmodified-Since is ignored. (RFC7232 3.4)"),
	{200, _, _} = do_get("/etag/custom", [
		{<<"if-unmodified-since">>, <<"Sat, 29 Oct 1994 19:43:31 GMT">>},
		{<<"if-match">>, <<"\"etag\"">>}
	], Config),
	ok.

if_unmodified_since_invalid(Config) ->
	doc("Get a file with an invalid If-Unmodified-Since header."),
	{200, _, _} = do_get("/etag/custom", [
		{<<"if-unmodified-since">>, <<"\"not a date\"">>}
	], Config),
	ok.

index_file(Config) ->
	doc("Get an index file."),
	{200, Headers, <<"<html><body>Hello!</body></html>\n">>} = do_get("/index", Config),
	{_, <<"text/html">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

index_file_slash(Config) ->
	doc("Get an index file with extra slash."),
	{200, Headers, <<"<html><body>Hello!</body></html>\n">>} = do_get("/index/", Config),
	{_, <<"text/html">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

last_modified(Config) ->
	doc("Get a file, modify it, get it again and make sure Last-Modified changes."),
	{200, Headers1, _} = do_get("/dir/file.cowboy", Config),
	{_, LastModified1} = lists:keyfind(<<"last-modified">>, 1, Headers1),
	ok = file:change_time(config(static_dir, Config) ++ "/file.cowboy",
		{{config(port, Config), 1, 1}, {1, 1, 1}}),
	{200, Headers2, _} = do_get("/dir/file.cowboy", Config),
	{_, LastModified2} = lists:keyfind(<<"last-modified">>, 1, Headers2),
	true = LastModified1 =/= LastModified2,
	ok.

mime_all_cowboy(Config) ->
	doc("Get a .cowboy file. The extension is unknown."),
	{200, Headers, _} = do_get("/mime/all/file.cowboy", Config),
	{_, <<"application/octet-stream">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

mime_all_css(Config) ->
	doc("Get a .css file."),
	{200, Headers, _} = do_get("/mime/all/style.css", Config),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

mime_all_txt(Config) ->
	doc("Get a .txt file."),
	{200, Headers, _} = do_get("/mime/all/plain.txt", Config),
	{_, <<"text/plain">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

mime_crash(Config) ->
	doc("Get a file with a crashing mimetype function."),
	{500, _, _} = do_get("/mime/crash/style.css", Config),
	ok.

mime_custom_cowboy(Config) ->
	doc("Get a .cowboy file."),
	{200, Headers, _} = do_get("/mime/custom/file.cowboy", Config),
	{_, <<"application/vnd.ninenines.cowboy+xml;v=1">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

mime_custom_css(Config) ->
	doc("Get a .css file. The extension is unknown."),
	{200, Headers, _} = do_get("/mime/custom/style.css", Config),
	{_, <<"application/octet-stream">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

mime_custom_txt(Config) ->
	doc("Get a .txt file."),
	{200, Headers, _} = do_get("/mime/custom/plain.txt", Config),
	{_, <<"text/plain">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

mime_hardcode_binary(Config) ->
	doc("Get a .cowboy file with hardcoded route and media type in binary form."),
	{200, Headers, _} = do_get("/mime/hardcode/binary-form", Config),
	{_, <<"application/vnd.ninenines.cowboy+xml;v=1">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

mime_hardcode_tuple(Config) ->
	doc("Get a .cowboy file with hardcoded route and media type in tuple form."),
	{200, Headers, _} = do_get("/mime/hardcode/tuple-form", Config),
	{_, <<"application/vnd.ninenines.cowboy+xml;v=1">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

charset_crash(Config) ->
	doc("Get a file with a crashing charset function."),
	{500, _, _} = do_get("/charset/crash/style.css", Config),
	ok.

charset_custom_cowboy(Config) ->
	doc("Get a .cowboy file."),
	{200, Headers, _} = do_get("/charset/custom/file.cowboy", Config),
	{_, <<"application/octet-stream">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

charset_custom_css(Config) ->
	doc("Get a .css file."),
	{200, Headers, _} = do_get("/charset/custom/style.css", Config),
	{_, <<"text/css; charset=utf-8">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

charset_custom_html(Config) ->
	doc("Get a .html file."),
	{200, Headers, _} = do_get("/charset/custom/index.html", Config),
	{_, <<"text/html; charset=utf-16">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

charset_hardcode_binary(Config) ->
	doc("Get a .html file with hardcoded route and charset."),
	{200, Headers, _} = do_get("/charset/hardcode", Config),
	{_, <<"text/html; charset=utf-8">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

priv_dir_in_ez_archive(Config) ->
	doc("Get a file from a priv_dir stored in Erlang application .ez archive."),
	{200, Headers, <<"<h1>It works!</h1>\n">>} = do_get("/ez_priv_dir/index.html", Config),
	{_, <<"text/html">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

priv_file(Config) ->
	doc("Get a file with hardcoded route."),
	{200, Headers, <<"body{color:red}\n">>} = do_get("/priv_file/style.css", Config),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

priv_file_in_ez_archive(Config) ->
	doc("Get a file stored in Erlang application .ez archive."),
	{200, Headers, <<"<h1>It works!</h1>\n">>} = do_get("/ez_priv_file/index.html", Config),
	{_, <<"text/html">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

range_request(Config) ->
	doc("Confirm that range requests are enabled."),
	{206, Headers, <<"less space.\n">>} = do_get("/dir/plain.txt",
		[{<<"range">>, <<"bytes=4-">>}], Config),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	{_, <<"bytes 4-15/16">>} = lists:keyfind(<<"content-range">>, 1, Headers),
	{_, <<"application/octet-stream">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

unicode_basic_latin(Config) ->
	doc("Get a file with non-urlencoded characters from Unicode Basic Latin block."),
	%% Excluding the dot which has a special meaning in URLs
	%% when they are the only content in a path segment,
	%% and is tested as part of filenames in other test cases.
	Chars0 =
		"abcdefghijklmnopqrstuvwxyz"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"0123456789"
		":@-_~!$&'()*+,;=",
	Chars = case config(case_sensitive, Config) of
		false -> Chars0 -- "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
		true -> Chars0
	end,
	_ = [case do_get("/char/" ++ [C], Config) of
		{200, _, << C >>} -> ok;
		Error -> exit({error, C, Error})
	end || C <- Chars],
	ok.

unicode_basic_error(Config) ->
	doc("Try to get a file with invalid non-urlencoded characters from Unicode Basic Latin block."),
	Exclude = case config(protocol, Config) of
		%% Some characters trigger different errors in HTTP/1.1
		%% because they are used for the protocol.
		%%
		%% # and ? indicate fragment and query components
		%% and are therefore not part of the path.
		http -> "\r\s#?";
		http2 -> "#?"
	end,
	_ = [case do_get("/char/" ++ [C], Config) of
		{400, _, _} -> ok;
		Error -> exit({error, C, Error})
	end || C <- (config(chars, Config) -- Exclude) --
		"abcdefghijklmnopqrstuvwxyz"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		"0123456789"
		":@-_~!$&'()*+,;="
	],
	ok.

unicode_basic_latin_urlencoded(Config) ->
	doc("Get a file with urlencoded characters from Unicode Basic Latin block."),
	_ = [case do_get(lists:flatten(["/char/%", io_lib:format("~2.16.0b", [C])]), Config) of
		{200, _, << C >>} -> ok;
		Error -> exit({error, C, Error})
	end || C <- config(chars, Config)],
	ok.

unknown_option(Config) ->
	doc("Get a file configured with unknown extra options."),
	{200, Headers, <<"body{color:red}\n">>} = do_get("/unknown/option", Config),
	{_, <<"text/css">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.
