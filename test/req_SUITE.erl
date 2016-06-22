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

-module(req_SUITE).
-compile(export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

%% ct.

all() ->
	cowboy_test:common_all().

groups() ->
	AllTests = ct_helper:all(?MODULE),
	[
		{http, [parallel], AllTests},
		{https, [parallel], AllTests},
		{h2, [parallel], AllTests},
		{h2c, [parallel], AllTests}
		%% @todo With compression enabled.
	].

init_per_group(Name, Config) ->
	cowboy_test:init_common_groups(Name, Config, ?MODULE).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

%% Routes.

init_dispatch(_) ->
	cowboy_router:compile([{"[...]", [
		{"/no/:key", echo_h, []},
		{"/args/:key/:arg[/:default]", echo_h, []},
		{"/:key/[...]", echo_h, []}
	]}]).

%% Internal.

do_body(Method, Path, Config) ->
	do_body(Method, Path, [], Config).

do_body(Method, Path, Headers, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:request(ConnPid, Method, Path, Headers),
	{response, IsFin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref);
		fin -> {ok, <<>>}
	end,
	gun:close(ConnPid),
	Body.

do_get_body(Path, Config) ->
	do_get_body(Path, [], Config).

do_get_body(Path, Headers, Config) ->
	do_body("GET", Path, Headers, Config).

%% Tests.

binding(Config) ->
	doc("Value bound from request URI path with/without default."),
	<<"binding">> = do_get_body("/args/binding/key", Config),
	<<"binding">> = do_get_body("/args/binding/key/default", Config),
	<<"default">> = do_get_body("/args/binding/undefined/default", Config),
	ok.

%% @todo Do we really want a key/value list here instead of a map?
bindings(Config) ->
	doc("Values bound from request URI path."),
	<<"[{key,<<\"bindings\">>}]">> = do_get_body("/bindings", Config),
	ok.

header(Config) ->
	doc("Request header with/without default."),
	<<"value">> = do_get_body("/args/header/defined", [{<<"defined">>, "value"}], Config),
	<<"value">> = do_get_body("/args/header/defined/default", [{<<"defined">>, "value"}], Config),
	<<"default">> = do_get_body("/args/header/undefined/default", [{<<"defined">>, "value"}], Config),
	ok.

headers(Config) ->
	doc("Request headers."),
	<< "#{<<\"header\">> => <<\"value\">>", _/bits >>
		= do_get_body("/headers", [{<<"header">>, "value"}], Config),
	ok.

host(Config) ->
	doc("Request URI host."),
	<<"localhost">> = do_get_body("/host", Config),
	ok.

host_info(Config) ->
	doc("Request host_info."),
	<<"[<<\"localhost\">>]">> = do_get_body("/host_info", Config),
	ok.

match_cookies(Config) ->
	doc("Matched request cookies."),
	<<"#{}">> = do_get_body("/match/cookies", [{<<"cookie">>, "a=b; c=d"}], Config),
	<<"#{a => <<\"b\">>}">> = do_get_body("/match/cookies/a", [{<<"cookie">>, "a=b; c=d"}], Config),
	<<"#{c => <<\"d\">>}">> = do_get_body("/match/cookies/c", [{<<"cookie">>, "a=b; c=d"}], Config),
	<<"#{a => <<\"b\">>,c => <<\"d\">>}">> = do_get_body("/match/cookies/a/c",
		[{<<"cookie">>, "a=b; c=d"}], Config),
	%% This function is tested more extensively through unit tests.
	ok.

match_qs(Config) ->
	doc("Matched request URI query string."),
	<<"#{}">> = do_get_body("/match/qs?a=b&c=d", Config),
	<<"#{a => <<\"b\">>}">> = do_get_body("/match/qs/a?a=b&c=d", Config),
	<<"#{c => <<\"d\">>}">> = do_get_body("/match/qs/c?a=b&c=d", Config),
	<<"#{a => <<\"b\">>,c => <<\"d\">>}">> = do_get_body("/match/qs/a/c?a=b&c=d", Config),
	%% This function is tested more extensively through unit tests.
	ok.

method(Config) ->
	doc("Request method."),
	<<"GET">> = do_body("GET", "/method", Config),
	<<"HEAD">> = do_body("HEAD", "/method", Config),
	<<"OPTIONS">> = do_body("OPTIONS", "/method", Config),
	<<"PATCH">> = do_body("PATCH", "/method", Config),
	<<"POST">> = do_body("POST", "/method", Config),
	<<"PUT">> = do_body("PUT", "/method", Config),
	<<"ZZZZZZZZ">> = do_body("ZZZZZZZZ", "/method", Config),
	ok.

%% @todo Do we really want a key/value list here instead of a map?
parse_cookies(Config) ->
	doc("Request cookies."),
	<<"[]">> = do_get_body("/parse_cookies", Config),
	<<"[{<<\"cake\">>,<<\"strawberry\">>}]">>
		= do_get_body("/parse_cookies", [{<<"cookie">>, "cake=strawberry"}], Config),
	<<"[{<<\"cake\">>,<<\"strawberry\">>},{<<\"color\">>,<<\"blue\">>}]">>
		= do_get_body("/parse_cookies", [{<<"cookie">>, "cake=strawberry; color=blue"}], Config),
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
	ok.

%% @todo Do we really want a key/value list here instead of a map?
parse_qs(Config) ->
	doc("Parsed request URI query string."),
	<<"[]">> = do_get_body("/parse_qs", Config),
	<<"[{<<\"abc\">>,true}]">> = do_get_body("/parse_qs?abc", Config),
	<<"[{<<\"a\">>,<<\"b\">>},{<<\"c\">>,<<\"d e\">>}]">> = do_get_body("/parse_qs?a=b&c=d+e", Config),
	ok.

path(Config) ->
	doc("Request URI path."),
	<<"/path/to/the/resource">> = do_get_body("/path/to/the/resource", Config),
	<<"/path/to/the/resource">> = do_get_body("/path/to/the/resource?query", Config),
	<<"/path/to/the/resource">> = do_get_body("/path/to/the/resource?query#fragment", Config),
	<<"/path/to/the/resource">> = do_get_body("/path/to/the/resource#fragment", Config),
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
	doc("Request peer."),
	<<"{{127,0,0,1},", _/bits >> = do_get_body("/peer", Config),
	ok.

port(Config) ->
	doc("Request URI port."),
	Port = integer_to_binary(config(port, Config)),
	Port = do_get_body("/port", Config),
	ok.

qs(Config) ->
	doc("Request URI query string."),
	<<>> = do_get_body("/qs", Config),
	<<"abc">> = do_get_body("/qs?abc", Config),
	<<"a=b&c=d+e">> = do_get_body("/qs?a=b&c=d+e", Config),
	ok.

scheme(Config) ->
	doc("Request URI scheme."),
	Transport = config(type, Config),
	case do_get_body("/scheme", Config) of
		<<"http">> when Transport =:= tcp -> ok;
		<<"https">> when Transport =:= ssl -> ok
	end.

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
	Protocol = config(protocol, Config),
	case do_get_body("/version", Config) of
		<<"HTTP/1.1">> when Protocol =:= http -> ok;
		<<"HTTP/2">> when Protocol =:= http2 -> ok
	end.
