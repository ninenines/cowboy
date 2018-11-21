%% Copyright (c) 2011-2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(old_http_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [gun_open/2]).
-import(cowboy_test, [gun_down/1]).
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
		{group, https_compress}
	].

groups() ->
	Tests = ct_helper:all(?MODULE),
	[
		{http, [], Tests}, %% @todo parallel
		{https, [parallel], Tests},
		{http_compress, [parallel], Tests},
		{https_compress, [parallel], Tests}
	].

init_per_group(Name = http, Config) ->
	cowboy_test:init_http(Name, #{env => #{dispatch => init_dispatch(Config)}}, Config);
init_per_group(Name = https, Config) ->
	cowboy_test:init_https(Name, #{env => #{dispatch => init_dispatch(Config)}}, Config);
init_per_group(Name = http_compress, Config) ->
	cowboy_test:init_http(Name, #{
		env => #{dispatch => init_dispatch(Config)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h]
	}, Config);
init_per_group(Name = https_compress, Config) ->
	cowboy_test:init_https(Name, #{
		env => #{dispatch => init_dispatch(Config)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h]
	}, Config).

end_per_group(Name, _) ->
	ok = cowboy:stop_listener(Name).

%% Dispatch configuration.

init_dispatch(_) ->
	cowboy_router:compile([
		{"localhost", [
			{"/chunked_response", http_chunked, []},
			{"/headers/dupe", http_handler,
				[{headers, #{<<"connection">> => <<"close">>}}]},
			{"/set_resp/header", http_set_resp,
				[{headers, #{<<"vary">> => <<"Accept">>}}]},
			{"/set_resp/overwrite", http_set_resp,
				[{headers, #{<<"server">> => <<"DesireDrive/1.0">>}}]},
			{"/set_resp/body", http_set_resp,
				[{body, <<"A flameless dance does not equal a cycle">>}]},
			{"/handler_errors", http_errors, []},
			{"/echo/body", http_echo_body, []},
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
			{"/", http_handler, []}
		]}
	]).

%% Tests.

rest_bad_content_type(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:patch(ConnPid, "/bad_content_type",
		[{<<"content-type">>, <<"text/plain, text/html">>}], <<"Whatever">>),
	{response, fin, 415, _} = gun:await(ConnPid, Ref),
	ok.

rest_nodelete(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:delete(ConnPid, "/nodelete"),
	{response, fin, 500, _} = gun:await(ConnPid, Ref),
	ok.

rest_patch(Config) ->
	Tests = [
		{204, [{<<"content-type">>, <<"text/plain">>}], <<"whatever">>},
		{400, [{<<"content-type">>, <<"text/plain">>}], <<"false">>},
		{400, [{<<"content-type">>, <<"text/plain">>}], <<"stop">>},
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
