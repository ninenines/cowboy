%% Copyright (c) 2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(rest_handler_SUITE).
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

init_per_group(Name, Config) ->
	cowboy_test:init_common_groups(Name, Config, ?MODULE).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

%% Dispatch configuration.

init_dispatch(_) ->
	cowboy_router:compile([{'_', [
		{"/", rest_hello_h, []},
		{"/charsets_provided", charsets_provided_h, []},
		{"/charsets_provided_empty", charsets_provided_empty_h, []},
		{"/charset_in_content_types_provided",
			charset_in_content_types_provided_h, []},
		{"/charset_in_content_types_provided_implicit",
			charset_in_content_types_provided_implicit_h, []},
		{"/charset_in_content_types_provided_implicit_no_callback",
			charset_in_content_types_provided_implicit_no_callback_h, []},
		{"/provide_callback_missing", provide_callback_missing_h, []},
		{"/switch_handler", switch_handler_h, run},
		{"/switch_handler_opts", switch_handler_h, hibernate}
	]}]).

%% Internal.

do_decode(Headers, Body) ->
	case lists:keyfind(<<"content-encoding">>, 1, Headers) of
		{_, <<"gzip">>} -> zlib:gunzip(Body);
		_ -> Body
	end.

%% Tests.

error_on_malformed_if_match(Config) ->
	doc("A malformed If-Match header must result in a 400 response."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"if-match">>, <<"bad">>}
	]),
	{response, _, 400, _} = gun:await(ConnPid, Ref),
	ok.

error_on_malformed_if_none_match(Config) ->
	doc("A malformed If-None-Match header must result in a 400 response."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"if-none-match">>, <<"bad">>}
	]),
	{response, _, 400, _} = gun:await(ConnPid, Ref),
	ok.

charset_in_content_types_provided(Config) ->
	doc("When a charset is matched explictly in content_types_provided, "
		"that charset is used and the charsets_provided callback is ignored."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charset_in_content_types_provided", [
		{<<"accept">>, <<"text/plain;charset=utf-8">>},
		{<<"accept-charset">>, <<"utf-16, utf-8;q=0.5">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"text/plain; charset=utf-8">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

charset_in_content_types_provided_implicit_match(Config) ->
	doc("When a charset is matched implicitly in content_types_provided, "
		"the charsets_provided callback is used to determine if the media "
		"type will match."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charset_in_content_types_provided_implicit", [
		{<<"accept">>, <<"text/plain;charset=utf-16">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"text/plain; charset=utf-16">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

charset_in_content_types_provided_implicit_nomatch(Config) ->
	doc("When a charset is matched implicitly in content_types_provided, "
		"the charsets_provided callback is used to determine if the media "
		"type will match. If it doesn't, try the next media type."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charset_in_content_types_provided_implicit", [
		{<<"accept">>, <<"text/plain;charset=utf-32, text/plain">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	%% We end up with the first charset listed in charsets_provided.
	{_, <<"text/plain; charset=utf-8">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

charset_in_content_types_provided_implicit_nomatch_error(Config) ->
	doc("When a charset is matched implicitly in content_types_provided, "
		"the charsets_provided callback is used to determine if the media "
		"type will match. If it doesn't, and there's no other media type, "
		"a 406 is returned."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charset_in_content_types_provided_implicit", [
		{<<"accept">>, <<"text/plain;charset=utf-32">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 406, _} = gun:await(ConnPid, Ref),
	ok.

charset_in_content_types_provided_implicit_no_callback(Config) ->
	doc("When a charset is matched implicitly in content_types_provided, "
		"and the charsets_provided callback is not exported, the media "
		"type will match but the charset will be ignored like all other "
		"parameters."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charset_in_content_types_provided_implicit_no_callback", [
		{<<"accept">>, <<"text/plain;charset=utf-32">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	%% The charset is ignored as if it was any other parameter.
	{_, <<"text/plain">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

charsets_provided_match_text(Config) ->
	doc("When the media type is text and the charsets_provided callback exists "
		"and the accept-charset header was sent, the selected charset is sent "
		"back in the content-type of the response."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charsets_provided", [
		{<<"accept">>, <<"text/plain">>},
		{<<"accept-charset">>, <<"utf-8;q=0.5, utf-16">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"text/plain; charset=utf-16">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

charsets_provided_match_other(Config) ->
	doc("When the media type is not text and the charsets_provided callback exists "
		"and the accept-charset header was sent, the selected charset is not sent "
		"back in the content-type of the response."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charsets_provided", [
		{<<"accept">>, <<"application/json">>},
		{<<"accept-charset">>, <<"utf-8;q=0.5, utf-16">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"application/json">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

charsets_provided_wildcard_text(Config) ->
	doc("When the media type is text and the charsets_provided callback exists "
		"and a wildcard accept-charset header was sent, the selected charset is sent "
		"back in the content-type of the response."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charsets_provided", [
		{<<"accept">>, <<"text/plain">>},
		{<<"accept-charset">>, <<"*">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"text/plain; charset=utf-8">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

charsets_provided_wildcard_other(Config) ->
	doc("When the media type is not text and the charsets_provided callback exists "
		"and a wildcard accept-charset header was sent, the selected charset is not sent "
		"back in the content-type of the response."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charsets_provided", [
		{<<"accept">>, <<"application/json">>},
		{<<"accept-charset">>, <<"*">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"application/json">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

charsets_provided_nomatch(Config) ->
	doc("Regardless of the media type negotiated, if no charset is found in the "
		"accept-charset header match a charset configured in charsets_provided, "
		"then a 406 not acceptable response is sent back."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charsets_provided", [
		{<<"accept">>, <<"text/plain">>},
		{<<"accept-charset">>, <<"utf-8;q=0, iso-8859-1">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 406, _} = gun:await(ConnPid, Ref),
	ok.

charsets_provided_noheader_text(Config) ->
	doc("When the media type is text and the charsets_provided callback exists "
		"but the accept-charset header was not sent, the first charset in the "
		"list is selected and sent back in the content-type of the response."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charsets_provided", [
		{<<"accept">>, <<"text/plain">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"text/plain; charset=utf-8">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

charsets_provided_noheader_other(Config) ->
	doc("When the media type is not text and the charsets_provided callback exists "
		"but the accept-charset header was not sent, the first charset in the "
		"list is selected but is not sent back in the content-type of the response."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charsets_provided", [
		{<<"accept">>, <<"application/json">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"application/json">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	ok.

charsets_provided_empty(Config) ->
	doc("Regardless of the media type negotiated, if the charsets_provided "
		"callback returns an empty list a 406 not acceptable response is sent back."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charsets_provided_empty", [
		{<<"accept">>, <<"text/plain">>},
		{<<"accept-charset">>, <<"utf-8q=0.5, utf-16">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 406, _} = gun:await(ConnPid, Ref),
	ok.

charsets_provided_empty_wildcard(Config) ->
	doc("Regardless of the media type negotiated, if the charsets_provided "
		"callback returns an empty list a 406 not acceptable response is sent back."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charsets_provided_empty", [
		{<<"accept">>, <<"text/plain">>},
		{<<"accept-charset">>, <<"*">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 406, _} = gun:await(ConnPid, Ref),
	ok.

charsets_provided_empty_noheader(Config) ->
	doc("Regardless of the media type negotiated, if the charsets_provided "
		"callback returns an empty list a 406 not acceptable response is sent back."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/charsets_provided_empty", [
		{<<"accept">>, <<"text/plain">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 406, _} = gun:await(ConnPid, Ref),
	ok.

provide_callback_missing(Config) ->
	doc("A 500 response must be sent when the ProvideCallback can't be called."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/provide_callback_missing", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, fin, 500, _} = gun:await(ConnPid, Ref),
	ok.

switch_handler(Config) ->
	doc("Switch REST to loop handler for streaming the response body."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/switch_handler", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	<<"Hello\nstreamed\nworld!\n">> = do_decode(Headers, Body),
	ok.

switch_handler_opts(Config) ->
	doc("Switch REST to loop handler for streaming the response body; with options."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/switch_handler_opts", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	<<"Hello\nstreamed\nworld!\n">> = do_decode(Headers, Body),
	ok.
