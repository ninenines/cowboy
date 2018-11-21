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
		{"/accept_callback", accept_callback_h, []},
		{"/accept_callback_missing", accept_callback_missing_h, []},
		{"/charsets_provided", charsets_provided_h, []},
		{"/charsets_provided_empty", charsets_provided_empty_h, []},
		{"/charset_in_content_types_provided",
			charset_in_content_types_provided_h, []},
		{"/charset_in_content_types_provided_implicit",
			charset_in_content_types_provided_implicit_h, []},
		{"/charset_in_content_types_provided_implicit_no_callback",
			charset_in_content_types_provided_implicit_no_callback_h, []},
		{"/content_types_accepted", content_types_accepted_h, []},
		{"/content_types_provided", content_types_provided_h, []},
		{"/delete_resource", delete_resource_h, []},
		{"/expires", expires_h, []},
		{"/generate_etag", generate_etag_h, []},
		{"/if_range", if_range_h, []},
		{"/last_modified", last_modified_h, []},
		{"/provide_callback_missing", provide_callback_missing_h, []},
		{"/provide_range_callback", provide_range_callback_h, []},
		{"/range_satisfiable", range_satisfiable_h, []},
		{"/ranges_provided", ranges_provided_h, []},
		{"/ranges_provided_auto", ranges_provided_auto_h, []},
		{"/rate_limited", rate_limited_h, []},
		{"/stop_handler", stop_handler_h, []},
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

accept_callback_missing(Config) ->
	doc("A 500 response must be sent when the AcceptCallback can't be called."),
	ConnPid = gun_open(Config),
	Ref = gun:put(ConnPid, "/accept_callback_missing", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"text/plain">>}
	], <<"Missing!">>),
	{response, fin, 500, _} = gun:await(ConnPid, Ref),
	ok.

accept_callback_patch_false(Config) ->
	do_accept_callback_false(Config, patch).

accept_callback_patch_true(Config) ->
	do_accept_callback_true(Config, patch).

accept_callback_post_false(Config) ->
	do_accept_callback_false(Config, post).

accept_callback_post_true(Config) ->
	do_accept_callback_true(Config, post).

accept_callback_put_false(Config) ->
	do_accept_callback_false(Config, put).

accept_callback_put_true(Config) ->
	do_accept_callback_true(Config, put).

do_accept_callback_false(Config, Fun) ->
	doc("When AcceptCallback returns false a 400 response must be returned."),
	ConnPid = gun_open(Config),
	Ref = gun:Fun(ConnPid, "/accept_callback?false", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"text/plain">>}
	], <<"Request body.">>),
	{response, _, 400, _} = gun:await(ConnPid, Ref),
	ok.

do_accept_callback_true(Config, Fun) ->
	doc("When AcceptCallback returns true a 204 response must be returned."),
	ConnPid = gun_open(Config),
	Ref = gun:Fun(ConnPid, "/accept_callback?true", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"text/plain">>}
	], <<"Request body.">>),
	{response, _, 204, _} = gun:await(ConnPid, Ref),
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

content_type_invalid(Config) ->
	doc("An invalid content-type in a POST/PATCH/PUT request "
		"must be rejected with a 415 unsupported media type response. (RFC7231 6.5.13)"),
	ConnPid = gun_open(Config),
	Ref = gun:put(ConnPid, "/content_types_accepted?wildcard-param", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"text/plain, text/html">>}
	]),
	{response, fin, 415, _} = gun:await(ConnPid, Ref),
	ok.

content_types_accepted_ignore_multipart_boundary(Config) ->
	doc("When a multipart content-type is provided for the request "
		"body, the boundary parameter is not expected to be returned "
		"from the content_types_accepted callback and will be "
		"automatically ignored."),
	ConnPid = gun_open(Config),
	Ref = gun:put(ConnPid, "/content_types_accepted?multipart", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"multipart/mixed; boundary=abcdef; v=1">>}
	], <<"Not really multipart!">>),
	{response, _, 204, _} = gun:await(ConnPid, Ref),
	ok.

content_types_accepted_param(Config) ->
	doc("When a parameter is returned from the content_types_accepted "
		"callback, and the same parameter is found in the content-type "
		"header, the negotiation succeeds and the request is processed."),
	ConnPid = gun_open(Config),
	Ref = gun:put(ConnPid, "/content_types_accepted?param", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"text/plain;charset=UTF-8">>}
	], "12345"),
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	ok.

content_types_accepted_wildcard_param_no_content_type_param(Config) ->
	doc("When a wildcard is returned for parameters from the "
		"content_types_accepted callback, a content-type header "
		"with no parameters must be accepted."),
	ConnPid = gun_open(Config),
	Ref = gun:put(ConnPid, "/content_types_accepted?wildcard-param", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"text/plain">>}
	]),
	gun:data(ConnPid, Ref, fin, "Hello world!"),
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	ok.

content_types_accepted_wildcard_param_content_type_with_param(Config) ->
	doc("When a wildcard is returned for parameters from the "
		"content_types_accepted callback, a content-type header "
		"with a parameter must be accepted."),
	ConnPid = gun_open(Config),
	Ref = gun:put(ConnPid, "/content_types_accepted?wildcard-param", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	]),
	gun:data(ConnPid, Ref, fin, "Hello world!"),
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	ok.

content_types_provided_wildcard_param_no_accept_param(Config) ->
	doc("When a wildcard is returned for parameters from the "
		"content_types_provided callback, an accept header "
		"with no parameters must be accepted."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/content_types_provided?wildcard-param", [
		{<<"accept">>, <<"text/plain">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"[]">>} = gun:await_body(ConnPid, Ref),
	ok.

content_types_provided_wildcard_param_accept_with_param(Config) ->
	doc("When a wildcard is returned for parameters from the "
		"content_types_provided callback, an accept header "
		"with a parameter must be accepted."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/content_types_provided?wildcard-param", [
		{<<"accept">>, <<"text/plain;level=1">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"level=1">>} = gun:await_body(ConnPid, Ref),
	ok.

content_types_provided_wildcard_param_accept_with_param_and_qvalue(Config) ->
	doc("When a wildcard is returned for parameters from the "
		"content_types_provided callback, an accept header "
		"with two media types containing parameters including a "
		"q-value must be accepted. The q-value determines which."),
	ConnPid = gun_open(Config),
	Ref1 = gun:get(ConnPid, "/content_types_provided?wildcard-param", [
		{<<"accept">>, <<"text/plain;level=1;q=0.8, text/plain;level=2;q=0.5">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref1),
	{ok, <<"level=1">>} = gun:await_body(ConnPid, Ref1),
	Ref2 = gun:get(ConnPid, "/content_types_provided?wildcard-param", [
		{<<"accept">>, <<"text/plain;level=1;q=0.5, text/plain;level=2;q=0.8">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref2),
	{ok, <<"level=2">>} = gun:await_body(ConnPid, Ref2),
	ok.

content_types_provided_wildcard_param_no_accept_header(Config) ->
	doc("When a wildcard is returned for parameters from the "
		"content_types_provided callback, the lack of accept header "
		"results in the first media type returned being accepted. "
		"The wildcard must however not be present in the media_type "
		"value added to the Req object."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/content_types_provided?wildcard-param", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"[]">>} = gun:await_body(ConnPid, Ref),
	ok.

delete_resource_missing(Config) ->
	doc("When a resource accepts the DELETE method and the "
		"delete_resource callback is not exported, the "
		"resource is incorrect and a 500 response is expected."),
	ConnPid = gun_open(Config),
	Ref = gun:delete(ConnPid, "/delete_resource?missing", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 500, _} = gun:await(ConnPid, Ref),
	ok.

error_on_malformed_accept(Config) ->
	doc("A malformed Accept header must result in a 400 response."),
	do_error_on_malformed_header(Config, <<"accept">>).

error_on_malformed_if_match(Config) ->
	doc("A malformed If-Match header must result in a 400 response."),
	do_error_on_malformed_header(Config, <<"if-match">>).

error_on_malformed_if_none_match(Config) ->
	doc("A malformed If-None-Match header must result in a 400 response."),
	do_error_on_malformed_header(Config, <<"if-none-match">>).

do_error_on_malformed_header(Config, Name) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/", [
		{<<"accept-encoding">>, <<"gzip">>},
		{Name, <<"bad">>}
	]),
	{response, _, 400, _} = gun:await(ConnPid, Ref),
	ok.

expires_binary(Config) ->
	doc("The expires header can also be given as a binary "
		"to indicate a date in the past. (RFC7234 5.3)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/expires?binary", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"0">>} = lists:keyfind(<<"expires">>, 1, Headers),
	ok.

expires_missing(Config) ->
	doc("The expires header must not be sent when the callback is not exported."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/expires?missing", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	false = lists:keyfind(<<"expires">>, 1, Headers),
	ok.

expires_tuple(Config) ->
	doc("The expires header can be given as a date tuple. (RFC7234 5.3)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/expires?tuple", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"Fri, 21 Sep 2012 22:36:14 GMT">>} = lists:keyfind(<<"expires">>, 1, Headers),
	ok.

expires_undefined(Config) ->
	doc("The expires header must not be sent when undefined is returned."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/expires?undefined", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	false = lists:keyfind(<<"expires">>, 1, Headers),
	ok.

generate_etag_missing(Config) ->
	doc("The etag header must not be sent when "
		"the generate_etag callback is not exported."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/generate_etag?missing", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	false = lists:keyfind(<<"etag">>, 1, Headers),
	ok.

generate_etag_binary_strong(Config) ->
	doc("The etag header must be sent when the generate_etag "
		"callback returns a strong binary. (RFC7232 2.3)"),
	do_generate_etag(Config, "binary-strong-quoted",
		[], 200, {<<"etag">>, <<"\"etag-header-value\"">>}).

generate_etag_binary_weak(Config) ->
	doc("The etag header must be sent when the generate_etag "
		"callback returns a weak binary. (RFC7232 2.3)"),
	do_generate_etag(Config, "binary-weak-quoted",
		[], 200, {<<"etag">>, <<"W/\"etag-header-value\"">>}).

generate_etag_invalid_binary_strong_unquoted(Config) ->
	doc("When Cowboy cannot parse the generate_etag callback's "
		"return value, a 500 response is returned without the etag header."),
	do_generate_etag(Config, "binary-strong-unquoted", [], 500, false).

generate_etag_invalid_binary_weak_unquoted(Config) ->
	doc("When Cowboy cannot parse the generate_etag callback's "
		"return value, a 500 response is returned without the etag header."),
	do_generate_etag(Config, "binary-weak-unquoted", [], 500, false).

generate_etag_tuple_strong(Config) ->
	doc("The etag header must be sent when the generate_etag "
		"callback returns a strong tuple. (RFC7232 2.3)"),
	do_generate_etag(Config, "tuple-strong",
		[], 200, {<<"etag">>, <<"\"etag-header-value\"">>}).

generate_etag_tuple_weak(Config) ->
	doc("The etag header must be sent when the generate_etag "
		"callback returns a weak tuple. (RFC7232 2.3)"),
	do_generate_etag(Config, "tuple-weak",
		[], 200, {<<"etag">>, <<"W/\"etag-header-value\"">>}).

if_none_match_binary_strong(Config) ->
	doc("When the if-none-match header matches a strong etag, "
		"a 304 not modified response is returned. (RFC7232 3.2)"),
	do_generate_etag(Config, "binary-strong-quoted",
		[{<<"if-none-match">>, <<"\"etag-header-value\"">>}],
		304, {<<"etag">>, <<"\"etag-header-value\"">>}).

if_none_match_binary_weak(Config) ->
	doc("When the if-none-match header matches a weak etag, "
		"a 304 not modified response is returned. (RFC7232 3.2)"),
	do_generate_etag(Config, "binary-weak-quoted",
		[{<<"if-none-match">>, <<"W/\"etag-header-value\"">>}],
		304, {<<"etag">>, <<"W/\"etag-header-value\"">>}).

if_none_match_tuple_strong(Config) ->
	doc("When the if-none-match header matches a strong etag, "
		"a 304 not modified response is returned. (RFC7232 3.2)"),
	do_generate_etag(Config, "tuple-strong",
		[{<<"if-none-match">>, <<"\"etag-header-value\"">>}],
		304, {<<"etag">>, <<"\"etag-header-value\"">>}).

if_none_match_tuple_weak(Config) ->
	doc("When the if-none-match header matches a weak etag, "
		"a 304 not modified response is returned. (RFC7232 3.2)"),
	do_generate_etag(Config, "tuple-weak",
		[{<<"if-none-match">>, <<"W/\"etag-header-value\"">>}],
		304, {<<"etag">>, <<"W/\"etag-header-value\"">>}).

do_generate_etag(Config, Qs, ReqHeaders, Status, Etag) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/generate_etag?" ++ Qs, [
		{<<"accept-encoding">>, <<"gzip">>}
		|ReqHeaders
	]),
	{response, _, Status, RespHeaders} = gun:await(ConnPid, Ref),
	Etag = lists:keyfind(<<"etag">>, 1, RespHeaders),
	ok.

if_range_etag_equal(Config) ->
	doc("When the if-range header matches, a 206 partial content "
		"response is expected for an otherwise valid range request. (RFC7233 3.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/if_range", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>},
		{<<"if-range">>, <<"\"strong-and-match\"">>}
	]),
	{response, nofin, 206, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	{_, <<"bytes 0-19/20">>} = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

if_range_etag_not_equal(Config) ->
	doc("When the if-range header does not match, the range header "
		"must be ignored and a 200 OK response is expected for "
		"an otherwise valid range request. (RFC7233 3.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/if_range", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>},
		{<<"if-range">>, <<"\"strong-but-no-match\"">>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	false = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

if_range_ignored_when_no_range_header(Config) ->
	doc("When there is no range header the if-range header is ignored "
		"and a 200 OK response is expected (RFC7233 3.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/if_range", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"if-range">>, <<"\"strong-and-match\"">>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	false = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

if_range_ignored_when_ranges_provided_missing(Config) ->
	doc("When the resource does not support range requests "
		"the range and if-range headers must be ignored"
		"and a 200 OK response is expected (RFC7233 3.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/if_range?missing-ranges_provided", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>},
		{<<"if-range">>, <<"\"strong-and-match\"">>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	false = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	false = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

if_range_ignored_when_ranges_provided_empty(Config) ->
	doc("When the resource does not support range requests "
		"the range and if-range headers must be ignored"
		"and a 200 OK response is expected (RFC7233 3.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/if_range?empty-ranges_provided", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>},
		{<<"if-range">>, <<"\"strong-and-match\"">>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"none">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	false = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

if_range_weak_etag_not_equal(Config) ->
	doc("The if-range header must not match weak etags; the range header "
		"must be ignored and a 200 OK response is expected for "
		"an otherwise valid range request. (RFC7233 3.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/if_range?weak-etag", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>},
		{<<"if-range">>, <<"W/\"weak-no-match\"">>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	false = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

if_range_date_not_equal(Config) ->
	doc("The if-range header must not match weak dates. Cowboy "
		"currently has no way of knowing whether a resource was "
		"updated twice within the same second. The range header "
		"must be ignored and a 200 OK response is expected for "
		"an otherwise valid range request. (RFC7233 3.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/if_range", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>},
		{<<"if-range">>, <<"Fri, 22 Feb 2222 11:11:11 GMT">>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	false = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

last_modified(Config) ->
	doc("The last-modified header can be given as a date tuple. (RFC7232 2.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/last_modified?tuple", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"Fri, 21 Sep 2012 22:36:14 GMT">>} = lists:keyfind(<<"last-modified">>, 1, Headers),
	ok.

last_modified_missing(Config) ->
	doc("The last-modified header must not be sent when the callback is not exported."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/last_modified?missing", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	false = lists:keyfind(<<"last-modified">>, 1, Headers),
	ok.

options_missing(Config) ->
	doc("A successful OPTIONS request to a simple handler results in "
		"a 200 OK response with the allow header set. (RFC7231 4.3.7)"),
	ConnPid = gun_open(Config),
	Ref = gun:options(ConnPid, "/", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, fin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"HEAD, GET, OPTIONS">>} = lists:keyfind(<<"allow">>, 1, Headers),
	ok.

provide_callback(Config) ->
	doc("A successful GET request to a simple handler results in "
		"a 200 OK response with the content-type set. (RFC7231 4.3.1, RFC7231 6.3.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/", [
		{<<"accept">>, <<"*/*">>},
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"text/plain">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	{ok, <<"This is REST!">>} = gun:await_body(ConnPid, Ref),
	ok.

provide_callback_missing(Config) ->
	doc("A 500 response must be sent when the ProvideCallback can't be called."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/provide_callback_missing", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, fin, 500, _} = gun:await(ConnPid, Ref),
	ok.

provide_range_callback(Config) ->
	doc("A successful request for a single range results in a "
		"206 partial content response with content-range set. (RFC7233 4.1, RFC7233 4.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/provide_range_callback", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>}
	]),
	{response, nofin, 206, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	{_, <<"bytes 0-19/20">>} = lists:keyfind(<<"content-range">>, 1, Headers),
	{_, <<"text/plain">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	{ok, <<"This is ranged REST!">>} = gun:await_body(ConnPid, Ref),
	ok.

provide_range_callback_sendfile(Config) ->
	doc("A successful request for a single range results in a "
		"206 partial content response with content-range set. (RFC7233 4.1, RFC7233 4.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/provide_range_callback?sendfile", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>}
	]),
	Path = code:lib_dir(cowboy) ++ "/ebin/cowboy.app",
	Size = filelib:file_size(Path),
	{ok, Body} = file:read_file(Path),
	{response, nofin, 206, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	{_, ContentRange} = lists:keyfind(<<"content-range">>, 1, Headers),
	ContentRange = iolist_to_binary([
		<<"bytes 0-">>,
		integer_to_binary(Size - 1),
		<<"/">>,
		integer_to_binary(Size)
	]),
	{_, <<"text/plain">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.

provide_range_callback_multipart(Config) ->
	doc("A successful request for multiple ranges results in a "
		"206 partial content response using the multipart/byteranges "
		"content-type and the content-range not being set. The real "
		"content-type and content-range of the parts can be found in "
		"the multipart headers. (RFC7233 4.1, RFC7233 A)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/provide_range_callback", [
		{<<"accept-encoding">>, <<"gzip">>},
		%% This range selects everything except the space characters.
		{<<"range">>, <<"bytes=0-3, 5-6, 8-13, 15-">>}
	]),
	{response, nofin, 206, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	false = lists:keyfind(<<"content-range">>, 1, Headers),
	{_, <<"multipart/byteranges; boundary=", Boundary/bits>>}
		= lists:keyfind(<<"content-type">>, 1, Headers),
	{ok, Body0} = gun:await_body(ConnPid, Ref),
	Body = do_decode(Headers, Body0),
	{ContentRanges, BodyAcc} = do_provide_range_callback_multipart_body(Body, Boundary, [], <<>>),
	[
		{bytes, 0, 3, 20},
		{bytes, 5, 6, 20},
		{bytes, 8, 13, 20},
		{bytes, 15, 19, 20}
	] = ContentRanges,
	<<"ThisisrangedREST!">> = BodyAcc,
	ok.

provide_range_callback_multipart_sendfile(Config) ->
	doc("A successful request for multiple ranges results in a "
		"206 partial content response using the multipart/byteranges "
		"content-type and the content-range not being set. The real "
		"content-type and content-range of the parts can be found in "
		"the multipart headers. (RFC7233 4.1, RFC7233 A)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/provide_range_callback?sendfile", [
		{<<"accept-encoding">>, <<"gzip">>},
		%% This range selects a few random chunks of the file.
		{<<"range">>, <<"bytes=50-99, 150-199, 250-299, -99">>}
	]),
	Path = code:lib_dir(cowboy) ++ "/ebin/cowboy.app",
	Size = filelib:file_size(Path),
	Skip = Size - 399,
	{ok, <<
		_:50/binary, Body1:50/binary,
		_:50/binary, Body2:50/binary,
		_:50/binary, Body3:50/binary,
		_:Skip/binary, Body4/bits>>} = file:read_file(Path),
	{response, nofin, 206, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	false = lists:keyfind(<<"content-range">>, 1, Headers),
	{_, <<"multipart/byteranges; boundary=", Boundary/bits>>}
		= lists:keyfind(<<"content-type">>, 1, Headers),
	{ok, Body0} = gun:await_body(ConnPid, Ref),
	Body = do_decode(Headers, Body0),
	%% We will receive the ranges in the same order as requested.
	{ContentRanges, BodyAcc} = do_provide_range_callback_multipart_body(Body, Boundary, [], <<>>),
	LastFrom = 300 + Skip,
	LastTo = Size - 1,
	[
		{bytes, 50, 99, Size},
		{bytes, 150, 199, Size},
		{bytes, 250, 299, Size},
		{bytes, LastFrom, LastTo, Size}
	] = ContentRanges,
	BodyAcc = <<Body1/binary, Body2/binary, Body3/binary, Body4/binary>>,
	ok.

do_provide_range_callback_multipart_body(Rest, Boundary, ContentRangesAcc, BodyAcc) ->
	case cow_multipart:parse_headers(Rest, Boundary) of
		{ok, Headers, Rest1} ->
			{_, ContentRange0} = lists:keyfind(<<"content-range">>, 1, Headers),
			ContentRange = cow_http_hd:parse_content_range(ContentRange0),
			{_, <<"text/plain">>} = lists:keyfind(<<"content-type">>, 1, Headers),
			case cow_multipart:parse_body(Rest1, Boundary) of
				{done, Body} ->
					do_provide_range_callback_multipart_body(<<>>, Boundary,
						[ContentRange|ContentRangesAcc],
						<<BodyAcc/binary, Body/binary>>);
				{done, Body, Rest2} ->
					do_provide_range_callback_multipart_body(Rest2, Boundary,
						[ContentRange|ContentRangesAcc],
						<<BodyAcc/binary, Body/binary>>)
			end;
		{done, <<>>} ->
			{lists:reverse(ContentRangesAcc), BodyAcc}
	end.

provide_range_callback_metadata(Config) ->
	doc("A successful request for a single range results in a "
		"206 partial content response with the same headers that "
		"a normal 200 OK response would, like vary or etag. (RFC7233 4.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/provide_range_callback", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>}
	]),
	{response, nofin, 206, Headers} = gun:await(ConnPid, Ref),
	{_, _} = lists:keyfind(<<"date">>, 1, Headers),
	{_, _} = lists:keyfind(<<"etag">>, 1, Headers),
	{_, _} = lists:keyfind(<<"expires">>, 1, Headers),
	{_, _} = lists:keyfind(<<"last-modified">>, 1, Headers),
	{_, _} = lists:keyfind(<<"vary">>, 1, Headers),
	%% Also cache-control and content-location but we don't send those.
	ok.

provide_range_callback_missing(Config) ->
	doc("A 500 response must be sent when the ProvideRangeCallback can't be called."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/provide_range_callback?missing", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>}
	]),
	{response, fin, 500, _} = gun:await(ConnPid, Ref),
	ok.

range_ignore_unknown_unit(Config) ->
	doc("The range header must be ignored when the range unit "
		"is not found in ranges_provided. (RFC7233 3.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/if_range", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"chapters=1-">>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	false = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

range_ignore_when_not_modified(Config) ->
	doc("The range header must be ignored when a conditional "
		"GET results in a 304 not modified response. (RFC7233 3.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/if_range", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>},
		{<<"if-none-match">>, <<"\"strong-and-match\"">>}
	]),
	{response, fin, 304, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	false = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

range_satisfiable(Config) ->
	doc("When the range_satisfiable callback returns true "
		"a 206 partial content response is expected for "
		"an otherwise valid range request. (RFC7233 4.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/range_satisfiable?true", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>}
	]),
	{response, nofin, 206, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	{_, <<"bytes 0-19/20">>} = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

range_not_satisfiable(Config) ->
	doc("When the range_satisfiable callback returns false "
		"a 416 range not satisfiable response is expected for "
		"an otherwise valid range request. (RFC7233 4.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/range_satisfiable?false", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>}
	]),
	{response, fin, 416, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	false = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

range_not_satisfiable_int(Config) ->
	doc("When the range_satisfiable callback returns false "
		"a 416 range not satisfiable response is expected for "
		"an otherwise valid range request. If an integer is "
		"provided it is used to construct the content-range "
		"header. (RFC7233 4.2, RFC7233 4.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/range_satisfiable?false-int", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>}
	]),
	{response, fin, 416, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	{_, <<"bytes */123">>} = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

range_not_satisfiable_bin(Config) ->
	doc("When the range_satisfiable callback returns false "
		"a 416 range not satisfiable response is expected for "
		"an otherwise valid range request. If a binary is "
		"provided it is used to construct the content-range "
		"header. (RFC7233 4.2, RFC7233 4.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/range_satisfiable?false-bin", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>}
	]),
	{response, fin, 416, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	{_, <<"bytes */456">>} = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

range_satisfiable_missing(Config) ->
	doc("When the range_satisfiable callback is missing "
		"a 206 partial content response is expected for "
		"an otherwise valid range request. (RFC7233 4.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/range_satisfiable?missing", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>}
	]),
	{response, _, 206, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	{_, <<"bytes ", _/bits>>} = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

ranges_provided_accept_ranges(Config) ->
	doc("When the ranges_provided callback exists the accept-ranges header "
		"is sent in the response. (RFC7233 2.3)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/ranges_provided?list", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes, pages, chapters">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	ok.

%% @todo Probably should have options to do this automatically for auto at least.
%%
%%   A server that supports range requests MAY ignore or reject a Range
%%   header field that consists of more than two overlapping ranges, or a
%%   set of many small ranges that are not listed in ascending order,
%%   since both are indications of either a broken client or a deliberate
%%   denial-of-service attack (Section 6.1).

%% @todo Probably should have options for auto as well to join ranges that
%% are very close from each other.

ranges_provided_auto_data(Config) ->
	doc("When the unit range is bytes and the callback is 'auto' "
		"Cowboy will call the normal ProvideCallback and perform "
		"the range calculations automatically."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/ranges_provided_auto?data", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=8-">>}
	]),
	{response, nofin, 206, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	{_, <<"bytes 8-19/20">>} = lists:keyfind(<<"content-range">>, 1, Headers),
	{_, <<"text/plain">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	{ok, <<"ranged REST!">>} = gun:await_body(ConnPid, Ref),
	ok.

ranges_provided_auto_sendfile(Config) ->
	doc("When the unit range is bytes and the callback is 'auto' "
		"Cowboy will call the normal ProvideCallback and perform "
		"the range calculations automatically."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/ranges_provided_auto?sendfile", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=8-">>}
	]),
	Path = code:lib_dir(cowboy) ++ "/ebin/cowboy.app",
	Size = filelib:file_size(Path),
	{ok, <<_:8/binary, Body/bits>>} = file:read_file(Path),
	{response, nofin, 206, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	{_, ContentRange} = lists:keyfind(<<"content-range">>, 1, Headers),
	ContentRange = iolist_to_binary([
		<<"bytes 8-">>,
		integer_to_binary(Size - 1),
		<<"/">>,
		integer_to_binary(Size)
	]),
	{_, <<"text/plain">>} = lists:keyfind(<<"content-type">>, 1, Headers),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.

ranges_provided_auto_multipart_data(Config) ->
	doc("When the unit range is bytes and the callback is 'auto' "
		"Cowboy will call the normal ProvideCallback and perform "
		"the range calculations automatically."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/ranges_provided_auto?data", [
		{<<"accept-encoding">>, <<"gzip">>},
		%% This range selects everything except the space characters.
		{<<"range">>, <<"bytes=0-3, 5-6, 8-13, 15-">>}
	]),
	{response, nofin, 206, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	false = lists:keyfind(<<"content-range">>, 1, Headers),
	{_, <<"multipart/byteranges; boundary=", Boundary/bits>>}
		= lists:keyfind(<<"content-type">>, 1, Headers),
	{ok, Body0} = gun:await_body(ConnPid, Ref),
	Body = do_decode(Headers, Body0),
	%% We will receive the ranges in the same order as requested.
	{ContentRanges, BodyAcc} = do_provide_range_callback_multipart_body(Body, Boundary, [], <<>>),
	[
		{bytes, 0, 3, 20},
		{bytes, 5, 6, 20},
		{bytes, 8, 13, 20},
		{bytes, 15, 19, 20}
	] = ContentRanges,
	<<"ThisisrangedREST!">> = BodyAcc,
	ok.

ranges_provided_auto_multipart_sendfile(Config) ->
	doc("When the unit range is bytes and the callback is 'auto' "
		"Cowboy will call the normal ProvideCallback and perform "
		"the range calculations automatically."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/ranges_provided_auto?sendfile", [
		{<<"accept-encoding">>, <<"gzip">>},
		%% This range selects a few random chunks of the file.
		{<<"range">>, <<"bytes=50-99, 150-199, 250-299, -99">>}
	]),
	Path = code:lib_dir(cowboy) ++ "/ebin/cowboy.app",
	Size = filelib:file_size(Path),
	Skip = Size - 399,
	{ok, <<
		_:50/binary, Body1:50/binary,
		_:50/binary, Body2:50/binary,
		_:50/binary, Body3:50/binary,
		_:Skip/binary, Body4/bits>>} = file:read_file(Path),
	{response, nofin, 206, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	false = lists:keyfind(<<"content-range">>, 1, Headers),
	{_, <<"multipart/byteranges; boundary=", Boundary/bits>>}
		= lists:keyfind(<<"content-type">>, 1, Headers),
	{ok, Body0} = gun:await_body(ConnPid, Ref),
	Body = do_decode(Headers, Body0),
	%% We will receive the ranges in the same order as requested.
	{ContentRanges, BodyAcc} = do_provide_range_callback_multipart_body(Body, Boundary, [], <<>>),
	LastFrom = 300 + Skip,
	LastTo = Size - 1,
	[
		{bytes, 50, 99, Size},
		{bytes, 150, 199, Size},
		{bytes, 250, 299, Size},
		{bytes, LastFrom, LastTo, Size}
	] = ContentRanges,
	BodyAcc = <<Body1/binary, Body2/binary, Body3/binary, Body4/binary>>,
	ok.

ranges_provided_auto_not_satisfiable_data(Config) ->
	doc("When the unit range is bytes and the callback is 'auto' "
		"Cowboy will call the normal ProvideCallback and perform "
		"the range calculations automatically. When the requested "
		"range is not satisfiable a 416 range not satisfiable response "
		"is expected. The content-range header will be set. (RFC7233 4.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/ranges_provided_auto?data", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=1000-">>}
	]),
	{response, fin, 416, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	{_, <<"bytes */20">>} = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

ranges_provided_auto_not_satisfiable_sendfile(Config) ->
	doc("When the unit range is bytes and the callback is 'auto' "
		"Cowboy will call the normal ProvideCallback and perform "
		"the range calculations automatically. When the requested "
		"range is not satisfiable a 416 range not satisfiable response "
		"is expected. The content-range header will be set. (RFC7233 4.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/ranges_provided_auto?sendfile", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=1000-">>}
	]),
	{response, fin, 416, Headers} = gun:await(ConnPid, Ref),
	{_, <<"bytes">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	Path = code:lib_dir(cowboy) ++ "/ebin/cowboy.app",
	Size = filelib:file_size(Path),
	ContentRange = iolist_to_binary([<<"bytes */">>, integer_to_binary(Size)]),
	{_, ContentRange} = lists:keyfind(<<"content-range">>, 1, Headers),
	ok.

ranges_provided_empty_accept_ranges_none(Config) ->
	doc("When the ranges_provided callback exists but returns an empty list "
		"the accept-ranges header is sent in the response with the value none. (RFC7233 2.3)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/ranges_provided?none", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"none">>} = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	ok.

ranges_provided_missing_no_accept_ranges(Config) ->
	doc("When the ranges_provided callback does not exist "
		"the accept-ranges header is not sent in the response."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/ranges_provided?missing", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	false = lists:keyfind(<<"accept-ranges">>, 1, Headers),
	ok.

rate_limited(Config) ->
	doc("A 429 response must be sent when the rate_limited callback returns true. "
		"The retry-after header is specified as an integer. (RFC6585 4, RFC7231 7.1.3)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/rate_limited?true", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, fin, 429, Headers} = gun:await(ConnPid, Ref),
	{_, <<"3600">>} = lists:keyfind(<<"retry-after">>, 1, Headers),
	ok.

rate_limited_datetime(Config) ->
	doc("A 429 response must be sent when the rate_limited callback returns true. "
		"The retry-after header is specified as a date/time tuple. (RFC6585 4, RFC7231 7.1.3)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/rate_limited?true-date", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, fin, 429, Headers} = gun:await(ConnPid, Ref),
	{_, <<"Fri, 22 Feb 2222 11:11:11 GMT">>} = lists:keyfind(<<"retry-after">>, 1, Headers),
	ok.

rate_not_limited(Config) ->
	doc("A success response must be sent when the rate_limited callback returns false."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/rate_limited?false", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	ok.

stop_handler_allowed_methods(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_allow_missing_post(Config) ->
	do_req_body_stop_handler(Config, post, ?FUNCTION_NAME).

stop_handler_charsets_provided(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_content_types_accepted(Config) ->
	do_req_body_stop_handler(Config, post, ?FUNCTION_NAME).

stop_handler_content_types_provided(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_delete_completed(Config) ->
	do_no_body_stop_handler(Config, delete, ?FUNCTION_NAME).

stop_handler_delete_resource(Config) ->
	do_no_body_stop_handler(Config, delete, ?FUNCTION_NAME).

stop_handler_forbidden(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_is_authorized(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_is_conflict(Config) ->
	do_req_body_stop_handler(Config, put, ?FUNCTION_NAME).

stop_handler_known_methods(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_languages_provided(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_malformed_request(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_moved_permanently(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_moved_temporarily(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_multiple_choices(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_options(Config) ->
	do_no_body_stop_handler(Config, options, ?FUNCTION_NAME).

stop_handler_previously_existed(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_range_satisfiable(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_ranges_provided(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_rate_limited(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_resource_exists(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_service_available(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_uri_too_long(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_valid_content_headers(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_valid_entity_length(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_accept(Config) ->
	do_req_body_stop_handler(Config, post, ?FUNCTION_NAME).

stop_handler_provide(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

stop_handler_provide_range(Config) ->
	do_no_body_stop_handler(Config, get, ?FUNCTION_NAME).

do_no_body_stop_handler(Config, Method, StateName0) ->
	doc("Send a response manually and stop the REST handler."),
	ConnPid = gun_open(Config),
	"stop_handler_" ++ StateName = atom_to_list(StateName0),
	Ref = gun:Method(ConnPid, "/stop_handler?" ++ StateName, [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>}
	]),
	{response, fin, 248, _} = gun:await(ConnPid, Ref),
	ok.

do_req_body_stop_handler(Config, Method, StateName0) ->
	doc("Send a response manually and stop the REST handler."),
	ConnPid = gun_open(Config),
	"stop_handler_" ++ StateName = atom_to_list(StateName0),
	Ref = gun:Method(ConnPid, "/stop_handler?" ++ StateName, [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"text/plain">>}
	], <<"Hocus PocuSwitch!">>),
	{response, fin, 248, _} = gun:await(ConnPid, Ref),
	ok.

switch_handler_allowed_methods(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_allow_missing_post(Config) ->
	do_req_body_switch_handler(Config, post, ?FUNCTION_NAME).

switch_handler_charsets_provided(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_content_types_accepted(Config) ->
	do_req_body_switch_handler(Config, post, ?FUNCTION_NAME).

switch_handler_content_types_provided(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_delete_completed(Config) ->
	do_no_body_switch_handler(Config, delete, ?FUNCTION_NAME).

switch_handler_delete_resource(Config) ->
	do_no_body_switch_handler(Config, delete, ?FUNCTION_NAME).

switch_handler_forbidden(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_is_authorized(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_is_conflict(Config) ->
	do_req_body_switch_handler(Config, put, ?FUNCTION_NAME).

switch_handler_known_methods(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_languages_provided(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_malformed_request(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_moved_permanently(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_moved_temporarily(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_multiple_choices(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_options(Config) ->
	do_no_body_switch_handler(Config, options, ?FUNCTION_NAME).

switch_handler_previously_existed(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_range_satisfiable(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_ranges_provided(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_rate_limited(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_resource_exists(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_service_available(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_uri_too_long(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_valid_content_headers(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_valid_entity_length(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_accept(Config) ->
	do_req_body_switch_handler(Config, post, ?FUNCTION_NAME).

switch_handler_provide(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

switch_handler_provide_range(Config) ->
	do_no_body_switch_handler(Config, get, ?FUNCTION_NAME).

do_no_body_switch_handler(Config, Method, StateName0) ->
	doc("Switch REST to loop handler for streaming the response body, "
		"with and without options."),
	"switch_handler_" ++ StateName = atom_to_list(StateName0),
	do_no_body_switch_handler1(Config, Method, "/switch_handler?" ++ StateName),
	do_no_body_switch_handler1(Config, Method, "/switch_handler_opts?" ++ StateName).

do_no_body_switch_handler1(Config, Method, Path) ->
	ConnPid = gun_open(Config),
	Ref = gun:Method(ConnPid, Path, [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"range">>, <<"bytes=0-">>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	<<"Hello\nstreamed\nworld!\n">> = do_decode(Headers, Body),
	ok.

do_req_body_switch_handler(Config, Method, StateName0) ->
	doc("Switch REST to loop handler for streaming the response body, "
		"with and without options."),
	"switch_handler_" ++ StateName = atom_to_list(StateName0),
	do_req_body_switch_handler1(Config, Method, "/switch_handler?" ++ StateName),
	do_req_body_switch_handler1(Config, Method, "/switch_handler_opts?" ++ StateName).

do_req_body_switch_handler1(Config, Method, Path) ->
	ConnPid = gun_open(Config),
	Ref = gun:Method(ConnPid, Path, [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"text/plain">>}
	], <<"Hocus PocuSwitch!">>),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	<<"Hello\nstreamed\nworld!\n">> = do_decode(Headers, Body),
	ok.
