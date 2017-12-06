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

-module(rfc7231_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

all() ->
	cowboy_test:common_all().

groups() ->
	cowboy_test:common_groups(ct_helper:all(?MODULE)).

init_per_group(Name, Config) ->
	cowboy_test:init_common_groups(Name, Config, ?MODULE).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

init_dispatch(_) ->
	cowboy_router:compile([{"[...]", [
		{"*", asterisk_h, []},
		{"/", hello_h, []},
		{"/echo/:key", echo_h, []},
		{"/resp/:key[/:arg]", resp_h, []}
	]}]).

%% @todo The documentation should list what methods, headers and status codes
%% are handled automatically so users can know what befalls to them to implement.

%% Methods.

method_get(Config) ->
	doc("The GET method is accepted. (RFC7231 4.3.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"Hello world!">>} = gun:await_body(ConnPid, Ref),
	ok.

method_head(Config) ->
	doc("The HEAD method is accepted. (RFC7231 4.3.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:head(ConnPid, "/", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, fin, 200, _} = gun:await(ConnPid, Ref),
	ok.

method_head_same_resp_headers_as_get(Config) ->
	doc("Responses to HEAD should return the same headers as GET. (RFC7231 4.3.2)"),
	ConnPid = gun_open(Config),
	Ref1 = gun:get(ConnPid, "/", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, nofin, 200, Headers1} = gun:await(ConnPid, Ref1),
	{ok, <<"Hello world!">>} = gun:await_body(ConnPid, Ref1),
	Ref2 = gun:head(ConnPid, "/", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, fin, 200, Headers2} = gun:await(ConnPid, Ref2),
	%% We remove the date header since the date might have changed between requests.
	Headers = lists:keydelete(<<"date">>, 1, Headers1),
	Headers = lists:keydelete(<<"date">>, 1, Headers2),
	ok.

method_head_same_resp_headers_as_get_stream_reply(Config) ->
	doc("Responses to HEAD should return the same headers as GET. (RFC7231 4.3.2)"),
	ConnPid = gun_open(Config),
	Ref1 = gun:get(ConnPid, "/resp/stream_reply2/200", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, nofin, 200, Headers1} = gun:await(ConnPid, Ref1),
	{ok, _} = gun:await_body(ConnPid, Ref1),
	Ref2 = gun:head(ConnPid, "/resp/stream_reply2/200", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, fin, 200, Headers2} = gun:await(ConnPid, Ref2),
	%% We remove the date header since the date might have changed between requests.
	Headers = lists:keydelete(<<"date">>, 1, Headers1),
	Headers = lists:keydelete(<<"date">>, 1, Headers2),
	ok.

method_post(Config) ->
	doc("The POST method is accepted. (RFC7231 4.3.3)"),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/read_body", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"application/x-www-form-urlencoded">>}
	], <<"hello=world">>),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"hello=world">>} = gun:await_body(ConnPid, Ref),
	ok.

method_put(Config) ->
	doc("The PUT method is accepted. (RFC7231 4.3.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:put(ConnPid, "/echo/read_body", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"application/x-www-form-urlencoded">>}
	], <<"hello=world">>),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"hello=world">>} = gun:await_body(ConnPid, Ref),
	ok.

method_delete(Config) ->
	doc("The DELETE method is accepted. (RFC7231 4.3.5)"),
	ConnPid = gun_open(Config),
	Ref = gun:delete(ConnPid, "/echo/method", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"DELETE">>} = gun:await_body(ConnPid, Ref),
	ok.

method_connect(Config) ->
	doc("The CONNECT method is currently not implemented. (RFC7231 4.3.6)"),
	ConnPid = gun_open(Config),
	Ref = gun:request(ConnPid, <<"CONNECT">>, "localhost:8080", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, fin, 501, _} = gun:await(ConnPid, Ref),
	ok.

method_options(Config) ->
	doc("The OPTIONS method is accepted. (RFC7231 4.3.7)"),
	ConnPid = gun_open(Config),
	Ref = gun:options(ConnPid, "/echo/method", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"OPTIONS">>} = gun:await_body(ConnPid, Ref),
	ok.

method_options_asterisk(Config) ->
	doc("The OPTIONS method is accepted with an asterisk. (RFC7231 4.3.7)"),
	ConnPid = gun_open(Config),
	Ref = gun:options(ConnPid, "*", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-echo">>, <<"method">>}
	]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"OPTIONS">>} = gun:await_body(ConnPid, Ref),
	ok.

method_options_content_length_0(Config) ->
	doc("The OPTIONS method must set the content-length header "
		"to 0 when no body is returned. (RFC7231 4.3.7)"),
	ConnPid = gun_open(Config),
	Ref = gun:options(ConnPid, "*", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, fin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"0">>} = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

method_trace(Config) ->
	doc("The TRACE method is currently not implemented. (RFC7231 4.3.8)"),
	ConnPid = gun_open(Config),
	Ref = gun:request(ConnPid, <<"TRACE">>, "/", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, fin, 501, _} = gun:await(ConnPid, Ref),
	ok.

%% Request headers.

%% @todo

%% Status codes.

status_code_100(Config) ->
	doc("The 100 Continue status code can be sent. (RFC7231 6.2.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/inform2/100", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{inform, 100, []} = gun:await(ConnPid, Ref),
	ok.

%http10_status_code_100(Config) ->

status_code_101(Config) ->
	doc("The 101 Switching Protocols status code can be sent. (RFC7231 6.2.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/inform2/101", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{inform, 101, []} = gun:await(ConnPid, Ref),
	ok.

%http10_status_code_100(Config) ->

status_code_200(Config) ->
	doc("The 200 OK status code can be sent. (RFC7231 6.3.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/200", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, _} = gun:await(ConnPid, Ref),
	ok.

status_code_201(Config) ->
	doc("The 201 Created status code can be sent. (RFC7231 6.3.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/201", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 201, _} = gun:await(ConnPid, Ref),
	ok.

status_code_202(Config) ->
	doc("The 202 Accepted status code can be sent. (RFC7231 6.3.3)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/202", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 202, _} = gun:await(ConnPid, Ref),
	ok.

status_code_203(Config) ->
	doc("The 203 Non-Authoritative Information status code can be sent. (RFC7231 6.3.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/203", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 203, _} = gun:await(ConnPid, Ref),
	ok.

status_code_204(Config) ->
	doc("The 204 No Content status code can be sent. (RFC7231 6.3.5)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/204", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 204, _} = gun:await(ConnPid, Ref),
	ok.

status_code_205(Config) ->
	doc("The 205 Reset Content status code can be sent. (RFC7231 6.3.6)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/205", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 205, _} = gun:await(ConnPid, Ref),
	ok.

status_code_300(Config) ->
	doc("The 300 Multiple Choices status code can be sent. (RFC7231 6.4.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/300", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 300, _} = gun:await(ConnPid, Ref),
	ok.

status_code_301(Config) ->
	doc("The 301 Moved Permanently status code can be sent. (RFC7231 6.4.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/301", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 301, _} = gun:await(ConnPid, Ref),
	ok.

status_code_302(Config) ->
	doc("The 302 Found status code can be sent. (RFC7231 6.4.3)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/302", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 302, _} = gun:await(ConnPid, Ref),
	ok.

status_code_303(Config) ->
	doc("The 303 See Other status code can be sent. (RFC7231 6.4.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/303", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 303, _} = gun:await(ConnPid, Ref),
	ok.

status_code_305(Config) ->
	doc("The 305 Use Proxy status code can be sent. (RFC7231 6.4.5)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/305", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 305, _} = gun:await(ConnPid, Ref),
	ok.

%% The status code 306 is no longer used. (RFC7231 6.4.6)

status_code_307(Config) ->
	doc("The 307 Temporary Redirect status code can be sent. (RFC7231 6.4.7)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/307", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 307, _} = gun:await(ConnPid, Ref),
	ok.

status_code_400(Config) ->
	doc("The 400 Bad Request status code can be sent. (RFC7231 6.5.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/400", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 400, _} = gun:await(ConnPid, Ref),
	ok.

status_code_402(Config) ->
	doc("The 402 Payment Required status code can be sent. (RFC7231 6.5.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/402", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 402, _} = gun:await(ConnPid, Ref),
	ok.

status_code_403(Config) ->
	doc("The 403 Forbidden status code can be sent. (RFC7231 6.5.3)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/403", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 403, _} = gun:await(ConnPid, Ref),
	ok.

status_code_404(Config) ->
	doc("The 404 Not Found status code can be sent. (RFC7231 6.5.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/404", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 404, _} = gun:await(ConnPid, Ref),
	ok.

status_code_405(Config) ->
	doc("The 405 Method Not Allowed status code can be sent. (RFC7231 6.5.5)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/405", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 405, _} = gun:await(ConnPid, Ref),
	ok.

status_code_406(Config) ->
	doc("The 406 Not Acceptable status code can be sent. (RFC7231 6.5.6)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/406", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 406, _} = gun:await(ConnPid, Ref),
	ok.

status_code_408(Config) ->
	doc("The 408 Request Timeout status code can be sent. (RFC7231 6.5.7)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/408", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 408, _} = gun:await(ConnPid, Ref),
	ok.

status_code_409(Config) ->
	doc("The 409 Conflict status code can be sent. (RFC7231 6.5.8)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/409", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 409, _} = gun:await(ConnPid, Ref),
	ok.

status_code_410(Config) ->
	doc("The 410 Gone status code can be sent. (RFC7231 6.5.9)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/410", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 410, _} = gun:await(ConnPid, Ref),
	ok.

status_code_411(Config) ->
	doc("The 411 Length Required status code can be sent. (RFC7231 6.5.10)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/411", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 411, _} = gun:await(ConnPid, Ref),
	ok.

status_code_413(Config) ->
	doc("The 413 Payload Too Large status code can be sent. (RFC7231 6.5.11)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/413", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 413, _} = gun:await(ConnPid, Ref),
	ok.

status_code_414(Config) ->
	doc("The 414 URI Too Long status code can be sent. (RFC7231 6.5.12)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/414", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 414, _} = gun:await(ConnPid, Ref),
	ok.

status_code_415(Config) ->
	doc("The 415 Unsupported Media Type status code can be sent. (RFC7231 6.5.13)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/415", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 415, _} = gun:await(ConnPid, Ref),
	ok.

status_code_417(Config) ->
	doc("The 417 Expectation Failed status code can be sent. (RFC7231 6.5.14)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/417", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 417, _} = gun:await(ConnPid, Ref),
	ok.

status_code_426(Config) ->
	doc("The 426 Upgrade Required status code can be sent. (RFC7231 6.5.15)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/426", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 426, _} = gun:await(ConnPid, Ref),
	ok.

status_code_500(Config) ->
	doc("The 500 Internal Server Error status code can be sent. (RFC7231 6.6.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/500", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 500, _} = gun:await(ConnPid, Ref),
	ok.

status_code_501(Config) ->
	doc("The 501 Not Implemented status code can be sent. (RFC7231 6.6.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/501", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 501, _} = gun:await(ConnPid, Ref),
	ok.

status_code_502(Config) ->
	doc("The 502 Bad Gateway status code can be sent. (RFC7231 6.6.3)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/502", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 502, _} = gun:await(ConnPid, Ref),
	ok.

status_code_503(Config) ->
	doc("The 503 Service Unavailable status code can be sent. (RFC7231 6.6.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/503", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 503, _} = gun:await(ConnPid, Ref),
	ok.

status_code_504(Config) ->
	doc("The 504 Gateway Timeout status code can be sent. (RFC7231 6.6.5)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/504", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 504, _} = gun:await(ConnPid, Ref),
	ok.

status_code_505(Config) ->
	doc("The 505 HTTP Version Not Supported status code can be sent. (RFC7231 6.6.6)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/505", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 505, _} = gun:await(ConnPid, Ref),
	ok.
