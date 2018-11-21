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
-import(cowboy_test, [gun_open/2]).
-import(cowboy_test, [raw_open/1]).
-import(cowboy_test, [raw_send/2]).
-import(cowboy_test, [raw_recv_head/1]).
-import(cowboy_test, [raw_recv/3]).

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
		{"/delay/echo/:key", echo_h, []},
		{"/resp/:key[/:arg]", resp_h, []},
		{"/ws", ws_init_h, []}
	]}]).

%% @todo The documentation should list what methods, headers and status codes
%% are handled automatically so users can know what befalls to them to implement.

%% Representations.

%% Cowboy has cowboy_compress_h that could be concerned with this.
%% However Cowboy will not attempt to compress if any content-coding
%% is already applied, regardless of what they are.
%
%   If one or more encodings have been applied to a representation, the
%   sender that applied the encodings MUST generate a Content-Encoding
%   header field that lists the content codings in the order in which
%   they were applied.  Additional information about the encoding
%   parameters can be provided by other header fields not defined by this
%   specification. (RFC7231 3.1.2.2)

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

%% @todo This test is currently broken because Gun does not
%% send a proper CONNECT request.
%method_connect(Config) ->
%	doc("The CONNECT method is currently not implemented. (RFC7231 4.3.6)"),
%	ConnPid = gun_open(Config),
%	Ref = gun:request(ConnPid, <<"CONNECT">>, "localhost:8080", [
%		{<<"accept-encoding">>, <<"gzip">>}
%	]),
%	{response, fin, 501, _} = gun:await(ConnPid, Ref),
%	ok.

%   A client sending a CONNECT request MUST send the authority form of
%   request-target (Section 5.3 of [RFC7230]); i.e., the request-target
%   consists of only the host name and port number of the tunnel
%   destination, separated by a colon.
%
%   A server MUST NOT send any Transfer-Encoding or Content-Length header
%   fields in a 2xx (Successful) response to CONNECT.  A client MUST
%   ignore any Content-Length or Transfer-Encoding header fields received
%   in a successful response to CONNECT.
%
%   A payload within a CONNECT request message has no defined semantics;
%   sending a payload body on a CONNECT request might cause some existing
%   implementations to reject the request.

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

%% @todo It could be useful to check that we can parse all request headers defined in this RFC.
%% @todo The same applies to any other RFC for which we have a test suite.

expect(Config) ->
	doc("A server that receives a 100-continue expectation should honor it. (RFC7231 5.1.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/read_body", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"application/x-www-form-urlencoded">>},
		{<<"expect">>, <<"100-continue">>}
	]),
	{inform, 100, _} = gun:await(ConnPid, Ref),
	ok.

http10_expect(Config) ->
	case config(protocol, Config) of
		http ->
			do_http10_expect(Config);
		http2 ->
			expect(Config)
	end.

do_http10_expect(Config) ->
	doc("A server that receives a 100-continue expectation "
		"in an HTTP/1.0 request must ignore it. (RFC7231 5.1.1)"),
	Body = <<"hello=world">>,
	ConnPid = gun_open(Config, #{http_opts => #{version => 'HTTP/1.0'}}),
	Ref = gun:post(ConnPid, "/echo/read_body", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"application/x-www-form-urlencoded">>},
		{<<"content-length">>, integer_to_binary(byte_size(Body))},
		{<<"expect">>, <<"100-continue">>}
	]),
	timer:sleep(500),
	ok = gun:data(ConnPid, Ref, fin, Body),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.

%% Cowboy ignores the expect header when the value is not 100-continue.
%
%   A server that receives an Expect field-value other than 100-continue
%   MAY respond with a 417 (Expectation Failed) status code to indicate
%   that the unexpected expectation cannot be met.

expect_receive_body_omit_100_continue(Config) ->
	doc("A server may omit sending a 100 Continue response if it has "
		"already started receiving the request body. (RFC7231 5.1.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/delay/echo/read_body", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"application/x-www-form-urlencoded">>},
		{<<"expect">>, <<"100-continue">>}
	], <<"hello=world">>),
	%% We receive the response directly without a 100 Continue.
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, <<"hello=world">>} = gun:await_body(ConnPid, Ref),
	ok.

expect_discard_body_skip(Config) ->
	doc("A server that responds with a final status code before reading "
		"the entire message body should keep the connection open and skip "
		"the body when appropriate. (RFC7231 5.1.1)"),
	ConnPid = gun_open(Config),
	Ref1 = gun:post(ConnPid, "/echo/method", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"application/x-www-form-urlencoded">>},
		{<<"expect">>, <<"100-continue">>}
	], <<"hello=world">>),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref1),
	{ok, <<"POST">>} = gun:await_body(ConnPid, Ref1),
	Ref2 = gun:get(ConnPid, "/echo/method", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-type">>, <<"application/x-www-form-urlencoded">>}
	]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref2),
	{ok, <<"GET">>} = gun:await_body(ConnPid, Ref2),
	ok.

expect_discard_body_close(Config) ->
	case config(protocol, Config) of
		http ->
			do_expect_discard_body_close(Config);
		http2 ->
			doc("There's no reason to close the connection when using HTTP/2, "
				"even if a stream body is too large. We just cancel the stream.")
	end.

do_expect_discard_body_close(Config) ->
	doc("A server that responds with a final status code before reading "
		"the entire message body may close the connection to avoid "
		"reading a potentially large request body. (RFC7231 5.1.1, RFC7230 6.6)"),
	ConnPid = gun_open(Config),
	Ref1 = gun:post(ConnPid, "/echo/method", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-length">>, <<"10000000">>},
		{<<"content-type">>, <<"application/x-www-form-urlencoded">>},
		{<<"expect">>, <<"100-continue">>}
	]),
	{response, nofin, 200, _Headers} = gun:await(ConnPid, Ref1),
	%% Ideally we would send a connection: close. Cowboy however
	%% cannot know the intent of the application until after we
	%% sent the response.
%	{_, <<"close">>} = lists:keyfind(<<"connection">>, 1, Headers),
	{ok, <<"POST">>} = gun:await_body(ConnPid, Ref1),
	%% The connection is gone.
	receive
		{gun_down, ConnPid, _, closed, _, _} ->
			ok
	after 1000 ->
		error(timeout)
	end.

no_accept_encoding(Config) ->
	doc("While a request with no accept-encoding header implies the "
		"user agent has no preferences and any would be acceptable, "
		"Cowboy will not serve content-codings by defaults to ensure "
		"the content can safely be read. (RFC7231 5.3.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/stream_reply2/200"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	false = lists:keyfind(<<"content-encoding">>, 1, Headers),
	ok.

%% Cowboy currently ignores any information about the identity content-coding
%% and instead considers it always acceptable.
%
%   2.  If the representation has no content-coding, then it is
%       acceptable by default unless specifically excluded by the
%       Accept-Encoding field stating either "identity;q=0" or "*;q=0"
%       without a more specific entry for "identity".

accept_encoding_gzip(Config) ->
	doc("No qvalue means the content-coding is acceptable. (RFC7231 5.3.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/stream_reply2/200", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	_ = case config(flavor, Config) of
		compress ->
			{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers);
		_ ->
			false = lists:keyfind(<<"content-encoding">>, 1, Headers)
	end,
	ok.

accept_encoding_gzip_1(Config) ->
	doc("A qvalue different than 0 means the content-coding is acceptable. (RFC7231 5.3.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/stream_reply2/200", [
		{<<"accept-encoding">>, <<"gzip;q=1.0">>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	_ = case config(flavor, Config) of
		compress ->
			{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers);
		_ ->
			false = lists:keyfind(<<"content-encoding">>, 1, Headers)
	end,
	ok.

accept_encoding_gzip_0(Config) ->
	doc("A qvalue of 0 means the content-coding is not acceptable. (RFC7231 5.3.1, RFC7231 5.3.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/stream_reply2/200", [
		{<<"accept-encoding">>, <<"gzip;q=0">>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	false = lists:keyfind(<<"content-encoding">>, 1, Headers),
	ok.

%% Cowboy currently only supports gzip automatically via cowboy_compress_h.
%
%   4.  If multiple content-codings are acceptable, then the acceptable
%       content-coding with the highest non-zero qvalue is preferred.

accept_encoding_empty(Config) ->
	doc("An empty content-coding means that the user agent does not "
		"want any content-coding applied to the response. (RFC7231 5.3.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/stream_reply2/200", [
		{<<"accept-encoding">>, <<>>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	false = lists:keyfind(<<"content-encoding">>, 1, Headers),
	ok.

accept_encoding_unknown(Config) ->
	doc("An accept-encoding header only containing unknown content-codings "
		"should result in no content-coding being applied. (RFC7231 5.3.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/stream_reply2/200", [
		{<<"accept-encoding">>, <<"deflate">>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	false = lists:keyfind(<<"content-encoding">>, 1, Headers),
	ok.

%% Status codes.

http10_status_code_100(Config) ->
	case config(protocol, Config) of
		http ->
			doc("The 100 Continue status code must not "
				"be sent to HTTP/1.0 endpoints. (RFC7231 6.2)"),
			do_http10_status_code_1xx(100, Config);
		http2 ->
			status_code_100(Config)
	end.

http10_status_code_101(Config) ->
	case config(protocol, Config) of
		http ->
			doc("The 101 Switching Protocols status code must not "
				"be sent to HTTP/1.0 endpoints. (RFC7231 6.2)"),
			do_http10_status_code_1xx(101, Config);
		http2 ->
			status_code_101(Config)
	end.

do_http10_status_code_1xx(StatusCode, Config) ->
	ConnPid = gun_open(Config, #{http_opts => #{version => 'HTTP/1.0'}}),
	Ref = gun:get(ConnPid, "/resp/inform2/" ++ integer_to_list(StatusCode), [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, _} = gun:await(ConnPid, Ref),
	ok.

status_code_100(Config) ->
	doc("The 100 Continue status code can be sent. (RFC7231 6.2.1)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/inform2/100", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{inform, 100, []} = gun:await(ConnPid, Ref),
	ok.

status_code_101(Config) ->
	doc("The 101 Switching Protocols status code can be sent. (RFC7231 6.2.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/inform2/101", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{inform, 101, []} = gun:await(ConnPid, Ref),
	ok.

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

status_code_404_not_found(Config) ->
	doc("The 404 Not Found status code is sent when the target "
		"resource does not exist. (RFC7231 6.5.4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/not/found", [
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

status_code_408_connection_close(Config) ->
	case config(protocol, Config) of
		http ->
			do_http11_status_code_408_connection_close(Config);
		http2 ->
			doc("HTTP/2 connections are not closed on 408 responses.")
	end.

do_http11_status_code_408_connection_close(Config) ->
	doc("A 408 response should result in a connection close "
		"for HTTP/1.1 connections. (RFC7231 6.5.7)"),
	Client = raw_open(Config),
	ok = raw_send(Client, "GET / HTTP/1.1\r\n"),
	{_, 408, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
	{Headers, <<>>} = cow_http:parse_headers(Rest),
	{_, <<"close">>} = lists:keyfind(<<"connection">>, 1, Headers),
	{error, closed} = raw_recv(Client, 0, 1000),
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

status_code_426_upgrade_header(Config) ->
	case config(protocol, Config) of
		http ->
			do_status_code_426_upgrade_header(Config);
		http2 ->
			doc("HTTP/2 does not support the HTTP/1.1 Upgrade mechanism.")
	end.

do_status_code_426_upgrade_header(Config) ->
	doc("A 426 response must include a upgrade header. (RFC7231 6.5.15)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/ws?ok", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 426, Headers} = gun:await(ConnPid, Ref),
	{_, <<"upgrade">>} = lists:keyfind(<<"connection">>, 1, Headers),
	{_, <<"websocket">>} = lists:keyfind(<<"upgrade">>, 1, Headers),
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

%% The 505 response code is supposed to be about the major HTTP version.
%% Cowboy instead rejects any version that isn't HTTP/1.0 or HTTP/1.1
%% when expecting an h1 request. While this is not correct in theory
%% it works in practice because there are no other minor versions.
%%
%% Cowboy does not do version checking for HTTP/2 since the protocol
%% does not include a version number in the messages.

%% Response headers.

%% @todo No such header in this suite, but some in other suites (if-(un)modified-since).
%   A recipient that parses a timestamp value in an HTTP header field
%   MUST accept all three HTTP-date formats. (RFC7231 7.1.1.1)

date_imf_fixdate(Config) ->
	doc("The date header uses the IMF-fixdate format. (RFC7231 7.1.1.1, RFC7231 7.1.1.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<_,_,_,", ",_,_," ",_,_,_," ",_,_,_,_," ",_,_,":",_,_,":",_,_," GMT">>}
		= lists:keyfind(<<"date">>, 1, Headers),
	ok.

%% @todo Applies to both date and other headers (if-(un)modified-since).
%   HTTP-date is case sensitive.  A sender MUST NOT generate additional
%   whitespace in an HTTP-date beyond that specifically included as SP in
%   the grammar.  The semantics of day-name, day, month, year, and
%   time-of-day are the same as those defined for the Internet Message
%   Format constructs with the corresponding name ([RFC5322], Section
%   3.3). (RFC7231 7.1.1.1)

%% @todo No such header in this suite, but some in other suites (if-(un)modified-since).
%   Recipients of a timestamp value in rfc850-date format, which uses a
%   two-digit year, MUST interpret a timestamp that appears to be more
%   than 50 years in the future as representing the most recent year in
%   the past that had the same last two digits. (RFC7231 7.1.1.1)

%% @todo Add an option to disable sending the date header.
%   An origin server MUST NOT send a Date header field if it does not
%   have a clock capable of providing a reasonable approximation of the
%   current instance in Coordinated Universal Time. (RFC7231 7.1.1.2)

no_date_1xx(Config) ->
	doc("The date header is optional for 1xx responses. "
		"Cowboy does not send it with those responses. (RFC7231 7.1.1.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/inform2/100", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{inform, 100, Headers} = gun:await(ConnPid, Ref),
	false = lists:keyfind(<<"date">>, 1, Headers),
	ok.

date_2xx(Config) ->
	doc("A date header must be sent for 2xx status codes. (RFC7231 7.1.1.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/200", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, _} = lists:keyfind(<<"date">>, 1, Headers),
	ok.

date_3xx(Config) ->
	doc("A date header must be sent for 3xx status codes. (RFC7231 7.1.1.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/300", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 300, Headers} = gun:await(ConnPid, Ref),
	{_, _} = lists:keyfind(<<"date">>, 1, Headers),
	ok.

date_4xx(Config) ->
	doc("A date header must be sent for 4xx status codes. (RFC7231 7.1.1.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/400", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 400, Headers} = gun:await(ConnPid, Ref),
	{_, _} = lists:keyfind(<<"date">>, 1, Headers),
	ok.

date_5xx(Config) ->
	doc("The date header is optional for 5xx status codes. "
		"Cowboy however does send it with those responses. (RFC7231 7.1.1.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/500", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 500, Headers} = gun:await(ConnPid, Ref),
	{_, _} = lists:keyfind(<<"date">>, 1, Headers),
	ok.

server_header(Config) ->
	doc("An origin server may generate a server header field. "
		"Cowboy generates a small one by default. (RFC7231 7.4.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/"),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"Cowboy">>} = lists:keyfind(<<"server">>, 1, Headers),
	ok.

server_header_override(Config) ->
	doc("An origin server may generate a server header field. "
		"Cowboy allows the user to override the default. (RFC7231 7.4.2)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/set_resp_header_server"),
	{response, _, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"nginx">>} = lists:keyfind(<<"server">>, 1, Headers),
	ok.

%% @todo It's worth revisiting this RFC in the context of cowboy_rest
%% to ensure the state machine is doing what's expected by the RFC.
