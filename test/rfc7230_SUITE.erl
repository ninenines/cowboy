%% Copyright (c) 2015-2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(rfc7230_SUITE).
-compile(export_all).

-import(ct_helper, [doc/1]).
-import(cowboy_test, [raw_open/1]).
-import(cowboy_test, [raw_send/2]).
-import(cowboy_test, [raw_recv_head/1]).
-import(cowboy_test, [raw_recv/3]).

all() -> [{group, http}].

groups() -> [{http, [parallel], ct_helper:all(?MODULE)}]. %% @todo parallel

init_per_group(Name = http, Config) ->
	cowboy_test:init_http(Name = http, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))}
	}, Config).

end_per_group(Name, _) ->
	ok = cowboy:stop_listener(Name).

init_routes(_) -> [
	{"localhost", [
		{"/", hello_h, []},
		{"/echo/:key", echo_h, []}
%% @todo Something is clearly wrong about routing * right now.
%%		{"*", asterisk_h, []}
	]},
	{"127.0.0.1", [{"/echo/:key", echo_h, []}]},
	{"example.org", [{"/echo/:key", echo_h, []}]}
%% @todo Add IPv6 addresses support to the router. This fails:
%%	{"[2001:db8:85a3::8a2e:370:7334]", [{"/echo/:key", echo_h, []}]}
].

do_raw(Config, Data) ->
	Client = raw_open(Config),
	ok = raw_send(Client, Data),
	{Version, Code, Reason, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
	{Headers, Rest2} = cow_http:parse_headers(Rest),
	case lists:keyfind(<<"content-length">>, 1, Headers) of
		{_, LengthBin} when LengthBin =/= <<"0">> ->
			Length = binary_to_integer(LengthBin),
			Body = if
				byte_size(Rest2) =:= Length -> Rest2;
				true ->
					{ok, Body0} = raw_recv(Client, binary_to_integer(LengthBin) - byte_size(Rest2), 5000),
					<< Rest2/bits, Body0/bits >>
			end,
			#{client => Client, version => Version, code => Code, reason => Reason, headers => Headers, body => Body};
		_ ->
			#{client => Client, version => Version, code => Code, reason => Reason, headers => Headers, body => <<>>}
	end.

%% Listener.

%% @todo Add to documentation.
%The default port for "http" connections is 80. The connection
%uses plain TCP. (RFC7230 2.7.1)
%
%The default port for "https" connections is 443. The connection
%uses TLS. (RFC7230 2.7.2)
%
%Any other port may be used for either of them.

%% Before the request.

accept_at_least_1_empty_line(Config) ->
	doc("A configurable number of empty lines (CRLF) preceding the request "
		"must be ignored. At least 1 empty line must be ignored. (RFC7230 3.5)"),
	#{code := 200} = do_raw(Config,
		"\r\n"
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

reject_response(Config) ->
	doc("When receiving a response instead of a request, identified by the "
		"status-line which starts with the HTTP version, the server must "
		"reject the message with a 400 status code and close the connection. (RFC7230 3.1)"),
	#{code := 400, client := Client} = do_raw(Config,
		"HTTP/1.1 200 OK\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

%% Request.

only_parse_necessary_elements(Config) ->
	doc("It is only necessary to parse elements required to process the request. (RFC7230 2.5)"),
	#{code := 200} = do_raw(Config,
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Content-type: purposefully bad header value\r\n"
		"\r\n").

%% @todo Add to documentation.
%Parsed elements are subject to configurable limits. A server must
%be able to parse elements at least as long as it generates. (RFC7230 2.5)

no_empty_line_after_request_line(Config) ->
	doc("The general format of HTTP requests is strict. No empty line is "
		"allowed in-between components except for the empty line "
		"indicating the end of the list of headers."),
	#{code := 400} = do_raw(Config,
		"GET / HTTP/1.1\r\n"
		"\r\n"
		"Host: localhost\r\n"
		"\r\n").

no_empty_line_in_headers(Config) ->
	doc("The general format of HTTP requests is strict. No empty line is "
		"allowed in-between components except for the empty line "
		"indicating the end of the list of headers."),
	#{code := 400} = do_raw(Config,
		"GET / HTTP/1.1\r\n"
		"User-Agent: RFC7230\r\n"
		"\r\n"
		"Host: localhost\r\n"
		"\r\n").

timeout_before_request_line(Config) ->
	doc("The time the request (request line and headers) takes to be "
		"received by the server must be limited and subject to configuration. "
		"No response must be sent before closing if no request was initiated "
		"by the reception of a complete request-line."),
	Client = raw_open(Config),
	ok = raw_send(Client, "GET / HTTP/1.1\r"),
	{error, closed} = raw_recv(Client, 0, 6000).

timeout_after_request_line(Config) ->
	doc("The time the request (request line and headers) takes to be "
		"received by the server must be limited and subject to configuration. "
		"A 408 status code must be sent if the request line was received."),
	#{code := 408, client := Client} = do_raw(Config, "GET / HTTP/1.1\r\n"),
	{error, closed} = raw_recv(Client, 0, 6000).

%% @todo Add an HTTP/1.0 test suite.
%An HTTP/1.1 server must understand any valid HTTP/1.0 request,
%and respond to those with an HTTP/1.1 message that only use
%features understood or safely ignored by HTTP/1.0 clients. (RFC7230 A)

%% Request line.

limit_request_line_8000(Config) ->
	doc("It is recommended to limit the request-line length to a configurable "
		"limit of at least 8000 octets."),
	LongPath = ["/long-path" || _ <- lists:seq(1, 799)],
	#{code := 200} = do_raw(Config, [
		"GET /?qs=", LongPath, " HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]).

limit_request_line_9000(Config) ->
	doc("It is recommended to limit the request-line length to a configurable "
		"limit of at least 8000 octets. A request line too long must be rejected "
		"with a 414 status code and the closing of the connection. (RFC7230 3.1.1)"),
	LongPath = ["/long-path" || _ <- lists:seq(1, 899)],
	#{code := 414, client := Client} = do_raw(Config, [
		"GET /very", LongPath, " HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

%% Method.

reject_invalid_method(Config) ->
	doc("The request method is defined as 1+ token characters. An invalid "
		"method must be rejected with a 400 status code and the "
		"closing of the connection. (RFC7230 3.1.1, RFC7230 3.2.6)"),
	#{code := 400, client := Client} = do_raw(Config,
		"GET\0 / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

reject_empty_method(Config) ->
	doc("The request method is defined as 1+ token characters. An empty "
		"method must be rejected with a 400 status code and the "
		"closing of the connection. (RFC7230 3.1.1, RFC7230 3.2.6)"),
	#{code := 400, client := Client} = do_raw(Config,
		" / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

%% @todo We probably want to directly match commonly used methods.
%In practice the only characters in use by registered methods are
%uppercase letters [A-Z] and the dash "-". (IANA HTTP Method Registry)

limit_method_name(Config) ->
	doc("The length of the method must be subject to a configurable limit. "
		"A method too long must be rejected with a 501 status code and the "
		"closing of the connection. A good default for the method length limit "
		"is the longest method length the server implements. (RFC7230 3.1.1)"),
	LongMethod = [$G || _ <- lists:seq(1, 1000)],
	#{code := 501, client := Client} = do_raw(Config, [
		LongMethod, " / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

%% Between method and request-target.

reject_tab_between_method_and_request_target(Config) ->
	doc("A request that uses anything other than SP as separator between "
		"the method and the request-target must be rejected with a 400 "
		"status code and the closing of the connection. (RFC7230 3.1.1, RFC7230 3.5)"),
	#{code := 400, client := Client} = do_raw(Config,
		"GET\t/ HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

reject_two_sp_between_method_and_request_target(Config) ->
	doc("A request that uses anything other than SP as separator between "
		"the method and the request-target must be rejected with a 400 "
		"status code and the closing of the connection. (RFC7230 3.1.1, RFC7230 3.5)"),
	#{code := 400, client := Client} = do_raw(Config,
		"GET  / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

%% Request target.

ignore_uri_fragment_after_path(Config) ->
	doc("The fragment part of the target URI is not sent. It must be "
		"ignored by a server receiving it. (RFC7230 5.1)"),
	Echo = <<"http://localhost/echo/url">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET /echo/url#fragment HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

ignore_uri_fragment_after_query(Config) ->
	doc("The fragment part of the target URI is not sent. It must be "
		"ignored by a server receiving it. (RFC7230 5.1)"),
	Echo = <<"http://localhost/echo/url?key=value">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET /echo/url?key=value#fragment HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

%% Request target: origin-form.

must_understand_origin_form(Config) ->
	doc("A server must be able to handle at least origin-form and absolute-form. (RFC7230 5.3.2)"),
	#{code := 200} = do_raw(Config,
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

origin_form_reject_if_connect(Config) ->
	doc("origin-form is used when the client does not connect to a proxy, "
		"does not use the CONNECT method and does not issue a site-wide "
		"OPTIONS request. (RFC7230 5.3.1)"),
	#{code := 400, client := Client} = do_raw(Config,
		"CONNECT / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

%% @todo Equivalent test for https.
origin_form_tcp_scheme(Config) ->
	doc("The scheme is either resolved from configuration or is \"https\" "
		"when on a TLS connection and \"http\" otherwise. (RFC7230 5.5)"),
	Echo = <<"http://localhost/echo/url">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET /echo/url HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

origin_form_path(Config) ->
	doc("The absolute-path always starts with \"/\" and ends with either \"?\", \"#\" "
		"or the end of the URI. (RFC3986 3.3)"),
	Echo = <<"/echo/path">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET /echo/path HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

origin_form_path_query(Config) ->
	doc("The absolute-path always starts with \"/\" and ends with either \"?\", \"#\" "
		"or the end of the URI. (RFC3986 3.3)"),
	Echo = <<"/echo/path">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET /echo/path?key=value HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

origin_form_path_fragment(Config) ->
	doc("The absolute-path always starts with \"/\" and ends with either \"?\", \"#\" "
		"or the end of the URI. (RFC3986 3.3)"),
	Echo = <<"/echo/path">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET /echo/path#fragment HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

origin_form_query(Config) ->
	doc("The query starts with \"?\" and ends with \"#\" or the end of the URI. (RFC3986 3.4)"),
	Echo = <<"key=value">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET /echo/qs?key=value HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

origin_form_query_fragment(Config) ->
	doc("The query starts with \"?\" and ends with \"#\" or the end of the URI. (RFC3986 3.4)"),
	Echo = <<"key=value">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET /echo/qs?key=value#fragment HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

%% @todo origin_form: reject paths with too large depth or query strings with too many keys

%% Request target: absolute-form.

must_understand_absolute_form(Config) ->
	doc("A server must be able to handle at least origin-form and absolute-form. (RFC7230 5.3.2)"),
	#{code := 200} = do_raw(Config,
		"GET http://localhost HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

absolute_form_case_insensitive_scheme(Config) ->
	doc("The scheme is case insensitive and normally provided in lowercase. (RFC7230 2.7.3)"),
	Echo = <<"http://localhost/echo/url">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET HttP://localhost/echo/url HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

absolute_form_case_insensitive_host(Config) ->
	doc("The host is case insensitive and normally provided in lowercase. (RFC7230 2.7.3)"),
	Echo = <<"http://localhost/echo/url">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET http://LoCaLHOsT/echo/url HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

absolute_form_reject_unknown_schemes(Config) ->
	doc("Unknown schemes must be rejected with a 400 status code and the closing of the connection."),
	#{code := 400, client := Client} = do_raw(Config,
		"GET bad://localhost/ HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

%% @todo Equivalent test for https.
absolute_form_drop_scheme_tcp(Config) ->
	doc("The scheme provided with the request must be dropped. The effective "
		"scheme is either resolved from configuration or is \"https\" when on "
		"a TLS connection and \"http\" otherwise. (RFC7230 5.5)"),
	Echo = <<"http://localhost/echo/url">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET https://localhost/echo/url HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

absolute_form_reject_userinfo(Config) ->
	doc("An authority component with a userinfo component (and its "
		"\"@\" delimiter) is invalid. The request must be rejected with "
		"a 400 status code and the closing of the connection. (RFC7230 2.7.1)"),
	#{code := 400, client := Client} = do_raw(Config,
		"GET http://username:password@localhost HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

absolute_form_reject_missing_host_without_path(Config) ->
	doc("A URI with a missing host identifier is invalid. The request must "
		"be rejected with a 400 status code and the closing of the connection. (RFC7230 2.7.1)"),
	#{code := 400, client := Client} = do_raw(Config,
		"GET http:// HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

absolute_form_reject_missing_host_with_path(Config) ->
	doc("A URI with a missing host identifier is invalid. The request must "
		"be rejected with a 400 status code and the closing of the connection. (RFC7230 2.7.1)"),
	#{code := 400, client := Client} = do_raw(Config,
		"GET http:/// HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

absolute_form_ipv4(Config) ->
	doc("Absolute form with an IPv4 address for the host. (RFC3986 3.2.2)"),
	Echo = <<"127.0.0.1">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET http://127.0.0.1/echo/host HTTP/1.1\r\n"
		"Host: 127.0.0.1\r\n"
		"\r\n").

absolute_form_ipv4_port(Config) ->
	doc("Absolute form with an IPv4 address for the host and a port number. (RFC3986 3.2.2)"),
	Host = <<"127.0.0.1">>,
	#{code := 200, body := Host} = do_raw(Config,
		"GET http://127.0.0.1:8080/echo/host HTTP/1.1\r\n"
		"Host: 127.0.0.1:8080\r\n"
		"\r\n"),
	Port = <<"8080">>,
	#{code := 200, body := Port} = do_raw(Config,
		"GET http://127.0.0.1:8080/echo/port HTTP/1.1\r\n"
		"Host: 127.0.0.1:8080\r\n"
		"\r\n").

%% @todo We need the router to support IPv6 addresses to write proper tests for these:
%absolute_form_ipv6(Config) ->
%absolute_form_ipv6_ipv4(Config) ->
%absolute_form_ipv6_zoneid(Config) ->

absolute_form_reg_name(Config) ->
	doc("Absolute form with a regular name for the host. (RFC3986 3.2.2)"),
	Echo = <<"example.org">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET http://example.org/echo/host HTTP/1.1\r\n"
		"Host: example.org\r\n"
		"\r\n").

absolute_form_reg_name_port(Config) ->
	doc("Absolute form with an IPv4 address for the host and a port number. (RFC3986 3.2.2)"),
	Host = <<"example.org">>,
	#{code := 200, body := Host} = do_raw(Config,
		"GET http://example.org:8080/echo/host HTTP/1.1\r\n"
		"Host: example.org:8080\r\n"
		"\r\n"),
	Port = <<"8080">>,
	#{code := 200, body := Port} = do_raw(Config,
		"GET http://example.org:8080/echo/port HTTP/1.1\r\n"
		"Host: example.org:8080\r\n"
		"\r\n").

absolute_form_limit_host(Config) ->
	doc("The maximum length for the host component of the URI must be subject "
		"to a configurable limit. A good default is 255 characters. "
		"(RFC7230 3.1.1, RFC3986 3.2.2, RFC1034 3.1)"),
	LongHost = ["host." || _ <- lists:seq(1, 100)],
	#{code := 414, client := Client} = do_raw(Config, [
		"GET http://", LongHost, "/ HTTP/1.1\r\n"
		"Host: ", LongHost, "\r\n"
		"\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

absolute_form_invalid_port_0(Config) ->
	doc("Port number 0 is reserved. The request must be rejected and the connection closed."),
	#{code := 400, client := Client} = do_raw(Config,
		"GET http://localhost:0/ HTTP/1.1\r\n"
		"Host: localhost:0\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

absolute_form_invalid_port_65536(Config) ->
	doc("Port numbers above 65535 are invalid. The request must be rejected and the connection closed."),
	#{code := 400, client := Client} = do_raw(Config,
		"GET http://localhost:65536/ HTTP/1.1\r\n"
		"Host: localhost:65536\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

%% @todo The RFC says to discard the Host header if we are a proxy,
%% and replace it with the content of absolute-form. This means
%% that we should probably keep the absolute-form value when
%% operating in proxy mode. Otherwise the absolute-form value
%% is simply dropped and the Host header is used.

%% @todo The authority is sent both in the URI and in the host header.
%% The authority from the URI must be dropped, and the host header
%% must be used instead. (RFC7230 5.5)
%%
%% It is not possible to test that the absolute-form value is dropped
%% because one of the Host header test ensures that the authority
%% is the same in both, and errors out otherwise.

absolute_form_path(Config) ->
	doc("The path always starts with \"/\" and ends with either \"?\", \"#\" "
		"or the end of the URI. (RFC3986 3.3)"),
	Echo = <<"/echo/path">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET http://localhost/echo/path HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

absolute_form_path_query(Config) ->
	doc("The path always starts with \"/\" and ends with either \"?\", \"#\" "
		"or the end of the URI. (RFC3986 3.3)"),
	Echo = <<"/echo/path">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET http://localhost/echo/path?key=value HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

absolute_form_path_fragment(Config) ->
	doc("The path always starts with \"/\" and ends with either \"?\", \"#\" "
		"or the end of the URI. (RFC3986 3.3)"),
	Echo = <<"/echo/path">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET http://localhost/echo/path#fragment HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

absolute_form_no_path(Config) ->
	doc("An empty path component is equivalent to \"/\". (RFC7230 2.7.3)"),
	#{code := 200, body := <<"Hello world!">>} = do_raw(Config,
		"GET http://localhost HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

absolute_form_no_path_then_query(Config) ->
	doc("An empty path component is equivalent to \"/\". (RFC7230 2.7.3)"),
	#{code := 200, body := <<"Hello world!">>} = do_raw(Config,
		"GET http://localhost?key=value HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

absolute_form_no_path_then_fragment(Config) ->
	doc("An empty path component is equivalent to \"/\". (RFC7230 2.7.3)"),
	#{code := 200, body := <<"Hello world!">>} = do_raw(Config,
		"GET http://localhost#fragment HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

absolute_form_query(Config) ->
	doc("The query starts with \"?\" and ends with \"#\" or the end of the URI. (RFC3986 3.4)"),
	Echo = <<"key=value">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET http://localhost/echo/qs?key=value HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

absolute_form_query_fragment(Config) ->
	doc("The query starts with \"?\" and ends with \"#\" or the end of the URI. (RFC3986 3.4)"),
	Echo = <<"key=value">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET http://localhost/echo/qs?key=value#fragment HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

%% @todo absolute_form: reject paths with too large depth or query strings with too many keys

%% Request-target: authority-form.

authority_form_reject_if_not_connect(Config) ->
	doc("When the method is CONNECT, authority-form must be used. This "
		"form does not apply to any other methods which must reject the "
		"request with a 400 status code and the closing of the connection. (RFC7230 5.3.3)"),
	#{code := 400, client := Client} = do_raw(Config,
		"GET localhost:80 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

%% @todo Implement CONNECT.
%authority_form_reject_userinfo(Config) ->
%An authority component with a userinfo component (and its
%"@" delimiter) is invalid. The request must be rejected with
%a 400 status code and the closing of the connection. (RFC7230 2.7.1)
%
%authority_form_limit_host(Config) ->
%authority_form_limit_port0(Config) ->
%authority_form_limit_port65536(Config) ->
%
%A request with a too long component of authority-form must be rejected with
%a 414 status code and the closing of the connection. (RFC7230 3.1.1)
%
%The authority is either resolved from configuration or is taken
%directly from authority-form. (RFC7230 5.5)
%
%authority_form_empty_path(Config) ->
%authority_form_empty_query(Config) ->
%The path and query are empty when using authority-form. (RFC7230 5.5)

%% Request-target: asterisk-form.

asterisk_form_reject_if_not_options(Config) ->
	doc("asterisk-form is used for server-wide OPTIONS requests. "
		"It is invalid with any other methods which must reject the "
		"request with a 400 status code and the closing of the connection. (RFC7230 5.3.4)"),
	#{code := 400, client := Client} = do_raw(Config,
		"GET * HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

asterisk_form_empty_path(Config) ->
	doc("The path is empty when using asterisk-form. (RFC7230 5.5)"),
	#{code := 200, body := <<>>} = do_raw(Config,
		"OPTIONS * HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"X-Echo: path\r\n"
		"\r\n").

asterisk_form_empty_query(Config) ->
	doc("The query is empty when using asterisk-form. (RFC7230 5.5)"),
	#{code := 200, body := <<>>} = do_raw(Config,
		"OPTIONS * HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"X-Echo: query\r\n"
		"\r\n").

%% Invalid request-target.

invalid_request_target(Config) ->
	doc("Any other form is invalid and must be rejected with a 400 status code "
		"and the closing of the connection."),
	#{code := 400, client := Client} = do_raw(Config,
		"GET \0 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

%% Between request-target and version.

reject_tab_between_request_target_and_version(Config) ->
	doc("A request that uses anything other than SP as separator between "
		"the request-target and the version must be rejected with a 400 "
		"status code and the closing of the connection. (RFC7230 3.1.1, RFC7230 3.5)"),
	#{code := 400, client := Client} = do_raw(Config,
		"GET /\tHTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

reject_two_sp_between_request_target_and_version(Config) ->
	doc("A request that uses anything other than SP as separator between "
		"the request-target and the version must be rejected with a 400 "
		"status code and the closing of the connection. (RFC7230 3.1.1, RFC7230 3.5)"),
	#{code := 400, client := Client} = do_raw(Config,
		"GET /  HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

%% Request version.

reject_invalid_version_http100(Config) ->
	doc("Any version number other than HTTP/1.0 or HTTP/1.1 must be "
		"rejected by a server or intermediary with a 505 status code. (RFC7230 2.6, RFC7230 A.2)"),
	#{code := 505} = do_raw(Config,
		"GET / HTTP/1.00\r\n"
		"Host: localhost\r\n"
		"\r\n").

reject_invalid_version_http111(Config) ->
	doc("Any version number other than HTTP/1.0 or HTTP/1.1 must be "
		"rejected by a server or intermediary with a 505 status code. (RFC7230 2.6, RFC7230 A.2)"),
	#{code := 505} = do_raw(Config,
		"GET / HTTP/1.11\r\n"
		"Host: localhost\r\n"
		"\r\n").

reject_invalid_version_http12(Config) ->
	doc("Any version number other than HTTP/1.0 or HTTP/1.1 must be "
		"rejected by a server or intermediary with a 505 status code. (RFC7230 2.6, RFC7230 A.2)"),
	#{code := 505} = do_raw(Config,
		"GET / HTTP/1.2\r\n"
		"Host: localhost\r\n"
		"\r\n").

reject_invalid_version_http2(Config) ->
	doc("Any version number other than HTTP/1.0 or HTTP/1.1 must be "
		"rejected by a server or intermediary with a 505 status code. (RFC7230 2.6, RFC7230 A.2)"),
	#{code := 505} = do_raw(Config,
		"GET / HTTP/2\r\n"
		"Host: localhost\r\n"
		"\r\n").

reject_empty_version(Config) ->
	doc("Any version number other than HTTP/1.0 or HTTP/1.1 must be "
		"rejected by a server or intermediary with a 505 status code. (RFC7230 2.6, RFC7230 A.2)"),
	#{code := 505} = do_raw(Config,
		"GET / \r\n"
		"Host: localhost\r\n"
		"\r\n").

reject_invalid_whitespace_after_version(Config) ->
	doc("A request that has whitespace different than CRLF following the "
		"version must be rejected with a 400 status code and the closing "
		"of the connection. (RFC7230 3.1.1)"),
	#{code := 400, client := Client} = do_raw(Config,
		"GET / HTTP/1.1 \r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{error, closed} = raw_recv(Client, 0, 1000).

%% Request headers.

%invalid_header(Config) ->
%```
%headers = *( header-field CRLF ) CRLF
%header-field = field-name ":" OWS field-value OWS
%
%field-name = token
%field-value = *( SP / HTAB / %21-7E / %80-FF )
%
%OWS = *( SP / HTAB )
%```
%
%lower_case_header(Config) ->
%upper_case_header(Config) ->
%mixed_case_header(Config) ->
%The header field name is case insensitive. (RFC7230 3.2)
%
%reject_whitespace_before_header_name(Config) ->
%Messages that contain whitespace before the header name must
%be rejected with a 400 status code and the closing of the
%connection. (RFC7230 3.2.4)
%
%reject_whitespace_between_header_name_and_colon(Config) ->
%Messages that contain whitespace between the header name and
%colon must be rejected with a 400 status code and the closing
%of the connection. (RFC7230 3.2.4)

limit_header_name(Config) ->
	doc("The header name must be subject to a configurable limit. A "
		"good default is 50 characters, well above the longest registered "
		"header. Such a request must be rejected with a 431 status code "
		"and the closing of the connection. "
		"(RFC7230 3.2.5, RFC6585 5, IANA Message Headers registry)"),
	#{code := 431, client := Client} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n",
		binary:copy(<<$a>>, 32768), ": bad\r\n"
		"\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

limit_header_value(Config) ->
	doc("The header value and the optional whitespace around it must be "
		"subject to a configurable limit. There is no recommendations "
		"for the default. 4096 characters is known to work well. Such "
		"a request must be rejected with a 431 status code and the closing "
		"of the connection. (RFC7230 3.2.5, RFC6585 5)"),
	#{code := 431, client := Client} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"bad: ", binary:copy(<<$a>>, 32768), "\r\n"
		"\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

%drop_whitespace_before_header_value(Config) ->
%drop_whitespace_after_header_value(Config) ->
%Optional whitespace before and after the header value is not
%part of the value and must be dropped.
%
%@todo
%The order of header fields with differing names is not significant. (RFC7230 3.2.2)
%
%@todo
%The normal procedure for parsing headers is to read each header
%field into a hash table by field name until the empty line. (RFC7230 3)
%
%reject_duplicate_content_length_header(Config) ->
%reject_duplicate_host_header(Config) ->
%Requests with duplicate content-length or host headers must be rejected
%with a 400 status code and the closing of the connection. (RFC7230 3.3.2)
%
%combine_duplicate_headers(Config) ->
%Other duplicate header fields must be combined by inserting a comma
%between the values in the order they were received. (RFC7230 3.2.2)
%
%Duplicate header field names are only allowed when their value is
%a comma-separated list. In practice there is no need to perform
%a check while reading the headers as the value will become invalid
%and the error can be handled while parsing the header later on. (RFC7230 3.2.2)
%
%wait_for_eoh_before_processing_request(Config) ->
%The request must not be processed until all headers have arrived. (RFC7230 3.2.2)
%
%limit_headers(Config) ->
%The number of headers allowed in a request must be subject to
%a configurable limit. There is no recommendations for the default.
%100 headers is known to work well. Such a request must be rejected
%with a 431 status code and the closing of the connection. (RFC7230 3.2.5, RFC6585 5)
%
%@todo
%When parsing header field values, the server must ignore empty
%list elements, and not count those as the count of elements present. (RFC7230 7)
%
%@todo
%The information in the via header is largely unreliable. (RFC7230 5.7.1)
%
%%% Request body.
%
%@todo
%The message body is the octets after decoding any transfer
%codings. (RFC7230 3.3)
%
%no_request_body(Config) ->
%no_request_body_content_length_zero(Config) ->
%request_body_content_length(Config) ->
%request_body_transfer_encoding(Config) ->
%A request has a message body only if it includes a transfer-encoding
%header or a non-zero content-length header. (RFC7230 3.3)
%
%```
%Transfer-Encoding = 1#transfer-coding
%
%transfer-coding = "chunked" / "compress" / "deflate" / "gzip" / transfer-extension
%transfer-extension = token *( OWS ";" OWS transfer-parameter )
%transfer-parameter = token BWS "=" BWS ( token / quoted-string )
%```
%
%case_insensitive_transfer_encoding(Config) ->
%The transfer-coding is case insensitive. (RFC7230 4)
%
%@todo
%There are no known other transfer-extension with the exception of
%deprecated aliases "x-compress" and "x-gzip". (IANA HTTP Transfer Coding Registry,
%RFC7230 4.2.1, RFC7230 4.2.3, RFC7230 8.4.2)
%
%must_understand_chunked(Config) ->
%A server must be able to handle at least chunked transfer-encoding.
%This is also the only coding that sees widespread use. (RFC7230 3.3.1, RFC7230 4.1)
%
%reject_double_chunked_encoding(Config) ->
%Messages encoded more than once with chunked transfer-encoding
%must be rejected with a 400 status code and the closing of the
%connection. (RFC7230 3.3.1)
%
%reject_non_terminal_chunked(Config) ->
%Messages where chunked, when present, is not the last
%transfer-encoding must be rejected with a 400 status code
%and the closing of the connection. (RFC7230 3.3.3)
%
%@todo
%Some non-conformant implementations send the "deflate" compressed
%data without the zlib wrapper. (RFC7230 4.2.2)
%
%reject_unknown_transfer_encoding(Config) ->
%Messages encoded with a transfer-encoding the server does not
%understand must be rejected with a 501 status code and the
%closing of the connection. (RFC7230 3.3.1)
%
%@todo
%A server can reject requests with a body and no content-length
%header with a 411 status code. (RFC7230 3.3.3)
%
%```
%Content-Length = 1*DIGIT
%```
%
%reject_invalid_content_length(Config) ->
%A request with an invalid content-length header must be rejected
%with a 400 status code and the closing of the connection. (RFC7230 3.3.3)
%
%@todo
%The content-length header ranges from 0 to infinity. Requests
%with a message body too large must be rejected with a 413 status
%code and the closing of the connection. (RFC7230 3.3.2)
%
%ignore_content_length_when_transfer_encoding(Config) ->
%When a message includes both transfer-encoding and content-length
%headers, the content-length header must be removed before processing
%the request. (RFC7230 3.3.3)
%
%socket_error_while_reading_body(Config) ->
%If a socket error occurs while reading the body the server
%must send a 400 status code response and close the connection. (RFC7230 3.3.3, RFC7230 3.4)
%
%timeout_while_reading_body(Config) ->
%If a timeout occurs while reading the body the server must
%send a 408 status code response and close the connection. (RFC7230 3.3.3, RFC7230 3.4)
%
%%% Body length.
%
%body_length_chunked_before(Config) ->
%The length of a message with a transfer-encoding header can
%only be determined on decoding completion. (RFC7230 3.3.3)
%
%body_length_chunked_after(Config) ->
%Upon completion of chunk decoding the server must add a content-length
%header with the value set to the total length of data read. (RFC7230 4.1.3)
%
%body_length_content_length(Config) ->
%The length of a message with a content-length header is
%the numeric value in octets found in the header. (RFC7230 3.3.3)
%
%body_length_zero(Config) ->
%A message with no transfer-encoding or content-length header
%has a body length of 0. (RFC7230 3.3.3)
%
%%% Chunked transfer-encoding.
%
%reject_invalid_chunk_size(Config) ->
%
%```
%chunked-body = *chunk last-chunk trailer-part CRLF
%
%chunk = chunk-size [ chunk-ext ] CRLF chunk-data CRLF
%chunk-size = 1*HEXDIG
%chunk-data = 1*OCTET ; a sequence of chunk-size octets
%
%last-chunk = 1*("0") [ chunk-ext ] CRLF
%```
%
%The chunk-size field is a string of hex digits indicating the size of
%the chunk-data in octets.
%
%```
%chunk-ext = *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
%chunk-ext-name = token
%chunk-ext-val = token / quoted-string
%```
%
%ignore_unknown_chunk_extensions(Config) ->
%Unknown chunk extensions must be ignored. (RFC7230 4.1.1)
%
%reject_invalid_chunk_extensions(Config) ->
%
%limit_chunk_size_line(Config) ->
%The chunk-size line length must be subject to configuration.
%There are no recommended defaults, although 100 octets should work.
%Requests with a too long line must be rejected with a 400 status
%code and the closing of the connection.
%
%reject_invalid_chunk_line_crlf(Config) ->
%reject_invalid_chunk_data_crlf(Config) ->
%
%```
%trailer-part = *( header-field CRLF )
%```
%
%%% @todo see headers above and reject the same way, space etc.
%reject_invalid_trailer_part(Config) ->
%
%ignore_trailer_transfer_encoding(Config) ->
%ignore_trailer_content_length(Config) ->
%ignore_trailer_host(Config) ->
%ignore_trailer_cache_control(Config) ->
%ignore_trailer_expect(Config) ->
%ignore_trailer_max_forwards(Config) ->
%ignore_trailer_pragma(Config) ->
%ignore_trailer_range(Config) ->
%ignore_trailer_te(Config) ->
%ignore_trailer_if_match(Config) ->
%ignore_trailer_if_none_match(Config) ->
%ignore_trailer_if_modified_since(Config) ->
%ignore_trailer_if_unmodified_since(Config) ->
%ignore_trailer_if_range(Config) ->
%ignore_trailer_www_authenticate(Config) ->
%ignore_trailer_authorization(Config) ->
%ignore_trailer_proxy_authenticate(Config) ->
%ignore_trailer_proxy_authorization(Config) ->
%ignore_trailer_content_encoding(Config) ->
%ignore_trailer_content_type(Config) ->
%ignore_trailer_content_range(Config) ->
%ignore_trailer_trailer(Config) ->
%
%ignore_trailer_header(Config, Header) ->
%Trailing headers must not include transfer-encoding, content-length,
%host, cache-control, expect, max-forwards, pragma, range, te,
%if-match, if-none-match, if-modified-since, if-unmodified-since,
%if-range, www-authenticate, authorization, proxy-authenticate,
%proxy-authorization, age, cache-control, expires, date, location,
%retry-after, vary, warning, content-encoding, content-type,
%content-range, or trailer. (RFC7230 4.1.2)
%
%Trailer headers can be ignored safely. (RFC7230 4.1.2)
%
%When trailer headers are processed, invalid headers must be ignored.
%Valid headers must be added to the list of headers of the request. (RFC7230 4.1.2)
%
%limit_trailer_headers(Config) ->
%The number of trailer headers must be subject to configuration.
%There is no known recommendations for the default. A value of 10
%should cover most cases. Requests with too many trailer headers
%must be rejected with a 431 status code and the closing of the
%connection. (RFC6585 5)
%
%remove_transfer_encoding_chunked_after_body_read(Config) ->
%Upon completion of chunk decoding the server must remove "chunked"
%from the transfer-encoding header. This header must be removed if
%it becomes empty following this removal. (RFC7230 4.1.3)
%
%remove_trailer_after_body_read(Config) ->
%Upon completion of chunk decoding the server must remove the trailer
%header from the list of headers. (RFC7230 4.1.3)
%
%```
%Trailer = 1#field-name
%```
%
%ignore_chunked_headers_not_in_trailer(Config) ->
%The trailer header can be used to list the headers found in the
%trailer. A server must have the option of ignoring trailer headers
%that were not listed in the trailer header. (RFC7230 4.4)
%
%ignore_chunked_headers_if_trailer_not_in_connection(Config) ->
%The trailer header must be listed in the connection header field.
%Trailers must be ignored otherwise.
%
%%% @todo Though we need a compatibility mode as some clients don't send it...
%reject_chunked_missing_end_crlf(Config) ->
%@todo ending CRLF
%
%%% Connection management.
%
%@todo can probably test using auth
%Never assume any two requests on a single connection come
%from the same user agent. (RFC7230 2.3)
%
%```
%Connection = 1#token ; case-insensitive
%```
%
%The connection token is either case insensitive "close", "keep-alive"
%or a header field name.
%
%There are no corresponding "close" or "keep-alive" headers. (RFC7230 8.1, RFC7230 A.2)
%
%The connection header is valid only for the immediate connection,
%alongside any header field it lists. (RFC7230 6.1)
%
%The server must determine if the connection is persistent for
%every message received by looking at the connection header and
%HTTP version. (RFC7230 6.3)
%
%no_connection_header_keepalive(Config) ->
%%% @todo http/1.0 suite? connection_keepalive(Config) ->
%HTTP/1.1 requests with no "close" option and HTTP/1.0 with the
%"keep-alive" option indicate the connection will persist. (RFC7230 6.1, RFC7230 6.3)
%
%connection_close(Config) ->
%%% @todo http/1.0 suite? no_connection_close(Config) ->
%HTTP/1.1 requests with the "close" option and HTTP/1.0 with no
%"keep-alive" option indicate the connection will be closed
%upon reception of the response by the client. (RFC7230 6.1, RFC7230 6.3)
%
%limit_requests_keepalive(Config) ->
%The maximum number of requests sent using a persistent connection
%must be subject to configuration. The connection must be closed
%when the limit is reached. (RFC7230 6.3)
%
%skip_request_body_by_closing_connection(Config) ->
%%A server that doesn't want to read the entire body of a message
%%must close the connection, if possible after sending the "close"
%%connection option in the response. (RFC7230 6.3)
%
%pipeline(Config) ->
%%% @todo pipeline_parallel (safe methods can, others can't)
%A server can receive more than one request before any response
%is sent. This is called pipelining. The requests can be processed
%in parallel if they all have safe methods. Responses must be sent
%in the same order as the requests. (RFC7230 6.3.2)
%
%@todo
%The server must reject abusive traffic by closing the connection.
%Abusive traffic can come from the form of too many requests in a
%given amount of time, or too many concurrent connections. Limits
%must be subject to configuration. (RFC7230 6.4)
%
%close_inactive_connections(Config) ->
%The server must close inactive connections. The timeout
%must be subject to configuration. (RFC7230 6.5)
%
%@todo
%The server must monitor connections for the close signal
%and close the socket on its end accordingly. (RFC7230 6.5)
%
%@todo
%A connection close may occur at any time. (RFC7230 6.5)
%
%ignore_requests_after_connection_close(Config) ->
%The server must not process any request after sending or
%receiving the "close" connection option. (RFC7230 6.6)
%
%@todo
%The server must close the connection in stages to avoid the
%TCP reset problem. The server starts by closing the write
%side of the socket. The server then reads until it detects
%the socket has been closed, until it can be certain its
%last response has been received by the client, or until
%a close or timeout occurs. The server then fully close the
%connection. (6.6)
%
%%% Routing.
%
%```
%Host = authority ; same as authority-form
%```
%
%reject_missing_host(Config) ->
%An HTTP/1.1 request that lacks a host header must be rejected with
%a 400 status code and the closing of the connection. (RFC7230 5.4)
%
%%% @todo http/1.0 missing_host(Config) ->
%An HTTP/1.0 request that lack a host header is valid. Behavior
%for these requests is configuration dependent. (RFC7230 5.5)
%
%reject_invalid_host(Config) ->
%A request with an invalid host header must be rejected with a
%400 status code and the closing of the connection. (RFC7230 5.4)
%
%reject_userinfo(Config) ->
%An authority component with a userinfo component (and its
%"@" delimiter) is invalid. The request must be rejected with
%a 400 status code and the closing of the connection. (RFC7230 2.7.1)
%
%reject_absolute_form_different_host(Config) ->
%When using absolute-form the URI authority component must be
%identical to the host header. Invalid requests must be rejected
%with a 400 status code and the closing of the connection. (RFC7230 5.4)
%
%reject_authority_form_different_host(Config) ->
%When using authority-form the URI authority component must be
%identical to the host header. Invalid requests must be rejected
%with a 400 status code and the closing of the connection.
%
%empty_host(Config) ->
%The host header is empty when the authority component is undefined. (RFC7230 5.4)
%
%@todo
%The effective request URI can be rebuilt by concatenating scheme,
%"://", authority, path and query components. (RFC7230 5.5)
%
%@todo
%Resources with identical URI except for the scheme component
%must be treated as different. (RFC7230 2.7.2)
%
%%% Response.
%
%@todo
%A server can send more than one response per request only when a
%1xx response is sent preceding the final response. (RFC7230 5.6)
%
%@todo
%A server that does parallel pipelining must send responses in the
%same order as the requests came in. (RFC7230 5.6)
%
%```
%HTTP-response = status-line *( header-field CRLF ) CRLF [ message-body ]
%```
%
%@todo
%The response format must be followed strictly.
%
%```
%status-line   = HTTP-version SP status-code SP reason-phrase CRLF
%status-code   = 3DIGIT
%reason-phrase = *( HTAB / SP / VCHAR / obs-text )
%```
%
%http10_request_http11_response(Config) ->
%A server must send its own version. (RFC7230 2.6)
%
%@todo
%An HTTP/1.1 server may send an HTTP/1.0 version for compatibility purposes. (RFC7230 2.6)
%
%@todo
%RFC6585 defines additional status code a server can use to reject
%messages. (RFC7230 9.3, RFC6585)
%
%%% Response headers.
%
%@todo
%In responses, OWS must be generated as SP or not generated
%at all. RWS must be generated as SP. BWS must not be
%generated. (RFC7230 3.2.3)
%
%```
%header-field = field-name ":" SP field-value
%
%field-name = token ; case-insensitive
%field-value = *( SP / %21-7E / %80-FF )
%```
%
%@todo
%In quoted-string found in field-value, quoted-pair must only be
%used for DQUOTE and backslash. (RFC7230 3.2.6)
%
%@todo
%HTTP header values must use US-ASCII encoding and must only send
%printable characters or SP. (RFC7230 3.2.4, RFC7230 9.4)
%
%@todo
%The server must not generate empty list elements in headers. (RFC7230 7)
%
%@todo
%When encoding an URI as part of a response, only characters that
%are reserved need to be percent-encoded. (RFC7230 2.7.3)
%
%special_set_cookie_handling(Config) ->
%The set-cookie header must be handled as a special case. There
%must be exactly one set-cookie header field per cookie. (RFC7230 3.2.2)
%
%@todo
%The server must list headers for or about the immediate connection
%in the connection header field. (RFC7230 6.1)
%
%@todo
%A server that does not support persistent connections must
%send "close" in every non-1xx response. (RFC7230 6.1)
%
%no_close_in_100_response(Config) ->
%no_close_in_101_response(Config) ->
%no_close_in_102_response(Config) ->
%A server must not send a "close" connection option
%in 1xx responses. (RFC7230 6.1)
%
%@todo
%The "close" connection must be sent in a message when the
%sender knows it will close the connection after fully sending
%the response. (RFC7230 6.6)
%
%@todo
%A server must close the connection after sending or
%receiving a "close" once the response has been sent. (RFC7230 6.6)
%
%close_request_close_response(Config) ->
%A server must send a "close" in a response to a request
%containing a "close". (RFC7230 6.6)
%
%%% Response body.
%
%no_body_in_head_response(Config) -> %% @todo test different ways to send a body in response
%Responses to HEAD requests never include a message body. (RFC7230 3.3)
%
%%% @todo Implement CONNECT
%2xx responses to CONNECT requests never include a message
%body. (RFC7230 3.3)
%
%no_body_in_100_response(Config) ->
%no_body_in_101_response(Config) ->
%no_body_in_102_response(Config) ->
%no_body_in_204_response(Config) ->
%no_body_in_304_response(Config) ->
%1xx, 204 and 304 responses never include a message body. (RFC7230 3.3)
%
%same_content_length_as_get_in_head_response(Config) ->
%same_transfer_encoding_as_get_in_head_response(Config) ->
%same_content_length_as_200_in_304_response(Config) ->
%same_transfer_encoding_as_200_in_304_response(Config) ->
%Responses to HEAD requests and 304 responses can include a
%content-length or transfer-encoding header. Their value must
%be the same as if the request was an unconditional GET. (RFC7230 3.3, RFC7230 3.3.1, RFC7230 3.3.2)
%
%no_transfer_encoding_in_100_response(Config) ->
%no_transfer_encoding_in_101_response(Config) ->
%no_transfer_encoding_in_102_response(Config) ->
%no_transfer_encoding_in_204_response(Config) ->
%%% @todo CONNECT no_transfer_encoding_in_2xx_response_to_connect_request(Config) ->
%no_content_length_in_100_response(Config) ->
%no_content_length_in_101_response(Config) ->
%no_content_length_in_102_response(Config) ->
%no_content_length_in_204_response(Config) ->
%%% @todo CONNECT no_content_length_in_2xx_response_to_connect_request(Config) ->
%1xx, 204 responses and "2xx responses to CONNECT requests" must
%not include a content-length or transfer-encoding header. (RFC7230 3.3.1, RFC7230 3.3.2)
%
%```
%message-body = *OCTET
%```
%
%The message body is the octets after decoding any transfer
%codings. (RFC7230 3.3)
%
%content_length_0_when_no_body(Config) ->
%content_length_response(Config) ->
%When the length is known in advance, the server must send a
%content-length header, including if the length is 0. (RFC7230 3.3.2, RFC7230 3.3.3)
%
%chunked_response(Config) ->
%When the length is not known in advance, the chunked transfer-encoding
%must be used. (RFC7230 3.3.2, RFC7230 3.3.3)
%
%compat_no_content_length_or_transfer_encoding_close_on_body_end(Config) ->
%For compatibility purposes a server can send no content-length or
%transfer-encoding header. In this case the connection must be
%closed after the response has been sent fully. (RFC7230 3.3.2, RFC7230 3.3.3)
%
%no_content_length_if_transfer_encoding(Config) ->
%The content-length header must not be sent when a transfer-encoding
%header already exists. (RFC7230 3.3.2)
%
%@todo
%The server must not apply the chunked transfer-encoding more than
%once. (RFC7230 3.3.1)
%
%@todo
%The server must apply the chunked transfer-encoding last. (RFC7230 3.3.1)
%
%http10_request_no_transfer_encoding_in_response(Config) ->
%The transfer-encoding header must not be sent in responses to
%HTTP/1.0 requests, or in responses that use the HTTP/1.0 version.
%No transfer codings must be applied in these cases. (RFC7230 3.3.1)
%
%```
%TE = #t-codings
%
%t-codings = "trailers" / ( transfer-coding [ t-ranking ] )
%t-ranking = OWS ";" OWS "q=" rank
%rank = ( "0" [ "." 0*3DIGIT ] ) / ( "1" [ "." 0*3("0") ] )
%```
%
%no_te_no_trailers(Config) ->
%te_trailers(Config) ->
%Trailers can only be sent if the request includes a TE header
%containing "trailers". (RFC7230 4.1.2)
%
%te_ignore_chunked(Config) ->
%te_ignore_chunked_0(Config) ->
%The presence of "chunked" in a TE header must be ignored as it
%is always acceptable with HTTP/1.1. (RFC7230 4.3)
%
%%% @todo te_not_acceptable_coding(Config) ->
%A qvalue of 0 in the TE header means "not acceptable". (RFC7230 4.3)
%
%@todo
%The lack of a TE header or an empty TE header means only "chunked"
%(with no trailers) or no transfer-encoding is acceptable. (RFC7230 4.3)
%
%ignore_te_if_not_in_connection_header(Config) ->
%The TE header must be listed in the connection header field,
%or must be ignored otherwise.
%
%@todo
%Trailer headers must be listed in the trailer header field value. (RFC7230 4.4)
%
%@todo
%When defined, the trailer header must also be listed in the connection header. (RFC7230 4.4)
%
%:: Upgrade
%
%```
%Upgrade = 1#protocol
%
%protocol = protocol-name ["/" protocol-version]
%protocol-name = token
%protocol-version = token
%```
%
%The upgrade header contains the list of protocols the
%client wishes to upgrade to, in order of preference. (RFC7230 6.7)
%
%upgrade_safely_ignored(Config) ->
%The upgrade header can be safely ignored. (RFC7230 6.7)
%
%upgrade_must_be_in_connection_header(Config) ->
%The upgrade header must be listed under the connection header,
%or must be ignored otherwise. (RFC7230 6.7)
%
%@todo
%A server accepting an upgrade request must send a 101 status
%code with a upgrade header listing the protocol(s) it upgrades
%to, in layer-ascending order. In addition the upgrade header
%must be listed in the connection header. (RFC7230 6.7)
%
%%A server must not switch to a protocol not listed in the
%%request's upgrade header. (RFC7230 6.7)
%
%@todo
%A server that sends a 426 status code must include a upgrade
%header listing acceptable protocols in order of preference. (RFC7230 6.7)
%
%@todo
%A server can send a upgrade header to any response to advertise
%its support for other protocols listed in order of preference. (RFC7230 6.7)
%
%@todo
%Immediately after a server responds with a 101 status code
%it must respond to the original request using the new protocol. (RFC7230 6.7)
%
%@todo
%%A server must not switch protocols unless the original message's
%%semantics can be honored by the new protocol. OPTIONS requests
%%can be honored by any protocol. (RFC7230 6.7)
%
%http10_ignore_upgrade_header(Config) ->
%A server must ignore an upgrade header received by an HTTP/1.0
%request. (RFC7230 6.7)
%
%expect_then_upgrade(Config) ->
%A server receiving both an upgrade header and an expect header
%containing "100-continue" must send a 100 response before the
%101 response. (RFC7230 6.7)
%
%The upgrade header field cannot be used for switching the
%connection protocol (e.g. TCP) or switching connections. (RFC7230 6.7)
%
%%% Compatibility.
%
%@todo
%A server can choose to be non-conformant to the specifications
%for the sake of compatibility. Such behavior can be enabled
%through configuration and/or software identification. (RFC7230 2.5)
