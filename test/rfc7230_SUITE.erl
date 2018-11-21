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
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [gun_down/1]).
-import(cowboy_test, [raw_open/1]).
-import(cowboy_test, [raw_send/2]).
-import(cowboy_test, [raw_recv_head/1]).
-import(cowboy_test, [raw_recv/3]).

all() -> [{group, http}].

groups() -> [{http, [parallel], ct_helper:all(?MODULE)}].

init_per_group(Name = http, Config) ->
	cowboy_test:init_http(Name = http, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))}
	}, Config).

end_per_group(Name, _) ->
	ok = cowboy:stop_listener(Name).

init_routes(_) -> [
	{"localhost", [
		{"/", hello_h, []},
		{"/echo/:key[/:arg]", echo_h, []},
		{"/length/echo/:key", echo_h, []},
		{"/resp/:key[/:arg]", resp_h, []},
		{"/send_message", send_message_h, []},
		{"*", asterisk_h, []}
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
					{ok, Body0} = raw_recv(Client, Length - byte_size(Rest2), 5000),
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
	#{code := 408, client := Client1} = do_raw(Config, "GET / HTTP/1.1\r\n"),
	{error, closed} = raw_recv(Client1, 0, 6000).

timeout_after_request_line_host(Config) ->
	doc("The time the request (request line and headers) takes to be "
		"received by the server must be limited and subject to configuration. "
		"A 408 status code must be sent if the request line was received."),
	#{code := 408, client := Client2} = do_raw(Config, "GET / HTTP/1.1\r\nHost: localhost"),
	{error, closed} = raw_recv(Client2, 0, 6000).

timeout_after_request_line_host_crlf(Config) ->
	doc("The time the request (request line and headers) takes to be "
		"received by the server must be limited and subject to configuration. "
		"A 408 status code must be sent if the request line was received."),
	#{code := 408, client := Client3} = do_raw(Config, "GET / HTTP/1.1\r\nHost: localhost\r\n"),
	{error, closed} = raw_recv(Client3, 0, 6000).

timeout_after_request_line_host_crlfcr(Config) ->
	doc("The time the request (request line and headers) takes to be "
		"received by the server must be limited and subject to configuration. "
		"A 408 status code must be sent if the request line was received."),
	#{code := 408, client := Client4} = do_raw(Config, "GET / HTTP/1.1\r\nHost: localhost\r\n\r"),
	{error, closed} = raw_recv(Client4, 0, 6000).

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
	Echo = <<"http://localhost/echo/uri">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET /echo/uri#fragment HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

ignore_uri_fragment_after_query(Config) ->
	doc("The fragment part of the target URI is not sent. It must be "
		"ignored by a server receiving it. (RFC7230 5.1)"),
	Echo = <<"http://localhost/echo/uri?key=value">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET /echo/uri?key=value#fragment HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

%% Request target: origin-form.

must_understand_origin_form(Config) ->
	doc("A server must be able to handle at least origin-form and absolute-form. (RFC7230 5.3.2)"),
	#{code := 200} = do_raw(Config,
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

%% @todo Reenable this test once support for CONNECT is added.
%origin_form_reject_if_connect(Config) ->
%	doc("origin-form is used when the client does not connect to a proxy, "
%		"does not use the CONNECT method and does not issue a site-wide "
%		"OPTIONS request. (RFC7230 5.3.1)"),
%	#{code := 400, client := Client} = do_raw(Config,
%		"CONNECT / HTTP/1.1\r\n"
%		"Host: localhost\r\n"
%		"\r\n"),
%	{error, closed} = raw_recv(Client, 0, 1000).

%% @todo Equivalent test for https.
origin_form_tcp_scheme(Config) ->
	doc("The scheme is either resolved from configuration or is \"https\" "
		"when on a TLS connection and \"http\" otherwise. (RFC7230 5.5)"),
	Echo = <<"http://localhost/echo/uri">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET /echo/uri HTTP/1.1\r\n"
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
	Echo = <<"http://localhost/echo/uri">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET HttP://localhost/echo/uri HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n").

absolute_form_case_insensitive_host(Config) ->
	doc("The host is case insensitive and normally provided in lowercase. (RFC7230 2.7.3)"),
	Echo = <<"http://localhost/echo/uri">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET http://LoCaLHOsT/echo/uri HTTP/1.1\r\n"
		"Host: LoCaLHOsT\r\n"
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
	Echo = <<"http://localhost/echo/uri">>,
	#{code := 200, body := Echo} = do_raw(Config,
		"GET https://localhost/echo/uri HTTP/1.1\r\n"
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
	doc("Port numbers above 65535 are invalid. The request must be rejected "
		"and the connection closed."),
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

asterisk_form_empty_path_query(Config) ->
	doc("The path and query components are empty when using asterisk-form. (RFC7230 5.5)"),
	#{code := 200, body := <<"http://localhost">>} = do_raw(Config,
		"OPTIONS * HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"X-Echo: uri\r\n"
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

missing_request_target(Config) ->
	doc("The lack of request target must be rejected with a 400 status code "
		"and the closing of the connection."),
	#{code := 400, client := Client} = do_raw(Config,
		"GET  HTTP/1.1\r\n"
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
		"rejected by a server or intermediary with a 505 status code. "
		"(RFC7230 2.6, RFC7230 A, RFC7230 A.2)"),
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

invalid_header_name(Config) ->
	doc("Header field names are tokens. (RFC7230 3.2)"),
	#{code := 400} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host\0: localhost\r\n"
		"\r\n"]).

invalid_header_value(Config) ->
	doc("Header field values are made of printable characters, "
		"horizontal tab or space. (RFC7230 3.2)"),
	#{code := 400} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\0rm rf the world\r\n"
		"\r\n"]).

lower_case_header(Config) ->
	doc("The header field name is case insensitive. (RFC7230 3.2)"),
	#{code := 200} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"host: localhost\r\n"
		"\r\n"]).

upper_case_header(Config) ->
	doc("The header field name is case insensitive. (RFC7230 3.2)"),
	#{code := 200} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"HOST: localhost\r\n"
		"\r\n"]).

mixed_case_header(Config) ->
	doc("The header field name is case insensitive. (RFC7230 3.2)"),
	#{code := 200} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"hOsT: localhost\r\n"
		"\r\n"]).

reject_whitespace_before_header_name(Config) ->
	doc("Messages that contain whitespace before the header name must "
		"be rejected with a 400 status code and the closing of the "
		"connection. (RFC7230 3.2.4)"),
	#{code := 400, client := Client} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		" Host: localhost\r\n"
		"\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

reject_whitespace_between_header_name_and_colon(Config) ->
	doc("Messages that contain whitespace between the header name and "
		"colon must be rejected with a 400 status code and the closing "
		"of the connection. (RFC7230 3.2.4)"),
	#{code := 400, client := Client} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host : localhost\r\n"
		"\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

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

drop_whitespace_before_header_value(Config) ->
	doc("Optional whitespace before and after the header value is not "
		"part of the value and must be dropped."),
	#{code := 200} = do_raw(Config, [
		"POST / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Content-length:      \t     12\r\n"
		"\r\n"
		"Hello world!"]).

drop_whitespace_after_header_value(Config) ->
	doc("Optional whitespace before and after the header value is not "
		"part of the value and must be dropped."),
	#{code := 200} = do_raw(Config, [
		"POST / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Content-length: 12     \t     \r\n"
		"\r\n"
		"Hello world!"]).

%@todo
%The order of header fields with differing names is not significant. (RFC7230 3.2.2)
%
%@todo
%The normal procedure for parsing headers is to read each header
%field into a hash table by field name until the empty line. (RFC7230 3)

reject_duplicate_content_length_header(Config) ->
	doc("Requests with duplicate content-length headers must be rejected "
		"with a 400 status code and the closing of the connection. (RFC7230 3.3.2)"),
	#{code := 400, client := Client} = do_raw(Config, [
		"POST / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Content-length: 12\r\n"
		"Content-length: 12\r\n"
		"\r\n"
		"Hello world!"]),
	{error, closed} = raw_recv(Client, 0, 1000).

reject_duplicate_host_header(Config) ->
	doc("Requests with duplicate host headers must be rejected "
		"with a 400 status code and the closing of the connection. (RFC7230 3.3.2)"),
	#{code := 400, client := Client} = do_raw(Config, [
		"POST / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Host: localhost\r\n"
		"\r\n"
		"Hello world!"]),
	{error, closed} = raw_recv(Client, 0, 1000).

combine_duplicate_headers(Config) ->
	doc("Other duplicate header fields must be combined by inserting a comma "
		"between the values in the order they were received. (RFC7230 3.2.2)"),
	#{code := 200, body := Body} = do_raw(Config, [
		"GET /echo/headers HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Accept-encoding: gzip\r\n"
		"Accept-encoding: brotli\r\n"
		"\r\n"]),
	<<"#{<<\"accept-encoding\">> => <<\"gzip, brotli\">>,", _/bits>> = Body,
	ok.

%Duplicate header field names are only allowed when their value is
%a comma-separated list. In practice there is no need to perform
%a check while reading the headers as the value will become invalid
%and the error can be handled while parsing the header later on. (RFC7230 3.2.2)
%
%wait_for_eoh_before_processing_request(Config) ->
%The request must not be processed until all headers have arrived. (RFC7230 3.2.2)

limit_headers(Config) ->
	doc("The number of headers allowed in a request must be subject to "
		"a configurable limit. There is no recommendations for the default. "
		"100 headers is known to work well. Such a request must be rejected "
		"with a 431 status code and the closing of the connection. (RFC7230 3.2.5, RFC6585 5)"),
	%% 100 headers.
	#{code := 200} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n",
		[["H-", integer_to_list(N), ": value\r\n"] || N <- lists:seq(1, 99)],
		"\r\n"]),
	%% 101 headers.
	#{code := 431, client := Client} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n",
		[["H-", integer_to_list(N), ": value\r\n"] || N <- lists:seq(1, 100)],
		"\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

%ignore_header_empty_list_elements(Config) ->
%When parsing header field values, the server must ignore empty
%list elements, and not count those as the count of elements present. (RFC7230 7)
%
%@todo
%The information in the via header is largely unreliable. (RFC7230 5.7.1)

%% Request body.

%@todo
%The message body is the octets after decoding any transfer
%codings. (RFC7230 3.3)

no_request_body(Config) ->
	doc("A request has a message body only if it includes a transfer-encoding "
		"header or a non-zero content-length header. (RFC7230 3.3)"),
	#{code := 200, body := <<"false">>} = do_raw(Config, [
		"POST /echo/has_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	#{code := 200, body := <<>>} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	ok.

no_request_body_content_length_zero(Config) ->
	doc("A request has a message body only if it includes a transfer-encoding "
		"header or a non-zero content-length header. (RFC7230 3.3)"),
	#{code := 200, body := <<"false">>} = do_raw(Config, [
		"POST /echo/has_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Content-length: 0\r\n"
		"\r\n"]),
	#{code := 200, body := <<>>} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Content-length: 0\r\n"
		"\r\n"]),
	ok.

request_body_content_length(Config) ->
	doc("A request has a message body only if it includes a transfer-encoding "
		"header or a non-zero content-length header. (RFC7230 3.3)"),
	#{code := 200, body := <<"true">>} = do_raw(Config, [
		"POST /echo/has_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Content-length: 12\r\n"
		"\r\n"
		"Hello world!"]),
	#{code := 200, body := <<"Hello world!">>} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Content-length: 12\r\n"
		"\r\n"
		"Hello world!"]),
	ok.

request_body_transfer_encoding(Config) ->
	doc("A request has a message body only if it includes a transfer-encoding "
		"header or a non-zero content-length header. (RFC7230 3.3)"),
	#{code := 200, body := <<"true">>} = do_raw(Config, [
		"POST /echo/has_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6\r\nHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	#{code := 200, body := <<"Hello world!">>} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6\r\nHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	ok.

%```
%Transfer-Encoding = 1#transfer-coding
%
%transfer-coding = "chunked" / "compress" / "deflate" / "gzip" / transfer-extension
%transfer-extension = token *( OWS ";" OWS transfer-parameter )
%transfer-parameter = token BWS "=" BWS ( token / quoted-string )
%```

case_insensitive_transfer_encoding(Config) ->
	doc("The transfer-coding is case insensitive. (RFC7230 4)"),
	#{code := 200, body := <<"Hello world!">>} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: ChUnKeD\r\n"
		"\r\n"
		"6\r\nHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	ok.

%@todo
%There are no known other transfer-extension with the exception of
%deprecated aliases "x-compress" and "x-gzip". (IANA HTTP Transfer Coding Registry,
%RFC7230 4.2.1, RFC7230 4.2.3, RFC7230 8.4.2)

%% This is the exact same test as request_body_transfer_encoding.
must_understand_chunked(Config) ->
	doc("A server must be able to handle at least chunked transfer-encoding. "
		"This is also the only coding that sees widespread use. (RFC7230 3.3.1, RFC7230 4.1)"),
	#{code := 200, body := <<"Hello world!">>} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6\r\nHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	ok.

reject_double_chunked_encoding(Config) ->
	doc("Messages encoded more than once with chunked transfer-encoding "
		"must be rejected with a 400 status code and the closing of the "
		"connection. (RFC7230 3.3.1)"),
	#{code := 400, client := Client} = do_raw(Config, [
		"POST / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked, chunked\r\n"
		"\r\n"
		"20\r\n6\r\nHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n\r\n0\r\n\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

reject_non_terminal_chunked(Config) ->
	doc("Messages where chunked, when present, is not the last "
		"transfer-encoding must be rejected with a 400 status code "
		"and the closing of the connection. (RFC7230 3.3.3)"),
	#{code := 400, client := Client} = do_raw(Config, [
		"POST / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked, gzip\r\n"
		"\r\n",
		zlib:gzip(<<"6\r\nHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n">>)]),
	{error, closed} = raw_recv(Client, 0, 1000).

%@todo
%Some non-conformant implementations send the "deflate" compressed
%data without the zlib wrapper. (RFC7230 4.2.2)

reject_unknown_transfer_encoding(Config) ->
	doc("Messages encoded with a transfer-encoding the server does not "
		"understand must be rejected with a 501 status code and the "
		"closing of the connection. (RFC7230 3.3.1)"),
	#{code := 400, client := Client} = do_raw(Config, [
		"POST / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: unknown, chunked\r\n"
		"\r\n",
		"6\r\nHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

%@todo
%A server may reject requests with a body and no content-length
%header with a 411 status code. (RFC7230 3.3.3)

%```
%Content-Length = 1*DIGIT
%```

reject_invalid_content_length(Config) ->
	doc("A request with an invalid content-length header must be rejected "
		"with a 400 status code and the closing of the connection. (RFC7230 3.3.3)"),
	#{code := 400, client := Client1} = do_raw(Config, [
		"POST / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Content-length: 12,12\r\n"
		"\r\n"
		"Hello world!"]),
	{error, closed} = raw_recv(Client1, 0, 1000),
	#{code := 400, client := Client2} = do_raw(Config, [
		"POST / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Content-length: NaN\r\n"
		"\r\n"
		"Hello world!"]),
	{error, closed} = raw_recv(Client2, 0, 1000).

%@todo
%The content-length header ranges from 0 to infinity. Requests
%with a message body too large must be rejected with a 413 status
%code and the closing of the connection. (RFC7230 3.3.2)

ignore_content_length_when_transfer_encoding(Config) ->
	doc("When a message includes both transfer-encoding and content-length "
		"headers, the content-length header must be removed before processing "
		"the request. (RFC7230 3.3.3)"),
	#{code := 200, body := <<"Hello world!">>} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"Content-length: 12\r\n"
		"\r\n"
		"6\r\nHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	ok.

%socket_error_while_reading_body(Config) ->
%If a socket error occurs while reading the body the server
%must send a 400 status code response and close the connection. (RFC7230 3.3.3, RFC7230 3.4)
%
%timeout_while_reading_body(Config) ->
%If a timeout occurs while reading the body the server must
%send a 408 status code response and close the connection. (RFC7230 3.3.3, RFC7230 3.4)

%% Body length.

body_length_chunked_before(Config) ->
	doc("The length of a message with a transfer-encoding header can "
		"only be determined on decoding completion. (RFC7230 3.3.3)"),
	#{code := 200, body := <<"undefined">>} = do_raw(Config, [
		"POST /echo/body_length HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6\r\nHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	ok.

body_length_chunked_after(Config) ->
	doc("Upon completion of chunk decoding the server must add a content-length "
		"header with the value set to the total length of data read. (RFC7230 4.1.3)"),
	#{code := 200, body := <<"12">>} = do_raw(Config, [
		"POST /length/echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6\r\nHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	ok.

body_length_content_length(Config) ->
	doc("The length of a message with a content-length header is "
		"the numeric value in octets found in the header. (RFC7230 3.3.3)"),
	#{code := 200, body := <<"12">>} = do_raw(Config, [
		"POST /echo/body_length HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Content-length: 12\r\n"
		"\r\n"
		"Hello world!"]),
	ok.

body_length_zero(Config) ->
	doc("A message with no transfer-encoding or content-length header "
		"has a body length of 0. (RFC7230 3.3.3)"),
	#{code := 200, body := <<"0">>} = do_raw(Config, [
		"POST /echo/body_length HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	ok.

%% Chunked transfer-encoding.

reject_invalid_chunk_size(Config) ->
	doc("A request with an invalid chunk size must be rejected "
		"with a 400 status code and the closing of the connection. (RFC7230 4.1)"),
	#{code := 400, client := Client} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6\r\nHello \r\nFIVE\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

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

ignore_unknown_chunk_extensions(Config) ->
	doc("Unknown chunk extensions must be ignored. (RFC7230 4.1.1)"),
	#{code := 200, body := <<"Hello world!">>} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6; hello=\"cool world\"\r\nHello \r\n"
		"5 ; one ; two ; three;four;five\r\nworld"
		"\r\n1;ok\r\n!\r\n0\r\n\r\n"]),
	ok.

%% Since we skip everything right now, the only reason
%% we might reject chunk extensions is if they are too large.
limit_chunk_size_line(Config) ->
	doc("A request with chunk extensions larger than the server allows must be rejected "
		"with a 400 status code and the closing of the connection. (RFC7230 4.1.1)"),
	#{code := 200, body := <<"Hello world!">>} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6; hello=\"cool world\"\r\nHello \r\n"
		"5;", lists:duplicate(128, $a), "\r\nworld"
		"\r\n1;ok\r\n!\r\n0\r\n\r\n"]),
	#{code := 400, client := Client} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6; hello=\"cool world\"\r\nHello \r\n"
		"5;", lists:duplicate(129, $a), "\r\nworld"
		"\r\n1;ok\r\n!\r\n0\r\n\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

reject_invalid_chunk_size_crlf(Config) ->
	doc("A request with an invalid line break after the chunk size must be rejected "
		"with a 400 status code and the closing of the connection. (RFC7230 4.1)"),
	#{code := 400, client := Client1} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6\rHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	{error, closed} = raw_recv(Client1, 0, 1000),
	#{code := 400, client := Client2} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6\nHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	{error, closed} = raw_recv(Client2, 0, 1000),
	#{code := 400, client := Client3} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6Hello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	{error, closed} = raw_recv(Client3, 0, 1000).

reject_invalid_chunk_ext_crlf(Config) ->
	doc("A request with an invalid line break after chunk extensions must be rejected "
		"with a 400 status code and the closing of the connection. (RFC7230 4.1)"),
	#{code := 400, client := Client1} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6; extensions\rHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	{error, closed} = raw_recv(Client1, 0, 1000),
	#{code := 400, client := Client2} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6; extensions\nHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	{error, closed} = raw_recv(Client2, 0, 1000),
	#{code := 400, client := Client3} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6; extensionsHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	{error, closed} = raw_recv(Client3, 0, 1000).

reject_invalid_chunk_data_crlf(Config) ->
	doc("A request with an invalid line break after the chunk data must be rejected "
		"with a 400 status code and the closing of the connection. (RFC7230 4.1)"),
	#{code := 400, client := Client1} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6\r\nHello \r5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	{error, closed} = raw_recv(Client1, 0, 1000),
	#{code := 400, client := Client2} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6\r\nHello \n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	{error, closed} = raw_recv(Client2, 0, 1000),
	#{code := 400, client := Client3} = do_raw(Config, [
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6\r\nHello 5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	{error, closed} = raw_recv(Client3, 0, 1000).

%```
%trailer-part = *( header-field CRLF )
%```
%
%%% @todo see headers above and reject the same way, space etc.
%reject_invalid_request_trailer(Config) ->
%
%ignore_request_trailer_transfer_encoding(Config) ->
%ignore_request_trailer_content_length(Config) ->
%ignore_request_trailer_host(Config) ->
%ignore_request_trailer_cache_control(Config) ->
%ignore_request_trailer_expect(Config) ->
%ignore_request_trailer_max_forwards(Config) ->
%ignore_request_trailer_pragma(Config) ->
%ignore_request_trailer_range(Config) ->
%ignore_request_trailer_te(Config) ->
%ignore_request_trailer_if_match(Config) ->
%ignore_request_trailer_if_none_match(Config) ->
%ignore_request_trailer_if_modified_since(Config) ->
%ignore_request_trailer_if_unmodified_since(Config) ->
%ignore_request_trailer_if_range(Config) ->
%ignore_request_trailer_www_authenticate(Config) ->
%ignore_request_trailer_authorization(Config) ->
%ignore_request_trailer_proxy_authenticate(Config) ->
%ignore_request_trailer_proxy_authorization(Config) ->
%ignore_request_trailer_content_encoding(Config) ->
%ignore_request_trailer_content_type(Config) ->
%ignore_request_trailer_content_range(Config) ->
%ignore_request_trailer_trailer(Config) ->
%
%ignore_response_trailer_header(Config, Header) ->
%Trailing headers must not include transfer-encoding, content-length,
%host, cache-control, expect, max-forwards, pragma, range, te,
%if-match, if-none-match, if-modified-since, if-unmodified-since,
%if-range, www-authenticate, authorization, proxy-authenticate,
%proxy-authorization, age, cache-control, expires, date, location,
%retry-after, vary, warning, content-encoding, content-type,
%content-range, or trailer. (RFC7230 4.1.2)
%
%When trailer headers are processed, invalid headers must be ignored.
%Valid headers must be added to the list of headers of the request. (RFC7230 4.1.2)
%
%ignore_request_trailers(Config) ->
%Trailer headers can be ignored safely. (RFC7230 4.1.2)
%
%limit_request_trailer_headers(Config) ->
%The number of trailer headers must be subject to configuration.
%There is no known recommendations for the default. A value of 10
%should cover most cases. Requests with too many trailer headers
%must be rejected with a 431 status code and the closing of the
%connection. (RFC6585 5)

%% We remove the header immediately so there's no need
%% to try to read the body before checking.
remove_transfer_encoding_chunked_after_body_read(Config) ->
	doc("Upon completion of chunk decoding the server must remove \"chunked\" "
		"from the transfer-encoding header. This header must be removed if "
		"it becomes empty following this removal. (RFC7230 4.1.3)"),
	#{code := 200, body := <<"undefined">>} = do_raw(Config, [
		"POST /echo/header/transfer-encoding HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n"
		"\r\n"
		"6\r\nHello \r\n5\r\nworld\r\n1\r\n!\r\n0\r\n\r\n"]),
	ok.

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

%% Connection management.

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

no_connection_header_keepalive(Config) ->
	doc("HTTP/1.1 requests with no \"close\" option "
		"indicate the connection will persist. (RFC7230 6.1, RFC7230 6.3)"),
	#{code := 200, headers := RespHeaders, client := Client} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	false = lists:keyfind(<<"connection">>, 1, RespHeaders),
	{error, timeout} = raw_recv(Client, 0, 1000).

http10_connection_keepalive(Config) ->
	doc("HTTP/1.0 requests with the \"keep-alive\" option "
		"indicate the connection will persist. "
		"(RFC7230 6.1, RFC7230 6.3, RFC7230 A.1.2)"),
	#{code := 200, headers := RespHeaders, client := Client} = do_raw(Config, [
		"GET / HTTP/1.0\r\n"
		"Host: localhost\r\n"
		"Connection: keep-alive\r\n"
		"\r\n"]),
	{_, <<"keep-alive">>} = lists:keyfind(<<"connection">>, 1, RespHeaders),
	{error, timeout} = raw_recv(Client, 0, 1000).

connection_close(Config) ->
	doc("HTTP/1.1 requests with the \"close\" option and HTTP/1.0 with no "
		"\"keep-alive\" option indicate the connection will be closed "
		"upon reception of the response by the client. (RFC7230 6.1, RFC7230 6.3)"),
	#{code := 200, headers := RespHeaders, client := Client} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: close\r\n"
		"\r\n"]),
	{_, <<"close">>} = lists:keyfind(<<"connection">>, 1, RespHeaders),
	{error, closed} = raw_recv(Client, 0, 1000).

http10_no_connection_header_close(Config) ->
	doc("HTTP/1.0 with no \"keep-alive\" option indicate "
		"the connection will be closed upon reception of "
		"the response by the client. (RFC7230 6.1, RFC7230 6.3, RFC7230 A.1.2)"),
	#{code := 200, headers := RespHeaders, client := Client} = do_raw(Config, [
		"GET / HTTP/1.0\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	%% Cowboy always sends a close header back to HTTP/1.0 clients
	%% that support keep-alive, even though it is not required.
	{_, <<"close">>} = lists:keyfind(<<"connection">>, 1, RespHeaders),
	{error, closed} = raw_recv(Client, 0, 1000).

limit_requests_keepalive(Config) ->
	doc("The maximum number of requests sent using a persistent connection "
		"must be subject to configuration. The connection must be closed "
		"when the limit is reached. (RFC7230 6.3)"),
	ConnPid = gun_open(Config),
	_ = [begin
		Ref = gun:get(ConnPid, "/"),
		{response, nofin, 200, RespHeaders} = gun:await(ConnPid, Ref),
		{ok, <<"Hello world!">>} = gun:await_body(ConnPid, Ref),
		false = lists:keyfind(<<"connection">>, 1, RespHeaders)
	end || _ <- lists:seq(1,99)],
	%% Final request closes the connection.
	Ref = gun:get(ConnPid, "/"),
	{response, nofin, 200, RespHeaders} = gun:await(ConnPid, Ref),
	{ok, <<"Hello world!">>} = gun:await_body(ConnPid, Ref),
	{_, <<"close">>} = lists:keyfind(<<"connection">>, 1, RespHeaders),
	gun_down(ConnPid).

accept_at_least_1_empty_line_keepalive(Config) ->
	doc("A configurable number of empty lines (CRLF) preceding the request "
		"must be ignored. At least 1 empty line must be ignored. (RFC7230 3.5)"),
	#{code := 200, client := Client} = do_raw(Config,
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"
		%% We send an extra CRLF that must be ignored.
		"\r\n"),
	ok = raw_send(Client,
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{'HTTP/1.1', 200, _, _} = cow_http:parse_status_line(raw_recv_head(Client)),
	ok.

%skip_request_body_by_closing_connection(Config) ->
%%A server that doesn't want to read the entire body of a message
%%must close the connection, if possible after sending the "close"
%%connection option in the response. (RFC7230 6.3)

pipeline(Config) ->
	doc("A server can receive more than one request before any response "
		"is sent. This is called pipelining. Responses must be sent "
		"in the same order as the requests. (RFC7230 6.3.2)"),
	ConnPid = gun_open(Config),
	Refs = [{
		gun:get(ConnPid, "/"),
		gun:delete(ConnPid, "/echo/method")
	} || _ <- lists:seq(1, 25)],
	_ = [begin
		{response, nofin, 200, _} = gun:await(ConnPid, Ref1),
		{ok, <<"Hello world!">>} = gun:await_body(ConnPid, Ref1),
		{response, nofin, 200, _} = gun:await(ConnPid, Ref2),
		{ok, <<"DELETE">>} = gun:await_body(ConnPid, Ref2)
	end || {Ref1, Ref2} <- Refs],
	ok.

%% @todo pipeline_parallel (safe methods can, others can't)
%The requests can be processed in parallel if they all have safe methods.

%@todo
%A server that does parallel pipelining must send responses in the
%same order as the requests came in. (RFC7230 5.6)

%@todo
%The server must reject abusive traffic by closing the connection.
%Abusive traffic can come from the form of too many requests in a
%given amount of time, or too many concurrent connections. Limits
%must be subject to configuration. (RFC7230 6.4)

close_inactive_connections(Config) ->
	doc("The server must close inactive connections. The timeout "
		"must be subject to configuration. (RFC7230 6.5)"),
	Client = raw_open(Config),
	{error, closed} = raw_recv(Client, 0, 6000).

%@todo
%The server must monitor connections for the close signal
%and close the socket on its end accordingly. (RFC7230 6.5)
%
%@todo
%A connection close may occur at any time. (RFC7230 6.5)

ignore_requests_after_request_connection_close(Config) ->
	doc("The server must not process any request after "
		"receiving the \"close\" connection option. (RFC7230 6.6)"),
	Self = self(),
	#{code := 200, client := Client} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: close\r\n"
		"\r\n"
		"GET /send_message HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"x-test-pid: ", pid_to_list(Self), "\r\n"
		"\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000),
	%% We receive a message if the second request is wrongly processed.
	receive
		{Self, _, init, Req, Opts} ->
			error({init, Req, Opts})
	after 1000 ->
		ok
	end.

ignore_requests_after_response_connection_close(Config) ->
	doc("The server must not process any request after "
		"sending the \"close\" connection option. (RFC7230 6.6)"),
	Self = self(),
	Client = raw_open(Config),
	ok = raw_send(Client, [
		[
			"GET / HTTP/1.1\r\n"
			"Host: localhost\r\n"
			"\r\n"
		|| _ <- lists:seq(1, 100)],
		"GET /send_message HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"x-test-pid: ", pid_to_list(Self), "\r\n"
		"\r\n"]),
	%% We have a separate test for the connection close so we don't
	%% double check the connection gets closed here. We only need to
	%% know whether the 101st request was wrongly processed.
	receive
		{Self, _, init, Req, Opts} ->
			error({init, Req, Opts})
	after 1000 ->
		ok
	end.

%@todo
%The server must close the connection in stages to avoid the
%TCP reset problem. The server starts by closing the write
%side of the socket. The server then reads until it detects
%the socket has been closed, until it can be certain its
%last response has been received by the client, or until
%a close or timeout occurs. The server then fully close the
%connection. (6.6)

%% Routing.

%```
%Host = authority ; same as authority-form
%```

reject_missing_host(Config) ->
	doc("An HTTP/1.1 request that lacks a host header must be rejected with "
		"a 400 status code and the closing of the connection. (RFC7230 5.4)"),
	#{code := 400, client := Client} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

http10_allow_missing_host(Config0) ->
	doc("An HTTP/1.0 request that lacks a host header may be accepted. "
		"(RFC7230 5.4, RFC7230 5.5, RFC7230 A.1.1)"),
	Routes = [{'_', [{"/echo/:key[/:arg]", echo_h, []}]}],
	Config = cowboy_test:init_http(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(Routes)}
	}, Config0),
	try
		#{code := 200, body := <<>>} = do_raw(Config, [
			"GET /echo/host HTTP/1.0\r\n"
			"\r\n"])
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

reject_invalid_host(Config) ->
	doc("A request with an invalid host header must be rejected with a "
		"400 status code and the closing of the connection. (RFC7230 5.4)"),
	#{code := 400, client := Client} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost:port\r\n"
		"\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

reject_userinfo(Config) ->
	doc("An authority component with a userinfo component (and its "
		"\"@\" delimiter) is invalid. The request must be rejected with "
		"a 400 status code and the closing of the connection. (RFC7230 2.7.1)"),
	#{code := 400, client := Client} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host: user@localhost\r\n"
		"\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

reject_absolute_form_different_host(Config) ->
	doc("When using absolute-form the URI authority component must be "
		"identical to the host header. Invalid requests must be rejected "
		"with a 400 status code and the closing of the connection. (RFC7230 5.4)"),
	#{code := 400, client := Client} = do_raw(Config, [
		"GET http://example.org/ HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{error, closed} = raw_recv(Client, 0, 1000).

%reject_authority_form_different_host(Config) ->
%When using authority-form the URI authority component must be
%identical to the host header. Invalid requests must be rejected
%with a 400 status code and the closing of the connection.

empty_host(Config0) ->
	doc("The host header is empty when the authority component is undefined. (RFC7230 5.4)"),
	Routes = [{'_', [{"/echo/:key[/:arg]", echo_h, []}]}],
	Config = cowboy_test:init_http(?FUNCTION_NAME, #{
		env => #{dispatch => cowboy_router:compile(Routes)}
	}, Config0),
	try
		#{code := 200, body := <<>>} = do_raw(Config, [
			"GET /echo/host HTTP/1.1\r\n"
			"Host:\r\n"
			"\r\n"]),
		#{code := 200, body := <<>>} = do_raw(Config, [
			"GET /echo/host HTTP/1.1\r\n"
			"Host: \r\n"
			"\r\n"])
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

%% The effective request URI can be rebuilt by concatenating scheme,
%% "://", authority, path and query components. (RFC7230 5.5)
%%
%% This is covered in req_SUITE in the tests for cowboy_req:uri/1,2.

reject_non_authoritative_host(Config) ->
	doc("A request with a host header for which the origin server is "
		"not authoritative must be rejected with a 400 status code. "
		"(RFC7230 5.5, RFC7230 9.1)"),
	#{code := 400} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host: ninenines.eu\r\n"
		"\r\n"]),
	ok.

%@todo
%Resources with identical URI except for the scheme component
%must be treated as different. (RFC7230 2.7.2)

%% Response.

%@todo
%A server can send more than one response per request only when a
%1xx response is sent preceding the final response. (RFC7230 5.6)
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

http10_request_http11_response(Config) ->
	doc("A server must send its own HTTP version in responses. (RFC7230 2.6)"),
	#{code := 200, version := 'HTTP/1.1'} = do_raw(Config, [
		"GET / HTTP/1.0\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	ok.

%@todo
%An HTTP/1.1 server may send an HTTP/1.0 version for compatibility purposes. (RFC7230 2.6)
%
%@todo
%RFC6585 defines additional status code a server can use to reject
%messages. (RFC7230 9.3, RFC6585)

%% Response headers.

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

special_set_cookie_handling(Config) ->
	doc("The set-cookie header must be handled as a special case. There "
		"must be exactly one set-cookie header field per cookie. (RFC7230 3.2.2)"),
	#{code := 200, headers := RespHeaders} = do_raw(Config, [
		"GET /resp/set_resp_cookie3/multiple HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	[_, _] = [H || H={<<"set-cookie">>, _} <- RespHeaders],
	ok.

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

close_request_close_response(Config) ->
	doc("A server must send a \"close\" in a response to a request "
		"containing a \"close\". (RFC7230 6.6)"),
	#{code := 200, headers := RespHeaders} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: close\r\n"
		"\r\n"]),
	{_, <<"close">>} = lists:keyfind(<<"connection">>, 1, RespHeaders),
	ok.

%% Response body.

no_body_in_head_response(Config) ->
	doc("Responses to HEAD requests never include a message body. (RFC7230 3.3)"),
	Client = raw_open(Config),
	ok = raw_send(Client, [
		"HEAD / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, 200, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
	{Headers, <<>>} = cow_http:parse_headers(Rest),
	{_, LengthBin} = lists:keyfind(<<"content-length">>, 1, Headers),
	Length = binary_to_integer(LengthBin),
	{error, timeout} = raw_recv(Client, Length, 1000),
	ok.

%% @todo test different ways to send a body in response

%%% @todo Implement CONNECT
%2xx responses to CONNECT requests never include a message
%body. (RFC7230 3.3)
%
%no_body_in_100_response(Config) ->
%no_body_in_101_response(Config) ->
%no_body_in_102_response(Config) ->
%1xx responses never include a message body. (RFC7230 3.3)

no_body_in_204_response(Config) ->
	doc("204 responses never include a message body. (RFC7230 3.3)"),
	Client = raw_open(Config),
	ok = raw_send(Client, [
		"GET /resp/reply2/204 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, 204, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
	{_, <<>>} = cow_http:parse_headers(Rest),
	{error, timeout} = raw_recv(Client, 1, 1000),
	ok.

no_body_in_204_response_stream(Config) ->
	doc("204 responses never include a message body. (RFC7230 3.3)"),
	Client = raw_open(Config),
	ok = raw_send(Client, [
		"GET /resp/stream_reply2/204 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, 204, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
	{_, <<>>} = cow_http:parse_headers(Rest),
	{error, timeout} = raw_recv(Client, 1, 1000),
	ok.

no_body_in_304_response(Config) ->
	doc("304 responses never include a message body. (RFC7230 3.3)"),
	Client = raw_open(Config),
	ok = raw_send(Client, [
		"GET /resp/reply2/304 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, 304, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
	{_, <<>>} = cow_http:parse_headers(Rest),
	{error, timeout} = raw_recv(Client, 1, 1000),
	ok.

no_body_in_304_response_stream(Config) ->
	doc("304 responses never include a message body. (RFC7230 3.3)"),
	Client = raw_open(Config),
	ok = raw_send(Client, [
		"GET /resp/stream_reply2/304 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, 304, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
	{_, <<>>} = cow_http:parse_headers(Rest),
	{error, timeout} = raw_recv(Client, 1, 1000),
	ok.

same_content_length_as_get_in_head_response(Config) ->
	doc("Responses to HEAD requests can include a content-length header. "
		"Its value must be the same as if the request was an unconditional "
		"GET. (RFC7230 3.3, RFC7230 3.3.1, RFC7230 3.3.2)"),
	Client = raw_open(Config),
	ok = raw_send(Client, [
		"HEAD / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, 200, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
	{Headers, <<>>} = cow_http:parse_headers(Rest),
	{_, <<"12">>} = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

same_transfer_encoding_as_get_in_head_response(Config) ->
	doc("Responses to HEAD requests can include a transfer-encoding header. "
		"Its value must be the same as if the request was an unconditional "
		"GET. (RFC7230 3.3, RFC7230 3.3.1, RFC7230 3.3.2)"),
	Client = raw_open(Config),
	ok = raw_send(Client, [
		"HEAD /resp/stream_reply2/200 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, 200, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
	{Headers, <<>>} = cow_http:parse_headers(Rest),
	{_, <<"chunked">>} = lists:keyfind(<<"transfer-encoding">>, 1, Headers),
	ok.

%same_content_length_as_200_in_304_response(Config) ->
%same_transfer_encoding_as_200_in_304_response(Config) ->
%304 responses can include a
%content-length or transfer-encoding header. Their value must
%be the same as if the request was an unconditional GET. (RFC7230 3.3, RFC7230 3.3.1, RFC7230 3.3.2)
%
%no_content_length_in_100_response(Config) ->
%no_content_length_in_101_response(Config) ->
%no_content_length_in_102_response(Config) ->
%1xx, 204 responses and "2xx responses to CONNECT requests" must
%not include a content-length or transfer-encoding header. (RFC7230 3.3.1, RFC7230 3.3.2)

no_content_length_in_204_response(Config) ->
	doc("204 responses must not include a content-length header. "
		"(RFC7230 3.3.1, RFC7230 3.3.2)"),
	Client = raw_open(Config),
	ok = raw_send(Client, [
		"GET /resp/reply3/204 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, 204, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
	{Headers, <<>>} = cow_http:parse_headers(Rest),
	false = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

no_content_length_in_empty_304_response(Config) ->
	doc("304 responses should not include a content-length header, "
		"unless it matches the resource's and was therefore set "
		"explicitly by the user. (RFC7230 3.3.1, RFC7230 3.3.2)"),
	Client = raw_open(Config),
	ok = raw_send(Client, [
		"GET /resp/reply3/304 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, 304, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
	{Headers, <<>>} = cow_http:parse_headers(Rest),
	false = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

%%% @todo CONNECT no_content_length_in_2xx_response_to_connect_request(Config) ->
%no_transfer_encoding_in_100_response(Config) ->
%no_transfer_encoding_in_101_response(Config) ->
%no_transfer_encoding_in_102_response(Config) ->
%1xx, 204 responses and "2xx responses to CONNECT requests" must
%not include a content-length or transfer-encoding header. (RFC7230 3.3.1, RFC7230 3.3.2)

%% We only send transfer-encoding when streaming a response body.
%% We therefore need a streamed response in order to see a potential bug.
no_transfer_encoding_in_204_response(Config) ->
	doc("204 responses must not include a transfer-encoding header. "
		"(RFC7230 3.3.1, RFC7230 3.3.2)"),
	Client = raw_open(Config),
	ok = raw_send(Client, [
		"GET /resp/stream_reply2/204 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, 204, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
	{Headers, <<>>} = cow_http:parse_headers(Rest),
	false = lists:keyfind(<<"transfer-encoding">>, 1, Headers),
	ok.

%%% @todo CONNECT no_transfer_encoding_in_2xx_response_to_connect_request(Config) ->
%1xx, 204 responses and "2xx responses to CONNECT requests" must
%not include a content-length or transfer-encoding header. (RFC7230 3.3.1, RFC7230 3.3.2)
%
%```
%message-body = *OCTET
%```
%
%The message body is the octets after decoding any transfer
%codings. (RFC7230 3.3)

content_length_0_when_no_body(Config) ->
	doc("When the length is known in advance, the server must send a "
		"content-length header, including if the length is 0. (RFC7230 3.3.2, RFC7230 3.3.3)"),
	#{code := 200, headers := RespHeaders} = do_raw(Config, [
		"GET /resp/reply2/200 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, <<"0">>} = lists:keyfind(<<"content-length">>, 1, RespHeaders),
	ok.

content_length_response(Config) ->
	doc("When the length is known in advance, the server must send a "
		"content-length header. (RFC7230 3.3.2, RFC7230 3.3.3)"),
	#{code := 200, headers := RespHeaders} = do_raw(Config, [
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, <<"12">>} = lists:keyfind(<<"content-length">>, 1, RespHeaders),
	ok.

chunked_response(Config) ->
	doc("When the length is not known in advance, the chunked transfer-encoding "
		"must be used. (RFC7230 3.3.2, RFC7230 3.3.3)"),
	#{code := 200, headers := RespHeaders} = do_raw(Config, [
		"GET /resp/stream_reply2/200 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, <<"chunked">>} = lists:keyfind(<<"transfer-encoding">>, 1, RespHeaders),
	%% @todo We probably want to check the body received too.
	ok.

%compat_no_content_length_or_transfer_encoding_close_on_body_end(Config) ->
%For compatibility purposes a server can send no content-length or
%transfer-encoding header. In this case the connection must be
%closed after the response has been sent fully. (RFC7230 3.3.2, RFC7230 3.3.3)

no_content_length_if_transfer_encoding(Config) ->
	doc("The content-length header must not be sent when a transfer-encoding "
		"header already exists. (RFC7230 3.3.2)"),
	#{code := 200, headers := RespHeaders} = do_raw(Config, [
		"GET /resp/stream_reply2/200 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	false = lists:keyfind(<<"content-length">>, 1, RespHeaders),
	ok.

%@todo
%The server must not apply the chunked transfer-encoding more than
%once. (RFC7230 3.3.1)
%
%@todo
%The server must apply the chunked transfer-encoding last. (RFC7230 3.3.1)

http10_request_no_transfer_encoding_in_response(Config) ->
	doc("The transfer-encoding header must not be sent in responses to "
		"HTTP/1.0 requests, or in responses that use the HTTP/1.0 version. "
		"No transfer codings must be applied in these cases. "
		"(RFC7230 3.3.1, RFC7230 A.1.3)"),
	Client = raw_open(Config),
	ok = raw_send(Client, [
		"GET /resp/stream_reply2/200 HTTP/1.0\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, 200, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
	{RespHeaders, Body0} = cow_http:parse_headers(Rest),
	false = lists:keyfind(<<"content-length">>, 1, RespHeaders),
	false = lists:keyfind(<<"transfer-encoding">>, 1, RespHeaders),
	Body = <<0:8000000>>,
	{ok, Body1} = raw_recv(Client, byte_size(Body) - byte_size(Body0), 5000),
	Body = << Body0/binary, Body1/binary >>,
	%% The end of body is indicated by a connection close.
	{error, closed} = raw_recv(Client, 0, 1000),
	ok.

no_te_no_trailers(Config) ->
	doc("Trailers can only be sent if the request includes a TE header "
		"containing \"trailers\". (RFC7230 4.1.2)"),
	#{code := 200, headers := RespHeaders} = do_raw(Config, [
		"GET /resp/stream_trailers HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"]),
	{_, <<"chunked">>} = lists:keyfind(<<"transfer-encoding">>, 1, RespHeaders),
	false = lists:keyfind(<<"trailer">>, 1, RespHeaders),
	%% @todo We probably want to check the body received too.
	ok.

te_trailers(Config) ->
	doc("Trailers can only be sent if the request includes a TE header "
		"containing \"trailers\". (RFC7230 4.1.2)"),
	#{code := 200, headers := RespHeaders} = do_raw(Config, [
		"GET /resp/stream_trailers HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"TE: trailers\r\n"
		"\r\n"]),
	{_, <<"chunked">>} = lists:keyfind(<<"transfer-encoding">>, 1, RespHeaders),
	{_, <<"grpc-status">>} = lists:keyfind(<<"trailer">>, 1, RespHeaders),
	%% @todo We probably want to check the body received too.
	ok.

te_ignore_chunked(Config) ->
	doc("The presence of \"chunked\" in a TE header must be ignored as it "
		"is always acceptable with HTTP/1.1. (RFC7230 4.3)"),
	#{code := 200, headers := RespHeaders} = do_raw(Config, [
		"GET /resp/stream_reply2/200 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"TE: chunked\r\n"
		"\r\n"]),
	{_, <<"chunked">>} = lists:keyfind(<<"transfer-encoding">>, 1, RespHeaders),
	%% @todo We probably want to check the body received too.
	ok.

te_ignore_chunked_0(Config) ->
	doc("The presence of \"chunked\" in a TE header must be ignored as it "
		"is always acceptable with HTTP/1.1. (RFC7230 4.3)"),
	#{code := 200, headers := RespHeaders} = do_raw(Config, [
		"GET /resp/stream_reply2/200 HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"TE: chunked;q=0\r\n"
		"\r\n"]),
	{_, <<"chunked">>} = lists:keyfind(<<"transfer-encoding">>, 1, RespHeaders),
	%% @todo We probably want to check the body received too.
	ok.

%%% @todo te_not_acceptable_coding(Config) ->
%A qvalue of 0 in the TE header means "not acceptable". (RFC7230 4.3)
%
%@todo
%The lack of a TE header or an empty TE header means only "chunked"
%(with no trailers) or no transfer-encoding is acceptable. (RFC7230 4.3)
%
%@todo
%Trailer headers must be listed in the trailer header field value. (RFC7230 4.4)

%% Upgrade.

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

upgrade_safely_ignored(Config) ->
	doc("The upgrade header can be safely ignored. (RFC7230 6.7)"),
	#{code := 200} = do_raw(Config,
		"GET / HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: upgrade\r\n"
		"Upgrade: websocket\r\n"
		"\r\n").

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

%% Compatibility.

%@todo
%A server can choose to be non-conformant to the specifications
%for the sake of compatibility. Such behavior can be enabled
%through configuration and/or software identification. (RFC7230 2.5)
