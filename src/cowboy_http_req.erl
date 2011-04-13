%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
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

-module(cowboy_http_req).

-export([
	method/1, version/1, peer/1,
	host/1, raw_host/1,
	path/1, raw_path/1,
	qs_val/2, qs_val/3, qs_vals/1, raw_qs/1,
	binding/2, binding/3, bindings/1,
	header/2, header/3, headers/1
%%	cookie/2, cookie/3, cookies/1 @todo
]). %% Request API.

-export([
	body/1, body/2, body_qs/1
]). %% Request Body API.

-export([
	reply/4
]). %% Response API.

-include("include/types.hrl").
-include("include/http.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Request API.

-spec method(Req::#http_req{}) -> {Method::http_method(), Req::#http_req{}}.
method(Req) ->
	{Req#http_req.method, Req}.

-spec version(Req::#http_req{}) -> {Version::http_version(), Req::#http_req{}}.
version(Req) ->
	{Req#http_req.version, Req}.

-spec peer(Req::#http_req{})
	-> {{Address::ip_address(), Port::port_number()}, Req::#http_req{}}.
peer(Req=#http_req{socket=Socket, transport=Transport, peer=undefined}) ->
	{ok, Peer} = Transport:peername(Socket),
	{Peer, Req#http_req{peer=Peer}};
peer(Req) ->
	{Req#http_req.peer, Req}.

-spec host(Req::#http_req{}) -> {Host::path_tokens(), Req::#http_req{}}.
host(Req) ->
	{Req#http_req.host, Req}.

-spec raw_host(Req::#http_req{}) -> {RawHost::string(), Req::#http_req{}}.
raw_host(Req) ->
	{Req#http_req.raw_host, Req}.

-spec path(Req::#http_req{}) -> {Path::path_tokens(), Req::#http_req{}}.
path(Req) ->
	{Req#http_req.path, Req}.

-spec raw_path(Req::#http_req{}) -> {RawPath::string(), Req::#http_req{}}.
raw_path(Req) ->
	{Req#http_req.raw_path, Req}.

-spec qs_val(Name::string(), Req::#http_req{})
	-> {Value::string() | true | undefined, Req::#http_req{}}.
%% @equiv qs_val(Name, Req) -> qs_val(Name, Req, undefined)
qs_val(Name, Req) ->
	qs_val(Name, Req, undefined).

-spec qs_val(Name::string(), Req::#http_req{}, Default)
	-> {Value::string() | true | Default, Req::#http_req{}}
	when Default::term().
qs_val(Name, Req=#http_req{raw_qs=RawQs, qs_vals=undefined}, Default) ->
	QsVals = parse_qs(RawQs),
	qs_val(Name, Req#http_req{qs_vals=QsVals}, Default);
qs_val(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.qs_vals) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

-spec qs_vals(Req::#http_req{})
	-> {list({Name::string(), Value::string() | true}), Req::#http_req{}}.
qs_vals(Req=#http_req{raw_qs=RawQs, qs_vals=undefined}) ->
	QsVals = parse_qs(RawQs),
	qs_vals(Req#http_req{qs_vals=QsVals});
qs_vals(Req=#http_req{qs_vals=QsVals}) ->
	{QsVals, Req}.

-spec raw_qs(Req::#http_req{}) -> {RawQs::string(), Req::#http_req{}}.
raw_qs(Req) ->
	{Req#http_req.raw_qs, Req}.

-spec binding(Name::atom(), Req::#http_req{})
	-> {Value::string() | undefined, Req::#http_req{}}.
%% @equiv binding(Name, Req) -> binding(Name, Req, undefined)
binding(Name, Req) ->
	binding(Name, Req, undefined).

-spec binding(Name::atom(), Req::#http_req{}, Default)
	-> {Value::string() | Default, Req::#http_req{}} when Default::term().
binding(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.bindings) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

-spec bindings(Req::#http_req{})
	-> {list({Name::atom(), Value::string()}), Req::#http_req{}}.
bindings(Req) ->
	{Req#http_req.bindings, Req}.

-spec header(Name::atom() | string(), Req::#http_req{})
	-> {Value::string() | undefined, Req::#http_req{}}.
%% @equiv header(Name, Req) -> header(Name, Req, undefined)
header(Name, Req) ->
	header(Name, Req, undefined).

-spec header(Name::atom() | string(), Req::#http_req{}, Default)
	-> {Value::string() | Default, Req::#http_req{}} when Default::term().
header(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.headers) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

-spec headers(Req::#http_req{})
	-> {list({Name::atom() | string(), Value::string()}), Req::#http_req{}}.
headers(Req) ->
	{Req#http_req.headers, Req}.

%% Request Body API.

%% @todo We probably want to allow a max length.
-spec body(Req::#http_req{})
	-> {ok, Body::binary(), Req::#http_req{}} | {error, Reason::posix()}.
body(Req) ->
	{Length, Req2} = cowboy_http_req:header('Content-Length', Req),
	case Length of
		undefined -> {error, badarg};
		_Any ->
			Length2 = list_to_integer(Length),
			body(Length2, Req2)
	end.

%% @todo We probably want to configure the timeout.
-spec body(Length::non_neg_integer(), Req::#http_req{})
	-> {ok, Body::binary(), Req::#http_req{}} | {error, Reason::posix()}.
body(Length, Req=#http_req{socket=Socket, transport=Transport,
		body_state=waiting}) ->
	Transport:setopts(Socket, [{packet, raw}]),
	case Transport:recv(Socket, Length, 5000) of
		{ok, Body} -> {ok, Body, Req#http_req{body_state=done}};
		{error, Reason} -> {error, Reason}
	end.

-spec body_qs(Req::#http_req{})
	-> {list({Name::string(), Value::string() | true}), Req::#http_req{}}.
body_qs(Req) ->
	{ok, Body, Req2} = body(Req),
	{parse_qs(binary_to_list(Body)), Req2}.

%% Response API.

-spec reply(Code::http_status(), Headers::http_headers(),
	Body::iolist(), Req::#http_req{}) -> {ok, Req::#http_req{}}.
reply(Code, Headers, Body, Req=#http_req{socket=Socket,
		transport=Transport, connection=Connection,
		resp_state=waiting}) ->
	StatusLine = ["HTTP/1.1 ", status(Code), "\r\n"],
	DefaultHeaders = [
		{"Connection", atom_to_connection(Connection)},
		{"Content-Length", integer_to_list(iolist_size(Body))}
	],
	Headers2 = lists:keysort(1, Headers),
	Headers3 = lists:ukeymerge(1, Headers2, DefaultHeaders),
	Headers4 = [[Key, ": ", Value, "\r\n"] || {Key, Value} <- Headers3],
	Transport:send(Socket, [StatusLine, Headers4, "\r\n", Body]),
	{ok, Req#http_req{resp_state=done}}.

%% Internal.

-spec parse_qs(Qs::string()) -> list({Name::string(), Value::string() | true}).
parse_qs(Qs) ->
	Tokens = string:tokens(Qs, "&"),
	[case string:chr(Token, $=) of
		0 ->
			{Token, true};
		N ->
			{Name, [$=|Value]} = lists:split(N - 1, Token),
			{Name, Value}
	end || Token <- Tokens].

-spec atom_to_connection(Atom::keepalive | close) -> string().
atom_to_connection(keepalive) ->
	"keep-alive";
atom_to_connection(close) ->
	"close".

-spec status(Code::http_status()) -> string().
status(100) -> "100 Continue";
status(101) -> "101 Switching Protocols";
status(102) -> "102 Processing";
status(200) -> "200 OK";
status(201) -> "201 Created";
status(202) -> "202 Accepted";
status(203) -> "203 Non-Authoritative Information";
status(204) -> "204 No Content";
status(205) -> "205 Reset Content";
status(206) -> "206 Partial Content";
status(207) -> "207 Multi-Status";
status(226) -> "226 IM Used";
status(300) -> "300 Multiple Choices";
status(301) -> "301 Moved Permanently";
status(302) -> "302 Found";
status(303) -> "303 See Other";
status(304) -> "304 Not Modified";
status(305) -> "305 Use Proxy";
status(306) -> "306 Switch Proxy";
status(307) -> "307 Temporary Redirect";
status(400) -> "400 Bad Request";
status(401) -> "401 Unauthorized";
status(402) -> "402 Payment Required";
status(403) -> "403 Forbidden";
status(404) -> "404 Not Found";
status(405) -> "405 Method Not Allowed";
status(406) -> "406 Not Acceptable";
status(407) -> "407 Proxy Authentication Required";
status(408) -> "408 Request Timeout";
status(409) -> "409 Conflict";
status(410) -> "410 Gone";
status(411) -> "411 Length Required";
status(412) -> "412 Precondition Failed";
status(413) -> "413 Request Entity Too Large";
status(414) -> "414 Request-URI Too Long";
status(415) -> "415 Unsupported Media Type";
status(416) -> "416 Requested Range Not Satisfiable";
status(417) -> "417 Expectation Failed";
status(418) -> "418 I'm a teapot";
status(422) -> "422 Unprocessable Entity";
status(423) -> "423 Locked";
status(424) -> "424 Failed Dependency";
status(425) -> "425 Unordered Collection";
status(426) -> "426 Upgrade Required";
status(500) -> "500 Internal Server Error";
status(501) -> "501 Not Implemented";
status(502) -> "502 Bad Gateway";
status(503) -> "503 Service Unavailable";
status(504) -> "504 Gateway Timeout";
status(505) -> "505 HTTP Version Not Supported";
status(506) -> "506 Variant Also Negotiates";
status(507) -> "507 Insufficient Storage";
status(510) -> "510 Not Extended";
status(L) when is_list(L) -> L.

%% Tests.

-ifdef(TEST).

parse_qs_test_() ->
	%% {Qs, Result}
	Tests = [
		{"", []},
		{"a=b", [{"a", "b"}]},
		{"aaa=bbb", [{"aaa", "bbb"}]},
		{"a&b", [{"a", true}, {"b", true}]},
		{"a=b&c&d=e", [{"a", "b"}, {"c", true}, {"d", "e"}]},
		{"a=b=c=d=e&f=g", [{"a", "b=c=d=e"}, {"f", "g"}]}
	],
	[{Qs, fun() -> R = parse_qs(Qs) end} || {Qs, R} <- Tests].

-endif.
