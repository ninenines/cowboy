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

%% @doc HTTP request manipulation API.
%%
%% Almost all functions in this module return a new <em>Req</em> variable.
%% It should always be used instead of the one used in your function call
%% because it keeps the state of the request. It also allows Cowboy to do
%% some lazy evaluation and cache results where possible.
-module(cowboy_http_req).

-export([
	method/1, version/1, peer/1,
	host/1, host_info/1, raw_host/1, port/1,
	path/1, path_info/1, raw_path/1,
	qs_val/2, qs_val/3, qs_vals/1, raw_qs/1,
	binding/2, binding/3, bindings/1,
	header/2, header/3, headers/1,
	cookie/2, cookie/3, cookies/1
]). %% Request API.

-export([
	body/1, body/2, body_qs/1
]). %% Request Body API.

-export([
	reply/4, chunked_reply/3, chunk/2
]). %% Response API.

-export([
	compact/1
]). %% Misc API.

-include("include/http.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Request API.

%% @doc Return the HTTP method of the request.
-spec method(#http_req{}) -> {http_method(), #http_req{}}.
method(Req) ->
	{Req#http_req.method, Req}.

%% @doc Return the HTTP version used for the request.
-spec version(#http_req{}) -> {http_version(), #http_req{}}.
version(Req) ->
	{Req#http_req.version, Req}.

%% @doc Return the peer address and port number of the remote host.
-spec peer(#http_req{}) -> {{inet:ip_address(), inet:ip_port()}, #http_req{}}.
peer(Req=#http_req{socket=Socket, transport=Transport, peer=undefined}) ->
	{ok, Peer} = Transport:peername(Socket),
	{Peer, Req#http_req{peer=Peer}};
peer(Req) ->
	{Req#http_req.peer, Req}.

%% @doc Return the tokens for the hostname requested.
-spec host(#http_req{}) -> {cowboy_dispatcher:tokens(), #http_req{}}.
host(Req) ->
	{Req#http_req.host, Req}.

%% @doc Return the extra host information obtained from partially matching
%% the hostname using <em>'...'</em>.
-spec host_info(#http_req{})
	-> {cowboy_dispatcher:tokens() | undefined, #http_req{}}.
host_info(Req) ->
	{Req#http_req.host_info, Req}.

%% @doc Return the raw host directly taken from the request.
-spec raw_host(#http_req{}) -> {binary(), #http_req{}}.
raw_host(Req) ->
	{Req#http_req.raw_host, Req}.

%% @doc Return the port used for this request.
-spec port(#http_req{}) -> {inet:ip_port(), #http_req{}}.
port(Req) ->
	{Req#http_req.port, Req}.

%% @doc Return the path segments for the path requested.
%%
%% Following RFC2396, this function may return path segments containing any
%% character, including <em>/</em> if, and only if, a <em>/</em> was escaped
%% and part of a path segment in the path requested.
-spec path(#http_req{}) -> {cowboy_dispatcher:tokens(), #http_req{}}.
path(Req) ->
	{Req#http_req.path, Req}.

%% @doc Return the extra path information obtained from partially matching
%% the patch using <em>'...'</em>.
-spec path_info(#http_req{})
	-> {cowboy_dispatcher:tokens() | undefined, #http_req{}}.
path_info(Req) ->
	{Req#http_req.path_info, Req}.

%% @doc Return the raw path directly taken from the request.
-spec raw_path(#http_req{}) -> {binary(), #http_req{}}.
raw_path(Req) ->
	{Req#http_req.raw_path, Req}.

%% @equiv qs_val(Name, Req, undefined)
-spec qs_val(binary(), #http_req{})
	-> {binary() | true | undefined, #http_req{}}.
qs_val(Name, Req) when is_binary(Name) ->
	qs_val(Name, Req, undefined).

%% @doc Return the query string value for the given key, or a default if
%% missing.
-spec qs_val(binary(), #http_req{}, Default)
	-> {binary() | true | Default, #http_req{}} when Default::any().
qs_val(Name, Req=#http_req{raw_qs=RawQs, qs_vals=undefined}, Default)
		when is_binary(Name) ->
	QsVals = parse_qs(RawQs),
	qs_val(Name, Req#http_req{qs_vals=QsVals}, Default);
qs_val(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.qs_vals) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of query string values.
-spec qs_vals(#http_req{}) -> {list({binary(), binary() | true}), #http_req{}}.
qs_vals(Req=#http_req{raw_qs=RawQs, qs_vals=undefined}) ->
	QsVals = parse_qs(RawQs),
	qs_vals(Req#http_req{qs_vals=QsVals});
qs_vals(Req=#http_req{qs_vals=QsVals}) ->
	{QsVals, Req}.

%% @doc Return the raw query string directly taken from the request.
-spec raw_qs(#http_req{}) -> {binary(), #http_req{}}.
raw_qs(Req) ->
	{Req#http_req.raw_qs, Req}.

%% @equiv binding(Name, Req, undefined)
-spec binding(atom(), #http_req{}) -> {binary() | undefined, #http_req{}}.
binding(Name, Req) when is_atom(Name) ->
	binding(Name, Req, undefined).

%% @doc Return the binding value for the given key obtained when matching
%% the host and path against the dispatch list, or a default if missing.
-spec binding(atom(), #http_req{}, Default)
	-> {binary() | Default, #http_req{}} when Default::any().
binding(Name, Req, Default) when is_atom(Name) ->
	case lists:keyfind(Name, 1, Req#http_req.bindings) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of binding values.
-spec bindings(#http_req{}) -> {list({atom(), binary()}), #http_req{}}.
bindings(Req) ->
	{Req#http_req.bindings, Req}.

%% @equiv header(Name, Req, undefined)
-spec header(atom() | binary(), #http_req{})
	-> {binary() | undefined, #http_req{}}.
header(Name, Req) when is_atom(Name) orelse is_binary(Name) ->
	header(Name, Req, undefined).

%% @doc Return the header value for the given key, or a default if missing.
-spec header(atom() | binary(), #http_req{}, Default)
	-> {binary() | Default, #http_req{}} when Default::any().
header(Name, Req, Default) when is_atom(Name) orelse is_binary(Name) ->
	case lists:keyfind(Name, 1, Req#http_req.headers) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of headers.
-spec headers(#http_req{}) -> {http_headers(), #http_req{}}.
headers(Req) ->
	{Req#http_req.headers, Req}.

%% @equiv cookie(Name, Req, undefined)
-spec cookie(binary(), #http_req{})
	-> {binary() | true | undefined, #http_req{}}.
cookie(Name, Req) when is_binary(Name) ->
	cookie(Name, Req, undefined).

%% @doc Return the cookie value for the given key, or a default if
%% missing.
-spec cookie(binary(), #http_req{}, Default)
	-> {binary() | true | Default, #http_req{}} when Default::any().
cookie(Name, Req=#http_req{cookies=undefined}, Default) when is_binary(Name) ->
	case header('Cookie', Req) of
		{undefined, Req2} ->
			{Default, Req2#http_req{cookies=[]}};
		{RawCookie, Req2} ->
			Cookies = cowboy_cookies:parse_cookie(RawCookie),
			cookie(Name, Req2#http_req{cookies=Cookies}, Default)
	end;
cookie(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.cookies) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of cookie values.
-spec cookies(#http_req{}) -> {list({binary(), binary() | true}), #http_req{}}.
cookies(Req=#http_req{cookies=undefined}) ->
	case header('Cookie', Req) of
		{undefined, Req2} ->
			{[], Req2#http_req{cookies=[]}};
		{RawCookie, Req2} ->
			Cookies = cowboy_cookies:parse_cookie(RawCookie),
			cookies(Req2#http_req{cookies=Cookies})
	end;
cookies(Req=#http_req{cookies=Cookies}) ->
	{Cookies, Req}.

%% Request Body API.

%% @doc Return the full body sent with the request, or <em>{error, badarg}</em>
%% if no <em>Content-Length</em> is available.
%% @todo We probably want to allow a max length.
-spec body(#http_req{}) -> {ok, binary(), #http_req{}} | {error, atom()}.
body(Req) ->
	{Length, Req2} = cowboy_http_req:header('Content-Length', Req),
	case Length of
		undefined -> {error, badarg};
		_Any ->
			Length2 = list_to_integer(binary_to_list(Length)),
			body(Length2, Req2)
	end.

%% @doc Return <em>Length</em> bytes of the request body.
%%
%% You probably shouldn't be calling this function directly, as it expects the
%% <em>Length</em> argument to be the full size of the body, and will consider
%% the body to be fully read from the socket.
%% @todo We probably want to configure the timeout.
-spec body(non_neg_integer(), #http_req{})
	-> {ok, binary(), #http_req{}} | {error, atom()}.
body(Length, Req=#http_req{body_state=waiting, buffer=Buffer})
		when Length =:= byte_size(Buffer) ->
	{ok, Buffer, Req#http_req{body_state=done, buffer= <<>>}};
body(Length, Req=#http_req{socket=Socket, transport=Transport,
		body_state=waiting, buffer=Buffer})
		when is_integer(Length) andalso Length > byte_size(Buffer) ->
	case Transport:recv(Socket, Length - byte_size(Buffer), 5000) of
		{ok, Body} -> {ok, << Buffer/binary, Body/binary >>,
			Req#http_req{body_state=done, buffer= <<>>}};
		{error, Reason} -> {error, Reason}
	end.

%% @doc Return the full body sent with the reqest, parsed as an
%% application/x-www-form-urlencoded string. Essentially a POST query string.
-spec body_qs(#http_req{}) -> {list({binary(), binary() | true}), #http_req{}}.
body_qs(Req) ->
	{ok, Body, Req2} = body(Req),
	{parse_qs(Body), Req2}.

%% Response API.

%% @doc Send a reply to the client.
-spec reply(http_status(), http_headers(), iodata(), #http_req{})
	-> {ok, #http_req{}}.
reply(Code, Headers, Body, Req=#http_req{socket=Socket,
		transport=Transport, connection=Connection,
		method=Method, resp_state=waiting}) ->
	Head = response_head(Code, Headers, [
		{<<"Connection">>, atom_to_connection(Connection)},
		{<<"Content-Length">>,
			list_to_binary(integer_to_list(iolist_size(Body)))},
		{<<"Date">>, cowboy_clock:rfc1123()},
		{<<"Server">>, <<"Cowboy">>}
	]),
	case Method of
		'HEAD' -> Transport:send(Socket, Head);
		_ -> Transport:send(Socket, [Head, Body])
	end,
	{ok, Req#http_req{resp_state=done}}.

%% @doc Initiate the sending of a chunked reply to the client.
%% @see cowboy_http_req:chunk/2
-spec chunked_reply(http_status(), http_headers(), #http_req{})
	-> {ok, #http_req{}}.
chunked_reply(Code, Headers, Req=#http_req{socket=Socket, transport=Transport,
		method='HEAD', resp_state=waiting}) ->
	Head = response_head(Code, Headers, [
		{<<"Date">>, cowboy_clock:rfc1123()},
		{<<"Server">>, <<"Cowboy">>}
	]),
	Transport:send(Socket, Head),
	{ok, Req#http_req{resp_state=done}};
chunked_reply(Code, Headers, Req=#http_req{socket=Socket, transport=Transport,
		resp_state=waiting}) ->
	Head = response_head(Code, Headers, [
		{<<"Connection">>, <<"close">>},
		{<<"Transfer-Encoding">>, <<"chunked">>},
		{<<"Date">>, cowboy_clock:rfc1123()},
		{<<"Server">>, <<"Cowboy">>}
	]),
	Transport:send(Socket, Head),
	{ok, Req#http_req{resp_state=chunks}}.

%% @doc Send a chunk of data.
%%
%% A chunked reply must have been initiated before calling this function.
-spec chunk(iodata(), #http_req{}) -> ok.
chunk(_Data, #http_req{socket=_Socket, transport=_Transport, method='HEAD'}) ->
	ok;
chunk(Data, #http_req{socket=Socket, transport=Transport, resp_state=chunks}) ->
	Transport:send(Socket, [integer_to_list(iolist_size(Data), 16),
		<<"\r\n">>, Data, <<"\r\n">>]).

%% Misc API.

%% @doc Compact the request data by removing all non-system information.
%%
%% This essentially removes the host, path, query string, bindings and headers.
%% Use it when you really need to save up memory, for example when having
%% many concurrent long-running connections.
-spec compact(#http_req{}) -> #http_req{}.
compact(Req) ->
	Req#http_req{host=undefined, host_info=undefined, path=undefined,
		path_info=undefined, qs_vals=undefined, raw_qs=undefined,
		bindings=undefined, headers=[]}.

%% Internal.

-spec parse_qs(binary()) -> list({binary(), binary() | true}).
parse_qs(<<>>) ->
	[];
parse_qs(Qs) ->
	Tokens = binary:split(Qs, <<"&">>, [global, trim]),
	[case binary:split(Token, <<"=">>) of
		[Token] -> {quoted:from_url(Token), true};
		[Name, Value] -> {quoted:from_url(Name), quoted:from_url(Value)}
	end || Token <- Tokens].

-spec response_head(http_status(), http_headers(), http_headers()) -> iolist().
response_head(Code, Headers, DefaultHeaders) ->
	StatusLine = <<"HTTP/1.1 ", (status(Code))/binary, "\r\n">>,
	Headers2 = [{header_to_binary(Key), Value} || {Key, Value} <- Headers],
	Headers3 = lists:keysort(1, Headers2),
	Headers4 = lists:ukeymerge(1, Headers3, DefaultHeaders),
	Headers5 = [[Key, <<": ">>, Value, <<"\r\n">>]
		|| {Key, Value} <- Headers4],
	[StatusLine, Headers5, <<"\r\n">>].

-spec atom_to_connection(keepalive) -> <<_:80>>;
						(close) -> <<_:40>>.
atom_to_connection(keepalive) ->
	<<"keep-alive">>;
atom_to_connection(close) ->
	<<"close">>.

-spec status(http_status()) -> binary().
status(100) -> <<"100 Continue">>;
status(101) -> <<"101 Switching Protocols">>;
status(102) -> <<"102 Processing">>;
status(200) -> <<"200 OK">>;
status(201) -> <<"201 Created">>;
status(202) -> <<"202 Accepted">>;
status(203) -> <<"203 Non-Authoritative Information">>;
status(204) -> <<"204 No Content">>;
status(205) -> <<"205 Reset Content">>;
status(206) -> <<"206 Partial Content">>;
status(207) -> <<"207 Multi-Status">>;
status(226) -> <<"226 IM Used">>;
status(300) -> <<"300 Multiple Choices">>;
status(301) -> <<"301 Moved Permanently">>;
status(302) -> <<"302 Found">>;
status(303) -> <<"303 See Other">>;
status(304) -> <<"304 Not Modified">>;
status(305) -> <<"305 Use Proxy">>;
status(306) -> <<"306 Switch Proxy">>;
status(307) -> <<"307 Temporary Redirect">>;
status(400) -> <<"400 Bad Request">>;
status(401) -> <<"401 Unauthorized">>;
status(402) -> <<"402 Payment Required">>;
status(403) -> <<"403 Forbidden">>;
status(404) -> <<"404 Not Found">>;
status(405) -> <<"405 Method Not Allowed">>;
status(406) -> <<"406 Not Acceptable">>;
status(407) -> <<"407 Proxy Authentication Required">>;
status(408) -> <<"408 Request Timeout">>;
status(409) -> <<"409 Conflict">>;
status(410) -> <<"410 Gone">>;
status(411) -> <<"411 Length Required">>;
status(412) -> <<"412 Precondition Failed">>;
status(413) -> <<"413 Request Entity Too Large">>;
status(414) -> <<"414 Request-URI Too Long">>;
status(415) -> <<"415 Unsupported Media Type">>;
status(416) -> <<"416 Requested Range Not Satisfiable">>;
status(417) -> <<"417 Expectation Failed">>;
status(418) -> <<"418 I'm a teapot">>;
status(422) -> <<"422 Unprocessable Entity">>;
status(423) -> <<"423 Locked">>;
status(424) -> <<"424 Failed Dependency">>;
status(425) -> <<"425 Unordered Collection">>;
status(426) -> <<"426 Upgrade Required">>;
status(500) -> <<"500 Internal Server Error">>;
status(501) -> <<"501 Not Implemented">>;
status(502) -> <<"502 Bad Gateway">>;
status(503) -> <<"503 Service Unavailable">>;
status(504) -> <<"504 Gateway Timeout">>;
status(505) -> <<"505 HTTP Version Not Supported">>;
status(506) -> <<"506 Variant Also Negotiates">>;
status(507) -> <<"507 Insufficient Storage">>;
status(510) -> <<"510 Not Extended">>;
status(B) when is_binary(B) -> B.

-spec header_to_binary(http_header()) -> binary().
header_to_binary('Cache-Control') -> <<"Cache-Control">>;
header_to_binary('Connection') -> <<"Connection">>;
header_to_binary('Date') -> <<"Date">>;
header_to_binary('Pragma') -> <<"Pragma">>;
header_to_binary('Transfer-Encoding') -> <<"Transfer-Encoding">>;
header_to_binary('Upgrade') -> <<"Upgrade">>;
header_to_binary('Via') -> <<"Via">>;
header_to_binary('Accept') -> <<"Accept">>;
header_to_binary('Accept-Charset') -> <<"Accept-Charset">>;
header_to_binary('Accept-Encoding') -> <<"Accept-Encoding">>;
header_to_binary('Accept-Language') -> <<"Accept-Language">>;
header_to_binary('Authorization') -> <<"Authorization">>;
header_to_binary('From') -> <<"From">>;
header_to_binary('Host') -> <<"Host">>;
header_to_binary('If-Modified-Since') -> <<"If-Modified-Since">>;
header_to_binary('If-Match') -> <<"If-Match">>;
header_to_binary('If-None-Match') -> <<"If-None-Match">>;
header_to_binary('If-Range') -> <<"If-Range">>;
header_to_binary('If-Unmodified-Since') -> <<"If-Unmodified-Since">>;
header_to_binary('Max-Forwards') -> <<"Max-Forwards">>;
header_to_binary('Proxy-Authorization') -> <<"Proxy-Authorization">>;
header_to_binary('Range') -> <<"Range">>;
header_to_binary('Referer') -> <<"Referer">>;
header_to_binary('User-Agent') -> <<"User-Agent">>;
header_to_binary('Age') -> <<"Age">>;
header_to_binary('Location') -> <<"Location">>;
header_to_binary('Proxy-Authenticate') -> <<"Proxy-Authenticate">>;
header_to_binary('Public') -> <<"Public">>;
header_to_binary('Retry-After') -> <<"Retry-After">>;
header_to_binary('Server') -> <<"Server">>;
header_to_binary('Vary') -> <<"Vary">>;
header_to_binary('Warning') -> <<"Warning">>;
header_to_binary('Www-Authenticate') -> <<"Www-Authenticate">>;
header_to_binary('Allow') -> <<"Allow">>;
header_to_binary('Content-Base') -> <<"Content-Base">>;
header_to_binary('Content-Encoding') -> <<"Content-Encoding">>;
header_to_binary('Content-Language') -> <<"Content-Language">>;
header_to_binary('Content-Length') -> <<"Content-Length">>;
header_to_binary('Content-Location') -> <<"Content-Location">>;
header_to_binary('Content-Md5') -> <<"Content-Md5">>;
header_to_binary('Content-Range') -> <<"Content-Range">>;
header_to_binary('Content-Type') -> <<"Content-Type">>;
header_to_binary('Etag') -> <<"Etag">>;
header_to_binary('Expires') -> <<"Expires">>;
header_to_binary('Last-Modified') -> <<"Last-Modified">>;
header_to_binary('Accept-Ranges') -> <<"Accept-Ranges">>;
header_to_binary('Set-Cookie') -> <<"Set-Cookie">>;
header_to_binary('Set-Cookie2') -> <<"Set-Cookie2">>;
header_to_binary('X-Forwarded-For') -> <<"X-Forwarded-For">>;
header_to_binary('Cookie') -> <<"Cookie">>;
header_to_binary('Keep-Alive') -> <<"Keep-Alive">>;
header_to_binary('Proxy-Connection') -> <<"Proxy-Connection">>;
header_to_binary(B) when is_binary(B) -> B.

%% Tests.

-ifdef(TEST).

parse_qs_test_() ->
	%% {Qs, Result}
	Tests = [
		{<<"">>, []},
		{<<"a=b">>, [{<<"a">>, <<"b">>}]},
		{<<"aaa=bbb">>, [{<<"aaa">>, <<"bbb">>}]},
		{<<"a&b">>, [{<<"a">>, true}, {<<"b">>, true}]},
		{<<"a=b&c&d=e">>, [{<<"a">>, <<"b">>},
			{<<"c">>, true}, {<<"d">>, <<"e">>}]},
		{<<"a=b=c=d=e&f=g">>, [{<<"a">>, <<"b=c=d=e">>}, {<<"f">>, <<"g">>}]},
		{<<"a+b=c+d">>, [{<<"a b">>, <<"c d">>}]}
	],
	[{Qs, fun() -> R = parse_qs(Qs) end} || {Qs, R} <- Tests].

-endif.
