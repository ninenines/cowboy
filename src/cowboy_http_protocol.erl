%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
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

-module(cowboy_http_protocol).
-export([start_link/3]). %% API.
-export([init/3]). %% FSM.

-include("include/types.hrl").
-include("include/http.hrl").

-record(state, {
	socket :: socket(),
	transport :: module(),
	dispatch :: dispatch(),
	handler :: {Handler::module(), Opts::term()},
	timeout :: timeout(),
	connection = keepalive :: keepalive | close
}).

%% API.

-spec start_link(Socket::socket(), Transport::module(), Opts::term())
	-> {ok, Pid::pid()}.
start_link(Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Socket, Transport, Opts]),
	{ok, Pid}.

%% FSM.

-spec init(Socket::socket(), Transport::module(), Opts::term())
	-> ok | {error, no_ammo}.
init(Socket, Transport, Opts) ->
	Dispatch = proplists:get_value(dispatch, Opts, []),
	Timeout = proplists:get_value(timeout, Opts, 5000),
	wait_request(#state{socket=Socket, transport=Transport,
		dispatch=Dispatch, timeout=Timeout}).

-spec wait_request(State::#state{}) -> ok.
wait_request(State=#state{socket=Socket, transport=Transport, timeout=T}) ->
	Transport:setopts(Socket, [{packet, http}]),
	case Transport:recv(Socket, 0, T) of
		{ok, Request} -> request(Request, State);
		{error, timeout} -> error_terminate(408, State);
		{error, closed} -> terminate(State)
	end.

-spec request({http_request, Method::http_method(), URI::http_uri(),
	Version::http_version()}, State::#state{}) -> ok.
%% @todo We probably want to handle some things differently between versions.
request({http_request, _Method, _URI, Version}, State)
		when Version =/= {1, 0}, Version =/= {1, 1} ->
	error_terminate(505, State);
%% @todo We need to cleanup the URI properly.
request({http_request, Method, {abs_path, AbsPath}, Version},
		State=#state{socket=Socket, transport=Transport}) ->
	{Path, Qs} = cowboy_dispatcher:split_path(AbsPath),
	{ok, Peer} = Transport:peername(Socket),
	wait_header(#http_req{method=Method, version=Version,
		peer=Peer, path=Path, raw_qs=Qs}, State).

-spec wait_header(Req::#http_req{}, State::#state{}) -> ok.
%% @todo We don't want to wait T at each header...
%%       We want to wait T total until we reach the body.
wait_header(Req, State=#state{socket=Socket,
		transport=Transport, timeout=T}) ->
	case Transport:recv(Socket, 0, T) of
		{ok, Header} -> header(Header, Req, State);
		{error, timeout} -> error_terminate(408, State);
		{error, closed} -> terminate(State)
	end.

-spec header({http_header, I::integer(), Field::http_header(), R::term(),
	Value::string()} | http_eoh, Req::#http_req{}, State::#state{}) -> ok.
header({http_header, _I, 'Host', _R, Value}, Req=#http_req{path=Path},
		State=#state{dispatch=Dispatch}) ->
	Host = cowboy_dispatcher:split_host(Value),
	%% @todo We probably want to filter the Host and Path here to allow
	%%       things like url rewriting.
	case cowboy_dispatcher:match(Host, Path, Dispatch) of
		{ok, Handler, Opts, Binds} ->
			wait_header(Req#http_req{host=Host, bindings=Binds,
				headers=[{'Host', Value}|Req#http_req.headers]},
				State#state{handler={Handler, Opts}});
		{error, notfound} ->
			error_terminate(404, State)
	end;
header({http_header, _I, 'Connection', _R, Connection}, Req, State) ->
	wait_header(Req#http_req{
		headers=[{'Connection', Connection}|Req#http_req.headers]},
		State#state{connection=connection_to_atom(Connection)});
header({http_header, _I, Field, _R, Value}, Req, State) ->
	wait_header(Req#http_req{headers=[{Field, Value}|Req#http_req.headers]},
		State);
%% The Host header is required.
header(http_eoh, #http_req{host=undefined}, State) ->
	error_terminate(400, State);
header(http_eoh, Req, State) ->
	handler_loop(Req, State).

-spec handler_loop(Req::#http_req{}, State::#state{}) -> ok.
handler_loop(Req, State=#state{handler={Handler, Opts}}) ->
	case Handler:handle(Opts, Req) of
		{reply, RCode, RHeaders, RBody} ->
			reply(RCode, RHeaders, RBody, State)
		%% @todo stream_reply, request_body, stream_request_body...
	end.

-spec error_terminate(Code::http_status(), State::#state{}) -> ok.
error_terminate(Code, State) ->
	reply(Code, [], [], State#state{connection=close}),
	terminate(State).

-spec terminate(State::#state{}) -> ok.
terminate(#state{socket=Socket, transport=Transport}) ->
	Transport:close(Socket),
	ok.

-spec reply(Code::http_status(), Headers::http_headers(), Body::iolist(),
	State::#state{}) -> ok.
%% @todo Don't be naive about the headers!
reply(Code, Headers, Body, State=#state{socket=Socket,
		transport=TransportMod, connection=Connection}) ->
	StatusLine = ["HTTP/1.1 ", status(Code), "\r\n"],
	BaseHeaders = ["Connection: ", atom_to_connection(Connection),
		"\r\nContent-Length: ", integer_to_list(iolist_size(Body)), "\r\n"],
	TransportMod:send(Socket,
		[StatusLine, BaseHeaders, Headers, "\r\n", Body]),
	next_request(State).

-spec next_request(State::#state{}) -> ok.
next_request(State=#state{connection=keepalive}) ->
	wait_request(State);
next_request(State=#state{connection=close}) ->
	terminate(State).

%% Internal.

-spec connection_to_atom(Connection::string()) -> keepalive | close.
connection_to_atom(Connection) ->
	case string:to_lower(Connection) of
		"close" -> close;
		_Any -> keepalive
	end.

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
