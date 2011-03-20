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
-export([init/3, wait_request/1]). %% FSM.

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
	{Path, RawPath, Qs} = cowboy_dispatcher:split_path(AbsPath),
	{ok, Peer} = Transport:peername(Socket),
	ConnAtom = version_to_connection(Version),
	wait_header(#http_req{socket=Socket, transport=Transport,
		connection=ConnAtom, method=Method, version=Version,
		peer=Peer, path=Path, raw_path=RawPath, raw_qs=Qs}, State);
request({http_request, Method, '*', Version},
		State=#state{socket=Socket, transport=Transport}) ->
	{ok, Peer} = Transport:peername(Socket),
	ConnAtom = version_to_connection(Version),
	wait_header(#http_req{socket=Socket, transport=Transport,
		connection=ConnAtom, method=Method, version=Version,
		peer=Peer, path='*', raw_path="*", raw_qs=[]}, State);
request({http_request, _Method, _URI, _Version}, State) ->
	error_terminate(501, State);
request({http_error, "\r\n"}, State) ->
	wait_request(State);
request({http_error, _Any}, State) ->
	error_terminate(400, State).

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
header({http_header, _I, 'Host', _R, RawHost}, Req=#http_req{path=Path,
		host=undefined}, State=#state{dispatch=Dispatch}) ->
	RawHost2 = string:to_lower(RawHost),
	Host = cowboy_dispatcher:split_host(RawHost2),
	%% @todo We probably want to filter the Host and Path here to allow
	%%       things like url rewriting.
	case cowboy_dispatcher:match(Host, Path, Dispatch) of
		{ok, Handler, Opts, Binds} ->
			wait_header(Req#http_req{
				host=Host, raw_host=RawHost2, bindings=Binds,
				headers=[{'Host', RawHost2}|Req#http_req.headers]},
				State#state{handler={Handler, Opts}});
		{error, notfound, host} ->
			error_terminate(400, State);
		{error, notfound, path} ->
			error_terminate(404, State)
	end;
%% Ignore Host headers if we already have it.
header({http_header, _I, 'Host', _R, _V}, Req, State) ->
	wait_header(Req, State);
header({http_header, _I, 'Connection', _R, Connection}, Req, State) ->
	ConnAtom = connection_to_atom(Connection),
	wait_header(Req#http_req{connection=ConnAtom,
		headers=[{'Connection', Connection}|Req#http_req.headers]},
		State#state{connection=ConnAtom});
header({http_header, _I, Field, _R, Value}, Req, State) ->
	wait_header(Req#http_req{headers=[{Field, Value}|Req#http_req.headers]},
		State);
%% The Host header is required.
header(http_eoh, #http_req{host=undefined}, State) ->
	error_terminate(400, State);
header(http_eoh, Req, State) ->
	handler_init(Req, State).

-spec handler_init(Req::#http_req{}, State::#state{}) -> ok.
handler_init(Req, State=#state{handler={Handler, Opts}}) ->
	case catch Handler:init(Req, Opts) of
		{ok, Req, HandlerState} ->
			handler_loop(HandlerState, Req, State);
		%% @todo {upgrade, transport, Module}; {upgrade, protocol, Module}
		{'EXIT', _Reason} ->
			error_terminate(500, State)
	end.

-spec handler_loop(HandlerState::term(), Req::#http_req{},
	State::#state{}) -> ok.
handler_loop(HandlerState, Req, State=#state{handler={Handler, _Opts}}) ->
	case catch Handler:handle(Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			handler_terminate(HandlerState2, Req2, State);
		{'EXIT', _Reason} ->
			terminate(State)
	end.

-spec handler_terminate(HandlerState::term(), Req::#http_req{},
	State::#state{}) -> ok.
handler_terminate(HandlerState, Req, State=#state{handler={Handler, _Opts}}) ->
	Res = (catch Handler:terminate(Req, HandlerState)),
	%% @todo We need to check if the Req has been replied to.
	%%       All requests must have a reply, at worst an error.
	%%       If a request started but wasn't completed, complete it.
	case {Res, State#state.connection} of
		{ok, keepalive} -> next_request(State);
		_Closed -> terminate(State)
	end.

-spec error_terminate(Code::http_status(), State::#state{}) -> ok.
error_terminate(Code, State=#state{socket=Socket, transport=Transport}) ->
	cowboy_http_req:reply(Code, [], [], #http_req{socket=Socket,
		transport=Transport, connection=close}),
	terminate(State).

-spec terminate(State::#state{}) -> ok.
terminate(#state{socket=Socket, transport=Transport}) ->
	Transport:close(Socket),
	ok.

-spec next_request(State::#state{}) -> ok.
next_request(State=#state{connection=keepalive}) ->
	?MODULE:wait_request(State);
next_request(State=#state{connection=close}) ->
	terminate(State).

%% Internal.

-spec version_to_connection(Version::http_version()) -> keepalive | close.
version_to_connection({1, 1}) -> keepalive;
version_to_connection(_Any) -> close.

-spec connection_to_atom(Connection::string()) -> keepalive | close.
connection_to_atom(Connection) ->
	case string:to_lower(Connection) of
		"close" -> close;
		_Any -> keepalive
	end.
