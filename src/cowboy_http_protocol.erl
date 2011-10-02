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

%% @doc HTTP protocol handler.
%%
%% The available options are:
%% <dl>
%%  <dt>dispatch</dt><dd>The dispatch list for this protocol.</dd>
%%  <dt>max_empty_lines</dt><dd>Max number of empty lines before a request.
%%   Defaults to 5.</dd>
%%  <dt>timeout</dt><dd>Time in milliseconds before an idle keep-alive
%%   connection is closed. Defaults to 5000 milliseconds.</dd>
%% </dl>
%%
%% Note that there is no need to monitor these processes when using Cowboy as
%% an application as it already supervises them under the listener supervisor.
%%
%% @see cowboy_dispatcher
%% @see cowboy_http_handler
-module(cowboy_http_protocol).
-behaviour(cowboy_protocol).

-export([start_link/4]). %% API.
-export([init/4, parse_request/1]). %% FSM.

-include("include/http.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {
	listener :: pid(),
	socket :: inet:socket(),
	transport :: module(),
	dispatch :: cowboy_dispatcher:dispatch_rules(),
	handler :: {module(), any()},
	req_empty_lines = 0 :: integer(),
	max_empty_lines :: integer(),
	timeout :: timeout(),
	connection = keepalive :: keepalive | close,
	buffer = <<>> :: binary()
}).

%% API.

%% @doc Start an HTTP protocol process.
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
	{ok, Pid}.

%% FSM.

%% @private
-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, Opts) ->
	Dispatch = proplists:get_value(dispatch, Opts, []),
	MaxEmptyLines = proplists:get_value(max_empty_lines, Opts, 5),
	Timeout = proplists:get_value(timeout, Opts, 5000),
	receive shoot -> ok end,
	wait_request(#state{listener=ListenerPid, socket=Socket, transport=Transport,
		dispatch=Dispatch, max_empty_lines=MaxEmptyLines, timeout=Timeout}).

%% @private
-spec parse_request(#state{}) -> ok.
%% @todo Use decode_packet options to limit length?
parse_request(State=#state{buffer=Buffer}) ->
	case erlang:decode_packet(http_bin, Buffer, []) of
		{ok, Request, Rest} -> request(Request, State#state{buffer=Rest});
		{more, _Length} -> wait_request(State);
		{error, _Reason} -> error_response(400, State)
	end.

-spec wait_request(#state{}) -> ok.
wait_request(State=#state{socket=Socket, transport=Transport,
		timeout=T, buffer=Buffer}) ->
	case Transport:recv(Socket, 0, T) of
		{ok, Data} -> parse_request(State#state{
			buffer= << Buffer/binary, Data/binary >>});
		{error, timeout} -> error_terminate(408, State);
		{error, closed} -> terminate(State)
	end.

-spec request({http_request, http_method(), http_uri(),
	http_version()}, #state{}) -> ok.
%% @todo We probably want to handle some things differently between versions.
request({http_request, _Method, _URI, Version}, State)
		when Version =/= {1, 0}, Version =/= {1, 1} ->
	error_terminate(505, State);
%% @todo We need to cleanup the URI properly.
request({http_request, Method, {abs_path, AbsPath}, Version},
		State=#state{socket=Socket, transport=Transport}) ->
	{Path, RawPath, Qs} = cowboy_dispatcher:split_path(AbsPath),
	ConnAtom = version_to_connection(Version),
	parse_header(#http_req{socket=Socket, transport=Transport,
		connection=ConnAtom, method=Method, version=Version,
		path=Path, raw_path=RawPath, raw_qs=Qs},
		State#state{connection=ConnAtom});
request({http_request, Method, '*', Version},
		State=#state{socket=Socket, transport=Transport}) ->
	ConnAtom = version_to_connection(Version),
	parse_header(#http_req{socket=Socket, transport=Transport,
		connection=ConnAtom, method=Method, version=Version,
		path='*', raw_path= <<"*">>, raw_qs= <<>>},
		State#state{connection=ConnAtom});
request({http_request, _Method, _URI, _Version}, State) ->
	error_terminate(501, State);
request({http_error, <<"\r\n">>},
		State=#state{req_empty_lines=N, max_empty_lines=N}) ->
	error_terminate(400, State);
request({http_error, <<"\r\n">>}, State=#state{req_empty_lines=N}) ->
	parse_request(State#state{req_empty_lines=N + 1});
request({http_error, _Any}, State) ->
	error_terminate(400, State).

-spec parse_header(#http_req{}, #state{}) -> ok.
parse_header(Req, State=#state{buffer=Buffer}) ->
	case erlang:decode_packet(httph_bin, Buffer, []) of
		{ok, Header, Rest} -> header(Header, Req, State#state{buffer=Rest});
		{more, _Length} -> wait_header(Req, State);
		{error, _Reason} -> error_response(400, State)
	end.

-spec wait_header(#http_req{}, #state{}) -> ok.
wait_header(Req, State=#state{socket=Socket,
		transport=Transport, timeout=T, buffer=Buffer}) ->
	case Transport:recv(Socket, 0, T) of
		{ok, Data} -> parse_header(Req, State#state{
			buffer= << Buffer/binary, Data/binary >>});
		{error, timeout} -> error_terminate(408, State);
		{error, closed} -> terminate(State)
	end.

-spec header({http_header, integer(), http_header(), any(), binary()}
	| http_eoh, #http_req{}, #state{}) -> ok.
header({http_header, _I, 'Host', _R, RawHost}, Req=#http_req{
		transport=Transport, host=undefined}, State) ->
	RawHost2 = binary_to_lower(RawHost),
	case catch cowboy_dispatcher:split_host(RawHost2) of
		{Host, RawHost3, undefined} ->
			Port = default_port(Transport:name()),
			dispatch(fun parse_header/2, Req#http_req{
				host=Host, raw_host=RawHost3, port=Port,
				headers=[{'Host', RawHost3}|Req#http_req.headers]}, State);
		{Host, RawHost3, Port} ->
			dispatch(fun parse_header/2, Req#http_req{
				host=Host, raw_host=RawHost3, port=Port,
				headers=[{'Host', RawHost3}|Req#http_req.headers]}, State);
		{'EXIT', _Reason} ->
			error_terminate(400, State)
	end;
%% Ignore Host headers if we already have it.
header({http_header, _I, 'Host', _R, _V}, Req, State) ->
	parse_header(Req, State);
header({http_header, _I, 'Connection', _R, Connection}, Req, State) ->
	ConnAtom = connection_to_atom(Connection),
	parse_header(Req#http_req{connection=ConnAtom,
		headers=[{'Connection', Connection}|Req#http_req.headers]},
		State#state{connection=ConnAtom});
header({http_header, _I, Field, _R, Value}, Req, State) ->
	Field2 = format_header(Field),
	parse_header(Req#http_req{headers=[{Field2, Value}|Req#http_req.headers]},
		State);
%% The Host header is required in HTTP/1.1.
header(http_eoh, #http_req{version={1, 1}, host=undefined}, State) ->
	error_terminate(400, State);
%% It is however optional in HTTP/1.0.
header(http_eoh, Req=#http_req{version={1, 0}, transport=Transport,
		host=undefined}, State=#state{buffer=Buffer}) ->
	Port = default_port(Transport:name()),
	dispatch(fun handler_init/2, Req#http_req{host=[], raw_host= <<>>,
		port=Port, buffer=Buffer}, State#state{buffer= <<>>});
header(http_eoh, Req, State=#state{buffer=Buffer}) ->
	handler_init(Req#http_req{buffer=Buffer}, State#state{buffer= <<>>});
header({http_error, _Bin}, _Req, State) ->
	error_terminate(500, State).

-spec dispatch(fun((#http_req{}, #state{}) -> ok),
	#http_req{}, #state{}) -> ok.
dispatch(Next, Req=#http_req{host=Host, path=Path},
		State=#state{dispatch=Dispatch}) ->
	%% @todo We probably want to filter the Host and Path here to allow
	%%       things like url rewriting.
	case cowboy_dispatcher:match(Host, Path, Dispatch) of
		{ok, Handler, Opts, Binds, HostInfo, PathInfo} ->
			Next(Req#http_req{host_info=HostInfo, path_info=PathInfo,
				bindings=Binds}, State#state{handler={Handler, Opts}});
		{error, notfound, host} ->
			error_terminate(400, State);
		{error, notfound, path} ->
			error_terminate(404, State)
	end.

-spec handler_init(#http_req{}, #state{}) -> ok.
handler_init(Req, State=#state{listener=ListenerPid,
		transport=Transport, handler={Handler, Opts}}) ->
	try Handler:init({Transport:name(), http}, Req, Opts) of
		{ok, Req2, HandlerState} ->
			handler_loop(HandlerState, Req2, State);
		%% @todo {upgrade, transport, Module}
		{upgrade, protocol, Module} ->
			Module:upgrade(ListenerPid, Handler, Opts, Req)
	catch Class:Reason ->
		error_terminate(500, State),
		error_logger:error_msg(
			"** Handler ~p terminating in init/3~n"
			"   for the reason ~p:~p~n"
			"** Options were ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts, Req, erlang:get_stacktrace()])
	end.

-spec handler_loop(any(), #http_req{}, #state{}) -> ok.
handler_loop(HandlerState, Req, State=#state{handler={Handler, Opts}}) ->
	try Handler:handle(Req#http_req{resp_state=waiting}, HandlerState) of
		{ok, Req2, HandlerState2} ->
			next_request(HandlerState2, Req2, State)
	catch Class:Reason ->
		error_logger:error_msg(
			"** Handler ~p terminating in handle/2~n"
			"   for the reason ~p:~p~n"
			"** Options were ~p~n** Handler state was ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts,
			 HandlerState, Req, erlang:get_stacktrace()]),
		handler_terminate(HandlerState, Req, State),
		terminate(State)
	end.

-spec handler_terminate(any(), #http_req{}, #state{}) -> ok | error.
handler_terminate(HandlerState, Req, #state{handler={Handler, Opts}}) ->
	try
		Handler:terminate(Req#http_req{resp_state=locked}, HandlerState)
	catch Class:Reason ->
		error_logger:error_msg(
			"** Handler ~p terminating in terminate/2~n"
			"   for the reason ~p:~p~n"
			"** Options were ~p~n** Handler state was ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts,
			 HandlerState, Req, erlang:get_stacktrace()]),
		error
	end.

-spec next_request(any(), #http_req{}, #state{}) -> ok.
next_request(HandlerState, Req=#http_req{buffer=Buffer}, State) ->
	HandlerRes = handler_terminate(HandlerState, Req, State),
	BodyRes = ensure_body_processed(Req),
	RespRes = ensure_response(Req, State),
	case {HandlerRes, BodyRes, RespRes, State#state.connection} of
		{ok, ok, ok, keepalive} ->
			?MODULE:parse_request(State#state{
				buffer=Buffer, req_empty_lines=0});
		_Closed ->
			terminate(State)
	end.

-spec ensure_body_processed(#http_req{}) -> ok | close.
ensure_body_processed(#http_req{body_state=done}) ->
	ok;
ensure_body_processed(Req=#http_req{body_state=waiting}) ->
	case cowboy_http_req:body(Req) of
		{error, badarg} -> ok; %% No body.
		{error, _Reason} -> close;
		_Any -> ok
	end.

-spec ensure_response(#http_req{}, #state{}) -> ok.
%% The handler has already fully replied to the client.
ensure_response(#http_req{resp_state=done}, _State) ->
	ok;
%% No response has been sent but everything apparently went fine.
%% Reply with 204 No Content to indicate this.
ensure_response(#http_req{resp_state=waiting}, State) ->
	error_response(204, State);
%% Close the chunked reply.
ensure_response(#http_req{socket=Socket, transport=Transport,
		resp_state=chunks}, _State) ->
	Transport:send(Socket, <<"0\r\n\r\n">>),
	close.

-spec error_response(http_status(), #state{}) -> ok.
error_response(Code, #state{socket=Socket,
		transport=Transport, connection=Connection}) ->
	_ = cowboy_http_req:reply(Code, [], [], #http_req{
		socket=Socket, transport=Transport,
		connection=Connection, resp_state=waiting}),
	ok.

-spec error_terminate(http_status(), #state{}) -> ok.
error_terminate(Code, State) ->
	error_response(Code, State#state{connection=close}),
	terminate(State).

-spec terminate(#state{}) -> ok.
terminate(#state{socket=Socket, transport=Transport}) ->
	Transport:close(Socket),
	ok.

%% Internal.

-spec version_to_connection(http_version()) -> keepalive | close.
version_to_connection({1, 1}) -> keepalive;
version_to_connection(_Any) -> close.

%% @todo Connection can take more than one value.
-spec connection_to_atom(binary()) -> keepalive | close.
connection_to_atom(<<"keep-alive">>) ->
	keepalive;
connection_to_atom(<<"close">>) ->
	close;
connection_to_atom(Connection) ->
	case binary_to_lower(Connection) of
		<<"close">> -> close;
		_Any -> keepalive
	end.

-spec default_port(atom()) -> 80 | 443.
default_port(ssl) -> 443;
default_port(_) -> 80.

%% @todo While 32 should be enough for everybody, we should probably make
%%       this configurable or something.
-spec format_header(atom()) -> atom(); (binary()) -> binary().
format_header(Field) when is_atom(Field) ->
	Field;
format_header(Field) when byte_size(Field) =< 20; byte_size(Field) > 32 ->
	Field;
format_header(Field) ->
	format_header(Field, true, <<>>).

-spec format_header(binary(), boolean(), binary()) -> binary().
format_header(<<>>, _Any, Acc) ->
	Acc;
%% Replicate a bug in OTP for compatibility reasons when there's a - right
%% after another. Proper use should always be 'true' instead of 'not Bool'.
format_header(<< $-, Rest/bits >>, Bool, Acc) ->
	format_header(Rest, not Bool, << Acc/binary, $- >>);
format_header(<< C, Rest/bits >>, true, Acc) ->
	format_header(Rest, false, << Acc/binary, (char_to_upper(C)) >>);
format_header(<< C, Rest/bits >>, false, Acc) ->
	format_header(Rest, false, << Acc/binary, (char_to_lower(C)) >>).

%% We are excluding a few characters on purpose.
-spec binary_to_lower(binary()) -> binary().
binary_to_lower(L) ->
	<< << (char_to_lower(C)) >> || << C >> <= L >>.

%% We gain noticeable speed by matching each value directly.
-spec char_to_lower(char()) -> char().
char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower(Ch) -> Ch.

-spec char_to_upper(char()) -> char().
char_to_upper($a) -> $A;
char_to_upper($b) -> $B;
char_to_upper($c) -> $C;
char_to_upper($d) -> $D;
char_to_upper($e) -> $E;
char_to_upper($f) -> $F;
char_to_upper($g) -> $G;
char_to_upper($h) -> $H;
char_to_upper($i) -> $I;
char_to_upper($j) -> $J;
char_to_upper($k) -> $K;
char_to_upper($l) -> $L;
char_to_upper($m) -> $M;
char_to_upper($n) -> $N;
char_to_upper($o) -> $O;
char_to_upper($p) -> $P;
char_to_upper($q) -> $Q;
char_to_upper($r) -> $R;
char_to_upper($s) -> $S;
char_to_upper($t) -> $T;
char_to_upper($u) -> $U;
char_to_upper($v) -> $V;
char_to_upper($w) -> $W;
char_to_upper($x) -> $X;
char_to_upper($y) -> $Y;
char_to_upper($z) -> $Z;
char_to_upper(Ch) -> Ch.

%% Tests.

-ifdef(TEST).

format_header_test_() ->
	%% {Header, Result}
	Tests = [
		{<<"Sec-Websocket-Version">>, <<"Sec-Websocket-Version">>},
		{<<"Sec-WebSocket-Version">>, <<"Sec-Websocket-Version">>},
		{<<"sec-websocket-version">>, <<"Sec-Websocket-Version">>},
		{<<"SEC-WEBSOCKET-VERSION">>, <<"Sec-Websocket-Version">>},
		%% These last tests ensures we're formatting headers exactly like OTP.
		%% Even though it's dumb, it's better for compatibility reasons.
		{<<"Sec-WebSocket--Version">>, <<"Sec-Websocket--version">>},
		{<<"Sec-WebSocket---Version">>, <<"Sec-Websocket---Version">>}
	],
	[{H, fun() -> R = format_header(H) end} || {H, R} <- Tests].

-endif.
