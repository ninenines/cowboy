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
%%  <dt>timeout</dt><dd>Time in milliseconds before an idle
%%   connection is closed. Defaults to 5000 milliseconds.</dd>
%%  <dt>urldecode</dt><dd>Function and options argument to use when decoding
%%   URL encoded strings. Defaults to `{fun cowboy_http:urldecode/2, crash}'.
%%   </dd>
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
-export([init/4, parse_request/1, handler_loop/3]). %% FSM.

-include("http.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {
	listener :: pid(),
	socket :: inet:socket(),
	transport :: module(),
	dispatch :: cowboy_dispatcher:dispatch_rules(),
	handler :: {module(), any()},
	onrequest :: undefined | fun((#http_req{}) -> #http_req{}),
	urldecode :: {fun((binary(), T) -> binary()), T},
	req_empty_lines = 0 :: integer(),
	max_empty_lines :: integer(),
	req_keepalive = 1 :: integer(),
	max_keepalive :: integer(),
	max_line_length :: integer(),
	timeout :: timeout(),
	buffer = <<>> :: binary(),
	hibernate = false :: boolean(),
	loop_timeout = infinity :: timeout(),
	loop_timeout_ref :: undefined | reference()
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
	MaxKeepalive = proplists:get_value(max_keepalive, Opts, infinity),
	MaxLineLength = proplists:get_value(max_line_length, Opts, 4096),
	OnRequest = proplists:get_value(onrequest, Opts),
	Timeout = proplists:get_value(timeout, Opts, 5000),
	URLDecDefault = {fun cowboy_http:urldecode/2, crash},
	URLDec = proplists:get_value(urldecode, Opts, URLDecDefault),
	ok = cowboy:accept_ack(ListenerPid),
	wait_request(#state{listener=ListenerPid, socket=Socket, transport=Transport,
		dispatch=Dispatch, max_empty_lines=MaxEmptyLines,
		max_keepalive=MaxKeepalive, max_line_length=MaxLineLength,
		timeout=Timeout, onrequest=OnRequest, urldecode=URLDec}).

%% @private
-spec parse_request(#state{}) -> ok.
%% We limit the length of the Request-line to MaxLength to avoid endlessly
%% reading from the socket and eventually crashing.
parse_request(State=#state{buffer=Buffer, max_line_length=MaxLength}) ->
	case erlang:decode_packet(http_bin, Buffer, []) of
		{ok, Request, Rest} -> request(Request, State#state{buffer=Rest});
		{more, _Length} when byte_size(Buffer) > MaxLength ->
			error_terminate(413, State);
		{more, _Length} -> wait_request(State);
		{error, _Reason} -> error_terminate(400, State)
	end.

-spec wait_request(#state{}) -> ok.
wait_request(State=#state{socket=Socket, transport=Transport,
		timeout=T, buffer=Buffer}) ->
	case Transport:recv(Socket, 0, T) of
		{ok, Data} -> parse_request(State#state{
			buffer= << Buffer/binary, Data/binary >>});
		{error, _Reason} -> terminate(State)
	end.

-spec request({http_request, cowboy_http:method(), cowboy_http:uri(),
	cowboy_http:version()}, #state{}) -> ok.
request({http_request, _Method, _URI, Version}, State)
		when Version =/= {1, 0}, Version =/= {1, 1} ->
	error_terminate(505, State);
%% We still receive the original Host header.
request({http_request, Method, {absoluteURI, _Scheme, _Host, _Port, Path},
		Version}, State) ->
	request({http_request, Method, {abs_path, Path}, Version}, State);
request({http_request, Method, {abs_path, AbsPath}, Version},
		State=#state{socket=Socket, transport=Transport,
		urldecode={URLDecFun, URLDecArg}=URLDec}) ->
	URLDecode = fun(Bin) -> URLDecFun(Bin, URLDecArg) end,
	{Path, RawPath, Qs} = cowboy_dispatcher:split_path(AbsPath, URLDecode),
	ConnAtom = version_to_connection(Version),
	parse_header(#http_req{socket=Socket, transport=Transport,
		connection=ConnAtom, pid=self(), method=Method, version=Version,
		path=Path, raw_path=RawPath, raw_qs=Qs, urldecode=URLDec}, State);
request({http_request, Method, '*', Version},
		State=#state{socket=Socket, transport=Transport, urldecode=URLDec}) ->
	ConnAtom = version_to_connection(Version),
	parse_header(#http_req{socket=Socket, transport=Transport,
		connection=ConnAtom, pid=self(), method=Method, version=Version,
		path='*', raw_path= <<"*">>, raw_qs= <<>>, urldecode=URLDec}, State);
request({http_request, _Method, _URI, _Version}, State) ->
	error_terminate(501, State);
request({http_error, <<"\r\n">>},
		State=#state{req_empty_lines=N, max_empty_lines=N}) ->
	error_terminate(400, State);
request({http_error, <<"\r\n">>}, State=#state{req_empty_lines=N}) ->
	parse_request(State#state{req_empty_lines=N + 1});
request(_Any, State) ->
	error_terminate(400, State).

-spec parse_header(#http_req{}, #state{}) -> ok.
parse_header(Req, State=#state{buffer=Buffer, max_line_length=MaxLength}) ->
	case erlang:decode_packet(httph_bin, Buffer, []) of
		{ok, Header, Rest} -> header(Header, Req, State#state{buffer=Rest});
		{more, _Length} when byte_size(Buffer) > MaxLength ->
			error_terminate(413, State);
		{more, _Length} -> wait_header(Req, State);
		{error, _Reason} -> error_terminate(400, State)
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

-spec header({http_header, integer(), cowboy_http:header(), any(), binary()}
	| http_eoh, #http_req{}, #state{}) -> ok.
header({http_header, _I, 'Host', _R, RawHost}, Req=#http_req{
		transport=Transport, host=undefined}, State) ->
	RawHost2 = cowboy_bstr:to_lower(RawHost),
	case catch cowboy_dispatcher:split_host(RawHost2) of
		{Host, RawHost3, undefined} ->
			Port = default_port(Transport:name()),
			parse_header(Req#http_req{
				host=Host, raw_host=RawHost3, port=Port,
				headers=[{'Host', RawHost3}|Req#http_req.headers]}, State);
		{Host, RawHost3, Port} ->
			parse_header(Req#http_req{
				host=Host, raw_host=RawHost3, port=Port,
				headers=[{'Host', RawHost3}|Req#http_req.headers]}, State);
		{'EXIT', _Reason} ->
			error_terminate(400, State)
	end;
%% Ignore Host headers if we already have it.
header({http_header, _I, 'Host', _R, _V}, Req, State) ->
	parse_header(Req, State);
header({http_header, _I, 'Connection', _R, Connection},
		Req=#http_req{headers=Headers}, State) ->
	Req2 = Req#http_req{headers=[{'Connection', Connection}|Headers]},
	{ConnTokens, Req3}
		= cowboy_http_req:parse_header('Connection', Req2),
	ConnAtom = cowboy_http:connection_to_atom(ConnTokens),
	parse_header(Req3#http_req{connection=ConnAtom}, State);
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
	onrequest(Req#http_req{host=[], raw_host= <<>>,
		port=Port, buffer=Buffer}, State#state{buffer= <<>>});
header(http_eoh, Req, State=#state{buffer=Buffer}) ->
	onrequest(Req#http_req{buffer=Buffer}, State#state{buffer= <<>>});
header(_Any, _Req, State) ->
	error_terminate(400, State).

%% Call the global onrequest callback. The callback can send a reply,
%% in which case we consider the request handled and move on to the next
%% one. Note that since we haven't dispatched yet, we don't know the
%% handler, host_info, path_info or bindings yet.
-spec onrequest(#http_req{}, #state{}) -> ok.
onrequest(Req, State=#state{onrequest=undefined}) ->
	dispatch(Req, State);
onrequest(Req, State=#state{onrequest=OnRequest}) ->
	Req2 = OnRequest(Req),
	case Req2#http_req.resp_state of
		waiting -> dispatch(Req2, State);
		_ -> next_request(Req2, State, ok)
	end.

-spec dispatch(#http_req{}, #state{}) -> ok.
dispatch(Req=#http_req{host=Host, path=Path},
		State=#state{dispatch=Dispatch}) ->
	case cowboy_dispatcher:match(Host, Path, Dispatch) of
		{ok, Handler, Opts, Binds, HostInfo, PathInfo} ->
			handler_init(Req#http_req{host_info=HostInfo, path_info=PathInfo,
				bindings=Binds}, State#state{handler={Handler, Opts}});
		{error, notfound, host} ->
			error_terminate(400, State);
		{error, notfound, path} ->
			error_terminate(404, State)
	end.

-spec handler_init(#http_req{}, #state{}) -> ok.
handler_init(Req, State=#state{transport=Transport,
		handler={Handler, Opts}}) ->
	try Handler:init({Transport:name(), http}, Req, Opts) of
		{ok, Req2, HandlerState} ->
			handler_handle(HandlerState, Req2, State);
		{loop, Req2, HandlerState} ->
			handler_before_loop(HandlerState, Req2, State);
		{loop, Req2, HandlerState, hibernate} ->
			handler_before_loop(HandlerState, Req2,
				State#state{hibernate=true});
		{loop, Req2, HandlerState, Timeout} ->
			handler_before_loop(HandlerState, Req2,
				State#state{loop_timeout=Timeout});
		{loop, Req2, HandlerState, Timeout, hibernate} ->
			handler_before_loop(HandlerState, Req2,
				State#state{hibernate=true, loop_timeout=Timeout});
		{shutdown, Req2, HandlerState} ->
			handler_terminate(HandlerState, Req2, State);
		%% @todo {upgrade, transport, Module}
		{upgrade, protocol, Module} ->
			upgrade_protocol(Req, State, Module)
	catch Class:Reason ->
		error_terminate(500, State),
		PLReq = lists:zip(record_info(fields, http_req), tl(tuple_to_list(Req))),
		error_logger:error_msg(
			"** Handler ~p terminating in init/3~n"
			"   for the reason ~p:~p~n"
			"** Options were ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts, PLReq, erlang:get_stacktrace()])
	end.

-spec upgrade_protocol(#http_req{}, #state{}, atom()) -> ok.
upgrade_protocol(Req, State=#state{listener=ListenerPid,
		handler={Handler, Opts}}, Module) ->
	case Module:upgrade(ListenerPid, Handler, Opts, Req) of
		{UpgradeRes, Req2} -> next_request(Req2, State, UpgradeRes);
		_Any -> terminate(State)
	end.

-spec handler_handle(any(), #http_req{}, #state{}) -> ok.
handler_handle(HandlerState, Req, State=#state{handler={Handler, Opts}}) ->
	try Handler:handle(Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			terminate_request(HandlerState2, Req2, State)
	catch Class:Reason ->
		PLReq = lists:zip(record_info(fields, http_req), tl(tuple_to_list(Req))),
		error_logger:error_msg(
			"** Handler ~p terminating in handle/2~n"
			"   for the reason ~p:~p~n"
			"** Options were ~p~n** Handler state was ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts,
			 HandlerState, PLReq, erlang:get_stacktrace()]),
		handler_terminate(HandlerState, Req, State),
		error_terminate(500, State)
	end.

%% We don't listen for Transport closes because that would force us
%% to receive data and buffer it indefinitely.
-spec handler_before_loop(any(), #http_req{}, #state{}) -> ok.
handler_before_loop(HandlerState, Req, State=#state{hibernate=true}) ->
	State2 = handler_loop_timeout(State),
	catch erlang:hibernate(?MODULE, handler_loop,
		[HandlerState, Req, State2#state{hibernate=false}]),
	ok;
handler_before_loop(HandlerState, Req, State) ->
	State2 = handler_loop_timeout(State),
	handler_loop(HandlerState, Req, State2).

%% Almost the same code can be found in cowboy_http_websocket.
-spec handler_loop_timeout(#state{}) -> #state{}.
handler_loop_timeout(State=#state{loop_timeout=infinity}) ->
	State#state{loop_timeout_ref=undefined};
handler_loop_timeout(State=#state{loop_timeout=Timeout,
		loop_timeout_ref=PrevRef}) ->
	_ = case PrevRef of undefined -> ignore; PrevRef ->
		erlang:cancel_timer(PrevRef) end,
	TRef = make_ref(),
	erlang:send_after(Timeout, self(), {?MODULE, timeout, TRef}),
	State#state{loop_timeout_ref=TRef}.

-spec handler_loop(any(), #http_req{}, #state{}) -> ok.
handler_loop(HandlerState, Req, State=#state{loop_timeout_ref=TRef}) ->
	receive
		{?MODULE, timeout, TRef} ->
			terminate_request(HandlerState, Req, State);
		{?MODULE, timeout, OlderTRef} when is_reference(OlderTRef) ->
			handler_loop(HandlerState, Req, State);
		Message ->
			handler_call(HandlerState, Req, State, Message)
	end.

-spec handler_call(any(), #http_req{}, #state{}, any()) -> ok.
handler_call(HandlerState, Req, State=#state{handler={Handler, Opts}},
		Message) ->
	try Handler:info(Message, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			terminate_request(HandlerState2, Req2, State);
		{loop, Req2, HandlerState2} ->
			handler_before_loop(HandlerState2, Req2, State);
		{loop, Req2, HandlerState2, hibernate} ->
			handler_before_loop(HandlerState2, Req2,
				State#state{hibernate=true})
	catch Class:Reason ->
		PLReq = lists:zip(record_info(fields, http_req), tl(tuple_to_list(Req))),
		error_logger:error_msg(
			"** Handler ~p terminating in info/3~n"
			"   for the reason ~p:~p~n"
			"** Options were ~p~n** Handler state was ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts,
			 HandlerState, PLReq, erlang:get_stacktrace()]),
		handler_terminate(HandlerState, Req, State),
		error_terminate(500, State)
	end.

-spec handler_terminate(any(), #http_req{}, #state{}) -> ok.
handler_terminate(HandlerState, Req, #state{handler={Handler, Opts}}) ->
	try
		Handler:terminate(Req#http_req{resp_state=locked}, HandlerState)
	catch Class:Reason ->
		PLReq = lists:zip(record_info(fields, http_req), tl(tuple_to_list(Req))),
		error_logger:error_msg(
			"** Handler ~p terminating in terminate/2~n"
			"   for the reason ~p:~p~n"
			"** Options were ~p~n** Handler state was ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts,
			 HandlerState, PLReq, erlang:get_stacktrace()])
	end.

-spec terminate_request(any(), #http_req{}, #state{}) -> ok.
terminate_request(HandlerState, Req, State) ->
	HandlerRes = handler_terminate(HandlerState, Req, State),
	next_request(Req, State, HandlerRes).

-spec next_request(#http_req{}, #state{}, any()) -> ok.
next_request(Req=#http_req{connection=Conn},
		State=#state{req_keepalive=Keepalive, max_keepalive=MaxKeepalive},
		HandlerRes) ->
	RespRes = ensure_response(Req),
	{BodyRes, Buffer} = ensure_body_processed(Req),
	%% Flush the resp_sent message before moving on.
	receive {cowboy_http_req, resp_sent} -> ok after 0 -> ok end,
	case {HandlerRes, BodyRes, RespRes, Conn} of
		{ok, ok, ok, keepalive} when Keepalive < MaxKeepalive ->
			?MODULE:parse_request(State#state{
				buffer=Buffer, req_empty_lines=0,
				req_keepalive=Keepalive + 1});
		_Closed ->
			terminate(State)
	end.

-spec ensure_body_processed(#http_req{}) -> {ok | close, binary()}.
ensure_body_processed(#http_req{body_state=done, buffer=Buffer}) ->
	{ok, Buffer};
ensure_body_processed(Req=#http_req{body_state=waiting}) ->
	case cowboy_http_req:skip_body(Req) of
		{ok, Req2} -> {ok, Req2#http_req.buffer};
		{error, _Reason} -> {close, <<>>}
	end;
ensure_body_processed(Req=#http_req{body_state={multipart, _, _}}) ->
	{ok, Req2} = cowboy_http_req:multipart_skip(Req),
	ensure_body_processed(Req2).

-spec ensure_response(#http_req{}) -> ok.
%% The handler has already fully replied to the client.
ensure_response(#http_req{resp_state=done}) ->
	ok;
%% No response has been sent but everything apparently went fine.
%% Reply with 204 No Content to indicate this.
ensure_response(Req=#http_req{resp_state=waiting}) ->
	_ = cowboy_http_req:reply(204, [], [], Req),
	ok;
%% Terminate the chunked body for HTTP/1.1 only.
ensure_response(#http_req{method='HEAD', resp_state=chunks}) ->
	ok;
ensure_response(#http_req{version={1, 0}, resp_state=chunks}) ->
	ok;
ensure_response(#http_req{socket=Socket, transport=Transport,
		resp_state=chunks}) ->
	Transport:send(Socket, <<"0\r\n\r\n">>),
	ok.

%% Only send an error reply if there is no resp_sent message.
-spec error_terminate(cowboy_http:status(), #state{}) -> ok.
error_terminate(Code, State=#state{socket=Socket, transport=Transport}) ->
	receive
		{cowboy_http_req, resp_sent} -> ok
	after 0 ->
		_ = cowboy_http_req:reply(Code, #http_req{
			socket=Socket, transport=Transport,
			connection=close, pid=self(), resp_state=waiting}),
		ok
	end,
	terminate(State).

-spec terminate(#state{}) -> ok.
terminate(#state{socket=Socket, transport=Transport}) ->
	Transport:close(Socket),
	ok.

%% Internal.

-spec version_to_connection(cowboy_http:version()) -> keepalive | close.
version_to_connection({1, 1}) -> keepalive;
version_to_connection(_Any) -> close.

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
	format_header(Rest, false, << Acc/binary, (cowboy_bstr:char_to_upper(C)) >>);
format_header(<< C, Rest/bits >>, false, Acc) ->
	format_header(Rest, false, << Acc/binary, (cowboy_bstr:char_to_lower(C)) >>).

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
