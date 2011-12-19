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

%% @doc WebSocket protocol implementation.
%%
%% Supports the protocol version 0 (hixie-76), version 7 (hybi-7)
%% and version 8 (hybi-8, hybi-9 and hybi-10).
%%
%% Version 0 is supported by the following browsers:
%% <ul>
%%  <li>Firefox 4-5 (disabled by default)</li>
%%  <li>Chrome 6-13</li>
%%  <li>Safari 5.0.1+</li>
%%  <li>Opera 11.00+ (disabled by default)</li>
%% </ul>
%%
%% Version 7 is supported by the following browser:
%% <ul>
%%  <li>Firefox 6</li>
%% </ul>
%%
%% Version 8+ is supported by the following browsers:
%% <ul>
%%  <li>Firefox 7+</li>
%%  <li>Chrome 14+</li>
%% </ul>
-module(cowboy_http_websocket).

-export([upgrade/4]). %% API.
-export([handler_loop/4]). %% Internal.

-include("include/http.hrl").
-include_lib("eunit/include/eunit.hrl").

-type opcode() :: 0 | 1 | 2 | 8 | 9 | 10.
-type mask_key() :: 0..16#ffffffff.

-record(state, {
	version :: 0 | 7 | 8 | 13,
	handler :: module(),
	opts :: any(),
	challenge = undefined :: undefined | binary() | {binary(), binary()},
	timeout = infinity :: timeout(),
	timeout_ref = undefined :: undefined | reference(),
	messages = undefined :: undefined | {atom(), atom(), atom()},
	hibernate = false :: boolean(),
	eop :: undefined | tuple(), %% hixie-76 specific.
	origin = undefined :: undefined | binary() %% hixie-76 specific.
}).

%% @doc Upgrade a HTTP request to the WebSocket protocol.
%%
%% You do not need to call this function manually. To upgrade to the WebSocket
%% protocol, you simply need to return <em>{upgrade, protocol, {@module}}</em>
%% in your <em>cowboy_http_handler:init/3</em> handler function.
-spec upgrade(pid(), module(), any(), #http_req{}) -> closed | none().
upgrade(ListenerPid, Handler, Opts, Req) ->
	cowboy_listener:move_connection(ListenerPid, websocket, self()),
	case catch websocket_upgrade(#state{handler=Handler, opts=Opts}, Req) of
		{ok, State, Req2} -> handler_init(State, Req2);
		{'EXIT', _Reason} -> upgrade_error(Req)
	end.

%% @todo We need a function to properly parse headers according to their ABNF,
%%       instead of having ugly code like this case here.
%% @todo Upgrade is a list of products and should be parsed as such.
-spec websocket_upgrade(#state{}, #http_req{}) -> {ok, #state{}, #http_req{}}.
websocket_upgrade(State, Req) ->
	{ConnTokens, Req2}
		= cowboy_http_req:parse_header('Connection', Req),
	true = lists:member(<<"upgrade">>, ConnTokens),
	{WS, Req3} = cowboy_http_req:header('Upgrade', Req2),
	<<"websocket">> = cowboy_bstr:to_lower(WS),
	{Version, Req4} = cowboy_http_req:header(<<"Sec-Websocket-Version">>, Req3),
	websocket_upgrade(Version, State, Req4).

%% @todo Handle the Sec-Websocket-Protocol header.
%% @todo Reply a proper error, don't die, if a required header is undefined.
-spec websocket_upgrade(undefined | <<_:8>>, #state{}, #http_req{})
	-> {ok, #state{}, #http_req{}}.
%% No version given. Assuming hixie-76 draft.
%%
%% We need to wait to send a reply back before trying to read the
%% third part of the challenge key, because proxies will wait for
%% a reply before sending it. Therefore we calculate the challenge
%% key only in websocket_handshake/3.
websocket_upgrade(undefined, State, Req=#http_req{meta=Meta}) ->
	{Origin, Req2} = cowboy_http_req:header(<<"Origin">>, Req),
	{Key1, Req3} = cowboy_http_req:header(<<"Sec-Websocket-Key1">>, Req2),
	{Key2, Req4} = cowboy_http_req:header(<<"Sec-Websocket-Key2">>, Req3),
	false = lists:member(undefined, [Origin, Key1, Key2]),
	EOP = binary:compile_pattern(<< 255 >>),
	{ok, State#state{version=0, origin=Origin, challenge={Key1, Key2},
		eop=EOP}, Req4#http_req{meta=[{websocket_version, 0}|Meta]}};
%% Versions 7 and 8. Implementation follows the hybi 7 through 17 drafts.
websocket_upgrade(Version, State, Req=#http_req{meta=Meta})
		when Version =:= <<"7">>; Version =:= <<"8">>;
			Version =:= <<"13">> ->
	{Key, Req2} = cowboy_http_req:header(<<"Sec-Websocket-Key">>, Req),
	false = Key =:= undefined,
	Challenge = hybi_challenge(Key),
	IntVersion = list_to_integer(binary_to_list(Version)),
	{ok, State#state{version=IntVersion, challenge=Challenge},
		Req2#http_req{meta=[{websocket_version, IntVersion}|Meta]}}.

-spec handler_init(#state{}, #http_req{}) -> closed | none().
handler_init(State=#state{handler=Handler, opts=Opts},
		Req=#http_req{transport=Transport}) ->
	try Handler:websocket_init(Transport:name(), Req, Opts) of
		{ok, Req2, HandlerState} ->
			websocket_handshake(State, Req2, HandlerState);
		{ok, Req2, HandlerState, hibernate} ->
			websocket_handshake(State#state{hibernate=true},
				Req2, HandlerState);
		{ok, Req2, HandlerState, Timeout} ->
			websocket_handshake(State#state{timeout=Timeout},
				Req2, HandlerState);
		{ok, Req2, HandlerState, Timeout, hibernate} ->
			websocket_handshake(State#state{timeout=Timeout,
				hibernate=true}, Req2, HandlerState);
		{shutdown, Req2} ->
			upgrade_denied(Req2)
	catch Class:Reason ->
		upgrade_error(Req),
		error_logger:error_msg(
			"** Handler ~p terminating in websocket_init/3~n"
			"   for the reason ~p:~p~n** Options were ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts, Req, erlang:get_stacktrace()])
	end.

-spec upgrade_error(#http_req{}) -> closed.
upgrade_error(Req) ->
	{ok, _Req2} = cowboy_http_req:reply(400, [], [],
		Req#http_req{resp_state=waiting}),
	closed.

%% @see cowboy_http_protocol:ensure_response/1
-spec upgrade_denied(#http_req{}) -> closed.
upgrade_denied(#http_req{resp_state=done}) ->
	closed;
upgrade_denied(Req=#http_req{resp_state=waiting}) ->
	{ok, _Req2} = cowboy_http_req:reply(400, [], [], Req),
	closed;
upgrade_denied(#http_req{method='HEAD', resp_state=chunks}) ->
	closed;
upgrade_denied(#http_req{socket=Socket, transport=Transport,
		resp_state=chunks}) ->
	Transport:send(Socket, <<"0\r\n\r\n">>),
	closed.

-spec websocket_handshake(#state{}, #http_req{}, any()) -> closed | none().
websocket_handshake(State=#state{version=0, origin=Origin,
		challenge={Key1, Key2}}, Req=#http_req{socket=Socket,
		transport=Transport, raw_host=Host, port=Port,
		raw_path=Path, raw_qs=QS}, HandlerState) ->
	Location = hixie76_location(Transport:name(), Host, Port, Path, QS),
	{ok, Req2} = cowboy_http_req:upgrade_reply(
		<<"101 WebSocket Protocol Handshake">>,
		[{<<"Upgrade">>, <<"WebSocket">>},
		 {<<"Sec-Websocket-Location">>, Location},
		 {<<"Sec-Websocket-Origin">>, Origin}],
		Req#http_req{resp_state=waiting}),
	%% We replied with a proper response. Proxies should be happy enough,
	%% we can now read the 8 last bytes of the challenge keys and send
	%% the challenge response directly to the socket.
	case cowboy_http_req:body(8, Req2) of
		{ok, Key3, Req3} ->
			Challenge = hixie76_challenge(Key1, Key2, Key3),
			Transport:send(Socket, Challenge),
			handler_before_loop(State#state{messages=Transport:messages()},
				Req3, HandlerState, <<>>);
		_Any ->
			closed %% If an error happened reading the body, stop there.
	end;
websocket_handshake(State=#state{challenge=Challenge},
		Req=#http_req{transport=Transport}, HandlerState) ->
	{ok, Req2} = cowboy_http_req:upgrade_reply(
		101,
		[{<<"Upgrade">>, <<"websocket">>},
		 {<<"Sec-Websocket-Accept">>, Challenge}],
		Req#http_req{resp_state=waiting}),
	handler_before_loop(State#state{messages=Transport:messages()},
		Req2, HandlerState, <<>>).

-spec handler_before_loop(#state{}, #http_req{}, any(), binary()) -> closed | none().
handler_before_loop(State=#state{hibernate=true},
		Req=#http_req{socket=Socket, transport=Transport},
		HandlerState, SoFar) ->
	Transport:setopts(Socket, [{active, once}]),
	State2 = handler_loop_timeout(State),
	erlang:hibernate(?MODULE, handler_loop, [State2#state{hibernate=false},
		Req, HandlerState, SoFar]);
handler_before_loop(State, Req=#http_req{socket=Socket, transport=Transport},
		HandlerState, SoFar) ->
	Transport:setopts(Socket, [{active, once}]),
	State2 = handler_loop_timeout(State),
	handler_loop(State2, Req, HandlerState, SoFar).

-spec handler_loop_timeout(#state{}) -> #state{}.
handler_loop_timeout(State=#state{timeout=infinity}) ->
	State#state{timeout_ref=undefined};
handler_loop_timeout(State=#state{timeout=Timeout, timeout_ref=PrevRef}) ->
	_ = case PrevRef of undefined -> ignore; PrevRef ->
		erlang:cancel_timer(PrevRef) end,
	TRef = make_ref(),
	erlang:send_after(Timeout, self(), {?MODULE, timeout, TRef}),
	State#state{timeout_ref=TRef}.

%% @private
-spec handler_loop(#state{}, #http_req{}, any(), binary()) -> closed | none().
handler_loop(State=#state{messages={OK, Closed, Error}, timeout_ref=TRef},
		Req=#http_req{socket=Socket}, HandlerState, SoFar) ->
	receive
		{OK, Socket, Data} ->
			websocket_data(State, Req, HandlerState,
				<< SoFar/binary, Data/binary >>);
		{Closed, Socket} ->
			handler_terminate(State, Req, HandlerState, {error, closed});
		{Error, Socket, Reason} ->
			handler_terminate(State, Req, HandlerState, {error, Reason});
		{?MODULE, timeout, TRef} ->
			websocket_close(State, Req, HandlerState, {normal, timeout});
		{?MODULE, timeout, OlderTRef} when is_reference(OlderTRef) ->
			handler_loop(State, Req, HandlerState, SoFar);
		Message ->
			handler_call(State, Req, HandlerState,
				SoFar, websocket_info, Message, fun handler_before_loop/4)
	end.

-spec websocket_data(#state{}, #http_req{}, any(), binary()) -> closed | none().
%% No more data.
websocket_data(State, Req, HandlerState, <<>>) ->
	handler_before_loop(State, Req, HandlerState, <<>>);
%% hixie-76 close frame.
websocket_data(State=#state{version=0}, Req, HandlerState,
		<< 255, 0, _Rest/binary >>) ->
	websocket_close(State, Req, HandlerState, {normal, closed});
%% hixie-76 data frame. We only support the frame type 0, same as the specs.
websocket_data(State=#state{version=0, eop=EOP}, Req, HandlerState,
		Data = << 0, _/binary >>) ->
	case binary:match(Data, EOP) of
		{Pos, 1} ->
			Pos2 = Pos - 1,
			<< 0, Payload:Pos2/binary, 255, Rest/bits >> = Data,
			handler_call(State, Req, HandlerState,
				Rest, websocket_handle, {text, Payload}, fun websocket_data/4);
		nomatch ->
			%% @todo We probably should allow limiting frame length.
			handler_before_loop(State, Req, HandlerState, Data)
	end;
%% incomplete hybi data frame.
websocket_data(State=#state{version=Version}, Req, HandlerState, Data)
		when Version =/= 0, byte_size(Data) =:= 1 ->
	handler_before_loop(State, Req, HandlerState, Data);
%% hybi data frame.
%% @todo Handle Fin.
websocket_data(State=#state{version=Version}, Req, HandlerState, Data)
		when Version =/= 0 ->
	<< 1:1, 0:3, Opcode:4, Mask:1, PayloadLen:7, Rest/bits >> = Data,
	{PayloadLen2, Rest2} = case {PayloadLen, Rest} of
		{126, << L:16, R/bits >>}  -> {L, R};
		{126, Rest} -> {undefined, Rest};
		{127, << 0:1, L:63, R/bits >>} -> {L, R};
		{127, Rest} -> {undefined, Rest};
		{PayloadLen, Rest} -> {PayloadLen, Rest}
	end,
	case {Mask, PayloadLen2} of
		{0, 0} ->
			websocket_dispatch(State, Req, HandlerState, Rest2, Opcode, <<>>);
		{1, N} when N + 4 > byte_size(Rest2); N =:= undefined ->
			%% @todo We probably should allow limiting frame length.
			handler_before_loop(State, Req, HandlerState, Data);
		{1, _N} ->
			<< MaskKey:32, Payload:PayloadLen2/binary, Rest3/bits >> = Rest2,
			websocket_unmask(State, Req, HandlerState, Rest3,
				Opcode, Payload, MaskKey)
	end;
%% Something was wrong with the frame. Close the connection.
websocket_data(State, Req, HandlerState, _Bad) ->
	websocket_close(State, Req, HandlerState, {error, badframe}).

%% hybi unmasking.
-spec websocket_unmask(#state{}, #http_req{}, any(), binary(),
	opcode(), binary(), mask_key()) -> closed | none().
websocket_unmask(State, Req, HandlerState, RemainingData,
		Opcode, Payload, MaskKey) ->
	websocket_unmask(State, Req, HandlerState, RemainingData,
		Opcode, Payload, MaskKey, <<>>).

-spec websocket_unmask(#state{}, #http_req{}, any(), binary(),
	opcode(), binary(), mask_key(), binary()) -> closed | none().
websocket_unmask(State, Req, HandlerState, RemainingData,
		Opcode, << O:32, Rest/bits >>, MaskKey, Acc) ->
	T = O bxor MaskKey,
	websocket_unmask(State, Req, HandlerState, RemainingData,
		Opcode, Rest, MaskKey, << Acc/binary, T:32 >>);
websocket_unmask(State, Req, HandlerState, RemainingData,
		Opcode, << O:24 >>, MaskKey, Acc) ->
	<< MaskKey2:24, _:8 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	websocket_dispatch(State, Req, HandlerState, RemainingData,
		Opcode, << Acc/binary, T:24 >>);
websocket_unmask(State, Req, HandlerState, RemainingData,
		Opcode, << O:16 >>, MaskKey, Acc) ->
	<< MaskKey2:16, _:16 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	websocket_dispatch(State, Req, HandlerState, RemainingData,
		Opcode, << Acc/binary, T:16 >>);
websocket_unmask(State, Req, HandlerState, RemainingData,
		Opcode, << O:8 >>, MaskKey, Acc) ->
	<< MaskKey2:8, _:24 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	websocket_dispatch(State, Req, HandlerState, RemainingData,
		Opcode, << Acc/binary, T:8 >>);
websocket_unmask(State, Req, HandlerState, RemainingData,
		Opcode, <<>>, _MaskKey, Acc) ->
	websocket_dispatch(State, Req, HandlerState, RemainingData,
		Opcode, Acc).

%% hybi dispatching.
-spec websocket_dispatch(#state{}, #http_req{}, any(), binary(),
	opcode(), binary()) -> closed | none().
%% @todo Fragmentation.
%~ websocket_dispatch(State, Req, HandlerState, RemainingData, 0, Payload) ->
%% Text frame.
websocket_dispatch(State, Req, HandlerState, RemainingData, 1, Payload) ->
	handler_call(State, Req, HandlerState, RemainingData,
		websocket_handle, {text, Payload}, fun websocket_data/4);
%% Binary frame.
websocket_dispatch(State, Req, HandlerState, RemainingData, 2, Payload) ->
	handler_call(State, Req, HandlerState, RemainingData,
		websocket_handle, {binary, Payload}, fun websocket_data/4);
%% Close control frame.
%% @todo Handle the optional Payload.
websocket_dispatch(State, Req, HandlerState, _RemainingData, 8, _Payload) ->
	websocket_close(State, Req, HandlerState, {normal, closed});
%% Ping control frame. Send a pong back and forward the ping to the handler.
websocket_dispatch(State, Req=#http_req{socket=Socket, transport=Transport},
		HandlerState, RemainingData, 9, Payload) ->
	Len = hybi_payload_length(byte_size(Payload)),
	Transport:send(Socket, << 1:1, 0:3, 10:4, 0:1, Len/bits, Payload/binary >>),
	handler_call(State, Req, HandlerState, RemainingData,
		websocket_handle, {ping, Payload}, fun websocket_data/4);
%% Pong control frame.
websocket_dispatch(State, Req, HandlerState, RemainingData, 10, Payload) ->
	handler_call(State, Req, HandlerState, RemainingData,
		websocket_handle, {pong, Payload}, fun websocket_data/4).

-spec handler_call(#state{}, #http_req{}, any(), binary(),
	atom(), any(), fun()) -> closed | none().
handler_call(State=#state{handler=Handler, opts=Opts}, Req, HandlerState,
		RemainingData, Callback, Message, NextState) ->
	try Handler:Callback(Message, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			NextState(State, Req2, HandlerState2, RemainingData);
		{ok, Req2, HandlerState2, hibernate} ->
			NextState(State#state{hibernate=true},
				Req2, HandlerState2, RemainingData);
		{reply, Payload, Req2, HandlerState2} ->
			websocket_send(Payload, State, Req2),
			NextState(State, Req2, HandlerState2, RemainingData);
		{reply, Payload, Req2, HandlerState2, hibernate} ->
			websocket_send(Payload, State, Req2),
			NextState(State#state{hibernate=true},
				Req2, HandlerState2, RemainingData);
		{shutdown, Req2, HandlerState2} ->
			websocket_close(State, Req2, HandlerState2, {normal, shutdown})
	catch Class:Reason ->
		error_logger:error_msg(
			"** Handler ~p terminating in ~p/3~n"
			"   for the reason ~p:~p~n** Message was ~p~n"
			"** Options were ~p~n** Handler state was ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Callback, Class, Reason, Message, Opts,
			 HandlerState, Req, erlang:get_stacktrace()]),
		websocket_close(State, Req, HandlerState, {error, handler})
	end.

-spec websocket_send(binary(), #state{}, #http_req{}) -> closed | ignore.
%% hixie-76 text frame.
websocket_send({text, Payload}, #state{version=0},
		#http_req{socket=Socket, transport=Transport}) ->
	Transport:send(Socket, [0, Payload, 255]);
%% Ignore all unknown frame types for compatibility with hixie 76.
websocket_send(_Any, #state{version=0}, _Req) ->
	ignore;
websocket_send({Type, Payload}, _State,
		#http_req{socket=Socket, transport=Transport}) ->
	Opcode = case Type of
		text -> 1;
		binary -> 2;
		ping -> 9;
		pong -> 10
	end,
	Len = hybi_payload_length(iolist_size(Payload)),
	Transport:send(Socket, [<< 1:1, 0:3, Opcode:4, 0:1, Len/bits >>,
		Payload]).

-spec websocket_close(#state{}, #http_req{}, any(), {atom(), atom()}) -> closed.
websocket_close(State=#state{version=0}, Req=#http_req{socket=Socket,
		transport=Transport}, HandlerState, Reason) ->
	Transport:send(Socket, << 255, 0 >>),
	handler_terminate(State, Req, HandlerState, Reason);
%% @todo Send a Payload? Using Reason is usually good but we're quite careless.
websocket_close(State, Req=#http_req{socket=Socket,
		transport=Transport}, HandlerState, Reason) ->
	Transport:send(Socket, << 1:1, 0:3, 8:4, 0:8 >>),
	handler_terminate(State, Req, HandlerState, Reason).

-spec handler_terminate(#state{}, #http_req{},
	any(), atom() | {atom(), atom()}) -> closed.
handler_terminate(#state{handler=Handler, opts=Opts},
		Req, HandlerState, TerminateReason) ->
	try
		Handler:websocket_terminate(TerminateReason, Req, HandlerState)
	catch Class:Reason ->
		error_logger:error_msg(
			"** Handler ~p terminating in websocket_terminate/3~n"
			"   for the reason ~p:~p~n** Initial reason was ~p~n"
			"** Options were ~p~n** Handler state was ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, TerminateReason, Opts,
			 HandlerState, Req, erlang:get_stacktrace()])
	end,
	closed.

%% hixie-76 specific.

-spec hixie76_challenge(binary(), binary(), binary()) -> binary().
hixie76_challenge(Key1, Key2, Key3) ->
	IntKey1 = hixie76_key_to_integer(Key1),
	IntKey2 = hixie76_key_to_integer(Key2),
	erlang:md5(<< IntKey1:32, IntKey2:32, Key3/binary >>).

-spec hixie76_key_to_integer(binary()) -> integer().
hixie76_key_to_integer(Key) ->
	Number = list_to_integer([C || << C >> <= Key, C >= $0, C =< $9]),
	Spaces = length([C || << C >> <= Key, C =:= 32]),
	Number div Spaces.

-spec hixie76_location(atom(), binary(), inet:ip_port(), binary(), binary())
	-> binary().
hixie76_location(Protocol, Host, Port, Path, <<>>) ->
    << (hixie76_location_protocol(Protocol))/binary, "://", Host/binary,
       (hixie76_location_port(Protocol, Port))/binary, Path/binary>>;
hixie76_location(Protocol, Host, Port, Path, QS) ->
    << (hixie76_location_protocol(Protocol))/binary, "://", Host/binary,
       (hixie76_location_port(Protocol, Port))/binary, Path/binary, "?", QS/binary >>.

-spec hixie76_location_protocol(atom()) -> binary().
hixie76_location_protocol(ssl) -> <<"wss">>;
hixie76_location_protocol(_)   -> <<"ws">>.

%% @todo We should add a secure/0 function to transports
%% instead of relying on their name.
-spec hixie76_location_port(atom(), inet:ip_port()) -> binary().
hixie76_location_port(ssl, 443) ->
	<<>>;
hixie76_location_port(tcp, 80) ->
	<<>>;
hixie76_location_port(_, Port) ->
	<<":", (list_to_binary(integer_to_list(Port)))/binary>>.

%% hybi specific.

-spec hybi_challenge(binary()) -> binary().
hybi_challenge(Key) ->
	Bin = << Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>,
	base64:encode(crypto:sha(Bin)).

-spec hybi_payload_length(0..16#7fffffffffffffff)
	-> << _:7 >> | << _:23 >> | << _:71 >>.
hybi_payload_length(N) ->
	case N of
		N when N =< 125 -> << N:7 >>;
		N when N =< 16#ffff -> << 126:7, N:16 >>;
		N when N =< 16#7fffffffffffffff -> << 127:7, N:64 >>
	end.

%% Tests.

-ifdef(TEST).

hixie76_location_test() ->
	?assertEqual(<<"ws://localhost/path">>,
		hixie76_location(tcp, <<"localhost">>, 80, <<"/path">>, <<>>)),
	?assertEqual(<<"ws://localhost:443/path">>,
		hixie76_location(tcp, <<"localhost">>, 443, <<"/path">>, <<>>)),
	?assertEqual(<<"ws://localhost:8080/path">>,
		hixie76_location(tcp, <<"localhost">>, 8080, <<"/path">>, <<>>)),
	?assertEqual(<<"ws://localhost:8080/path?dummy=2785">>,
		hixie76_location(tcp, <<"localhost">>, 8080, <<"/path">>, <<"dummy=2785">>)),
	?assertEqual(<<"wss://localhost/path">>,
		hixie76_location(ssl, <<"localhost">>, 443, <<"/path">>, <<>>)),
	?assertEqual(<<"wss://localhost:8443/path">>,
		hixie76_location(ssl, <<"localhost">>, 8443, <<"/path">>, <<>>)),
	?assertEqual(<<"wss://localhost:8443/path?dummy=2785">>,
		hixie76_location(ssl, <<"localhost">>, 8443, <<"/path">>, <<"dummy=2785">>)),
	ok.

-endif.
