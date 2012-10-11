%% Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>
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
-module(cowboy_websocket).

%% API.
-export([upgrade/4]).

%% Internal.
-export([handler_loop/4]).

-type opcode() :: 0 | 1 | 2 | 8 | 9 | 10.
-type mask_key() :: 0..16#ffffffff.

%% The websocket_data/4 function may be called multiple times for a message.
%% The websocket_dispatch/4 function is only called once for each message.
-type frag_state() ::
	undefined | %% no fragmentation has been seen.
	{nofin, opcode()} | %% first fragment has been seen.
	{nofin, opcode(), binary()} | %% first fragment has been unmasked.
	{fin, opcode(), binary()}. %% last fragment has been seen.

-record(state, {
	socket = undefined :: inet:socket(),
	transport = undefined :: module(),
	version :: 0 | 7 | 8 | 13,
	handler :: module(),
	opts :: any(),
	challenge = undefined :: undefined | binary() | {binary(), binary()},
	timeout = infinity :: timeout(),
	timeout_ref = undefined :: undefined | reference(),
	messages = undefined :: undefined | {atom(), atom(), atom()},
	hibernate = false :: boolean(),
	eop :: undefined | tuple(), %% hixie-76 specific.
	origin = undefined :: undefined | binary(), %% hixie-76 specific.
	frag_state = undefined :: frag_state()
}).

%% @doc Upgrade a HTTP request to the WebSocket protocol.
%%
%% You do not need to call this function manually. To upgrade to the WebSocket
%% protocol, you simply need to return <em>{upgrade, protocol, {@module}}</em>
%% in your <em>cowboy_http_handler:init/3</em> handler function.
-spec upgrade(pid(), module(), any(), cowboy_req:req()) -> closed.
upgrade(ListenerPid, Handler, Opts, Req) ->
	ranch_listener:remove_connection(ListenerPid),
	{ok, Transport, Socket} = cowboy_req:transport(Req),
	State = #state{socket=Socket, transport=Transport,
		handler=Handler, opts=Opts},
	case catch websocket_upgrade(State, Req) of
		{ok, State2, Req2} -> handler_init(State2, Req2);
		{'EXIT', _Reason} -> upgrade_error(Req)
	end.

-spec websocket_upgrade(#state{}, Req)
	-> {ok, #state{}, Req} when Req::cowboy_req:req().
websocket_upgrade(State, Req) ->
	{ok, ConnTokens, Req2}
		= cowboy_req:parse_header(<<"connection">>, Req),
	true = lists:member(<<"upgrade">>, ConnTokens),
	%% @todo Should probably send a 426 if the Upgrade header is missing.
	{ok, [<<"websocket">>], Req3}
		= cowboy_req:parse_header(<<"upgrade">>, Req2),
	{Version, Req4} = cowboy_req:header(<<"sec-websocket-version">>, Req3),
	websocket_upgrade(Version, State, Req4).

%% @todo Handle the Sec-Websocket-Protocol header.
%% @todo Reply a proper error, don't die, if a required header is undefined.
-spec websocket_upgrade(undefined | <<_:8>>, #state{}, Req)
	-> {ok, #state{}, Req} when Req::cowboy_req:req().
%% No version given. Assuming hixie-76 draft.
%%
%% We need to wait to send a reply back before trying to read the
%% third part of the challenge key, because proxies will wait for
%% a reply before sending it. Therefore we calculate the challenge
%% key only in websocket_handshake/3.
websocket_upgrade(undefined, State, Req) ->
	{Origin, Req2} = cowboy_req:header(<<"origin">>, Req),
	{Key1, Req3} = cowboy_req:header(<<"sec-websocket-key1">>, Req2),
	{Key2, Req4} = cowboy_req:header(<<"sec-websocket-key2">>, Req3),
	false = lists:member(undefined, [Origin, Key1, Key2]),
	EOP = binary:compile_pattern(<< 255 >>),
	{ok, State#state{version=0, origin=Origin, challenge={Key1, Key2},
		eop=EOP}, cowboy_req:set_meta(websocket_version, 0, Req4)};
%% Versions 7 and 8. Implementation follows the hybi 7 through 17 drafts.
websocket_upgrade(Version, State, Req)
		when Version =:= <<"7">>; Version =:= <<"8">>;
			Version =:= <<"13">> ->
	{Key, Req2} = cowboy_req:header(<<"sec-websocket-key">>, Req),
	false = Key =:= undefined,
	Challenge = hybi_challenge(Key),
	IntVersion = list_to_integer(binary_to_list(Version)),
	{ok, State#state{version=IntVersion, challenge=Challenge},
		cowboy_req:set_meta(websocket_version, IntVersion, Req2)}.

-spec handler_init(#state{}, cowboy_req:req()) -> closed.
handler_init(State=#state{transport=Transport, handler=Handler, opts=Opts},
		Req) ->
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
			cowboy_req:ensure_response(Req2, 400),
			closed
	catch Class:Reason ->
		upgrade_error(Req),
		PLReq = cowboy_req:to_list(Req),
		error_logger:error_msg(
			"** Handler ~p terminating in websocket_init/3~n"
			"   for the reason ~p:~p~n** Options were ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts, PLReq, erlang:get_stacktrace()])
	end.

-spec upgrade_error(cowboy_req:req()) -> closed.
upgrade_error(Req) ->
	receive
		{cowboy_req, resp_sent} -> closed
	after 0 ->
		_ = cowboy_req:reply(400, [], [], Req),
		closed
	end.

-spec websocket_handshake(#state{}, cowboy_req:req(), any()) -> closed.
websocket_handshake(State=#state{socket=Socket, transport=Transport,
		version=0, origin=Origin, challenge={Key1, Key2}},
		Req, HandlerState) ->
	{<< "http", Location/binary >>, Req1} = cowboy_req:url(Req),
	{ok, Req2} = cowboy_req:upgrade_reply(
		<<"101 WebSocket Protocol Handshake">>,
		[{<<"Upgrade">>, <<"WebSocket">>},
		 {<<"Sec-Websocket-Location">>, << "ws", Location/binary >>},
		 {<<"Sec-Websocket-Origin">>, Origin}],
		Req1),
	%% Flush the resp_sent message before moving on.
	receive {cowboy_req, resp_sent} -> ok after 0 -> ok end,
	%% We replied with a proper response. Proxies should be happy enough,
	%% we can now read the 8 last bytes of the challenge keys and send
	%% the challenge response directly to the socket.
	%%
	%% We use a trick here to read exactly 8 bytes of the body regardless
	%% of what's in the buffer.
	{ok, Req3} = cowboy_req:init_stream(
		fun cowboy_http:te_identity/2, {0, 8},
		fun cowboy_http:ce_identity/1, Req2),
	case cowboy_req:body(Req3) of
		{ok, Key3, Req4} ->
			Challenge = hixie76_challenge(Key1, Key2, Key3),
			Transport:send(Socket, Challenge),
			handler_before_loop(State#state{messages=Transport:messages()},
				Req4, HandlerState, <<>>);
		_Any ->
			closed %% If an error happened reading the body, stop there.
	end;
websocket_handshake(State=#state{transport=Transport, challenge=Challenge},
		Req, HandlerState) ->
	{ok, Req2} = cowboy_req:upgrade_reply(
		101,
		[{<<"Upgrade">>, <<"websocket">>},
		 {<<"Sec-Websocket-Accept">>, Challenge}],
		Req),
	%% Flush the resp_sent message before moving on.
	receive {cowboy_req, resp_sent} -> ok after 0 -> ok end,
	handler_before_loop(State#state{messages=Transport:messages()},
		Req2, HandlerState, <<>>).

-spec handler_before_loop(#state{}, cowboy_req:req(), any(), binary()) -> closed.
handler_before_loop(State=#state{
			socket=Socket, transport=Transport, hibernate=true},
		Req, HandlerState, SoFar) ->
	Transport:setopts(Socket, [{active, once}]),
	State2 = handler_loop_timeout(State),
	catch erlang:hibernate(?MODULE, handler_loop,
		[State2#state{hibernate=false}, Req, HandlerState, SoFar]),
	closed;
handler_before_loop(State=#state{socket=Socket, transport=Transport},
		Req, HandlerState, SoFar) ->
	Transport:setopts(Socket, [{active, once}]),
	State2 = handler_loop_timeout(State),
	handler_loop(State2, Req, HandlerState, SoFar).

-spec handler_loop_timeout(#state{}) -> #state{}.
handler_loop_timeout(State=#state{timeout=infinity}) ->
	State#state{timeout_ref=undefined};
handler_loop_timeout(State=#state{timeout=Timeout, timeout_ref=PrevRef}) ->
	_ = case PrevRef of undefined -> ignore; PrevRef ->
		erlang:cancel_timer(PrevRef) end,
	TRef = erlang:start_timer(Timeout, self(), ?MODULE),
	State#state{timeout_ref=TRef}.

%% @private
-spec handler_loop(#state{}, cowboy_req:req(), any(), binary()) -> closed.
handler_loop(State=#state{
			socket=Socket, messages={OK, Closed, Error}, timeout_ref=TRef},
		Req, HandlerState, SoFar) ->
	receive
		{OK, Socket, Data} ->
			websocket_data(State, Req, HandlerState,
				<< SoFar/binary, Data/binary >>);
		{Closed, Socket} ->
			handler_terminate(State, Req, HandlerState, {error, closed});
		{Error, Socket, Reason} ->
			handler_terminate(State, Req, HandlerState, {error, Reason});
		{timeout, TRef, ?MODULE} ->
			websocket_close(State, Req, HandlerState, {normal, timeout});
		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
			handler_loop(State, Req, HandlerState, SoFar);
		Message ->
			handler_call(State, Req, HandlerState,
				SoFar, websocket_info, Message, fun handler_before_loop/4)
	end.

-spec websocket_data(#state{}, cowboy_req:req(), any(), binary()) -> closed.
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
%% 7 bit payload length prefix exists
websocket_data(State, Req, HandlerState,
		<< Fin:1, Rsv:3, Opcode:4, Mask:1, PayloadLen:7, Rest/bits >>
		= Data) when PayloadLen < 126 ->
	websocket_data(State, Req, HandlerState,
		Fin, Rsv, Opcode, Mask, PayloadLen, Rest, Data);
%% 7+16 bits payload length prefix exists
websocket_data(State, Req, HandlerState,
		<< Fin:1, Rsv:3, Opcode:4, Mask:1, 126:7, PayloadLen:16, Rest/bits >>
		= Data) when PayloadLen > 125 ->
	websocket_data(State, Req, HandlerState,
		Fin, Rsv, Opcode, Mask, PayloadLen, Rest, Data);
%% 7+16 bits payload length prefix missing
websocket_data(State, Req, HandlerState,
		<< _Fin:1, _Rsv:3, _Opcode:4, _Mask:1, 126:7, Rest/bits >>
		= Data) when byte_size(Rest) < 2 ->
	handler_before_loop(State, Req, HandlerState, Data);
%% 7+64 bits payload length prefix exists
websocket_data(State, Req, HandlerState,
		<< Fin:1, Rsv:3, Opcode:4, Mask:1, 127:7, 0:1, PayloadLen:63,
		   Rest/bits >> = Data) when PayloadLen > 16#FFFF ->
	websocket_data(State, Req, HandlerState,
		Fin, Rsv, Opcode, Mask, PayloadLen, Rest, Data);
%% 7+64 bits payload length prefix missing
websocket_data(State, Req, HandlerState,
		<< _Fin:1, _Rsv:3, _Opcode:4, _Mask:1, 127:7, Rest/bits >>
		= Data) when byte_size(Rest) < 8 ->
	handler_before_loop(State, Req, HandlerState, Data);
%% invalid payload length prefix.
websocket_data(State, Req, HandlerState, _Data) ->
	websocket_close(State, Req, HandlerState, {error, badframe}).

-spec websocket_data(#state{}, cowboy_req:req(), any(), non_neg_integer(),
		non_neg_integer(), non_neg_integer(), non_neg_integer(),
		non_neg_integer(), binary(), binary()) -> closed.
%% A fragmented message MUST start a non-zero opcode.
websocket_data(State=#state{frag_state=undefined}, Req, HandlerState,
		_Fin=0, _Rsv=0, _Opcode=0, _Mask, _PayloadLen, _Rest, _Buffer) ->
	websocket_close(State, Req, HandlerState, {error, badframe});
%% A control message MUST NOT be fragmented.
websocket_data(State, Req, HandlerState, _Fin=0, _Rsv=0, Opcode, _Mask,
		_PayloadLen, _Rest, _Buffer) when Opcode >= 8 ->
	websocket_close(State, Req, HandlerState, {error, badframe});
%% The opcode is only included in the first message fragment.
websocket_data(State=#state{frag_state=undefined}, Req, HandlerState,
		_Fin=0, _Rsv=0, Opcode, Mask, PayloadLen, Rest, Data) ->
	websocket_before_unmask(
		State#state{frag_state={nofin, Opcode}}, Req, HandlerState,
		Data, Rest, 0, Mask, PayloadLen);
%% non-control opcode when expecting control message or next fragment.
websocket_data(State=#state{frag_state={nofin, _, _}}, Req, HandlerState, _Fin,
		_Rsv=0, Opcode, _Mask, _Ln, _Rest, _Data) when Opcode > 0, Opcode < 8 ->
	websocket_close(State, Req, HandlerState, {error, badframe});
%% If the first message fragment was incomplete, retry unmasking.
websocket_data(State=#state{frag_state={nofin, Opcode}}, Req, HandlerState,
		_Fin=0, _Rsv=0, Opcode, Mask, PayloadLen, Rest, Data) ->
	websocket_before_unmask(
		State#state{frag_state={nofin, Opcode}}, Req, HandlerState,
		Data, Rest, 0, Mask, PayloadLen);
%% if the opcode is zero and the fin flag is zero, unmask and await next.
websocket_data(State=#state{frag_state={nofin, _Opcode, _Payloads}}, Req,
		HandlerState, _Fin=0, _Rsv=0, _Opcode2=0, Mask, PayloadLen, Rest,
		Data) ->
	websocket_before_unmask(
		State, Req, HandlerState, Data, Rest, 0, Mask, PayloadLen);
%% when the last fragment is seen. Update the fragmentation status.
websocket_data(State=#state{frag_state={nofin, Opcode, Payloads}}, Req,
		HandlerState, _Fin=1, _Rsv=0, _Opcode=0, Mask, PayloadLen, Rest,
		Data) ->
	websocket_before_unmask(
		State#state{frag_state={fin, Opcode, Payloads}},
		Req, HandlerState, Data, Rest, 0, Mask, PayloadLen);
%% control messages MUST NOT use 7+16 bits or 7+64 bits payload length prefixes
websocket_data(State, Req, HandlerState, _Fin, _Rsv, Opcode, _Mask, PayloadLen,
		_Rest, _Data) when Opcode >= 8, PayloadLen > 125 ->
	 websocket_close(State, Req, HandlerState, {error, badframe});
%% unfragmented message. unmask and dispatch the message.
websocket_data(State=#state{version=Version}, Req, HandlerState, _Fin=1, _Rsv=0,
		Opcode, Mask, PayloadLen, Rest, Data) when Version =/= 0 ->
	websocket_before_unmask(
			State, Req, HandlerState, Data, Rest, Opcode, Mask, PayloadLen);
%% Something was wrong with the frame. Close the connection.
websocket_data(State, Req, HandlerState, _Fin, _Rsv, _Opcode, _Mask,
		_PayloadLen, _Rest, _Data) ->
		websocket_close(State, Req, HandlerState, {error, badframe}).

%% hybi routing depending on whether unmasking is needed.
-spec websocket_before_unmask(#state{}, cowboy_req:req(), any(), binary(),
	binary(), opcode(), 0 | 1, non_neg_integer() | undefined) -> closed.
websocket_before_unmask(State, Req, HandlerState, Data,
		Rest, Opcode, Mask, PayloadLen) ->
	case {Mask, PayloadLen} of
		{0, 0} ->
			websocket_dispatch(State, Req, HandlerState, Rest, Opcode, <<>>);
		{1, N} when N + 4 > byte_size(Rest); N =:= undefined ->
			%% @todo We probably should allow limiting frame length.
			handler_before_loop(State, Req, HandlerState, Data);
		{1, _N} ->
			<< MaskKey:32, Payload:PayloadLen/binary, Rest2/bits >> = Rest,
			websocket_unmask(State, Req, HandlerState, Rest2,
				Opcode, Payload, MaskKey)
	end.

%% hybi unmasking.
-spec websocket_unmask(#state{}, cowboy_req:req(), any(), binary(),
	opcode(), binary(), mask_key()) -> closed.
websocket_unmask(State, Req, HandlerState, RemainingData,
		Opcode, Payload, MaskKey) ->
	websocket_unmask(State, Req, HandlerState, RemainingData,
		Opcode, Payload, MaskKey, <<>>).

-spec websocket_unmask(#state{}, cowboy_req:req(), any(), binary(),
	opcode(), binary(), mask_key(), binary()) -> closed.
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
-spec websocket_dispatch(#state{}, cowboy_req:req(), any(), binary(),
	opcode(), binary()) -> closed.
%% First frame of a fragmented message unmasked. Expect intermediate or last.
websocket_dispatch(State=#state{frag_state={nofin, Opcode}}, Req, HandlerState,
		RemainingData, 0, Payload) ->
	websocket_data(State#state{frag_state={nofin, Opcode, Payload}},
		Req, HandlerState, RemainingData);
%% Intermediate frame of a fragmented message unmasked. Add payload to buffer.
websocket_dispatch(State=#state{frag_state={nofin, Opcode, Payloads}}, Req,
		HandlerState, RemainingData, 0, Payload) ->
	websocket_data(State#state{frag_state={nofin, Opcode,
		<<Payloads/binary, Payload/binary>>}}, Req, HandlerState,
		RemainingData);
%% Last frame of a fragmented message unmasked. Dispatch to handler.
websocket_dispatch(State=#state{frag_state={fin, Opcode, Payloads}}, Req,
		HandlerState, RemainingData, 0, Payload) ->
	websocket_dispatch(State#state{frag_state=undefined}, Req, HandlerState,
		RemainingData, Opcode, <<Payloads/binary, Payload/binary>>);
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
websocket_dispatch(State=#state{socket=Socket, transport=Transport},
		Req, HandlerState, RemainingData, 9, Payload) ->
	Len = hybi_payload_length(byte_size(Payload)),
	Transport:send(Socket, << 1:1, 0:3, 10:4, 0:1, Len/bits, Payload/binary >>),
	handler_call(State, Req, HandlerState, RemainingData,
		websocket_handle, {ping, Payload}, fun websocket_data/4);
%% Pong control frame.
websocket_dispatch(State, Req, HandlerState, RemainingData, 10, Payload) ->
	handler_call(State, Req, HandlerState, RemainingData,
		websocket_handle, {pong, Payload}, fun websocket_data/4).

-spec handler_call(#state{}, cowboy_req:req(), any(), binary(),
	atom(), any(), fun()) -> closed.
handler_call(State=#state{handler=Handler, opts=Opts}, Req, HandlerState,
		RemainingData, Callback, Message, NextState) ->
	try Handler:Callback(Message, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			NextState(State, Req2, HandlerState2, RemainingData);
		{ok, Req2, HandlerState2, hibernate} ->
			NextState(State#state{hibernate=true},
				Req2, HandlerState2, RemainingData);
		{reply, Payload, Req2, HandlerState2}
				when is_tuple(Payload) ->
			ok = websocket_send(Payload, State),
			NextState(State, Req2, HandlerState2, RemainingData);
		{reply, Payload, Req2, HandlerState2, hibernate}
				when is_tuple(Payload) ->
			ok = websocket_send(Payload, State),
			NextState(State#state{hibernate=true},
				Req2, HandlerState2, RemainingData);
		{reply, Payload, Req2, HandlerState2}
				when is_list(Payload) ->
			ok = websocket_send_many(Payload, State),
			NextState(State, Req2, HandlerState2, RemainingData);
		{reply, Payload, Req2, HandlerState2, hibernate}
				when is_list(Payload) ->
			ok = websocket_send_many(Payload, State),
			NextState(State#state{hibernate=true},
				Req2, HandlerState2, RemainingData);
		{shutdown, Req2, HandlerState2} ->
			websocket_close(State, Req2, HandlerState2, {normal, shutdown})
	catch Class:Reason ->
		PLReq = cowboy_req:to_list(Req),
		error_logger:error_msg(
			"** Handler ~p terminating in ~p/3~n"
			"   for the reason ~p:~p~n** Message was ~p~n"
			"** Options were ~p~n** Handler state was ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Callback, Class, Reason, Message, Opts,
			 HandlerState, PLReq, erlang:get_stacktrace()]),
		websocket_close(State, Req, HandlerState, {error, handler})
	end.

-spec websocket_send({text | binary | ping | pong, binary()}, #state{})
	-> ok | {error, atom()}.
%% hixie-76 text frame.
websocket_send({text, Payload}, #state{
		socket=Socket, transport=Transport, version=0}) ->
	Transport:send(Socket, [0, Payload, 255]);
%% Ignore all unknown frame types for compatibility with hixie 76.
websocket_send(_Any, #state{version=0}) ->
	ok;
websocket_send({Type, Payload}, #state{socket=Socket, transport=Transport}) ->
	Opcode = case Type of
		text -> 1;
		binary -> 2;
		ping -> 9;
		pong -> 10
	end,
	Len = hybi_payload_length(iolist_size(Payload)),
	Transport:send(Socket, [<< 1:1, 0:3, Opcode:4, 0:1, Len/bits >>,
		Payload]).

-spec websocket_send_many([{text | binary | ping | pong, binary()}], #state{})
	-> ok | {error, atom()}.
websocket_send_many([], _) ->
	ok;
websocket_send_many([Frame|Tail], State) ->
	ok = websocket_send(Frame, State),
	websocket_send_many(Tail, State).

-spec websocket_close(#state{}, cowboy_req:req(), any(), {atom(), atom()})
	-> closed.
websocket_close(State=#state{socket=Socket, transport=Transport, version=0},
		Req, HandlerState, Reason) ->
	Transport:send(Socket, << 255, 0 >>),
	handler_terminate(State, Req, HandlerState, Reason);
%% @todo Send a Payload? Using Reason is usually good but we're quite careless.
websocket_close(State=#state{socket=Socket, transport=Transport},
		Req, HandlerState, Reason) ->
	Transport:send(Socket, << 1:1, 0:3, 8:4, 0:8 >>),
	handler_terminate(State, Req, HandlerState, Reason).

-spec handler_terminate(#state{}, cowboy_req:req(),
	any(), atom() | {atom(), atom()}) -> closed.
handler_terminate(#state{handler=Handler, opts=Opts},
		Req, HandlerState, TerminateReason) ->
	try
		Handler:websocket_terminate(TerminateReason, Req, HandlerState)
	catch Class:Reason ->
		PLReq = cowboy_req:to_list(Req),
		error_logger:error_msg(
			"** Handler ~p terminating in websocket_terminate/3~n"
			"   for the reason ~p:~p~n** Initial reason was ~p~n"
			"** Options were ~p~n** Handler state was ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, TerminateReason, Opts,
			 HandlerState, PLReq, erlang:get_stacktrace()])
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
