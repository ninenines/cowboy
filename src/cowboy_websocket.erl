%% Copyright (c) 2011-2014, Loïc Hoguin <essen@ninenines.eu>
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

%% Cowboy supports versions 7 through 17 of the Websocket drafts.
%% It also supports RFC6455, the proposed standard for Websocket.
-module(cowboy_websocket).
-behaviour(cowboy_sub_protocol).

-export([upgrade/6]).
-export([handler_loop/4]).

-type terminate_reason() :: normal | stop | timeout
	| remote | {remote, cow_ws:close_code(), binary()}
	| {error, badencoding | badframe | closed | atom()}
	| {crash, error | exit | throw, any()}.

-callback init(Req, any())
	-> {ok | module(), Req, any()}
	| {module(), Req, any(), hibernate}
	| {module(), Req, any(), timeout()}
	| {module(), Req, any(), timeout(), hibernate}
	when Req::cowboy_req:req().
-callback websocket_handle({text | binary | ping | pong, binary()}, Req, State)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {reply, cow_ws:frame() | [cow_ws:frame()], Req, State}
	| {reply, cow_ws:frame() | [cow_ws:frame()], Req, State, hibernate}
	| {stop, Req, State}
	when Req::cowboy_req:req(), State::any().
-callback websocket_info(any(), Req, State)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {reply, cow_ws:frame() | [cow_ws:frame()], Req, State}
	| {reply, cow_ws:frame() | [cow_ws:frame()], Req, State, hibernate}
	| {stop, Req, State}
	when Req::cowboy_req:req(), State::any().
%% @todo optional -callback terminate(terminate_reason(), cowboy_req:req(), state()) -> ok.

-record(state, {
	env :: cowboy_middleware:env(),
	socket = undefined :: inet:socket(),
	transport = undefined :: module(),
	handler :: module(),
	key = undefined :: undefined | binary(),
	timeout = infinity :: timeout(),
	timeout_ref = undefined :: undefined | reference(),
	messages = undefined :: undefined | {atom(), atom(), atom()},
	hibernate = false :: boolean(),
	frag_state = undefined :: cow_ws:frag_state(),
	frag_buffer = <<>> :: binary(),
	utf8_state = <<>> :: binary(),
	recv_extensions = #{} :: map(),
	send_extensions = #{} :: map()
}).

-spec upgrade(Req, Env, module(), any(), timeout(), run | hibernate)
	-> {ok, Req, Env} | {suspend, module(), atom(), [any()]}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade(Req, Env, Handler, HandlerState, Timeout, Hibernate) ->
	{_, Ref} = lists:keyfind(listener, 1, Env),
	ranch:remove_connection(Ref),
	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
	State = #state{env=Env, socket=Socket, transport=Transport, handler=Handler,
		timeout=Timeout, hibernate=Hibernate =:= hibernate},
	try websocket_upgrade(State, Req) of
		{ok, State2, Req2} ->
			websocket_handshake(State2, Req2, HandlerState)
	catch _:_ ->
		receive
			{cowboy_req, resp_sent} -> ok
		after 0 ->
			_ = cowboy_req:reply(400, Req),
			exit(normal)
		end
	end.

-spec websocket_upgrade(#state{}, Req)
	-> {ok, #state{}, Req} when Req::cowboy_req:req().
websocket_upgrade(State, Req) ->
	ConnTokens = cowboy_req:parse_header(<<"connection">>, Req),
	true = lists:member(<<"upgrade">>, ConnTokens),
	%% @todo Should probably send a 426 if the Upgrade header is missing.
	[<<"websocket">>] = cowboy_req:parse_header(<<"upgrade">>, Req),
	Version = cowboy_req:header(<<"sec-websocket-version">>, Req),
	IntVersion = list_to_integer(binary_to_list(Version)),
	true = (IntVersion =:= 7) orelse (IntVersion =:= 8)
		orelse (IntVersion =:= 13),
	Key = cowboy_req:header(<<"sec-websocket-key">>, Req),
	false = Key =:= undefined,
	websocket_extensions(State#state{key=Key},
		cowboy_req:set_meta(websocket_version, IntVersion, Req)).

-spec websocket_extensions(#state{}, Req)
	-> {ok, #state{}, Req} when Req::cowboy_req:req().
websocket_extensions(State, Req) ->
	case cowboy_req:parse_header(<<"sec-websocket-extensions">>, Req) of
		undefined ->
			{ok, State, cowboy_req:set_meta(websocket_compress, false, Req)};
		Extensions ->
			[Compress] = cowboy_req:get([resp_compress], Req),
			case lists:keyfind(<<"x-webkit-deflate-frame">>, 1, Extensions) of
				{<<"x-webkit-deflate-frame">>, []} when Compress =:= true ->
					Inflate = zlib:open(),
					Deflate = zlib:open(),
					% Since we are negotiating an unconstrained deflate-frame
					% then we must be willing to accept frames using the
					% maximum window size which is 2^15. The negative value
					% indicates that zlib headers are not used.
					ok = zlib:inflateInit(Inflate, -15),
					% Initialize the deflater with a window size of 2^15 bits and disable
					% the zlib headers.
					ok = zlib:deflateInit(Deflate, best_compression, deflated, -15, 8, default),
					{ok, State#state{
						recv_extensions = #{deflate_frame => Inflate},
						send_extensions = #{deflate_frame => Deflate}
					}, cowboy_req:set_meta(websocket_compress, true, Req)};
				_ ->
					{ok, State, cowboy_req:set_meta(websocket_compress, false, Req)}
			end
	end.

-spec websocket_handshake(#state{}, Req, any())
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	when Req::cowboy_req:req().
websocket_handshake(State=#state{
			transport=Transport, key=Key, recv_extensions=Extensions},
		Req, HandlerState) ->
	Challenge = base64:encode(crypto:hash(sha,
		<< Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>)),
	ExtHeader = case Extensions of
		#{deflate_frame := _} -> [{<<"sec-websocket-extensions">>, <<"x-webkit-deflate-frame">>}];
		_ -> []
	end,
	Req2 = cowboy_req:upgrade_reply(101, [{<<"upgrade">>, <<"websocket">>},
		{<<"sec-websocket-accept">>, Challenge}|ExtHeader], Req),
	%% Flush the resp_sent message before moving on.
	receive {cowboy_req, resp_sent} -> ok after 0 -> ok end,
	State2 = handler_loop_timeout(State),
	handler_before_loop(State2#state{key=undefined,
		messages=Transport:messages()}, Req2, HandlerState, <<>>).

-spec handler_before_loop(#state{}, Req, any(), binary())
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	when Req::cowboy_req:req().
handler_before_loop(State=#state{
			socket=Socket, transport=Transport, hibernate=true},
		Req, HandlerState, SoFar) ->
	Transport:setopts(Socket, [{active, once}]),
	{suspend, ?MODULE, handler_loop,
		[State#state{hibernate=false}, Req, HandlerState, SoFar]};
handler_before_loop(State=#state{socket=Socket, transport=Transport},
		Req, HandlerState, SoFar) ->
	Transport:setopts(Socket, [{active, once}]),
	handler_loop(State, Req, HandlerState, SoFar).

-spec handler_loop_timeout(#state{}) -> #state{}.
handler_loop_timeout(State=#state{timeout=infinity}) ->
	State#state{timeout_ref=undefined};
handler_loop_timeout(State=#state{timeout=Timeout, timeout_ref=PrevRef}) ->
	_ = case PrevRef of undefined -> ignore; PrevRef ->
		erlang:cancel_timer(PrevRef) end,
	TRef = erlang:start_timer(Timeout, self(), ?MODULE),
	State#state{timeout_ref=TRef}.

-spec handler_loop(#state{}, Req, any(), binary())
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	when Req::cowboy_req:req().
handler_loop(State=#state{socket=Socket, messages={OK, Closed, Error},
		timeout_ref=TRef}, Req, HandlerState, SoFar) ->
	receive
		{OK, Socket, Data} ->
			State2 = handler_loop_timeout(State),
			websocket_data(State2, Req, HandlerState,
				<< SoFar/binary, Data/binary >>);
		{Closed, Socket} ->
			handler_terminate(State, Req, HandlerState, {error, closed});
		{Error, Socket, Reason} ->
			handler_terminate(State, Req, HandlerState, {error, Reason});
		{timeout, TRef, ?MODULE} ->
			websocket_close(State, Req, HandlerState, timeout);
		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
			handler_loop(State, Req, HandlerState, SoFar);
		Message ->
			handler_call(State, Req, HandlerState,
				SoFar, websocket_info, Message, fun handler_before_loop/4)
	end.

-spec websocket_data(#state{}, Req, any(), binary())
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	when Req::cowboy_req:req().
websocket_data(State=#state{frag_state=FragState, recv_extensions=Extensions}, Req, HandlerState, Data) ->
	case cow_ws:parse_header(Data, Extensions, FragState) of
		%% All frames sent from the client to the server are masked.
		{_, _, _, _, undefined, _} ->
			websocket_close(State, Req, HandlerState, {error, badframe});
		%% No payload.
		{Type, FragState2, _, 0, _, Rest} ->
			websocket_dispatch(State#state{frag_state=FragState2}, Req, HandlerState, Type, <<>>, undefined, Rest);
		{Type, FragState2, Rsv, Len, MaskKey, Rest} ->
			websocket_payload(State#state{frag_state=FragState2}, Req, HandlerState, Type, Len, MaskKey, Rsv, Rest);
		more ->
			handler_before_loop(State, Req, HandlerState, Data);
		error ->
			websocket_close(State, Req, HandlerState, {error, badframe})
	end.

websocket_payload(State, Req, HandlerState, Type = close, Len, MaskKey, Rsv, Data) ->
	case cow_ws:parse_close_code(Data, MaskKey) of
		{ok, CloseCode, Rest} ->
			websocket_payload(State, Req, HandlerState, Type, Len - 2, MaskKey, Rsv, CloseCode, <<>>, 2, Rest);
		error ->
			websocket_close(State, Req, HandlerState, {error, badframe})
	end;
websocket_payload(State, Req, HandlerState, Type, Len, MaskKey, Rsv, Data) ->
	websocket_payload(State, Req, HandlerState, Type, Len, MaskKey, Rsv, undefined, <<>>, 0, Data).

websocket_payload(State=#state{frag_state=FragState, utf8_state=Incomplete, recv_extensions=Extensions},
		Req, HandlerState, Type, Len, MaskKey, Rsv, CloseCode, Unmasked, UnmaskedLen, Data) ->
	case cow_ws:parse_payload(Data, MaskKey, Incomplete, UnmaskedLen, Type, Len, FragState, Extensions, Rsv) of
		{ok, Payload, Utf8State, Rest} ->
			websocket_dispatch(State#state{utf8_state=Utf8State},
				Req, HandlerState, Type, << Unmasked/binary, Payload/binary >>, CloseCode, Rest);
		{more, Payload, Utf8State} ->
			websocket_payload_loop(State#state{utf8_state=Utf8State},
				Req, HandlerState, Type, Len - byte_size(Data), MaskKey, Rsv, CloseCode,
				<< Unmasked/binary, Payload/binary >>, UnmaskedLen + byte_size(Data));
		error ->
			websocket_close(State, Req, HandlerState, {error, badencoding})
	end.

websocket_payload_loop(State=#state{socket=Socket, transport=Transport,
		messages={OK, Closed, Error}, timeout_ref=TRef},
		Req, HandlerState, Type, Len, MaskKey, Rsv, CloseCode, Unmasked, UnmaskedLen) ->
	Transport:setopts(Socket, [{active, once}]),
	receive
		{OK, Socket, Data} ->
			State2 = handler_loop_timeout(State),
			websocket_payload(State2, Req, HandlerState,
				Type, Len, MaskKey, Rsv, CloseCode, Unmasked, UnmaskedLen, Data);
		{Closed, Socket} ->
			handler_terminate(State, Req, HandlerState, {error, closed});
		{Error, Socket, Reason} ->
			handler_terminate(State, Req, HandlerState, {error, Reason});
		{timeout, TRef, ?MODULE} ->
			websocket_close(State, Req, HandlerState, timeout);
		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
			websocket_payload_loop(State, Req, HandlerState,
				Type, Len, MaskKey, Rsv, CloseCode, Unmasked, UnmaskedLen);
		Message ->
			handler_call(State, Req, HandlerState,
				<<>>, websocket_info, Message,
				fun (State2, Req2, HandlerState2, _) ->
					websocket_payload_loop(State2, Req2, HandlerState2,
						Type, Len, MaskKey, Rsv, CloseCode, Unmasked, UnmaskedLen)
				end)
	end.

%% Continuation frame.
websocket_dispatch(State=#state{frag_state={nofin, _}, frag_buffer=SoFar},
		Req, HandlerState, fragment, Payload, _, RemainingData) ->
	websocket_data(State#state{frag_buffer= << SoFar/binary, Payload/binary >>}, Req, HandlerState, RemainingData);
%% Last continuation frame.
websocket_dispatch(State=#state{frag_state={fin, Type}, frag_buffer=SoFar},
		Req, HandlerState, fragment, Payload, CloseCode, RemainingData) ->
	websocket_dispatch(State#state{frag_state=undefined, frag_buffer= <<>>}, Req, HandlerState,
		Type, << SoFar/binary, Payload/binary >>, CloseCode, RemainingData);
%% Text frame.
websocket_dispatch(State, Req, HandlerState, text, Payload, _, RemainingData) ->
	handler_call(State, Req, HandlerState, RemainingData,
		websocket_handle, {text, Payload}, fun websocket_data/4);
%% Binary frame.
websocket_dispatch(State, Req, HandlerState, binary, Payload, _, RemainingData) ->
	handler_call(State, Req, HandlerState, RemainingData,
		websocket_handle, {binary, Payload}, fun websocket_data/4);
%% Close control frame.
websocket_dispatch(State, Req, HandlerState, close, _, undefined, _) ->
	websocket_close(State, Req, HandlerState, remote);
websocket_dispatch(State, Req, HandlerState, close, Payload, Code, _) ->
	websocket_close(State, Req, HandlerState, {remote, Code, Payload});
%% Ping control frame. Send a pong back and forward the ping to the handler.
websocket_dispatch(State=#state{socket=Socket, transport=Transport, send_extensions=Extensions},
		Req, HandlerState, ping, Payload, _, RemainingData) ->
	Transport:send(Socket, cow_ws:frame({pong, Payload}, Extensions)),
	handler_call(State, Req, HandlerState, RemainingData,
		websocket_handle, {ping, Payload}, fun websocket_data/4);
%% Pong control frame.
websocket_dispatch(State, Req, HandlerState, pong, Payload, _, RemainingData) ->
	handler_call(State, Req, HandlerState, RemainingData,
		websocket_handle, {pong, Payload}, fun websocket_data/4).

-spec handler_call(#state{}, Req, any(), binary(), atom(), any(), fun())
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	when Req::cowboy_req:req().
handler_call(State=#state{handler=Handler}, Req, HandlerState,
		RemainingData, Callback, Message, NextState) ->
	try Handler:Callback(Message, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			NextState(State, Req2, HandlerState2, RemainingData);
		{ok, Req2, HandlerState2, hibernate} ->
			NextState(State#state{hibernate=true},
				Req2, HandlerState2, RemainingData);
		{reply, Payload, Req2, HandlerState2}
				when is_list(Payload) ->
			case websocket_send_many(Payload, State) of
				ok ->
					NextState(State, Req2, HandlerState2, RemainingData);
				stop ->
					handler_terminate(State, Req2, HandlerState2, stop);
				Error = {error, _} ->
					handler_terminate(State, Req2, HandlerState2, Error)
			end;
		{reply, Payload, Req2, HandlerState2, hibernate}
				when is_list(Payload) ->
			case websocket_send_many(Payload, State) of
				ok ->
					NextState(State#state{hibernate=true},
						Req2, HandlerState2, RemainingData);
				stop ->
					handler_terminate(State, Req2, HandlerState2, stop);
				Error = {error, _} ->
					handler_terminate(State, Req2, HandlerState2, Error)
			end;
		{reply, Payload, Req2, HandlerState2} ->
			case websocket_send(Payload, State) of
				ok ->
					NextState(State, Req2, HandlerState2, RemainingData);
				stop ->
					handler_terminate(State, Req2, HandlerState2, stop);
				Error = {error, _} ->
					handler_terminate(State, Req2, HandlerState2, Error)
			end;
		{reply, Payload, Req2, HandlerState2, hibernate} ->
			case websocket_send(Payload, State) of
				ok ->
					NextState(State#state{hibernate=true},
						Req2, HandlerState2, RemainingData);
				stop ->
					handler_terminate(State, Req2, HandlerState2, stop);
				Error = {error, _} ->
					handler_terminate(State, Req2, HandlerState2, Error)
			end;
		{stop, Req2, HandlerState2} ->
			websocket_close(State, Req2, HandlerState2, stop)
	catch Class:Reason ->
		_ = websocket_close(State, Req, HandlerState, {crash, Class, Reason}),
		erlang:Class([
			{reason, Reason},
			{mfa, {Handler, Callback, 3}},
			{stacktrace, erlang:get_stacktrace()},
			{msg, Message},
			{req, cowboy_req:to_list(Req)},
			{state, HandlerState}
		])
	end.

-spec websocket_send(cow_ws:frame(), #state{}) -> ok | stop | {error, atom()}.
websocket_send(Frame, #state{socket=Socket, transport=Transport, send_extensions=Extensions}) ->
	Res = Transport:send(Socket, cow_ws:frame(Frame, Extensions)),
	case Frame of
		close -> stop;
		{close, _} -> stop;
		{close, _, _} -> stop;
		_ -> Res
	end.

-spec websocket_send_many([cow_ws:frame()], #state{}) -> ok | stop | {error, atom()}.
websocket_send_many([], _) ->
	ok;
websocket_send_many([Frame|Tail], State) ->
	case websocket_send(Frame, State) of
		ok -> websocket_send_many(Tail, State);
		stop -> stop;
		Error -> Error
	end.

-spec websocket_close(#state{}, Req, any(), terminate_reason())
	-> {ok, Req, cowboy_middleware:env()}
	when Req::cowboy_req:req().
websocket_close(State=#state{socket=Socket, transport=Transport, send_extensions=Extensions},
		Req, HandlerState, Reason) ->
	case Reason of
		Normal when Normal =:= stop; Normal =:= timeout ->
			Transport:send(Socket, cow_ws:frame({close, 1000, <<>>}, Extensions));
		{error, badframe} ->
			Transport:send(Socket, cow_ws:frame({close, 1002, <<>>}, Extensions));
		{error, badencoding} ->
			Transport:send(Socket, cow_ws:frame({close, 1007, <<>>}, Extensions));
		{crash, _, _} ->
			Transport:send(Socket, cow_ws:frame({close, 1011, <<>>}, Extensions));
		remote ->
			Transport:send(Socket, cow_ws:frame(close, Extensions));
		{remote, Code, _} ->
			Transport:send(Socket, cow_ws:frame({close, Code, <<>>}, Extensions))
	end,
	handler_terminate(State, Req, HandlerState, Reason).

-spec handler_terminate(#state{}, Req, any(), terminate_reason())
	-> {ok, Req, cowboy_middleware:env()}
	when Req::cowboy_req:req().
handler_terminate(#state{env=Env, handler=Handler},
		Req, HandlerState, Reason) ->
	cowboy_handler:terminate(Reason, Req, HandlerState, Handler),
	{ok, Req, [{result, closed}|Env]}.
