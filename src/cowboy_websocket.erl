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
-behaviour(cowboy_system).

-export([upgrade/6]).
-export([handler_loop/4]).
-export([continue/3]).
-export([terminate/4]).
-export([code_change/5]).
-export([get_state/2]).
-export([replace_state/3]).
-export([format_status/2]).

-type close_code() :: 1000..4999.
-export_type([close_code/0]).

-type frame() :: close | ping | pong
	| {text | binary | close | ping | pong, iodata()}
	| {close, close_code(), iodata()}.
-export_type([frame/0]).

-type opcode() :: 0 | 1 | 2 | 8 | 9 | 10.
-type mask_key() :: 0..16#ffffffff.
-type frag_state() :: undefined
	| {nofin, opcode(), binary()} | {fin, opcode(), binary()}.
-type rsv() :: << _:3 >>.
-type terminate_reason() :: normal | shutdown | timeout
	| remote | {remote, close_code(), binary()}
	| {error, badencoding | badframe | closed | atom()}
	| {crash, error | exit | throw, any()}
	| {shutdown, any()}.

-callback init(Req, any())
	-> {ok | module(), Req, any()}
	| {module(), Req, any(), hibernate}
	| {module(), Req, any(), timeout()}
	| {module(), Req, any(), timeout(), hibernate}
	when Req::cowboy_req:req().
-callback websocket_handle({text | binary | ping | pong, binary()}, Req, State)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {reply, frame() | [frame()], Req, State}
	| {reply, frame() | [frame()], Req, State, hibernate}
	| {shutdown, Req, State}
	when Req::cowboy_req:req(), State::any().
-callback websocket_info(any(), Req, State)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {reply, frame() | [frame()], Req, State}
	| {reply, frame() | [frame()], Req, State, hibernate}
	| {shutdown, Req, State}
	when Req::cowboy_req:req(), State::any().
%% @todo optional -callback terminate(terminate_reason(), cowboy_req:req(), state()) -> ok.

-record(state, {
	env :: cowboy_middleware:env(),
	parent :: pid(),
	socket = undefined :: inet:socket(),
	transport = undefined :: module(),
	handler :: module(),
	key = undefined :: undefined | binary(),
	timeout = infinity :: timeout(),
	timeout_ref = undefined :: undefined | reference(),
	messages = undefined :: undefined | {atom(), atom(), atom()},
	hibernate = false :: boolean(),
	frag_state = undefined :: frag_state(),
	utf8_state = <<>> :: binary(),
	deflate_frame = false :: boolean(),
	inflate_state :: undefined | port(),
	deflate_state :: undefined | port()
}).

-type misc() :: {handler_loop, #state{}, any(), binary()}
	| {websocket_payload_loop, #state{}, any(), {opcode(), non_neg_integer(),
		mask_key(), binary(), non_neg_integer(), rsv()}}.

-spec upgrade(Req, Env, module(), any(), timeout(), run | hibernate)
	-> {ok, Req, Env}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), any()}, any(), module(), Req, Env, misc()}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade(Req, Env, Handler, HandlerState, Timeout, Hibernate) ->
	{_, Ref} = lists:keyfind(listener, 1, Env),
	ranch:remove_connection(Ref),
	{_, Parent} = lists:keyfind(parent, 1, Env),
	[Socket, Transport] = cowboy_req:get([socket, transport], Req),
	State = #state{env=Env, parent=Parent, socket=Socket, transport=Transport,
		handler=Handler, timeout=Timeout, hibernate=Hibernate =:= hibernate},
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
						deflate_frame = true,
						inflate_state = Inflate,
						deflate_state = Deflate
					}, cowboy_req:set_meta(websocket_compress, true, Req)};
				_ ->
					{ok, State, cowboy_req:set_meta(websocket_compress, false, Req)}
			end
	end.

-spec websocket_handshake(#state{}, Req, any())
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), any()}, any(), module(), Req,
		cowboy_middleware:env(), misc()}
	when Req::cowboy_req:req().
websocket_handshake(State=#state{
			transport=Transport, key=Key, deflate_frame=DeflateFrame},
		Req, HandlerState) ->
	Challenge = base64:encode(crypto:hash(sha,
		<< Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>)),
	Extensions = case DeflateFrame of
		false -> [];
		true -> [{<<"sec-websocket-extensions">>, <<"x-webkit-deflate-frame">>}]
	end,
	Req2 = cowboy_req:upgrade_reply(101, [{<<"upgrade">>, <<"websocket">>},
		{<<"sec-websocket-accept">>, Challenge}|Extensions], Req),
	%% Flush the resp_sent message before moving on.
	receive {cowboy_req, resp_sent} -> ok after 0 -> ok end,
	State2 = handler_loop_timeout(State),
	handler_before_loop(State2#state{key=undefined,
		messages=Transport:messages()}, Req2, HandlerState, <<>>).

-spec handler_before_loop(#state{}, Req, any(), binary())
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), any()}, any(), module(), Req,
		cowboy_middleware:env(), misc()}
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
	| {system, {pid(), any()}, any(), module(), Req,
		cowboy_middleware:env(), misc()}
	when Req::cowboy_req:req().
handler_loop(State=#state{socket=Socket, messages={OK, Closed, Error},
		timeout_ref=TRef, env=Env, parent=Parent}, Req, HandlerState, SoFar) ->
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
		{'EXIT', Parent, Reason} ->
			websocket_close(State, Req, HandlerState, {shutdown, Reason});
		{system, From, Msg} ->
			Misc = {handler_loop, State, HandlerState, SoFar},
			{system, From, Msg, ?MODULE, Req, Env, Misc};
		Message ->
			handler_call(State, Req, HandlerState,
				SoFar, websocket_info, Message, fun handler_before_loop/4)
	end.

%% All frames passing through this function are considered valid,
%% with the only exception of text and close frames with a payload
%% which may still contain errors.
-spec websocket_data(#state{}, Req, any(), binary())
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), any()}, any(), module(), Req,
		cowboy_middleware:env(), misc()}
	when Req::cowboy_req:req().
%% RSV bits MUST be 0 unless an extension is negotiated
%% that defines meanings for non-zero values.
websocket_data(State, Req, HandlerState, << _:1, Rsv:3, _/bits >>)
		when Rsv =/= 0, State#state.deflate_frame =:= false ->
	websocket_close(State, Req, HandlerState, {error, badframe});
%% Invalid opcode. Note that these opcodes may be used by extensions.
websocket_data(State, Req, HandlerState, << _:4, Opcode:4, _/bits >>)
		when Opcode > 2, Opcode =/= 8, Opcode =/= 9, Opcode =/= 10 ->
	websocket_close(State, Req, HandlerState, {error, badframe});
%% Control frames MUST NOT be fragmented.
websocket_data(State, Req, HandlerState, << 0:1, _:3, Opcode:4, _/bits >>)
		when Opcode >= 8 ->
	websocket_close(State, Req, HandlerState, {error, badframe});
%% A frame MUST NOT use the zero opcode unless fragmentation was initiated.
websocket_data(State=#state{frag_state=undefined}, Req, HandlerState,
		<< _:4, 0:4, _/bits >>) ->
	websocket_close(State, Req, HandlerState, {error, badframe});
%% Non-control opcode when expecting control message or next fragment.
websocket_data(State=#state{frag_state={nofin, _, _}}, Req, HandlerState,
		<< _:4, Opcode:4, _/bits >>)
		when Opcode =/= 0, Opcode < 8 ->
	websocket_close(State, Req, HandlerState, {error, badframe});
%% Close control frame length MUST be 0 or >= 2.
websocket_data(State, Req, HandlerState, << _:4, 8:4, _:1, 1:7, _/bits >>) ->
	websocket_close(State, Req, HandlerState, {error, badframe});
%% Close control frame with incomplete close code. Need more data.
websocket_data(State, Req, HandlerState,
		Data = << _:4, 8:4, 1:1, Len:7, _/bits >>)
		when Len > 1, byte_size(Data) < 8 ->
	handler_before_loop(State, Req, HandlerState, Data);
%% 7 bits payload length.
websocket_data(State, Req, HandlerState, << Fin:1, Rsv:3/bits, Opcode:4, 1:1,
		Len:7, MaskKey:32, Rest/bits >>)
		when Len < 126 ->
	websocket_data(State, Req, HandlerState,
		Opcode, Len, MaskKey, Rest, Rsv, Fin);
%% 16 bits payload length.
websocket_data(State, Req, HandlerState, << Fin:1, Rsv:3/bits, Opcode:4, 1:1,
		126:7, Len:16, MaskKey:32, Rest/bits >>)
		when Len > 125, Opcode < 8 ->
	websocket_data(State, Req, HandlerState,
		Opcode, Len, MaskKey, Rest, Rsv, Fin);
%% 63 bits payload length.
websocket_data(State, Req, HandlerState, << Fin:1, Rsv:3/bits, Opcode:4, 1:1,
		127:7, 0:1, Len:63, MaskKey:32, Rest/bits >>)
		when Len > 16#ffff, Opcode < 8 ->
	websocket_data(State, Req, HandlerState,
		Opcode, Len, MaskKey, Rest, Rsv, Fin);
%% When payload length is over 63 bits, the most significant bit MUST be 0.
websocket_data(State, Req, HandlerState, << _:8, 1:1, 127:7, 1:1, _:7, _/bits >>) ->
	websocket_close(State, Req, HandlerState, {error, badframe});
%% All frames sent from the client to the server are masked.
websocket_data(State, Req, HandlerState, << _:8, 0:1, _/bits >>) ->
	websocket_close(State, Req, HandlerState, {error, badframe});
%% For the next two clauses, it can be one of the following:
%%
%%  *  The minimal number of bytes MUST be used to encode the length
%%  *  All control frames MUST have a payload length of 125 bytes or less
websocket_data(State, Req, HandlerState, << _:9, 126:7, _:48, _/bits >>) ->
	websocket_close(State, Req, HandlerState, {error, badframe});
websocket_data(State, Req, HandlerState, << _:9, 127:7, _:96, _/bits >>) ->
	websocket_close(State, Req, HandlerState, {error, badframe});
%% Need more data.
websocket_data(State, Req, HandlerState, Data) ->
	handler_before_loop(State, Req, HandlerState, Data).

%% Initialize or update fragmentation state.
-spec websocket_data(#state{}, Req, any(),
	opcode(), non_neg_integer(), mask_key(), binary(), rsv(), 0 | 1)
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), any()}, any(), module(), Req,
		cowboy_middleware:env(), misc()}
	when Req::cowboy_req:req().
%% The opcode is only included in the first frame fragment.
websocket_data(State=#state{frag_state=undefined}, Req, HandlerState,
		Opcode, Len, MaskKey, Data, Rsv, 0) ->
	websocket_payload(State#state{frag_state={nofin, Opcode, <<>>}},
		Req, HandlerState, 0, Len, MaskKey, <<>>, 0, Data, Rsv);
%% Subsequent frame fragments.
websocket_data(State=#state{frag_state={nofin, _, _}}, Req, HandlerState,
		0, Len, MaskKey, Data, Rsv, 0) ->
	websocket_payload(State, Req, HandlerState,
		0, Len, MaskKey, <<>>, 0, Data, Rsv);
%% Final frame fragment.
websocket_data(State=#state{frag_state={nofin, Opcode, SoFar}},
		Req, HandlerState, 0, Len, MaskKey, Data, Rsv, 1) ->
	websocket_payload(State#state{frag_state={fin, Opcode, SoFar}},
		Req, HandlerState, 0, Len, MaskKey, <<>>, 0, Data, Rsv);
%% Unfragmented frame.
websocket_data(State, Req, HandlerState, Opcode, Len, MaskKey, Data, Rsv, 1) ->
	websocket_payload(State, Req, HandlerState,
		Opcode, Len, MaskKey, <<>>, 0, Data, Rsv).

-spec websocket_payload(#state{}, Req, any(),
	opcode(), non_neg_integer(), mask_key(), binary(), non_neg_integer(),
	binary(), rsv())
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), any()}, any(), module(), Req,
		cowboy_middleware:env(), misc()}
	when Req::cowboy_req:req().
%% Close control frames with a payload MUST contain a valid close code.
websocket_payload(State, Req, HandlerState,
		Opcode=8, Len, MaskKey, <<>>, 0,
		<< MaskedCode:2/binary, Rest/bits >>, Rsv) ->
	Unmasked = << Code:16 >> = websocket_unmask(MaskedCode, MaskKey, <<>>),
	if	Code < 1000; Code =:= 1004; Code =:= 1005; Code =:= 1006;
				(Code > 1011) and (Code < 3000); Code > 4999 ->
			websocket_close(State, Req, HandlerState, {error, badframe});
		true ->
			websocket_payload(State, Req, HandlerState,
				Opcode, Len - 2, MaskKey, Unmasked, byte_size(MaskedCode),
				Rest, Rsv)
	end;
%% Text frames and close control frames MUST have a payload that is valid UTF-8.
websocket_payload(State=#state{utf8_state=Incomplete},
		Req, HandlerState, Opcode, Len, MaskKey, Unmasked, UnmaskedLen,
		Data, Rsv)
		when (byte_size(Data) < Len) andalso ((Opcode =:= 1) orelse
			((Opcode =:= 8) andalso (Unmasked =/= <<>>))) ->
	Unmasked2 = websocket_unmask(Data,
		rotate_mask_key(MaskKey, UnmaskedLen), <<>>),
	{Unmasked3, State2} = websocket_inflate_frame(Unmasked2, Rsv, false, State),
	case is_utf8(<< Incomplete/binary, Unmasked3/binary >>) of
		false ->
			websocket_close(State2, Req, HandlerState, {error, badencoding});
		Utf8State ->
			websocket_payload_loop(State2#state{utf8_state=Utf8State},
				Req, HandlerState, Opcode, Len - byte_size(Data), MaskKey,
				<< Unmasked/binary, Unmasked3/binary >>,
				UnmaskedLen + byte_size(Data), Rsv)
	end;
websocket_payload(State=#state{utf8_state=Incomplete},
		Req, HandlerState, Opcode, Len, MaskKey, Unmasked, UnmaskedLen,
		Data, Rsv)
		when Opcode =:= 1; (Opcode =:= 8) and (Unmasked =/= <<>>) ->
	<< End:Len/binary, Rest/bits >> = Data,
	Unmasked2 = websocket_unmask(End,
		rotate_mask_key(MaskKey, UnmaskedLen), <<>>),
	{Unmasked3, State2} = websocket_inflate_frame(Unmasked2, Rsv, true, State),
	case is_utf8(<< Incomplete/binary, Unmasked3/binary >>) of
		<<>> ->
			websocket_dispatch(State2#state{utf8_state= <<>>},
				Req, HandlerState, Rest, Opcode,
				<< Unmasked/binary, Unmasked3/binary >>);
		_ ->
			websocket_close(State2, Req, HandlerState, {error, badencoding})
	end;
%% Fragmented text frames may cut payload in the middle of UTF-8 codepoints.
websocket_payload(State=#state{frag_state={_, 1, _}, utf8_state=Incomplete},
		Req, HandlerState, Opcode=0, Len, MaskKey, Unmasked, UnmaskedLen,
		Data, Rsv)
		when byte_size(Data) < Len ->
	Unmasked2 = websocket_unmask(Data,
		rotate_mask_key(MaskKey, UnmaskedLen), <<>>),
	{Unmasked3, State2} = websocket_inflate_frame(Unmasked2, Rsv, false, State),
	case is_utf8(<< Incomplete/binary, Unmasked3/binary >>) of
		false ->
			websocket_close(State2, Req, HandlerState, {error, badencoding});
		Utf8State ->
			websocket_payload_loop(State2#state{utf8_state=Utf8State},
				Req, HandlerState, Opcode, Len - byte_size(Data), MaskKey,
				<< Unmasked/binary, Unmasked3/binary >>,
				UnmaskedLen + byte_size(Data), Rsv)
	end;
websocket_payload(State=#state{frag_state={Fin, 1, _}, utf8_state=Incomplete},
		Req, HandlerState, Opcode=0, Len, MaskKey, Unmasked, UnmaskedLen,
		Data, Rsv) ->
	<< End:Len/binary, Rest/bits >> = Data,
	Unmasked2 = websocket_unmask(End,
		rotate_mask_key(MaskKey, UnmaskedLen), <<>>),
	{Unmasked3, State2} = websocket_inflate_frame(Unmasked2, Rsv, Fin =:= fin, State),
	case is_utf8(<< Incomplete/binary, Unmasked3/binary >>) of
		<<>> ->
			websocket_dispatch(State2#state{utf8_state= <<>>},
				Req, HandlerState, Rest, Opcode,
				<< Unmasked/binary, Unmasked3/binary >>);
		Utf8State when is_binary(Utf8State), Fin =:= nofin ->
			websocket_dispatch(State2#state{utf8_state=Utf8State},
				Req, HandlerState, Rest, Opcode,
				<< Unmasked/binary, Unmasked3/binary >>);
		_ ->
			websocket_close(State, Req, HandlerState, {error, badencoding})
	end;
%% Other frames have a binary payload.
websocket_payload(State, Req, HandlerState,
		Opcode, Len, MaskKey, Unmasked, UnmaskedLen, Data, Rsv)
		when byte_size(Data) < Len ->
	Unmasked2 = websocket_unmask(Data,
		rotate_mask_key(MaskKey, UnmaskedLen), <<>>),
	{Unmasked3, State2} = websocket_inflate_frame(Unmasked2, Rsv, false, State),
	websocket_payload_loop(State2, Req, HandlerState,
		Opcode, Len - byte_size(Data), MaskKey,
		<< Unmasked/binary, Unmasked3/binary >>, UnmaskedLen + byte_size(Data),
		Rsv);
websocket_payload(State, Req, HandlerState,
		Opcode, Len, MaskKey, Unmasked, UnmaskedLen, Data, Rsv) ->
	<< End:Len/binary, Rest/bits >> = Data,
	Unmasked2 = websocket_unmask(End,
		rotate_mask_key(MaskKey, UnmaskedLen), <<>>),
	{Unmasked3, State2} = websocket_inflate_frame(Unmasked2, Rsv, true, State),
	websocket_dispatch(State2, Req, HandlerState, Rest, Opcode,
		<< Unmasked/binary, Unmasked3/binary >>).

-spec websocket_inflate_frame(binary(), rsv(), boolean(), #state{}) ->
		{binary(), #state{}}.
websocket_inflate_frame(Data, << Rsv1:1, _:2 >>, _,
		#state{deflate_frame = DeflateFrame} = State)
		when DeflateFrame =:= false orelse Rsv1 =:= 0 ->
	{Data, State};
websocket_inflate_frame(Data, << 1:1, _:2 >>, false, State) ->
	Result = zlib:inflate(State#state.inflate_state, Data),
	{iolist_to_binary(Result), State};
websocket_inflate_frame(Data, << 1:1, _:2 >>, true, State) ->
	Result = zlib:inflate(State#state.inflate_state,
		<< Data/binary, 0:8, 0:8, 255:8, 255:8 >>),
	{iolist_to_binary(Result), State}.

-spec websocket_unmask(B, mask_key(), B) -> B when B::binary().
websocket_unmask(<<>>, _, Unmasked) ->
	Unmasked;
websocket_unmask(<< O:32, Rest/bits >>, MaskKey, Acc) ->
	T = O bxor MaskKey,
	websocket_unmask(Rest, MaskKey, << Acc/binary, T:32 >>);
websocket_unmask(<< O:24 >>, MaskKey, Acc) ->
	<< MaskKey2:24, _:8 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:24 >>;
websocket_unmask(<< O:16 >>, MaskKey, Acc) ->
	<< MaskKey2:16, _:16 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:16 >>;
websocket_unmask(<< O:8 >>, MaskKey, Acc) ->
	<< MaskKey2:8, _:24 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:8 >>.

%% Because we unmask on the fly we need to continue from the right mask byte.
-spec rotate_mask_key(mask_key(), non_neg_integer()) -> mask_key().
rotate_mask_key(MaskKey, UnmaskedLen) ->
	Left = UnmaskedLen rem 4,
	Right = 4 - Left,
	(MaskKey bsl (Left * 8)) + (MaskKey bsr (Right * 8)).

%% Returns <<>> if the argument is valid UTF-8, false if not,
%% or the incomplete part of the argument if we need more data.
-spec is_utf8(binary()) -> false | binary().
is_utf8(Valid = <<>>) ->
	Valid;
is_utf8(<< _/utf8, Rest/bits >>) ->
	is_utf8(Rest);
%% 2 bytes. Codepages C0 and C1 are invalid; fail early.
is_utf8(<< 2#1100000:7, _/bits >>) ->
	false;
is_utf8(Incomplete = << 2#110:3, _:5 >>) ->
	Incomplete;
%% 3 bytes.
is_utf8(Incomplete = << 2#1110:4, _:4 >>) ->
	Incomplete;
is_utf8(Incomplete = << 2#1110:4, _:4, 2#10:2, _:6 >>) ->
	Incomplete;
%% 4 bytes. Codepage F4 may have invalid values greater than 0x10FFFF.
is_utf8(<< 2#11110100:8, 2#10:2, High:6, _/bits >>) when High >= 2#10000 ->
	false;
is_utf8(Incomplete = << 2#11110:5, _:3 >>) ->
	Incomplete;
is_utf8(Incomplete = << 2#11110:5, _:3, 2#10:2, _:6 >>) ->
	Incomplete;
is_utf8(Incomplete = << 2#11110:5, _:3, 2#10:2, _:6, 2#10:2, _:6 >>) ->
	Incomplete;
%% Invalid.
is_utf8(_) ->
	false.

-spec websocket_payload_loop(#state{}, Req, any(),
		opcode(), non_neg_integer(), mask_key(), binary(),
		non_neg_integer(), rsv())
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), any()}, any(), module(), Req,
		cowboy_middleware:env(), misc()}
	when Req::cowboy_req:req().
websocket_payload_loop(State=#state{socket=Socket, transport=Transport,
		messages={OK, Closed, Error}, timeout_ref=TRef, env=Env, parent=Parent},
		Req, HandlerState, Opcode, Len, MaskKey, Unmasked, UnmaskedLen, Rsv) ->
	Transport:setopts(Socket, [{active, once}]),
	receive
		{OK, Socket, Data} ->
			State2 = handler_loop_timeout(State),
			websocket_payload(State2, Req, HandlerState,
				Opcode, Len, MaskKey, Unmasked, UnmaskedLen, Data, Rsv);
		{Closed, Socket} ->
			handler_terminate(State, Req, HandlerState, {error, closed});
		{Error, Socket, Reason} ->
			handler_terminate(State, Req, HandlerState, {error, Reason});
		{timeout, TRef, ?MODULE} ->
			websocket_close(State, Req, HandlerState, timeout);
		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
			websocket_payload_loop(State, Req, HandlerState,
				Opcode, Len, MaskKey, Unmasked, UnmaskedLen, Rsv);
		{'EXIT', Parent, Reason} ->
			websocket_close(State, Req, HandlerState, {shutdown, Reason});
		{system, From, Msg} ->
			Args = {Opcode, Len, MaskKey, Unmasked, UnmaskedLen, Rsv},
			Misc = {websocket_payload_loop, State, HandlerState, Args},
			{system, From, Msg, ?MODULE, Req, Env, Misc};
		Message ->
			handler_call(State, Req, HandlerState,
				<<>>, websocket_info, Message,
				fun (State2, Req2, HandlerState2, _) ->
					websocket_payload_loop(State2, Req2, HandlerState2,
						Opcode, Len, MaskKey, Unmasked, UnmaskedLen, Rsv)
				end)
	end.

-spec websocket_dispatch(#state{}, Req, any(), binary(), opcode(), binary())
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), any()}, any(), module(), Req,
		cowboy_middleware:env(), misc()}
	when Req::cowboy_req:req().
%% Continuation frame.
websocket_dispatch(State=#state{frag_state={nofin, Opcode, SoFar}},
		Req, HandlerState, RemainingData, 0, Payload) ->
	websocket_data(State#state{frag_state={nofin, Opcode,
		<< SoFar/binary, Payload/binary >>}}, Req, HandlerState, RemainingData);
%% Last continuation frame.
websocket_dispatch(State=#state{frag_state={fin, Opcode, SoFar}},
		Req, HandlerState, RemainingData, 0, Payload) ->
	websocket_dispatch(State#state{frag_state=undefined}, Req, HandlerState,
		RemainingData, Opcode, << SoFar/binary, Payload/binary >>);
%% Text frame.
websocket_dispatch(State, Req, HandlerState, RemainingData, 1, Payload) ->
	handler_call(State, Req, HandlerState, RemainingData,
		websocket_handle, {text, Payload}, fun websocket_data/4);
%% Binary frame.
websocket_dispatch(State, Req, HandlerState, RemainingData, 2, Payload) ->
	handler_call(State, Req, HandlerState, RemainingData,
		websocket_handle, {binary, Payload}, fun websocket_data/4);
%% Close control frame.
websocket_dispatch(State, Req, HandlerState, _RemainingData, 8, <<>>) ->
	websocket_close(State, Req, HandlerState, remote);
websocket_dispatch(State, Req, HandlerState, _RemainingData, 8,
		<< Code:16, Payload/bits >>) ->
	websocket_close(State, Req, HandlerState, {remote, Code, Payload});
%% Ping control frame. Send a pong back and forward the ping to the handler.
websocket_dispatch(State=#state{socket=Socket, transport=Transport},
		Req, HandlerState, RemainingData, 9, Payload) ->
	Len = payload_length_to_binary(byte_size(Payload)),
	Transport:send(Socket, << 1:1, 0:3, 10:4, 0:1, Len/bits, Payload/binary >>),
	handler_call(State, Req, HandlerState, RemainingData,
		websocket_handle, {ping, Payload}, fun websocket_data/4);
%% Pong control frame.
websocket_dispatch(State, Req, HandlerState, RemainingData, 10, Payload) ->
	handler_call(State, Req, HandlerState, RemainingData,
		websocket_handle, {pong, Payload}, fun websocket_data/4).

-spec handler_call(#state{}, Req, any(), binary(), atom(), any(), fun())
	-> {ok, Req, cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), any()}, any(), module(), Req,
		cowboy_middleware:env(), misc()}
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
				{ok, State2} ->
					NextState(State2, Req2, HandlerState2, RemainingData);
				{shutdown, State2} ->
					handler_terminate(State2, Req2, HandlerState2, shutdown);
				{{error, _} = Error, State2} ->
					handler_terminate(State2, Req2, HandlerState2, Error)
			end;
		{reply, Payload, Req2, HandlerState2, hibernate}
				when is_list(Payload) ->
			case websocket_send_many(Payload, State) of
				{ok, State2} ->
					NextState(State2#state{hibernate=true},
						Req2, HandlerState2, RemainingData);
				{shutdown, State2} ->
					handler_terminate(State2, Req2, HandlerState2, shutdown);
				{{error, _} = Error, State2} ->
					handler_terminate(State2, Req2, HandlerState2, Error)
			end;
		{reply, Payload, Req2, HandlerState2} ->
			case websocket_send(Payload, State) of
				{ok, State2} ->
					NextState(State2, Req2, HandlerState2, RemainingData);
				{shutdown, State2} ->
					handler_terminate(State2, Req2, HandlerState2, shutdown);
				{{error, _} = Error, State2} ->
					handler_terminate(State2, Req2, HandlerState2, Error)
			end;
		{reply, Payload, Req2, HandlerState2, hibernate} ->
			case websocket_send(Payload, State) of
				{ok, State2} ->
					NextState(State2#state{hibernate=true},
						Req2, HandlerState2, RemainingData);
				{shutdown, State2} ->
					handler_terminate(State2, Req2, HandlerState2, shutdown);
				{{error, _} = Error, State2} ->
					handler_terminate(State2, Req2, HandlerState2, Error)
			end;
		{shutdown, Req2, HandlerState2} ->
			websocket_close(State, Req2, HandlerState2, shutdown)
	catch Class:Reason ->
		_ = websocket_close(State, Req, HandlerState, {crash, Class, Reason}),
		State = cowboy_handler:format_status(terminate, get(), Req, HandlerState, Handler),
		erlang:exit([
			{class, Class},
			{reason, Reason},
			{mfa, {Handler, Callback, 3}},
			{stacktrace, erlang:get_stacktrace()},
			{msg, Message},
			{req, cowboy_req:to_list(Req)},
			{state, State}
		])
	end.

websocket_opcode(text) -> 1;
websocket_opcode(binary) -> 2;
websocket_opcode(close) -> 8;
websocket_opcode(ping) -> 9;
websocket_opcode(pong) -> 10.

-spec websocket_deflate_frame(opcode(), binary(), #state{}) ->
	{binary(), rsv(), #state{}}.
websocket_deflate_frame(Opcode, Payload,
		State=#state{deflate_frame = DeflateFrame})
		when DeflateFrame =:= false orelse Opcode >= 8 ->
	{Payload, << 0:3 >>, State};
websocket_deflate_frame(_, Payload, State=#state{deflate_state = Deflate}) ->
	Deflated = iolist_to_binary(zlib:deflate(Deflate, Payload, sync)),
	DeflatedBodyLength = erlang:size(Deflated) - 4,
	Deflated1 = case Deflated of
		<< Body:DeflatedBodyLength/binary, 0:8, 0:8, 255:8, 255:8 >> -> Body;
		_ -> Deflated
	end,
	{Deflated1, << 1:1, 0:2 >>, State}.

-spec websocket_send(frame(), #state{})
-> {ok, #state{}} | {shutdown, #state{}} | {{error, atom()}, #state{}}.
websocket_send(Type, State=#state{socket=Socket, transport=Transport})
		when Type =:= close ->
	Opcode = websocket_opcode(Type),
	case Transport:send(Socket, << 1:1, 0:3, Opcode:4, 0:8 >>) of
		ok -> {shutdown, State};
		Error -> {Error, State}
	end;
websocket_send(Type, State=#state{socket=Socket, transport=Transport})
		when Type =:= ping; Type =:= pong ->
	Opcode = websocket_opcode(Type),
	{Transport:send(Socket, << 1:1, 0:3, Opcode:4, 0:8 >>), State};
websocket_send({close, Payload}, State) ->
	websocket_send({close, 1000, Payload}, State);
websocket_send({Type = close, StatusCode, Payload}, State=#state{
		socket=Socket, transport=Transport}) ->
	Opcode = websocket_opcode(Type),
	Len = 2 + iolist_size(Payload),
	%% Control packets must not be > 125 in length.
	true = Len =< 125,
	BinLen = payload_length_to_binary(Len),
	Transport:send(Socket,
		[<< 1:1, 0:3, Opcode:4, 0:1, BinLen/bits, StatusCode:16 >>, Payload]),
	{shutdown, State};
websocket_send({Type, Payload0}, State=#state{socket=Socket, transport=Transport}) ->
	Opcode = websocket_opcode(Type),
	{Payload, Rsv, State2} = websocket_deflate_frame(Opcode, iolist_to_binary(Payload0), State),
	Len = iolist_size(Payload),
	%% Control packets must not be > 125 in length.
	true = if Type =:= ping; Type =:= pong ->
			Len =< 125;
		true ->
			true
	end,
	BinLen = payload_length_to_binary(Len),
	{Transport:send(Socket,
		[<< 1:1, Rsv/bits, Opcode:4, 0:1, BinLen/bits >>, Payload]), State2}.

-spec payload_length_to_binary(0..16#7fffffffffffffff)
	-> << _:7 >> | << _:23 >> | << _:71 >>.
payload_length_to_binary(N) ->
	case N of
		N when N =< 125 -> << N:7 >>;
		N when N =< 16#ffff -> << 126:7, N:16 >>;
		N when N =< 16#7fffffffffffffff -> << 127:7, N:64 >>
	end.

-spec websocket_send_many([frame()], #state{})
	-> {ok, #state{}} | {shutdown, #state{}} | {{error, atom()}, #state{}}.
websocket_send_many([], State) ->
	{ok, State};
websocket_send_many([Frame|Tail], State) ->
	case websocket_send(Frame, State) of
		{ok, State2} -> websocket_send_many(Tail, State2);
		{shutdown, State2} -> {shutdown, State2};
		{Error, State2} -> {Error, State2}
	end.

-spec websocket_close(#state{}, Req, any(), terminate_reason())
	-> {ok, Req, cowboy_middleware:env()}
	when Req::cowboy_req:req().
websocket_close(State=#state{socket=Socket, transport=Transport},
		Req, HandlerState, Reason) ->
	case Reason of
		Normal when Normal =:= shutdown; Normal =:= timeout ->
			Transport:send(Socket, << 1:1, 0:3, 8:4, 0:1, 2:7, 1000:16 >>);
		{shutdown, _} ->
			Transport:send(Socket, << 1:1, 0:3, 8:4, 0:1, 2:7, 1000:16 >>);
		{error, badframe} ->
			Transport:send(Socket, << 1:1, 0:3, 8:4, 0:1, 2:7, 1002:16 >>);
		{error, badencoding} ->
			Transport:send(Socket, << 1:1, 0:3, 8:4, 0:1, 2:7, 1007:16 >>);
		{crash, _, _} ->
			Transport:send(Socket, << 1:1, 0:3, 8:4, 0:1, 2:7, 1011:16 >>);
		remote ->
			Transport:send(Socket, << 1:1, 0:3, 8:4, 0:8 >>);
		{remote, Code, _} ->
			Transport:send(Socket, << 1:1, 0:3, 8:4, 0:1, 2:7, Code:16 >>)
	end,
	handler_terminate(State, Req, HandlerState, Reason).

-spec handler_terminate(#state{}, Req, any(), terminate_reason())
	-> {ok, Req, cowboy_middleware:env()}
	when Req::cowboy_req:req().
handler_terminate(#state{env=Env, handler=Handler},
		Req, HandlerState, Reason) ->
	cowboy_handler:terminate(Reason, Req, HandlerState, Handler),
	{ok, Req, [{result, closed}|Env]}.

-spec continue(cowboy_req:req(), cowboy_middleware:env(), misc())
	-> {ok, cowboy_req:req(), cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), any()}, any(), module(), cowboy_req:req(),
		cowboy_middleware:env(), misc()}.
continue(Req, Env, {handler_loop, State, HandlerState, SoFar}) ->
	handler_loop(State#state{env=Env}, Req, HandlerState, SoFar);
continue(Req, Env, {websocket_payload_loop, State, HandlerState,
		{Opcode, Len, MaskKey, Unmasked, UnmaskedLen, Rsv}}) ->
	websocket_payload_loop(State#state{env=Env}, Req, HandlerState,
		Opcode, Len, MaskKey, Unmasked, UnmaskedLen, Rsv).

-spec terminate(any(), cowboy_req:req(), cowboy_middleware:env(), misc())
	-> no_return().
terminate(Reason, Req, Env, {_Fun, State, HandlerState, _Args}) ->
	_ = websocket_close(State#state{env=Env}, Req, HandlerState, {shutdown, Reason}),
	exit(Reason).

-spec code_change(cowboy_req:req(), misc(), module(), any(), any())
	-> {ok, cowboy_req:req(), misc()}.
code_change(Req, {Fun, State=#state{handler=Handler}, HandlerState, Args},
		Module, OldVsn, Extra) ->
	{ok, Req2, HandlerState2} = cowboy_handler:code_change(OldVsn, Req, HandlerState,
		Module, Extra, Handler),
	{ok, Req2, {Fun, State, HandlerState2, Args}}.

-spec get_state(cowboy_req:req(), misc())
	-> {ok, {module(), cowboy_req:req(), any()}}.
get_state(Req, {_Fun, #state{handler=Handler}, HandlerState, _Args}) ->
	{ok, {Handler, Req, HandlerState}}.

-spec replace_state(cowboy_system:replace_state(), cowboy_req:req(), misc())
	-> {ok, {module(), cowboy_req:req(), any()}, cowboy_req:req(), misc()}.
replace_state(Replace, Req,
		{Fun, State=#state{handler=Handler}, HandlerState, Args}) ->
	{Handler, Req2, HandlerState2} = Result = Replace({Handler, Req, HandlerState}),
	{ok, Result, Req2, {Fun, State, HandlerState2, Args}}.

-spec format_status(normal, [[{term(), term()}] | running | suspended |
		cowboy_req:req() | cowboy_middleware:env() | misc()])
	-> maybe_improper_list().
format_status(Opt, [PDict, SysState, Req, Env,
		{Fun, State=#state{handler=Handler}, HandlerState, Args}]) ->
	{_, Parent} = lists:keyfind(parent, 1, Env),
	{_, Dbg} = lists:keyfind(dbg, 1, Env),
	Log = sys:get_debug(log, Dbg, []),
	Data = [
		{"Status", SysState},
		{"Parent", Parent},
		{"Logged events", Log},
		{"Request", cowboy_req:to_list(Req)},
		{"Environment", Env},
		{"Websocket state", state_to_list(State)},
		{"Websocket loop function", Fun},
		{"Websocket loop data", format_args(Fun, Args)},
		{"Handler", Handler}
	],
	HandlerStatus = cowboy_handler:format_status(Opt, PDict, Req, HandlerState, Handler),
	[{header, "Cowboy websocket handler"}, {data, Data} | HandlerStatus].

state_to_list(State) ->
	lists:zip(record_info(fields, state), tl(tuple_to_list(State))).

format_args(handler_loop, SoFar) ->
	[{so_far, SoFar}];
format_args(websocket_payload_loop,
		{Opcode, Len, MaskKey, Unmasked, UnmaskedLen, Rsv}) ->
	[{opcode, Opcode}, {length, Len}, {mask_key, MaskKey},
	 {unmasked_length, UnmaskedLen}, {unmasked, Unmasked}, {rsv, Rsv}].
