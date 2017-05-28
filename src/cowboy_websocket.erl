%% Copyright (c) 2011-2017, Loïc Hoguin <essen@ninenines.eu>
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

-export([upgrade/4]).
-export([upgrade/5]).
-export([takeover/7]).
-export([handler_loop/3]).

-type call_result(State) :: {ok, State}
	| {ok, State, hibernate}
	| {reply, cow_ws:frame() | [cow_ws:frame()], State}
	| {reply, cow_ws:frame() | [cow_ws:frame()], State, hibernate}
	| {stop, State}.

-type terminate_reason() :: normal | stop | timeout
	| remote | {remote, cow_ws:close_code(), binary()}
	| {error, badencoding | badframe | closed | atom()}
	| {crash, error | exit | throw, any()}.

-callback init(Req, any())
	-> {ok | module(), Req, any()}
	| {module(), Req, any(), any()}
	when Req::cowboy_req:req().

-callback websocket_init(State)
	-> call_result(State) when State::any().
-optional_callbacks([websocket_init/1]).

-callback websocket_handle({text | binary | ping | pong, binary()}, State)
	-> call_result(State) when State::any().
-callback websocket_info(any(), State)
	-> call_result(State) when State::any().

-callback terminate(any(), cowboy_req:req(), any()) -> ok.
-optional_callbacks([terminate/3]).

-type opts() :: #{
	compress => boolean(),
	idle_timeout => timeout(),
	req_filter => fun((cowboy_req:req()) -> map())
}.
-export_type([opts/0]).

-record(state, {
	socket = undefined :: inet:socket() | undefined,
	transport = undefined :: module(),
	handler :: module(),
	key = undefined :: undefined | binary(),
	timeout = infinity :: timeout(),
	timeout_ref = undefined :: undefined | reference(),
	compress = false :: boolean(),
	messages = undefined :: undefined | {atom(), atom(), atom()},
	hibernate = false :: boolean(),
	frag_state = undefined :: cow_ws:frag_state(),
	frag_buffer = <<>> :: binary(),
	utf8_state = 0 :: cow_ws:utf8_state(),
	extensions = #{} :: map(),
	req = #{} :: map()
}).

%% Stream process.

-spec upgrade(Req, Env, module(), any())
	-> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade(Req, Env, Handler, HandlerState) ->
	upgrade(Req, Env, Handler, HandlerState, #{}).

-spec upgrade(Req, Env, module(), any(), opts())
	-> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
%% @todo Immediately crash if a response has already been sent.
%% @todo Error out if HTTP/2.
upgrade(Req0, Env, Handler, HandlerState, Opts) ->
	Timeout = maps:get(idle_timeout, Opts, 60000),
	Compress = maps:get(compress, Opts, false),
	FilteredReq = case maps:get(req_filter, Opts, undefined) of
		undefined -> maps:with([method, version, scheme, host, port, path, qs, peer], Req0);
		FilterFun -> FilterFun(Req0)
	end,
	State0 = #state{handler=Handler, timeout=Timeout, compress=Compress, req=FilteredReq},
	try websocket_upgrade(State0, Req0) of
		{ok, State, Req} ->
			websocket_handshake(State, Req, HandlerState, Env)
	catch _:_ ->
		%% @todo Probably log something here?
		%% @todo Test that we can have 2 /ws 400 status code in a row on the same connection.
		%% @todo Does this even work?
		{ok, cowboy_req:reply(400, Req0), Env}
	end.

-spec websocket_upgrade(#state{}, Req)
	-> {ok, #state{}, Req} when Req::cowboy_req:req().
websocket_upgrade(State, Req) ->
	ConnTokens = cowboy_req:parse_header(<<"connection">>, Req),
	true = lists:member(<<"upgrade">>, ConnTokens),
	%% @todo Should probably send a 426 if the Upgrade header is missing.
	[<<"websocket">>] = cowboy_req:parse_header(<<"upgrade">>, Req),
	Version = cowboy_req:header(<<"sec-websocket-version">>, Req),
	IntVersion = binary_to_integer(Version),
	true = (IntVersion =:= 7) orelse (IntVersion =:= 8)
		orelse (IntVersion =:= 13),
	Key = cowboy_req:header(<<"sec-websocket-key">>, Req),
	false = Key =:= undefined,
	websocket_extensions(State#state{key=Key}, Req#{websocket_version => IntVersion}).

-spec websocket_extensions(#state{}, Req)
	-> {ok, #state{}, Req} when Req::cowboy_req:req().
websocket_extensions(State=#state{compress=Compress}, Req) ->
	%% @todo We want different options for this. For example
	%% * compress everything auto
	%% * compress only text auto
	%% * compress only binary auto
	%% * compress nothing auto (but still enabled it)
	%% * disable compression
	case {Compress, cowboy_req:parse_header(<<"sec-websocket-extensions">>, Req)} of
		{true, Extensions} when Extensions =/= undefined ->
			websocket_extensions(State, Req, Extensions, []);
		_ ->
			{ok, State, Req}
	end.

websocket_extensions(State, Req, [], []) ->
	{ok, State, Req};
websocket_extensions(State, Req, [], [<<", ">>|RespHeader]) ->
	{ok, State, cowboy_req:set_resp_header(<<"sec-websocket-extensions">>, lists:reverse(RespHeader), Req)};
websocket_extensions(State=#state{extensions=Extensions}, Req=#{pid := Pid},
		[{<<"permessage-deflate">>, Params}|Tail], RespHeader) ->
	%% @todo Make deflate options configurable.
	Opts = #{level => best_compression, mem_level => 8, strategy => default},
	case cow_ws:negotiate_permessage_deflate(Params, Extensions, Opts#{owner => Pid}) of
		{ok, RespExt, Extensions2} ->
			websocket_extensions(State#state{extensions=Extensions2},
				Req, Tail, [<<", ">>, RespExt|RespHeader]);
		ignore ->
			websocket_extensions(State, Req, Tail, RespHeader)
	end;
websocket_extensions(State=#state{extensions=Extensions}, Req=#{pid := Pid},
		[{<<"x-webkit-deflate-frame">>, Params}|Tail], RespHeader) ->
	%% @todo Make deflate options configurable.
	Opts = #{level => best_compression, mem_level => 8, strategy => default},
	case cow_ws:negotiate_x_webkit_deflate_frame(Params, Extensions, Opts#{owner => Pid}) of
		{ok, RespExt, Extensions2} ->
			websocket_extensions(State#state{extensions=Extensions2},
				Req, Tail, [<<", ">>, RespExt|RespHeader]);
		ignore ->
			websocket_extensions(State, Req, Tail, RespHeader)
	end;
websocket_extensions(State, Req, [_|Tail], RespHeader) ->
	websocket_extensions(State, Req, Tail, RespHeader).

-spec websocket_handshake(#state{}, Req, any(), Env)
	-> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
websocket_handshake(State=#state{key=Key},
		Req=#{pid := Pid, streamid := StreamID}, HandlerState, Env) ->
	Challenge = base64:encode(crypto:hash(sha,
		<< Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>)),
	Headers = cowboy_req:response_headers(#{
		<<"connection">> => <<"Upgrade">>,
		<<"upgrade">> => <<"websocket">>,
		<<"sec-websocket-accept">> => Challenge
	}, Req),
	Pid ! {{Pid, StreamID}, {switch_protocol, Headers, ?MODULE, {State, HandlerState}}},
	{ok, Req, Env}.

%% Connection process.

%% @todo Keep parent and handle system messages.
-spec takeover(pid(), ranch:ref(), inet:socket(), module(), any(), binary(),
	{#state{}, any()}) -> ok.
takeover(_Parent, Ref, Socket, Transport, _Opts, Buffer,
		{State0=#state{handler=Handler}, HandlerState}) ->
	%% @todo We should have an option to disable this behavior.
	ranch:remove_connection(Ref),
	State1 = handler_loop_timeout(State0#state{socket=Socket, transport=Transport}),
	State = State1#state{key=undefined, messages=Transport:messages()},
	case erlang:function_exported(Handler, websocket_init, 1) of
		true -> handler_call(State, HandlerState, Buffer, websocket_init, undefined, fun handler_before_loop/3);
		false -> handler_before_loop(State, HandlerState, Buffer)
	end.

-spec handler_before_loop(#state{}, any(), binary())
%% @todo Yeah not env.
	-> {ok, cowboy_middleware:env()}.
handler_before_loop(State=#state{
			socket=Socket, transport=Transport, hibernate=true},
		HandlerState, SoFar) ->
	Transport:setopts(Socket, [{active, once}]),
	proc_lib:hibernate(?MODULE, handler_loop,
		[State#state{hibernate=false}, HandlerState, SoFar]);
handler_before_loop(State=#state{socket=Socket, transport=Transport},
		HandlerState, SoFar) ->
	Transport:setopts(Socket, [{active, once}]),
	handler_loop(State, HandlerState, SoFar).

-spec handler_loop_timeout(#state{}) -> #state{}.
handler_loop_timeout(State=#state{timeout=infinity}) ->
	State#state{timeout_ref=undefined};
handler_loop_timeout(State=#state{timeout=Timeout, timeout_ref=PrevRef}) ->
	_ = case PrevRef of undefined -> ignore; PrevRef ->
		erlang:cancel_timer(PrevRef) end,
	TRef = erlang:start_timer(Timeout, self(), ?MODULE),
	State#state{timeout_ref=TRef}.

-spec handler_loop(#state{}, any(), binary())
	-> {ok, cowboy_middleware:env()}.
handler_loop(State=#state{socket=Socket, messages={OK, Closed, Error},
		timeout_ref=TRef}, HandlerState, SoFar) ->
	receive
		{OK, Socket, Data} ->
			State2 = handler_loop_timeout(State),
			websocket_data(State2, HandlerState,
				<< SoFar/binary, Data/binary >>);
		{Closed, Socket} ->
			terminate(State, HandlerState, {error, closed});
		{Error, Socket, Reason} ->
			terminate(State, HandlerState, {error, Reason});
		{timeout, TRef, ?MODULE} ->
			websocket_close(State, HandlerState, timeout);
		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
			handler_loop(State, HandlerState, SoFar);
		Message ->
			handler_call(State, HandlerState,
				SoFar, websocket_info, Message, fun handler_before_loop/3)
	end.

-spec websocket_data(#state{}, any(), binary())
	-> {ok, cowboy_middleware:env()}.
websocket_data(State=#state{frag_state=FragState, extensions=Extensions}, HandlerState, Data) ->
	case cow_ws:parse_header(Data, Extensions, FragState) of
		%% All frames sent from the client to the server are masked.
		{_, _, _, _, undefined, _} ->
			websocket_close(State, HandlerState, {error, badframe});
		{Type, FragState2, Rsv, Len, MaskKey, Rest} ->
			websocket_payload(State#state{frag_state=FragState2}, HandlerState, Type, Len, MaskKey, Rsv, undefined, <<>>, 0, Rest);
		more ->
			handler_before_loop(State, HandlerState, Data);
		error ->
			websocket_close(State, HandlerState, {error, badframe})
	end.

websocket_payload(State=#state{frag_state=FragState, utf8_state=Incomplete, extensions=Extensions},
		HandlerState, Type, Len, MaskKey, Rsv, CloseCode, Unmasked, UnmaskedLen, Data) ->
	case cow_ws:parse_payload(Data, MaskKey, Incomplete, UnmaskedLen, Type, Len, FragState, Extensions, Rsv) of
		{ok, CloseCode2, Payload, Utf8State, Rest} ->
			websocket_dispatch(State#state{utf8_state=Utf8State},
				HandlerState, Type, << Unmasked/binary, Payload/binary >>, CloseCode2, Rest);
		{ok, Payload, Utf8State, Rest} ->
			websocket_dispatch(State#state{utf8_state=Utf8State},
				HandlerState, Type, << Unmasked/binary, Payload/binary >>, CloseCode, Rest);
		{more, CloseCode2, Payload, Utf8State} ->
			websocket_payload_loop(State#state{utf8_state=Utf8State},
				HandlerState, Type, Len - byte_size(Data), MaskKey, Rsv, CloseCode2,
				<< Unmasked/binary, Payload/binary >>, UnmaskedLen + byte_size(Data));
		{more, Payload, Utf8State} ->
			websocket_payload_loop(State#state{utf8_state=Utf8State},
				HandlerState, Type, Len - byte_size(Data), MaskKey, Rsv, CloseCode,
				<< Unmasked/binary, Payload/binary >>, UnmaskedLen + byte_size(Data));
		Error = {error, _Reason} ->
			websocket_close(State, HandlerState, Error)
	end.

websocket_payload_loop(State=#state{socket=Socket, transport=Transport,
		messages={OK, Closed, Error}, timeout_ref=TRef},
		HandlerState, Type, Len, MaskKey, Rsv, CloseCode, Unmasked, UnmaskedLen) ->
	Transport:setopts(Socket, [{active, once}]),
	receive
		{OK, Socket, Data} ->
			State2 = handler_loop_timeout(State),
			websocket_payload(State2, HandlerState,
				Type, Len, MaskKey, Rsv, CloseCode, Unmasked, UnmaskedLen, Data);
		{Closed, Socket} ->
			terminate(State, HandlerState, {error, closed});
		{Error, Socket, Reason} ->
			terminate(State, HandlerState, {error, Reason});
		{timeout, TRef, ?MODULE} ->
			websocket_close(State, HandlerState, timeout);
		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
			websocket_payload_loop(State, HandlerState,
				Type, Len, MaskKey, Rsv, CloseCode, Unmasked, UnmaskedLen);
		Message ->
			handler_call(State, HandlerState,
				<<>>, websocket_info, Message,
				fun (State2, HandlerState2, _) ->
					websocket_payload_loop(State2, HandlerState2,
						Type, Len, MaskKey, Rsv, CloseCode, Unmasked, UnmaskedLen)
				end)
	end.

websocket_dispatch(State=#state{socket=Socket, transport=Transport, frag_state=FragState, frag_buffer=SoFar, extensions=Extensions},
		HandlerState, Type0, Payload0, CloseCode0, RemainingData) ->
	case cow_ws:make_frame(Type0, Payload0, CloseCode0, FragState) of
		%% @todo Allow receiving fragments.
		{fragment, nofin, _, Payload} ->
			websocket_data(State#state{frag_buffer= << SoFar/binary, Payload/binary >>}, HandlerState, RemainingData);
		{fragment, fin, Type, Payload} ->
			handler_call(State#state{frag_state=undefined, frag_buffer= <<>>}, HandlerState, RemainingData,
				websocket_handle, {Type, << SoFar/binary, Payload/binary >>}, fun websocket_data/3);
		close ->
			websocket_close(State, HandlerState, remote);
		{close, CloseCode, Payload} ->
			websocket_close(State, HandlerState, {remote, CloseCode, Payload});
		Frame = ping ->
			Transport:send(Socket, cow_ws:frame(pong, Extensions)),
			handler_call(State, HandlerState, RemainingData, websocket_handle, Frame, fun websocket_data/3);
		Frame = {ping, Payload} ->
			Transport:send(Socket, cow_ws:frame({pong, Payload}, Extensions)),
			handler_call(State, HandlerState, RemainingData, websocket_handle, Frame, fun websocket_data/3);
		Frame ->
			handler_call(State, HandlerState, RemainingData, websocket_handle, Frame, fun websocket_data/3)
	end.

-spec handler_call(#state{}, any(), binary(), atom(), any(), fun()) -> no_return().
handler_call(State=#state{handler=Handler}, HandlerState,
		RemainingData, Callback, Message, NextState) ->
	try case Callback of
		websocket_init -> Handler:websocket_init(HandlerState);
		_ -> Handler:Callback(Message, HandlerState)
	end of
		{ok, HandlerState2} ->
			NextState(State, HandlerState2, RemainingData);
		{ok, HandlerState2, hibernate} ->
			NextState(State#state{hibernate=true}, HandlerState2, RemainingData);
		{reply, Payload, HandlerState2} ->
			case websocket_send(Payload, State) of
				ok ->
					NextState(State, HandlerState2, RemainingData);
				stop ->
					terminate(State, HandlerState2, stop);
				Error = {error, _} ->
					terminate(State, HandlerState2, Error)
			end;
		{reply, Payload, HandlerState2, hibernate} ->
			case websocket_send(Payload, State) of
				ok ->
					NextState(State#state{hibernate=true},
						HandlerState2, RemainingData);
				stop ->
					terminate(State, HandlerState2, stop);
				Error = {error, _} ->
					terminate(State, HandlerState2, Error)
			end;
		{stop, HandlerState2} ->
			websocket_close(State, HandlerState2, stop)
	catch Class:Reason ->
		websocket_send_close(State, {crash, Class, Reason}),
		handler_terminate(State, HandlerState, {crash, Class, Reason}),
		erlang:raise(Class, Reason, erlang:get_stacktrace())
	end.

-spec websocket_send(cow_ws:frame(), #state{}) -> ok | stop | {error, atom()}.
websocket_send(Frames, State) when is_list(Frames) ->
	websocket_send_many(Frames, State, []);
websocket_send(Frame, #state{socket=Socket, transport=Transport, extensions=Extensions}) ->
	Res = Transport:send(Socket, cow_ws:frame(Frame, Extensions)),
	case is_close_frame(Frame) of
		true -> stop;
		false -> Res
	end.

websocket_send_many([], #state{socket=Socket, transport=Transport}, Acc) ->
	Transport:send(Socket, lists:reverse(Acc));
websocket_send_many([Frame|Tail], State=#state{socket=Socket, transport=Transport,
		extensions=Extensions}, Acc0) ->
	Acc = [cow_ws:frame(Frame, Extensions)|Acc0],
	case is_close_frame(Frame) of
		true ->
			_ = Transport:send(Socket, lists:reverse(Acc)),
			stop;
		false ->
			websocket_send_many(Tail, State, Acc)
	end.

is_close_frame(close) -> true;
is_close_frame({close, _}) -> true;
is_close_frame({close, _, _}) -> true;
is_close_frame(_) -> false.

-spec websocket_close(#state{}, any(), terminate_reason()) -> no_return().
websocket_close(State, HandlerState, Reason) ->
	websocket_send_close(State, Reason),
	terminate(State, HandlerState, Reason).

websocket_send_close(#state{socket=Socket, transport=Transport,
		extensions=Extensions}, Reason) ->
	_ = case Reason of
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
	ok.

-spec terminate(#state{}, any(), terminate_reason()) -> no_return().
terminate(State, HandlerState, Reason) ->
	handler_terminate(State, HandlerState, Reason),
	exit(normal).

handler_terminate(#state{handler=Handler, req=Req}, HandlerState, Reason) ->
	cowboy_handler:terminate(Reason, Req, HandlerState, Handler).
