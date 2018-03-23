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
-export([loop/3]).

-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

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
	parent :: undefined | pid(),
	ref :: ranch:ref(),
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
			websocket_handshake(State, Req, HandlerState, Env);
		{error, upgrade_required} ->
			{ok, cowboy_req:reply(426, #{
				<<"connection">> => <<"upgrade">>,
				<<"upgrade">> => <<"websocket">>
			}, Req0), Env}
	catch _:_ ->
		%% @todo Probably log something here?
		%% @todo Test that we can have 2 /ws 400 status code in a row on the same connection.
		%% @todo Does this even work?
		{ok, cowboy_req:reply(400, Req0), Env}
	end.

websocket_upgrade(State, Req) ->
	ConnTokens = cowboy_req:parse_header(<<"connection">>, Req, []),
	case lists:member(<<"upgrade">>, ConnTokens) of
		false ->
			{error, upgrade_required};
		true ->
			UpgradeTokens = cowboy_req:parse_header(<<"upgrade">>, Req, []),
			case lists:member(<<"websocket">>, UpgradeTokens) of
				false ->
					{error, upgrade_required};
				true ->
					Version = cowboy_req:header(<<"sec-websocket-version">>, Req),
					IntVersion = binary_to_integer(Version),
					true = (IntVersion =:= 7) orelse (IntVersion =:= 8)
						orelse (IntVersion =:= 13),
					Key = cowboy_req:header(<<"sec-websocket-key">>, Req),
					false = Key =:= undefined,
					websocket_extensions(State#state{key=Key}, Req#{websocket_version => IntVersion})
			end
	end.

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
	try cow_ws:negotiate_permessage_deflate(Params, Extensions, Opts#{owner => Pid}) of
		{ok, RespExt, Extensions2} ->
			websocket_extensions(State#state{extensions=Extensions2},
				Req, Tail, [<<", ">>, RespExt|RespHeader]);
		ignore ->
			websocket_extensions(State, Req, Tail, RespHeader)
	catch exit:{error, incompatible_zlib_version, _} ->
		websocket_extensions(State, Req, Tail, RespHeader)
	end;
websocket_extensions(State=#state{extensions=Extensions}, Req=#{pid := Pid},
		[{<<"x-webkit-deflate-frame">>, Params}|Tail], RespHeader) ->
	%% @todo Make deflate options configurable.
	Opts = #{level => best_compression, mem_level => 8, strategy => default},
	try cow_ws:negotiate_x_webkit_deflate_frame(Params, Extensions, Opts#{owner => Pid}) of
		{ok, RespExt, Extensions2} ->
			websocket_extensions(State#state{extensions=Extensions2},
				Req, Tail, [<<", ">>, RespExt|RespHeader]);
		ignore ->
			websocket_extensions(State, Req, Tail, RespHeader)
	catch exit:{error, incompatible_zlib_version, _} ->
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
	%% @todo We don't want date and server headers.
	Headers = cowboy_req:response_headers(#{
		<<"connection">> => <<"Upgrade">>,
		<<"upgrade">> => <<"websocket">>,
		<<"sec-websocket-accept">> => Challenge
	}, Req),
	Pid ! {{Pid, StreamID}, {switch_protocol, Headers, ?MODULE, {State, HandlerState}}},
	{ok, Req, Env}.

%% Connection process.

-record(ps_header, {
	buffer = <<>> :: binary()
}).

-record(ps_payload, {
	type :: cow_ws:frame_type(),
	len :: non_neg_integer(),
	mask_key :: cow_ws:mask_key(),
	rsv :: cow_ws:rsv(),
	close_code = undefined :: undefined | cow_ws:close_code(),
	unmasked = <<>> :: binary(),
	unmasked_len = 0 :: non_neg_integer(),
	buffer = <<>> :: binary()
}).

-type parse_state() :: #ps_header{} | #ps_payload{}.

-spec takeover(pid(), ranch:ref(), inet:socket(), module(), any(), binary(),
	{#state{}, any()}) -> no_return().
takeover(Parent, Ref, Socket, Transport, _Opts, Buffer,
		{State0=#state{handler=Handler}, HandlerState}) ->
	%% @todo We should have an option to disable this behavior.
	ranch:remove_connection(Ref),
	State = loop_timeout(State0#state{parent=Parent,
		ref=Ref, socket=Socket, transport=Transport,
		key=undefined, messages=Transport:messages()}),
	case erlang:function_exported(Handler, websocket_init, 1) of
		true -> handler_call(State, HandlerState, #ps_header{buffer=Buffer},
			websocket_init, undefined, fun before_loop/3);
		false -> before_loop(State, HandlerState, #ps_header{buffer=Buffer})
	end.

before_loop(State=#state{socket=Socket, transport=Transport, hibernate=true},
		HandlerState, ParseState) ->
	Transport:setopts(Socket, [{active, once}]),
	proc_lib:hibernate(?MODULE, loop,
		[State#state{hibernate=false}, HandlerState, ParseState]);
before_loop(State=#state{socket=Socket, transport=Transport},
		HandlerState, ParseState) ->
	Transport:setopts(Socket, [{active, once}]),
	loop(State, HandlerState, ParseState).

-spec loop_timeout(#state{}) -> #state{}.
loop_timeout(State=#state{timeout=infinity}) ->
	State#state{timeout_ref=undefined};
loop_timeout(State=#state{timeout=Timeout, timeout_ref=PrevRef}) ->
	_ = case PrevRef of undefined -> ignore; PrevRef ->
		erlang:cancel_timer(PrevRef) end,
	TRef = erlang:start_timer(Timeout, self(), ?MODULE),
	State#state{timeout_ref=TRef}.

-spec loop(#state{}, any(), parse_state()) -> no_return().
loop(State=#state{parent=Parent, socket=Socket, messages={OK, Closed, Error},
		timeout_ref=TRef}, HandlerState, ParseState) ->
	receive
		{OK, Socket, Data} ->
			State2 = loop_timeout(State),
			parse(State2, HandlerState, ParseState, Data);
		{Closed, Socket} ->
			terminate(State, HandlerState, {error, closed});
		{Error, Socket, Reason} ->
			terminate(State, HandlerState, {error, Reason});
		{timeout, TRef, ?MODULE} ->
			websocket_close(State, HandlerState, timeout);
		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
			loop(State, HandlerState, ParseState);
		%% System messages.
		{'EXIT', Parent, Reason} ->
			%% @todo We should exit gracefully.
			exit(Reason);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
				{State, HandlerState, ParseState});
		%% Calls from supervisor module.
		{'$gen_call', From, Call} ->
			cowboy_children:handle_supervisor_call(Call, From, [], ?MODULE),
			loop(State, HandlerState, ParseState);
		Message ->
			handler_call(State, HandlerState, ParseState,
				websocket_info, Message, fun before_loop/3)
	end.

parse(State, HandlerState, PS=#ps_header{buffer=Buffer}, Data) ->
	parse_header(State, HandlerState, PS#ps_header{
		buffer= <<Buffer/binary, Data/binary>>});
parse(State, HandlerState, PS=#ps_payload{buffer=Buffer}, Data) ->
	parse_payload(State, HandlerState, PS#ps_payload{buffer= <<>>},
		<<Buffer/binary, Data/binary>>).

parse_header(State=#state{frag_state=FragState, extensions=Extensions}, HandlerState,
		ParseState=#ps_header{buffer=Data}) ->
	case cow_ws:parse_header(Data, Extensions, FragState) of
		%% All frames sent from the client to the server are masked.
		{_, _, _, _, undefined, _} ->
			websocket_close(State, HandlerState, {error, badframe});
		{Type, FragState2, Rsv, Len, MaskKey, Rest} ->
			parse_payload(State#state{frag_state=FragState2}, HandlerState,
				#ps_payload{type=Type, len=Len, mask_key=MaskKey, rsv=Rsv}, Rest);
		more ->
			before_loop(State, HandlerState, ParseState);
		error ->
			websocket_close(State, HandlerState, {error, badframe})
	end.

parse_payload(State=#state{frag_state=FragState, utf8_state=Incomplete, extensions=Extensions},
		HandlerState, ParseState=#ps_payload{
			type=Type, len=Len, mask_key=MaskKey, rsv=Rsv,
			unmasked=Unmasked, unmasked_len=UnmaskedLen}, Data) ->
	case cow_ws:parse_payload(Data, MaskKey, Incomplete, UnmaskedLen,
			Type, Len, FragState, Extensions, Rsv) of
		{ok, CloseCode, Payload, Utf8State, Rest} ->
			dispatch_frame(State#state{utf8_state=Utf8State}, HandlerState,
				ParseState#ps_payload{unmasked= <<Unmasked/binary, Payload/binary>>,
					close_code=CloseCode}, Rest);
		{ok, Payload, Utf8State, Rest} ->
			dispatch_frame(State#state{utf8_state=Utf8State}, HandlerState,
				ParseState#ps_payload{unmasked= <<Unmasked/binary, Payload/binary>>},
				Rest);
		{more, CloseCode, Payload, Utf8State} ->
			before_loop(State#state{utf8_state=Utf8State}, HandlerState,
				ParseState#ps_payload{len=Len - byte_size(Data), close_code=CloseCode,
					unmasked= <<Unmasked/binary, Payload/binary>>,
					unmasked_len=UnmaskedLen + byte_size(Data)});
		{more, Payload, Utf8State} ->
			before_loop(State#state{utf8_state=Utf8State}, HandlerState,
				ParseState#ps_payload{len=Len - byte_size(Data),
					unmasked= <<Unmasked/binary, Payload/binary>>,
					unmasked_len=UnmaskedLen + byte_size(Data)});
		Error = {error, _Reason} ->
			websocket_close(State, HandlerState, Error)
	end.

dispatch_frame(State=#state{socket=Socket, transport=Transport,
		frag_state=FragState, frag_buffer=SoFar, extensions=Extensions},
		HandlerState, #ps_payload{type=Type0, unmasked=Payload0, close_code=CloseCode0},
		RemainingData) ->
	case cow_ws:make_frame(Type0, Payload0, CloseCode0, FragState) of
		%% @todo Allow receiving fragments.
		{fragment, nofin, _, Payload} ->
			parse_header(State#state{frag_buffer= << SoFar/binary, Payload/binary >>},
				HandlerState, #ps_header{buffer=RemainingData});
		{fragment, fin, Type, Payload} ->
			handler_call(State#state{frag_state=undefined, frag_buffer= <<>>}, HandlerState,
				#ps_header{buffer=RemainingData},
				websocket_handle, {Type, << SoFar/binary, Payload/binary >>},
				fun parse_header/3);
		close ->
			websocket_close(State, HandlerState, remote);
		{close, CloseCode, Payload} ->
			websocket_close(State, HandlerState, {remote, CloseCode, Payload});
		Frame = ping ->
			Transport:send(Socket, cow_ws:frame(pong, Extensions)),
			handler_call(State, HandlerState,
				#ps_header{buffer=RemainingData},
				websocket_handle, Frame, fun parse_header/3);
		Frame = {ping, Payload} ->
			Transport:send(Socket, cow_ws:frame({pong, Payload}, Extensions)),
			handler_call(State, HandlerState,
				#ps_header{buffer=RemainingData},
				websocket_handle, Frame, fun parse_header/3);
		Frame ->
			handler_call(State, HandlerState,
				#ps_header{buffer=RemainingData},
				websocket_handle, Frame, fun parse_header/3)
	end.

handler_call(State=#state{handler=Handler}, HandlerState,
		ParseState, Callback, Message, NextState) ->
	try case Callback of
		websocket_init -> Handler:websocket_init(HandlerState);
		_ -> Handler:Callback(Message, HandlerState)
	end of
		{ok, HandlerState2} ->
			NextState(State, HandlerState2, ParseState);
		{ok, HandlerState2, hibernate} ->
			NextState(State#state{hibernate=true}, HandlerState2, ParseState);
		{reply, Payload, HandlerState2} ->
			case websocket_send(Payload, State) of
				ok ->
					NextState(State, HandlerState2, ParseState);
				stop ->
					terminate(State, HandlerState2, stop);
				Error = {error, _} ->
					terminate(State, HandlerState2, Error)
			end;
		{reply, Payload, HandlerState2, hibernate} ->
			case websocket_send(Payload, State) of
				ok ->
					NextState(State#state{hibernate=true},
						HandlerState2, ParseState);
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

%% System callbacks.

-spec system_continue(_, _, {#state{}, any(), parse_state()}) -> no_return().
system_continue(_, _, {State, HandlerState, ParseState}) ->
	loop(State, HandlerState, ParseState).

-spec system_terminate(any(), _, _, {#state{}, any(), parse_state()}) -> no_return().
system_terminate(Reason, _, _, {State, HandlerState, _}) ->
	%% @todo We should exit gracefully, if possible.
	terminate(State, HandlerState, Reason).

-spec system_code_change(Misc, _, _, _)
	-> {ok, Misc} when Misc::{#state{}, any(), parse_state()}.
system_code_change(Misc, _, _, _) ->
	{ok, Misc}.
