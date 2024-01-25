%% Copyright (c) 2011-2024, Loïc Hoguin <essen@ninenines.eu>
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

-export([is_upgrade_request/1]).
-export([upgrade/4]).
-export([upgrade/5]).
-export([takeover/7]).
-export([loop/3]).

-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-type commands() :: [cow_ws:frame()
	| {active, boolean()}
	| {deflate, boolean()}
	| {set_options, map()}
	| {shutdown_reason, any()}
].
-export_type([commands/0]).

-type call_result(State) :: {commands(), State} | {commands(), State, hibernate}.

-type deprecated_call_result(State) :: {ok, State}
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
	-> call_result(State) | deprecated_call_result(State) when State::any().
-optional_callbacks([websocket_init/1]).

-callback websocket_handle(ping | pong | {text | binary | ping | pong, binary()}, State)
	-> call_result(State) | deprecated_call_result(State) when State::any().
-callback websocket_info(any(), State)
	-> call_result(State) | deprecated_call_result(State) when State::any().

-callback terminate(any(), cowboy_req:req(), any()) -> ok.
-optional_callbacks([terminate/3]).

-type opts() :: #{
	active_n => pos_integer(),
	compress => boolean(),
	deflate_opts => cow_ws:deflate_opts(),
	idle_timeout => timeout(),
	max_frame_size => non_neg_integer() | infinity,
	req_filter => fun((cowboy_req:req()) -> map()),
	validate_utf8 => boolean()
}.
-export_type([opts/0]).

-record(state, {
	parent :: undefined | pid(),
	ref :: ranch:ref(),
	socket = undefined :: inet:socket() | {pid(), cowboy_stream:streamid()} | undefined,
	transport = undefined :: module() | undefined,
	opts = #{} :: opts(),
	active = true :: boolean(),
	handler :: module(),
	key = undefined :: undefined | binary(),
	timeout_ref = undefined :: undefined | reference(),
	messages = undefined :: undefined | {atom(), atom(), atom()}
		| {atom(), atom(), atom(), atom()},
	hibernate = false :: boolean(),
	frag_state = undefined :: cow_ws:frag_state(),
	frag_buffer = <<>> :: binary(),
	utf8_state :: cow_ws:utf8_state(),
	deflate = true :: boolean(),
	extensions = #{} :: map(),
	req = #{} :: map(),
	shutdown_reason = normal :: any()
}).

%% Because the HTTP/1.1 and HTTP/2 handshakes are so different,
%% this function is necessary to figure out whether a request
%% is trying to upgrade to the Websocket protocol.

-spec is_upgrade_request(cowboy_req:req()) -> boolean().
is_upgrade_request(#{version := 'HTTP/2', method := <<"CONNECT">>, protocol := Protocol}) ->
	<<"websocket">> =:= cowboy_bstr:to_lower(Protocol);
is_upgrade_request(Req=#{version := 'HTTP/1.1', method := <<"GET">>}) ->
	ConnTokens = cowboy_req:parse_header(<<"connection">>, Req, []),
	case lists:member(<<"upgrade">>, ConnTokens) of
		false ->
			false;
		true ->
			UpgradeTokens = cowboy_req:parse_header(<<"upgrade">>, Req),
			lists:member(<<"websocket">>, UpgradeTokens)
	end;
is_upgrade_request(_) ->
	false.

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
upgrade(Req0=#{version := Version}, Env, Handler, HandlerState, Opts) ->
	FilteredReq = case maps:get(req_filter, Opts, undefined) of
		undefined -> maps:with([method, version, scheme, host, port, path, qs, peer], Req0);
		FilterFun -> FilterFun(Req0)
	end,
	Utf8State = case maps:get(validate_utf8, Opts, true) of
		true -> 0;
		false -> undefined
	end,
	State0 = #state{opts=Opts, handler=Handler, utf8_state=Utf8State, req=FilteredReq},
	try websocket_upgrade(State0, Req0) of
		{ok, State, Req} ->
			websocket_handshake(State, Req, HandlerState, Env);
		%% The status code 426 is specific to HTTP/1.1 connections.
		{error, upgrade_required} when Version =:= 'HTTP/1.1' ->
			{ok, cowboy_req:reply(426, #{
				<<"connection">> => <<"upgrade">>,
				<<"upgrade">> => <<"websocket">>
			}, Req0), Env};
		%% Use a generic 400 error for HTTP/2.
		{error, upgrade_required} ->
			{ok, cowboy_req:reply(400, Req0), Env}
	catch _:_ ->
		%% @todo Probably log something here?
		%% @todo Test that we can have 2 /ws 400 status code in a row on the same connection.
		%% @todo Does this even work?
		{ok, cowboy_req:reply(400, Req0), Env}
	end.

websocket_upgrade(State, Req=#{version := Version}) ->
	case is_upgrade_request(Req) of
		false ->
			{error, upgrade_required};
		true when Version =:= 'HTTP/1.1' ->
			Key = cowboy_req:header(<<"sec-websocket-key">>, Req),
			false = Key =:= undefined,
			websocket_version(State#state{key=Key}, Req);
		true ->
			websocket_version(State, Req)
	end.

websocket_version(State, Req) ->
	WsVersion = cowboy_req:parse_header(<<"sec-websocket-version">>, Req),
	case WsVersion of
		7 -> ok;
		8 -> ok;
		13 -> ok
	end,
	websocket_extensions(State, Req#{websocket_version => WsVersion}).

websocket_extensions(State=#state{opts=Opts}, Req) ->
	%% @todo We want different options for this. For example
	%% * compress everything auto
	%% * compress only text auto
	%% * compress only binary auto
	%% * compress nothing auto (but still enabled it)
	%% * disable compression
	Compress = maps:get(compress, Opts, false),
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
%% For HTTP/2 we ARE on the controlling process and do NOT want to update the owner.
websocket_extensions(State=#state{opts=Opts, extensions=Extensions},
		Req=#{pid := Pid, version := Version},
		[{<<"permessage-deflate">>, Params}|Tail], RespHeader) ->
	DeflateOpts0 = maps:get(deflate_opts, Opts, #{}),
	DeflateOpts = case Version of
		'HTTP/1.1' -> DeflateOpts0#{owner => Pid};
		_ -> DeflateOpts0
	end,
	try cow_ws:negotiate_permessage_deflate(Params, Extensions, DeflateOpts) of
		{ok, RespExt, Extensions2} ->
			websocket_extensions(State#state{extensions=Extensions2},
				Req, Tail, [<<", ">>, RespExt|RespHeader]);
		ignore ->
			websocket_extensions(State, Req, Tail, RespHeader)
	catch exit:{error, incompatible_zlib_version, _} ->
		websocket_extensions(State, Req, Tail, RespHeader)
	end;
websocket_extensions(State=#state{opts=Opts, extensions=Extensions},
		Req=#{pid := Pid, version := Version},
		[{<<"x-webkit-deflate-frame">>, Params}|Tail], RespHeader) ->
	DeflateOpts0 = maps:get(deflate_opts, Opts, #{}),
	DeflateOpts = case Version of
		'HTTP/1.1' -> DeflateOpts0#{owner => Pid};
		_ -> DeflateOpts0
	end,
	try cow_ws:negotiate_x_webkit_deflate_frame(Params, Extensions, DeflateOpts) of
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
		Req=#{version := 'HTTP/1.1', pid := Pid, streamid := StreamID},
		HandlerState, Env) ->
	Challenge = base64:encode(crypto:hash(sha,
		<< Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>)),
	%% @todo We don't want date and server headers.
	Headers = cowboy_req:response_headers(#{
		<<"connection">> => <<"Upgrade">>,
		<<"upgrade">> => <<"websocket">>,
		<<"sec-websocket-accept">> => Challenge
	}, Req),
	Pid ! {{Pid, StreamID}, {switch_protocol, Headers, ?MODULE, {State, HandlerState}}},
	{ok, Req, Env};
%% For HTTP/2 we do not let the process die, we instead keep it
%% for the Websocket stream. This is because in HTTP/2 we only
%% have a stream, it doesn't take over the whole connection.
websocket_handshake(State, Req=#{ref := Ref, pid := Pid, streamid := StreamID},
		HandlerState, _Env) ->
	%% @todo We don't want date and server headers.
	Headers = cowboy_req:response_headers(#{}, Req),
	Pid ! {{Pid, StreamID}, {switch_protocol, Headers, ?MODULE, {State, HandlerState}}},
	takeover(Pid, Ref, {Pid, StreamID}, undefined, undefined, <<>>,
		{State, HandlerState}).

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

-spec takeover(pid(), ranch:ref(), inet:socket() | {pid(), cowboy_stream:streamid()},
	module() | undefined, any(), binary(),
	{#state{}, any()}) -> no_return().
takeover(Parent, Ref, Socket, Transport, _Opts, Buffer,
		{State0=#state{handler=Handler}, HandlerState}) ->
	%% @todo We should have an option to disable this behavior.
	ranch:remove_connection(Ref),
	Messages = case Transport of
		undefined -> undefined;
		_ -> Transport:messages()
	end,
	State = loop_timeout(State0#state{parent=Parent,
		ref=Ref, socket=Socket, transport=Transport,
		key=undefined, messages=Messages}),
	%% We call parse_header/3 immediately because there might be
	%% some data in the buffer that was sent along with the handshake.
	%% While it is not allowed by the protocol to send frames immediately,
	%% we still want to process that data if any.
	case erlang:function_exported(Handler, websocket_init, 1) of
		true -> handler_call(State, HandlerState, #ps_header{buffer=Buffer},
			websocket_init, undefined, fun after_init/3);
		false -> after_init(State, HandlerState, #ps_header{buffer=Buffer})
	end.

after_init(State=#state{active=true}, HandlerState, ParseState) ->
	%% Enable active,N for HTTP/1.1, and auto read_body for HTTP/2.
	%% We must do this only after calling websocket_init/1 (if any)
	%% to give the handler a chance to disable active mode immediately.
	setopts_active(State),
	maybe_read_body(State),
	parse_header(State, HandlerState, ParseState);
after_init(State, HandlerState, ParseState) ->
	parse_header(State, HandlerState, ParseState).

%% We have two ways of reading the body for Websocket. For HTTP/1.1
%% we have full control of the socket and can therefore use active,N.
%% For HTTP/2 we are just a stream, and are instead using read_body
%% (automatic mode). Technically HTTP/2 will only go passive after
%% receiving the next data message, while HTTP/1.1 goes passive
%% immediately but there might still be data to be processed in
%% the message queue.

setopts_active(#state{transport=undefined}) ->
	ok;
setopts_active(#state{socket=Socket, transport=Transport, opts=Opts}) ->
	N = maps:get(active_n, Opts, 100),
	Transport:setopts(Socket, [{active, N}]).

maybe_read_body(#state{socket=Stream={Pid, _}, transport=undefined, active=true}) ->
	%% @todo Keep Ref around.
	ReadBodyRef = make_ref(),
	Pid ! {Stream, {read_body, self(), ReadBodyRef, auto, infinity}},
	ok;
maybe_read_body(_) ->
	ok.

active(State) ->
	setopts_active(State),
	maybe_read_body(State),
	State#state{active=true}.

passive(State=#state{transport=undefined}) ->
	%% Unfortunately we cannot currently cancel read_body.
	%% But that's OK, we will just stop reading the body
	%% after the next message.
	State#state{active=false};
passive(State=#state{socket=Socket, transport=Transport, messages=Messages}) ->
	Transport:setopts(Socket, [{active, false}]),
	flush_passive(Socket, Messages),
	State#state{active=false}.

flush_passive(Socket, Messages) ->
	receive
		{Passive, Socket} when Passive =:= element(4, Messages);
				%% Hardcoded for compatibility with Ranch 1.x.
				Passive =:= tcp_passive; Passive =:= ssl_passive ->
			flush_passive(Socket, Messages)
	after 0 ->
		ok
	end.

before_loop(State=#state{hibernate=true}, HandlerState, ParseState) ->
	proc_lib:hibernate(?MODULE, loop,
		[State#state{hibernate=false}, HandlerState, ParseState]);
before_loop(State, HandlerState, ParseState) ->
	loop(State, HandlerState, ParseState).

-spec loop_timeout(#state{}) -> #state{}.
loop_timeout(State=#state{opts=Opts, timeout_ref=PrevRef}) ->
	_ = case PrevRef of
		undefined -> ignore;
		PrevRef -> erlang:cancel_timer(PrevRef)
	end,
	case maps:get(idle_timeout, Opts, 60000) of
		infinity ->
			State#state{timeout_ref=undefined};
		Timeout ->
			TRef = erlang:start_timer(Timeout, self(), ?MODULE),
			State#state{timeout_ref=TRef}
	end.

-spec loop(#state{}, any(), parse_state()) -> no_return().
loop(State=#state{parent=Parent, socket=Socket, messages=Messages,
		timeout_ref=TRef}, HandlerState, ParseState) ->
	receive
		%% Socket messages. (HTTP/1.1)
		{OK, Socket, Data} when OK =:= element(1, Messages) ->
			State2 = loop_timeout(State),
			parse(State2, HandlerState, ParseState, Data);
		{Closed, Socket} when Closed =:= element(2, Messages) ->
			terminate(State, HandlerState, {error, closed});
		{Error, Socket, Reason} when Error =:= element(3, Messages) ->
			terminate(State, HandlerState, {error, Reason});
		{Passive, Socket} when Passive =:= element(4, Messages);
				%% Hardcoded for compatibility with Ranch 1.x.
				Passive =:= tcp_passive; Passive =:= ssl_passive ->
			setopts_active(State),
			loop(State, HandlerState, ParseState);
		%% Body reading messages. (HTTP/2)
		{request_body, _Ref, nofin, Data} ->
			maybe_read_body(State),
			State2 = loop_timeout(State),
			parse(State2, HandlerState, ParseState, Data);
		%% @todo We need to handle this case as if it was an {error, closed}
		%% but not before we finish processing frames. We probably should have
		%% a check in before_loop to let us stop looping if a flag is set.
		{request_body, _Ref, fin, _, Data} ->
			maybe_read_body(State),
			State2 = loop_timeout(State),
			parse(State2, HandlerState, ParseState, Data);
		%% Timeouts.
		{timeout, TRef, ?MODULE} ->
			websocket_close(State, HandlerState, timeout);
		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
			before_loop(State, HandlerState, ParseState);
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
			before_loop(State, HandlerState, ParseState);
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

parse_header(State=#state{opts=Opts, frag_state=FragState, extensions=Extensions},
		HandlerState, ParseState=#ps_header{buffer=Data}) ->
	MaxFrameSize = maps:get(max_frame_size, Opts, infinity),
	case cow_ws:parse_header(Data, Extensions, FragState) of
		%% All frames sent from the client to the server are masked.
		{_, _, _, _, undefined, _} ->
			websocket_close(State, HandlerState, {error, badframe});
		{_, _, _, Len, _, _} when Len > MaxFrameSize ->
			websocket_close(State, HandlerState, {error, badsize});
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

dispatch_frame(State=#state{opts=Opts, frag_state=FragState, frag_buffer=SoFar}, HandlerState,
		#ps_payload{type=Type0, unmasked=Payload0, close_code=CloseCode0}, RemainingData) ->
	MaxFrameSize = maps:get(max_frame_size, Opts, infinity),
	case cow_ws:make_frame(Type0, Payload0, CloseCode0, FragState) of
		%% @todo Allow receiving fragments.
		{fragment, _, _, Payload} when byte_size(Payload) + byte_size(SoFar) > MaxFrameSize ->
			websocket_close(State, HandlerState, {error, badsize});
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
			transport_send(State, nofin, frame(pong, State)),
			handler_call(State, HandlerState,
				#ps_header{buffer=RemainingData},
				websocket_handle, Frame, fun parse_header/3);
		Frame = {ping, Payload} ->
			transport_send(State, nofin, frame({pong, Payload}, State)),
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
		{Commands, HandlerState2} when is_list(Commands) ->
			handler_call_result(State,
				HandlerState2, ParseState, NextState, Commands);
		{Commands, HandlerState2, hibernate} when is_list(Commands) ->
			handler_call_result(State#state{hibernate=true},
				HandlerState2, ParseState, NextState, Commands);
		%% The following call results are deprecated.
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
	catch Class:Reason:Stacktrace ->
		websocket_send_close(State, {crash, Class, Reason}),
		handler_terminate(State, HandlerState, {crash, Class, Reason}),
		erlang:raise(Class, Reason, Stacktrace)
	end.

-spec handler_call_result(#state{}, any(), parse_state(), fun(), commands()) -> no_return().
handler_call_result(State0, HandlerState, ParseState, NextState, Commands) ->
	case commands(Commands, State0, []) of
		{ok, State} ->
			NextState(State, HandlerState, ParseState);
		{stop, State} ->
			terminate(State, HandlerState, stop);
		{Error = {error, _}, State} ->
			terminate(State, HandlerState, Error)
	end.

commands([], State, []) ->
	{ok, State};
commands([], State, Data) ->
	Result = transport_send(State, nofin, lists:reverse(Data)),
	{Result, State};
commands([{active, Active}|Tail], State0=#state{active=Active0}, Data) when is_boolean(Active) ->
	State = if
		Active, not Active0 ->
			active(State0);
		Active0, not Active ->
			passive(State0);
		true ->
			State0
	end,
	commands(Tail, State#state{active=Active}, Data);
commands([{deflate, Deflate}|Tail], State, Data) when is_boolean(Deflate) ->
	commands(Tail, State#state{deflate=Deflate}, Data);
commands([{set_options, SetOpts}|Tail], State0=#state{opts=Opts}, Data) ->
	State = case SetOpts of
		#{idle_timeout := IdleTimeout} ->
			loop_timeout(State0#state{opts=Opts#{idle_timeout => IdleTimeout}});
		_ ->
			State0
	end,
	commands(Tail, State, Data);
commands([{shutdown_reason, ShutdownReason}|Tail], State, Data) ->
	commands(Tail, State#state{shutdown_reason=ShutdownReason}, Data);
commands([Frame|Tail], State, Data0) ->
	Data = [frame(Frame, State)|Data0],
	case is_close_frame(Frame) of
		true ->
			_ = transport_send(State, fin, lists:reverse(Data)),
			{stop, State};
		false ->
			commands(Tail, State, Data)
	end.

transport_send(#state{socket=Stream={Pid, _}, transport=undefined}, IsFin, Data) ->
	Pid ! {Stream, {data, IsFin, Data}},
	ok;
transport_send(#state{socket=Socket, transport=Transport}, _, Data) ->
	Transport:send(Socket, Data).

-spec websocket_send(cow_ws:frame(), #state{}) -> ok | stop | {error, atom()}.
websocket_send(Frames, State) when is_list(Frames) ->
	websocket_send_many(Frames, State, []);
websocket_send(Frame, State) ->
	Data = frame(Frame, State),
	case is_close_frame(Frame) of
		true ->
			_ = transport_send(State, fin, Data),
			stop;
		false ->
			transport_send(State, nofin, Data)
	end.

websocket_send_many([], State, Acc) ->
	transport_send(State, nofin, lists:reverse(Acc));
websocket_send_many([Frame|Tail], State, Acc0) ->
	Acc = [frame(Frame, State)|Acc0],
	case is_close_frame(Frame) of
		true ->
			_ = transport_send(State, fin, lists:reverse(Acc)),
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

websocket_send_close(State, Reason) ->
	_ = case Reason of
		Normal when Normal =:= stop; Normal =:= timeout ->
			transport_send(State, fin, frame({close, 1000, <<>>}, State));
		{error, badframe} ->
			transport_send(State, fin, frame({close, 1002, <<>>}, State));
		{error, badencoding} ->
			transport_send(State, fin, frame({close, 1007, <<>>}, State));
		{error, badsize} ->
			transport_send(State, fin, frame({close, 1009, <<>>}, State));
		{crash, _, _} ->
			transport_send(State, fin, frame({close, 1011, <<>>}, State));
		remote ->
			transport_send(State, fin, frame(close, State));
		{remote, Code, _} ->
			transport_send(State, fin, frame({close, Code, <<>>}, State))
	end,
	ok.

%% Don't compress frames while deflate is disabled.
frame(Frame, #state{deflate=false, extensions=Extensions}) ->
	cow_ws:frame(Frame, Extensions#{deflate => false});
frame(Frame, #state{extensions=Extensions}) ->
	cow_ws:frame(Frame, Extensions).

-spec terminate(#state{}, any(), terminate_reason()) -> no_return().
terminate(State=#state{shutdown_reason=Shutdown}, HandlerState, Reason) ->
	handler_terminate(State, HandlerState, Reason),
	case Shutdown of
		normal -> exit(normal);
		_ -> exit({shutdown, Shutdown})
	end.

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
