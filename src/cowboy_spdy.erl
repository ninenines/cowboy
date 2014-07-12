%% Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_spdy).

%% API.
-export([start_link/4]).

%% Internal.
-export([init/5]).
-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

%% Internal request process.
-export([request_init/11]).
-export([resume/5]).
-export([reply/4]).
-export([stream_reply/3]).
-export([stream_data/2]).
-export([stream_close/1]).

%% Internal transport functions.
-export([name/0]).
-export([messages/0]).
-export([recv/3]).
-export([send/2]).
-export([sendfile/2]).
-export([setopts/2]).

-type streamid() :: non_neg_integer().
-type socket() :: {pid(), streamid()}.

-record(child, {
	streamid :: streamid(),
	pid :: pid(),
	input = nofin :: fin | nofin,
	in_buffer = <<>> :: binary(),
	is_recv = false :: false | {active, socket(), pid()}
		| {passive, socket(), pid(), non_neg_integer(), reference()},
	output = nofin :: fin | nofin
}).

-record(state, {
	parent = undefined :: pid(),
	socket,
	transport,
	buffer = <<>> :: binary(),
	middlewares,
	env,
	onrequest,
	onresponse,
	peer,
	zdef,
	zinf,
	last_streamid = 0 :: streamid(),
	children = [] :: [#child{}]
}).

-type opts() :: [{env, cowboy_middleware:env()}
	| {middlewares, [module()]}
	| {onrequest, cowboy:onrequest_fun()}
	| {onresponse, cowboy:onresponse_fun()}].
-export_type([opts/0]).

%% API.

-spec start_link(any(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
	proc_lib:start_link(?MODULE, init,
		[self(), Ref, Socket, Transport, Opts]).

%% Internal.

%% Faster alternative to proplists:get_value/3.
get_value(Key, Opts, Default) ->
	case lists:keyfind(Key, 1, Opts) of
		{_, Value} -> Value;
		_ -> Default
	end.

-spec init(pid(), ranch:ref(), inet:socket(), module(), opts()) -> ok.
init(Parent, Ref, Socket, Transport, Opts) ->
	process_flag(trap_exit, true),
	ok = proc_lib:init_ack(Parent, {ok, self()}),
	{ok, Peer} = Transport:peername(Socket),
	Middlewares = get_value(middlewares, Opts, [cowboy_router, cowboy_handler]),
	Env = [{listener, Ref}|get_value(env, Opts, [])],
	OnRequest = get_value(onrequest, Opts, undefined),
	OnResponse = get_value(onresponse, Opts, undefined),
	Zdef = cow_spdy:deflate_init(),
	Zinf = cow_spdy:inflate_init(),
	ok = ranch:accept_ack(Ref),
	loop(#state{parent=Parent, socket=Socket, transport=Transport,
		middlewares=Middlewares, env=Env, onrequest=OnRequest,
		onresponse=OnResponse, peer=Peer, zdef=Zdef, zinf=Zinf}).

loop(State=#state{parent=Parent, socket=Socket, transport=Transport,
		buffer=Buffer, children=Children}) ->
	{OK, Closed, Error} = Transport:messages(),
	Transport:setopts(Socket, [{active, once}]),
	receive
		{OK, Socket, Data} ->
			parse_frame(State, << Buffer/binary, Data/binary >>);
		{Closed, Socket} ->
			terminate(State);
		{Error, Socket, _Reason} ->
			terminate(State);
		{recv, FromSocket = {Pid, StreamID}, FromPid, Length, Timeout}
				when Pid =:= self() ->
			Child = #child{in_buffer=InBuffer, is_recv=false}
				= get_child(StreamID, State),
			if
				Length =:= 0, InBuffer =/= <<>> ->
					FromPid ! {recv, FromSocket, {ok, InBuffer}},
					loop(replace_child(Child#child{in_buffer= <<>>}, State));
				byte_size(InBuffer) >= Length ->
					<< Data:Length/binary, Rest/binary >> = InBuffer,
					FromPid ! {recv, FromSocket, {ok, Data}},
					loop(replace_child(Child#child{in_buffer=Rest}, State));
				true ->
					TRef = erlang:send_after(Timeout, self(),
						{recv_timeout, FromSocket}),
					loop(replace_child(Child#child{
						is_recv={passive, FromSocket, FromPid, Length, TRef}},
						State))
			end;
		{recv_timeout, {Pid, StreamID}}
				when Pid =:= self() ->
			Child = #child{is_recv={passive, FromSocket, FromPid, _, _}}
				= get_child(StreamID, State),
			FromPid ! {recv, FromSocket, {error, timeout}},
			loop(replace_child(Child, State));
		{reply, {Pid, StreamID}, Status, Headers}
				when Pid =:= self() ->
			Child = #child{output=nofin} = get_child(StreamID, State),
			syn_reply(State, StreamID, true, Status, Headers),
			loop(replace_child(Child#child{output=fin}, State));
		{reply, {Pid, StreamID}, Status, Headers, Body}
				when Pid =:= self() ->
			Child = #child{output=nofin} = get_child(StreamID, State),
			syn_reply(State, StreamID, false, Status, Headers),
			data(State, StreamID, true, Body),
			loop(replace_child(Child#child{output=fin}, State));
		{stream_reply, {Pid, StreamID}, Status, Headers}
				when Pid =:= self() ->
			#child{output=nofin} = get_child(StreamID, State),
			syn_reply(State, StreamID, false, Status, Headers),
			loop(State);
		{stream_data, {Pid, StreamID}, Data}
				when Pid =:= self() ->
			#child{output=nofin} = get_child(StreamID, State),
			data(State, StreamID, false, Data),
			loop(State);
		{stream_close, {Pid, StreamID}}
				when Pid =:= self() ->
			Child = #child{output=nofin} = get_child(StreamID, State),
			data(State, StreamID, true, <<>>),
			loop(replace_child(Child#child{output=fin}, State));
		{sendfile, {Pid, StreamID}, Filepath}
				when Pid =:= self() ->
			Child = #child{output=nofin} = get_child(StreamID, State),
			data_from_file(State, StreamID, Filepath),
			loop(replace_child(Child#child{output=fin}, State));
		{active, FromSocket = {Pid, StreamID}, FromPid} when Pid =:= self() ->
			Child = #child{in_buffer=InBuffer, is_recv=false}
				= get_child(StreamID, State),
			case InBuffer of
				<<>> ->
					loop(replace_child(Child#child{
						is_recv={active, FromSocket, FromPid}}, State));
				_ ->
					FromPid ! {spdy, FromSocket, InBuffer},
					loop(replace_child(Child#child{in_buffer= <<>>}, State))
			end;
		{passive, FromSocket = {Pid, StreamID}, FromPid} when Pid =:= self() ->
			Child = #child{is_recv=IsRecv} = get_child(StreamID, State),
			%% Make sure we aren't in the middle of a recv call.
			case IsRecv of false -> ok; {active, FromSocket, FromPid} -> ok end,
			loop(replace_child(Child#child{is_recv=false}, State));
		{'EXIT', Parent, Reason} ->
			exit(Reason);
		{'EXIT', Pid, _} ->
			%% @todo Report the error if any.
			loop(delete_child(Pid, State));
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, [], State);
		%% Calls from the supervisor module.
		{'$gen_call', {To, Tag}, which_children} ->
			Workers = [{?MODULE, Pid, worker, [?MODULE]}
				|| #child{pid=Pid} <- Children],
			To ! {Tag, Workers},
			loop(State);
		{'$gen_call', {To, Tag}, count_children} ->
			NbChildren = length(Children),
			Counts = [{specs, 1}, {active, NbChildren},
				{supervisors, 0}, {workers, NbChildren}],
			To ! {Tag, Counts},
			loop(State);
		{'$gen_call', {To, Tag}, _} ->
			To ! {Tag, {error, ?MODULE}},
			loop(State)
	after 60000 ->
		goaway(State, ok),
		terminate(State)
	end.

-spec system_continue(_, _, #state{}) -> ok.
system_continue(_, _, State) ->
	loop(State).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, _) ->
	exit(Reason).

-spec system_code_change(Misc, _, _, _) -> {ok, Misc} when Misc::#state{}.
system_code_change(Misc, _, _, _) ->
	{ok, Misc}.

parse_frame(State=#state{zinf=Zinf}, Data) ->
	case cow_spdy:split(Data) of
		{true, Frame, Rest} ->
			P = cow_spdy:parse(Frame, Zinf),
			case handle_frame(State#state{buffer = Rest}, P) of
				error ->
					terminate(State);
				State2 ->
					parse_frame(State2, Rest)
			end;
		false ->
			loop(State#state{buffer=Data})
	end.

%% FLAG_UNIDIRECTIONAL can only be set by the server.
handle_frame(State, {syn_stream, StreamID, _, _, true,
		_, _, _, _, _, _, _}) ->
	rst_stream(State, StreamID, protocol_error),
	State;
%% We do not support Associated-To-Stream-ID.
handle_frame(State, {syn_stream, StreamID, AssocToStreamID,
		_, _, _, _, _, _, _, _, _}) when AssocToStreamID =/= 0 ->
	rst_stream(State, StreamID, internal_error),
	State;
%% SYN_STREAM.
%%
%% Erlang does not allow us to control the priority of processes
%% so we ignore that value entirely.
handle_frame(State=#state{middlewares=Middlewares, env=Env,
		onrequest=OnRequest, onresponse=OnResponse, peer=Peer},
		{syn_stream, StreamID, _, IsFin, _, _,
		Method, _, Host, Path, Version, Headers}) ->
	Pid = spawn_link(?MODULE, request_init, [
		{self(), StreamID}, Peer, OnRequest, OnResponse,
		Env, Middlewares, Method, Host, Path, Version, Headers
	]),
	new_child(State, StreamID, Pid, IsFin);
%% RST_STREAM.
handle_frame(State, {rst_stream, StreamID, Status}) ->
	error_logger:error_msg("Received RST_STREAM frame ~p ~p",
		[StreamID, Status]),
	%% @todo Stop StreamID.
	State;
%% PING initiated by the server; ignore, we don't send any.
handle_frame(State, {ping, PingID}) when PingID rem 2 =:= 0 ->
	error_logger:error_msg("Ignored PING control frame: ~p~n", [PingID]),
	State;
%% PING initiated by the client; send it back.
handle_frame(State=#state{socket=Socket, transport=Transport},
		{ping, PingID}) ->
	Transport:send(Socket, cow_spdy:ping(PingID)),
	State;
%% Data received for a stream.
handle_frame(State, {data, StreamID, IsFin, Data}) ->
	Child = #child{input=nofin, in_buffer=Buffer, is_recv=IsRecv}
		= get_child(StreamID, State),
	Data2 = << Buffer/binary, Data/binary >>,
	IsFin2 = if IsFin -> fin; true -> nofin end,
	Child2 = case IsRecv of
		{active, FromSocket, FromPid} ->
			FromPid ! {spdy, FromSocket, Data},
			Child#child{input=IsFin2, is_recv=false};
		{passive, FromSocket, FromPid, 0, TRef} ->
			FromPid ! {recv, FromSocket, {ok, Data2}},
			cancel_recv_timeout(StreamID, TRef),
			Child#child{input=IsFin2, in_buffer= <<>>, is_recv=false};
		{passive, FromSocket, FromPid, Length, TRef}
				when byte_size(Data2) >= Length ->
			<< Data3:Length/binary, Rest/binary >> = Data2,
			FromPid ! {recv, FromSocket, {ok, Data3}},
			cancel_recv_timeout(StreamID, TRef),
			Child#child{input=IsFin2, in_buffer=Rest, is_recv=false};
		_ ->
			Child#child{input=IsFin2, in_buffer=Data2}
	end,
	replace_child(Child2, State);
%% General error, can't recover.
handle_frame(State, {error, badprotocol}) ->
	goaway(State, protocol_error),
	error;
%% Ignore all other frames for now.
handle_frame(State, Frame) ->
	error_logger:error_msg("Ignored frame ~p", [Frame]),
	State.

cancel_recv_timeout(StreamID, TRef) ->
	_ = erlang:cancel_timer(TRef),
	receive
		{recv_timeout, {Pid, StreamID}}
				when Pid =:= self() ->
			ok
	after 0 ->
		ok
	end.

%% @todo We must wait for the children to finish here,
%% but only up to N milliseconds. Then we shutdown.
terminate(_State) ->
	ok.

syn_reply(#state{socket=Socket, transport=Transport, zdef=Zdef},
		StreamID, IsFin, Status, Headers) ->
	Transport:send(Socket, cow_spdy:syn_reply(Zdef, StreamID, IsFin,
		Status, <<"HTTP/1.1">>, Headers)).

rst_stream(#state{socket=Socket, transport=Transport}, StreamID, Status) ->
	Transport:send(Socket, cow_spdy:rst_stream(StreamID, Status)).

goaway(#state{socket=Socket, transport=Transport, last_streamid=LastStreamID},
		Status) ->
	Transport:send(Socket, cow_spdy:goaway(LastStreamID, Status)).

data(#state{socket=Socket, transport=Transport}, StreamID, IsFin, Data) ->
	Transport:send(Socket, cow_spdy:data(StreamID, IsFin, Data)).

data_from_file(#state{socket=Socket, transport=Transport},
		StreamID, Filepath) ->
	{ok, IoDevice} = file:open(Filepath, [read, binary, raw]),
	data_from_file(Socket, Transport, StreamID, IoDevice).

data_from_file(Socket, Transport, StreamID, IoDevice) ->
	case file:read(IoDevice, 16#1fff) of
		eof ->
			_ = Transport:send(Socket, cow_spdy:data(StreamID, true, <<>>)),
			ok;
		{ok, Data} ->
			case Transport:send(Socket, cow_spdy:data(StreamID, false, Data)) of
				ok ->
					data_from_file(Socket, Transport, StreamID, IoDevice);
				{error, _} ->
					ok
			end
	end.

%% Children.

new_child(State=#state{children=Children}, StreamID, Pid, IsFin) ->
	IsFin2 = if IsFin -> fin; true -> nofin end,
	State#state{last_streamid=StreamID,
		children=[#child{streamid=StreamID,
		pid=Pid, input=IsFin2}|Children]}.

get_child(StreamID, #state{children=Children}) ->
	lists:keyfind(StreamID, #child.streamid, Children).

replace_child(Child=#child{streamid=StreamID},
		State=#state{children=Children}) ->
	Children2 = lists:keyreplace(StreamID, #child.streamid, Children, Child),
	State#state{children=Children2}.

delete_child(Pid, State=#state{children=Children}) ->
	Children2 = lists:keydelete(Pid, #child.pid, Children),
	State#state{children=Children2}.

%% Request process.

-spec request_init(socket(), {inet:ip_address(), inet:port_number()},
		cowboy:onrequest_fun(), cowboy:onresponse_fun(),
		cowboy_middleware:env(), [module()],
		binary(), binary(), binary(), binary(), [{binary(), binary()}])
	-> ok.
request_init(FakeSocket, Peer, OnRequest, OnResponse,
		Env, Middlewares, Method, Host, Path, Version, Headers) ->
	{Host2, Port} = cow_http:parse_fullhost(Host),
	{Path2, Qs} = cow_http:parse_fullpath(Path),
	Version2 = cow_http:parse_version(Version),
	Req = cowboy_req:new(FakeSocket, ?MODULE, Peer,
		Method, Path2, Qs, Version2, Headers,
		Host2, Port, <<>>, true, false, OnResponse),
	case OnRequest of
		undefined ->
			execute(Req, Env, Middlewares);
		_ ->
			Req2 = OnRequest(Req),
			case cowboy_req:get(resp_state, Req2) of
				waiting -> execute(Req2, Env, Middlewares);
				_ -> ok
			end
	end.

-spec execute(cowboy_req:req(), cowboy_middleware:env(), [module()])
	-> ok.
execute(Req, _, []) ->
	cowboy_req:ensure_response(Req, 204);
execute(Req, Env, [Middleware|Tail]) ->
	case Middleware:execute(Req, Env) of
		{ok, Req2, Env2} ->
			execute(Req2, Env2, Tail);
		{suspend, Module, Function, Args} ->
			erlang:hibernate(?MODULE, resume,
				[Env, Tail, Module, Function, Args]);
		{halt, Req2} ->
			cowboy_req:ensure_response(Req2, 204);
		{error, Status, Req2} ->
			cowboy_req:reply(Status, Req2)
	end.

-spec resume(cowboy_middleware:env(), [module()],
	module(), module(), [any()]) -> ok.
resume(Env, Tail, Module, Function, Args) ->
	case apply(Module, Function, Args) of
		{ok, Req2, Env2} ->
			execute(Req2, Env2, Tail);
		{suspend, Module2, Function2, Args2} ->
			erlang:hibernate(?MODULE, resume,
				[Env, Tail, Module2, Function2, Args2]);
		{halt, Req2} ->
			cowboy_req:ensure_response(Req2, 204);
		{error, Status, Req2} ->
			cowboy_req:reply(Status, Req2)
	end.

%% Reply functions used by cowboy_req.

-spec reply(socket(), binary(), cowboy:http_headers(), iodata()) -> ok.
reply(Socket = {Pid, _}, Status, Headers, Body) ->
	_ = case iolist_size(Body) of
		0 -> Pid ! {reply, Socket, Status, Headers};
		_ -> Pid ! {reply, Socket, Status, Headers, Body}
	end,
	ok.

-spec stream_reply(socket(), binary(), cowboy:http_headers()) -> ok.
stream_reply(Socket = {Pid, _}, Status, Headers) ->
	_ = Pid ! {stream_reply, Socket, Status, Headers},
	ok.

-spec stream_data(socket(), iodata()) -> ok.
stream_data(Socket = {Pid, _}, Data) ->
	_ = Pid ! {stream_data, Socket, Data},
	ok.

-spec stream_close(socket()) -> ok.
stream_close(Socket = {Pid, _}) ->
	_ = Pid ! {stream_close, Socket},
	ok.

%% Internal transport functions.

-spec name() -> spdy.
name() ->
	spdy.

-spec messages() -> {spdy, spdy_closed, spdy_error}.
messages() ->
	{spdy, spdy_closed, spdy_error}.

-spec recv(socket(), non_neg_integer(), timeout())
	-> {ok, binary()} | {error, timeout}.
recv(Socket = {Pid, _}, Length, Timeout) ->
	_ = Pid ! {recv, Socket, self(), Length, Timeout},
	receive
		{recv, Socket, Ret} ->
			Ret
	end.

-spec send(socket(), iodata()) -> ok.
send(Socket, Data) ->
	stream_data(Socket, Data).

%% We don't wait for the result of the actual sendfile call,
%% therefore we can't know how much was actually sent.
%% This isn't a problem as we don't use this value in Cowboy.
-spec sendfile(socket(), file:name_all()) -> {ok, undefined}.
sendfile(Socket = {Pid, _}, Filepath) ->
	_ = Pid ! {sendfile, Socket, Filepath},
	{ok, undefined}.

-spec setopts({pid(), _}, list()) -> ok.
setopts(Socket = {Pid, _}, [{active, once}]) ->
	_ = Pid ! {active, Socket, self()},
	ok;
setopts(Socket = {Pid, _}, [{active, false}]) ->
	_ = Pid ! {passive, Socket, self()},
	ok.
