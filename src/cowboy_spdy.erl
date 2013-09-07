%% Copyright (c) 2013, Loïc Hoguin <essen@ninenines.eu>
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

%% @doc SPDY protocol handler.
%%
%% The available options are:
%% <dl>
%% </dl>
%%
%% Note that there is no need to monitor these processes when using Cowboy as
%% an application as it already supervises them under the listener supervisor.
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
-export([recv/3]).
-export([send/2]).
-export([sendfile/2]).

-record(child, {
	streamid :: non_neg_integer(),
	pid :: pid(),
	input = nofin :: fin | nofin,
	in_buffer = <<>> :: binary(),
	is_recv = false :: {true, {non_neg_integer(), pid()},
		pid(), non_neg_integer(), reference()} | false,
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
	last_streamid = 0 :: non_neg_integer(),
	children = [] :: [#child{}]
}).

-type opts() :: [].
-export_type([opts/0]).

%% API.

%% @doc Start a SPDY protocol process.
-spec start_link(any(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
	proc_lib:start_link(?MODULE, init,
		[self(), Ref, Socket, Transport, Opts]).

%% Internal.

%% @doc Faster alternative to proplists:get_value/3.
%% @private
get_value(Key, Opts, Default) ->
	case lists:keyfind(Key, 1, Opts) of
		{_, Value} -> Value;
		_ -> Default
	end.

%% @private
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
		buffer=Buffer, zinf=Zinf, children=Children}) ->
	{OK, Closed, Error} = Transport:messages(),
	Transport:setopts(Socket, [{active, once}]),
	receive
		{OK, Socket, Data} ->
			Data2 = << Buffer/binary, Data/binary >>,
			case cow_spdy:split(Data2) of
				{true, Frame, Rest} ->
					P = cow_spdy:parse(Frame, Zinf),
					handle_frame(State#state{buffer=Rest}, P);
				false ->
					loop(State#state{buffer=Data2})
			end;
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
						is_recv={true, FromSocket, FromPid, Length, TRef}},
						State))
			end;
		{recv_timeout, {Pid, StreamID}}
				when Pid =:= self() ->
			Child = #child{is_recv={true, FromSocket, FromPid, _, _}}
				= get_child(StreamID, State),
			FromPid ! {recv, FromSocket, {error, timeout}},
			loop(replace_child(Child#child{is_recv=false}, State));
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

system_continue(_, _, State) ->
	loop(State).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, _) ->
	exit(Reason).

system_code_change(Misc, _, _, _) ->
	{ok, Misc}.

%% FLAG_UNIDIRECTIONAL can only be set by the server.
handle_frame(State, {syn_stream, StreamID, _, _, true,
		_, _, _, _, _, _, _}) ->
	rst_stream(State, StreamID, protocol_error),
	loop(State);
%% We do not support Associated-To-Stream-ID.
handle_frame(State, {syn_stream, StreamID, AssocToStreamID,
		_, _, _, _, _, _, _, _, _}) when AssocToStreamID =/= 0 ->
	rst_stream(State, StreamID, internal_error),
	loop(State);
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
	loop(new_child(State, StreamID, Pid, IsFin));
%% RST_STREAM.
handle_frame(State, {rst_stream, StreamID, Status}) ->
	error_logger:error_msg("Received RST_STREAM frame ~p ~p",
		[StreamID, Status]),
	%% @todo Stop StreamID.
	loop(State);
%% PING initiated by the server; ignore, we don't send any.
handle_frame(State, {ping, PingID}) when PingID rem 2 =:= 0 ->
	error_logger:error_msg("Ignored PING control frame: ~p~n", [PingID]),
	loop(State);
%% PING initiated by the client; send it back.
handle_frame(State=#state{socket=Socket, transport=Transport},
		{ping, PingID}) ->
	Transport:send(Socket, cow_spdy:ping(PingID)),
	loop(State);
%% Data received for a stream.
handle_frame(State, {data, StreamID, IsFin, Data}) ->
	Child = #child{input=nofin, in_buffer=Buffer, is_recv=IsRecv}
		= get_child(StreamID, State),
	Data2 = << Buffer/binary, Data/binary >>,
	IsFin2 = if IsFin -> fin; true -> nofin end,
	Child2 = case IsRecv of
		{true, FromSocket, FromPid, 0, TRef} ->
			FromPid ! {recv, FromSocket, {ok, Data2}},
			cancel_recv_timeout(StreamID, TRef),
			Child#child{input=IsFin2, in_buffer= <<>>, is_recv=false};
		{true, FromSocket, FromPid, Length, TRef}
				when byte_size(Data2) >= Length ->
			<< Data3:Length/binary, Rest/binary >> = Data2,
			FromPid ! {recv, FromSocket, {ok, Data3}},
			cancel_recv_timeout(StreamID, TRef),
			Child#child{input=IsFin2, in_buffer=Rest, is_recv=false};
		_ ->
			Child#child{input=IsFin2, in_buffer=Data2}
	end,
	loop(replace_child(Child2, State));
%% General error, can't recover.
handle_frame(State, {error, badprotocol}) ->
	goaway(State, protocol_error),
	terminate(State);
%% Ignore all other frames for now.
handle_frame(State, Frame) ->
	error_logger:error_msg("Ignored frame ~p", [Frame]),
	loop(State).

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
			cowboy_req:maybe_reply(Status, Req2)
	end.

%% @private
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
			cowboy_req:maybe_reply(Status, Req2)
	end.

%% Reply functions used by cowboy_req.

reply(Socket = {Pid, _}, Status, Headers, Body) ->
	_ = case iolist_size(Body) of
		0 -> Pid ! {reply, Socket, Status, Headers};
		_ -> Pid ! {reply, Socket, Status, Headers, Body}
	end,
	ok.

stream_reply(Socket = {Pid, _}, Status, Headers) ->
	_ = Pid ! {stream_reply, Socket, Status, Headers},
	ok.

stream_data(Socket = {Pid, _}, Data) ->
	_ = Pid ! {stream_data, Socket, Data},
	ok.

stream_close(Socket = {Pid, _}) ->
	_ = Pid ! {stream_close, Socket},
	ok.

%% Internal transport functions.

name() ->
	spdy.

recv(Socket = {Pid, _}, Length, Timeout) ->
	_ = Pid ! {recv, Socket, self(), Length, Timeout},
	receive
		{recv, Socket, Ret} ->
			Ret
	end.

send(Socket, Data) ->
	stream_data(Socket, Data).

%% We don't wait for the result of the actual sendfile call,
%% therefore we can't know how much was actually sent.
%% This isn't a problem as we don't use this value in Cowboy.
sendfile(Socket = {Pid, _}, Filepath) ->
	_ = Pid ! {sendfile, Socket, Filepath},
	{ok, undefined}.
