%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
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

%% @todo To enable WebTransport the following options need to be set:
%%
%% QUIC:
%%  - max_datagram_frame_size > 0
%%
%% HTTP/3:
%%  - SETTINGS_H3_DATAGRAM = 1
%%  - SETTINGS_ENABLE_CONNECT_PROTOCOL = 1
%%  - SETTINGS_WT_MAX_SESSIONS >= 1

%% Cowboy supports versions 07 through 13 of the WebTransport drafts.
%% Cowboy also has some compatibility with version 02.
%%
%% WebTransport CONNECT requests go through cowboy_stream as normal
%% and then an upgrade/switch_protocol is issued (just like Websocket).
%% After that point none of the events go through cowboy_stream except
%% the final terminate event. The request process becomes the process
%% handling all events in the WebTransport session.
%%
%% WebTransport sessions can be ended via a command, via a crash or
%% exit, via the closing of the connection (client or server inititated),
%% via the client ending the session (mirroring the command) or via
%% the client terminating the CONNECT stream.
-module(cowboy_webtransport).

-export([upgrade/4]).
-export([upgrade/5]).

%% cowboy_stream.
-export([info/3]).
-export([terminate/3]).

-type stream_type() :: unidi | bidi.
-type open_stream_ref() :: any().

-type event() ::
	{stream_opened, cow_http3:stream_id(), stream_type()} |
	{opened_stream_id, open_stream_ref(), cow_http3:stream_id()} | %% Unfortunate but unavoidable.
	{stream_data, cow_http3:stream_id(), cow_http:fin(), binary()} |
	{stream_reset, cow_http3:stream_id(), cow_http3:wt_app_error_code() | undefined} | %% @todo Add. (for undefined see end of 4.4)
	{stream_stop_sending, cow_http3:stream_id(), cow_http3:wt_app_error_code() | undefined} | %% @todo Add.
	{datagram, binary()} |
	drain_session. %% @todo Add a test, command tested, event not.

-type commands() :: [
	{open_stream, open_stream_ref(), stream_type(), iodata()} |
	{send, cow_http3:stream_id(), iodata()} |
	{send, cow_http3:stream_id(), cow_http:fin(), iodata()} |
	{reset_stream, cow_http3:stream_id(), cow_http3:wt_app_error_code()} | %% @todo Separated from close_stream.
	{stop_sending, cow_http3:stream_id(), cow_http3:wt_app_error_code()} | %% @todo Separated from close_stream.
	{send_datagram, iodata()} |
	drain_session |
	close_session |
	{close_session, cow_http3:wt_app_error_code()} |
	{close_session, cow_http3:wt_app_error_code(), iodata()}
].
-export_type([commands/0]).

-type call_result(State) :: {commands(), State} | {commands(), State, hibernate}.

-callback init(Req, any())
	-> {ok | module(), Req, any()}
	| {module(), Req, any(), any()}
	when Req::cowboy_req:req().

-callback webtransport_init(State)
	-> call_result(State) when State::any().
-optional_callbacks([webtransport_init/1]).

-callback webtransport_handle(event(), State)
	-> call_result(State) when State::any().
-optional_callbacks([webtransport_handle/2]).

-callback webtransport_info(any(), State)
	-> call_result(State) when State::any().
-optional_callbacks([webtransport_info/2]).

-callback terminate(any(), cowboy_req:req(), any()) -> ok.
-optional_callbacks([terminate/3]).

-type opts() :: #{
	req_filter => fun((cowboy_req:req()) -> map())
}.
-export_type([opts/0]).

-record(state, {
	id :: cow_http3:stream_id(),
	parent :: pid(),
	opts = #{} :: opts(),
	handler :: module(),
	hibernate = false :: boolean(),
	req = #{} :: map()
}).

%% This function mirrors a similar function for Websocket.

-spec is_upgrade_request(cowboy_req:req()) -> boolean().

is_upgrade_request(#{version := Version, method := <<"CONNECT">>, protocol := Protocol})
		when Version =:= 'HTTP/3' ->
	%% @todo scheme MUST BE "https"
	<<"webtransport">> =:= cowboy_bstr:to_lower(Protocol);

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
upgrade(Req=#{version := 'HTTP/3', pid := Pid, streamid := StreamID}, Env, Handler, HandlerState, Opts) ->
	FilteredReq = case maps:get(req_filter, Opts, undefined) of
		undefined -> maps:with([method, version, scheme, host, port, path, qs, peer], Req);
		FilterFun -> FilterFun(Req)
	end,
	State = #state{id=StreamID, parent=Pid, opts=Opts, handler=Handler, req=FilteredReq},
	%% @todo Must ensure the relevant settings are enabled (QUIC and H3).
	%% Either we check them BEFORE, or we check them when the handler
	%% is OK to initiate a webtransport session. Probably need to
	%% check them BEFORE as we need to become (takeover) the webtransport process
	%% after we are done with the upgrade. Maybe in cow_http3_machine but
	%% it doesn't have QUIC settings currently (max_datagram_size).
	case is_upgrade_request(Req) of
		true ->
			Headers = cowboy_req:response_headers(#{}, Req),
			Pid ! {{Pid, StreamID}, {switch_protocol, Headers, ?MODULE,
				#{session_pid => self()}}},
			webtransport_init(State, HandlerState);
		%% Use 501 Not Implemented to mirror the recommendation in
		%% by RFC9220 3 (WebSockets Upgrade over HTTP/3).
		false ->
			%% @todo I don't think terminate will be called.
			{ok, cowboy_req:reply(501, Req), Env}
	end.

webtransport_init(State=#state{handler=Handler}, HandlerState) ->
	case erlang:function_exported(Handler, webtransport_init, 1) of
		true -> handler_call(State, HandlerState, webtransport_init, undefined);
		false -> before_loop(State, HandlerState)
	end.

before_loop(State=#state{hibernate=true}, HandlerState) ->
	proc_lib:hibernate(?MODULE, loop, [State#state{hibernate=false}, HandlerState]);
before_loop(State, HandlerState) ->
	loop(State, HandlerState).

-spec loop(#state{}, any()) -> no_return().

loop(State=#state{id=SessionID, parent=Parent}, HandlerState) ->
	receive
		%% Application close via WT_CLOSE_SESSION.
		{'$webtransport_event', SessionID, Event={close_session, _, _}} -> %% @todo Renamed.
			terminate_proc(State, HandlerState, Event);
		%% Abrupt session close.
		{'$webtransport_event', SessionID, Event={session_error, _, _}} -> %% @todo Renamed and added info (atom reason, atom/binary string explaining).
			terminate_proc(State, HandlerState, Event);
		{'$webtransport_event', SessionID, Event} ->
			handler_call(State, HandlerState, webtransport_handle, Event);
		%% Timeouts.
%% @todo idle_timeout
%		{timeout, TRef, ?MODULE} ->
%			tick_idle_timeout(State, HandlerState, ParseState);
%		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
%			before_loop(State, HandlerState, ParseState);
		%% System messages.
		{'EXIT', Parent, Reason} ->
			%% @todo We should exit gracefully.
			exit(Reason);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
				{State, HandlerState});
		%% Calls from supervisor module.
		{'$gen_call', From, Call} ->
			cowboy_children:handle_supervisor_call(Call, From, [], ?MODULE),
			before_loop(State, HandlerState);
		Message ->
			handler_call(State, HandlerState, webtransport_info, Message)
	end.

handler_call(State=#state{handler=Handler}, HandlerState, Callback, Message) ->
	try case Callback of
		webtransport_init -> Handler:webtransport_init(HandlerState);
		_ -> Handler:Callback(Message, HandlerState)
	end of
		{Commands, HandlerState2} when is_list(Commands) ->
			handler_call_result(State, HandlerState2, Commands);
		{Commands, HandlerState2, hibernate} when is_list(Commands) ->
			handler_call_result(State#state{hibernate=true}, HandlerState2, Commands)
	catch Class:Reason:Stacktrace ->
		%% @todo Do we need to send a close? Let cowboy_http3 detect and handle it?
		handler_terminate(State, HandlerState, {crash, Class, Reason}),
		erlang:raise(Class, Reason, Stacktrace)
	end.

handler_call_result(State0, HandlerState, Commands) ->
	case commands(Commands, State0, ok, []) of
		{ok, State} ->
			before_loop(State, HandlerState);
		{stop, State} ->
			terminate_proc(State, HandlerState, stop)
	end.

%% We accumulate the commands that must be sent to the connection process
%% because we want to send everything into one message. Other commands are
%% processed immediately.

commands([], State, Res, []) ->
	{Res, State};
commands([], State=#state{id=SessionID, parent=Pid}, Res, Commands) ->
	Pid ! {'$webtransport_commands', SessionID, lists:reverse(Commands)},
	{Res, State};
%% {open_stream, OpenStreamRef, StreamType, InitialData}.
commands([Command={open_stream, _, _, _}|Tail], State, Res, Acc) ->
	commands(Tail, State, Res, [Command|Acc]);
%% {close_stream, StreamID, Code}.
commands([Command={close_stream, _, _}|Tail], State, Res, Acc) ->
	commands(Tail, State, Res, [Command|Acc]);
%% @todo We must reject send to a remote unidi stream.
%% {send, StreamID, Data}.
commands([Command={send, _, _}|Tail], State, Res, Acc) ->
	commands(Tail, State, Res, [Command|Acc]);
%% {send, StreamID, IsFin, Data}.
commands([Command={send, _, _, _}|Tail], State, Res, Acc) ->
	commands(Tail, State, Res, [Command|Acc]);
%% {send_datagram, Data}.
commands([Command={send_datagram, _}|Tail], State, Res, Acc) ->
	commands(Tail, State, Res, [Command|Acc]);
%% drain_session - DRAIN_WT_SESSION
commands([Command=drain_session|Tail], State, Res, Acc) ->
	commands(Tail, State, Res, [Command|Acc]);
%% close_session | {close_session, Code} | {close_session, Code, Msg} - CLOSE_WT_SESSION
%% @todo At this point the handler must not issue stream or send commands.
commands([Command=close_session|Tail], State, _, Acc) ->
	commands(Tail, State, stop, [Command|Acc]);
commands([Command={close_session, _}|Tail], State, _, Acc) ->
	commands(Tail, State, stop, [Command|Acc]);
commands([Command={close_session, _, _}|Tail], State, _, Acc) ->
	commands(Tail, State, stop, [Command|Acc]).
%% @todo A set_options command could be useful to increase the number of allowed streams
%%       or other forms of flow control. Alternatively a flow command. Or both.
%% @todo A shutdown_reason command could be useful for the same reasons as Websocekt.

-spec terminate_proc(_, _, _) -> no_return().

terminate_proc(State, HandlerState, Reason) ->
	handler_terminate(State, HandlerState, Reason),
	%% @todo This is what should be done if shutdown_reason gets implemented.
%	case Shutdown of
%		normal -> exit(normal);
%		_ -> exit({shutdown, Shutdown})
%	end.
	exit(normal).

handler_terminate(#state{handler=Handler, req=Req}, HandlerState, Reason) ->
	cowboy_handler:terminate(Reason, Req, HandlerState, Handler).

%% cowboy_stream callbacks.
%%
%% We shortcut stream handlers but still need to process some events
%% such as process exiting or termination. We implement the relevant
%% callbacks here. Note that as far as WebTransport is concerned,
%% receiving stream data here would be an error therefore the data
%% callback is not implemented.
%%
%% @todo Better type than map() for the cowboy_stream state.
%% @todo Is this really useful?

-spec info(cowboy_stream:streamid(), any(), State)
	-> {cowboy_stream:commands(), State} when State::map().

info(StreamID, Msg, WTState=#{stream_state := StreamState0}) ->
	{Commands, StreamState} = cowboy_stream:info(StreamID, Msg, StreamState0),
	{Commands, WTState#{stream_state => StreamState}}.

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), map())
	-> any().

terminate(StreamID, Reason, #{stream_state := StreamState}) ->
	cowboy_stream:terminate(StreamID, Reason, StreamState).
