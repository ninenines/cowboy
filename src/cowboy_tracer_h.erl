%% Copyright (c) 2017-2024, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_tracer_h).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

-export([set_trace_patterns/0]).

-export([tracer_process/3]).
-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-type match_predicate()
	:: fun((cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts()) -> boolean()).

-type tracer_match_specs() :: [match_predicate()
	| {method, binary()}
	| {host, binary()}
	| {path, binary()}
	| {path_start, binary()}
	| {header, binary()}
	| {header, binary(), binary()}
	| {peer_ip, inet:ip_address()}
].
-export_type([tracer_match_specs/0]).

-type tracer_callback() :: fun((init | terminate | tuple(), any()) -> any()).
-export_type([tracer_callback/0]).

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
	-> {cowboy_stream:commands(), any()}.
init(StreamID, Req, Opts) ->
	init_tracer(StreamID, Req, Opts),
	cowboy_stream:init(StreamID, Req, Opts).

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State)
	-> {cowboy_stream:commands(), State} when State::any().
data(StreamID, IsFin, Data, Next) ->
	cowboy_stream:data(StreamID, IsFin, Data, Next).

-spec info(cowboy_stream:streamid(), any(), State)
	-> {cowboy_stream:commands(), State} when State::any().
info(StreamID, Info, Next) ->
	cowboy_stream:info(StreamID, Info, Next).

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), any()) -> any().
terminate(StreamID, Reason, Next) ->
	cowboy_stream:terminate(StreamID, Reason, Next).

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(),
	cowboy_stream:partial_req(), Resp, cowboy:opts()) -> Resp
	when Resp::cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
	cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

%% API.

%% These trace patterns are most likely not suitable for production.
-spec set_trace_patterns() -> ok.
set_trace_patterns() ->
	erlang:trace_pattern({'_', '_', '_'}, [{'_', [], [{return_trace}]}], [local]),
	erlang:trace_pattern(on_load, [{'_', [], [{return_trace}]}], [local]),
	ok.

%% Internal.

init_tracer(StreamID, Req, Opts=#{tracer_match_specs := List, tracer_callback := _}) ->
	case match(List, StreamID, Req, Opts) of
		false ->
			ok;
		true ->
			start_tracer(StreamID, Req, Opts)
	end;
%% When the options tracer_match_specs or tracer_callback
%% are not provided we do not enable tracing.
init_tracer(_, _, _) ->
	ok.

match([], _, _, _) ->
	true;
match([Predicate|Tail], StreamID, Req, Opts) when is_function(Predicate) ->
	case Predicate(StreamID, Req, Opts) of
		true -> match(Tail, StreamID, Req, Opts);
		false -> false
	end;
match([{method, Value}|Tail], StreamID, Req=#{method := Value}, Opts) ->
	match(Tail, StreamID, Req, Opts);
match([{host, Value}|Tail], StreamID, Req=#{host := Value}, Opts) ->
	match(Tail, StreamID, Req, Opts);
match([{path, Value}|Tail], StreamID, Req=#{path := Value}, Opts) ->
	match(Tail, StreamID, Req, Opts);
match([{path_start, PathStart}|Tail], StreamID, Req=#{path := Path}, Opts) ->
	Len = byte_size(PathStart),
	case Path of
		<<PathStart:Len/binary, _/bits>> -> match(Tail, StreamID, Req, Opts);
		_ -> false
	end;
match([{header, Name}|Tail], StreamID, Req=#{headers := Headers}, Opts) ->
	case Headers of
		#{Name := _} -> match(Tail, StreamID, Req, Opts);
		_ -> false
	end;
match([{header, Name, Value}|Tail], StreamID, Req=#{headers := Headers}, Opts) ->
	case Headers of
		#{Name := Value} -> match(Tail, StreamID, Req, Opts);
		_ -> false
	end;
match([{peer_ip, IP}|Tail], StreamID, Req=#{peer := {IP, _}}, Opts) ->
	match(Tail, StreamID, Req, Opts);
match(_, _, _, _) ->
	false.

%% We only start the tracer if one wasn't started before.
start_tracer(StreamID, Req, Opts) ->
	case erlang:trace_info(self(), tracer) of
		{tracer, []} ->
			TracerPid = proc_lib:spawn_link(?MODULE, tracer_process, [StreamID, Req, Opts]),
			%% The default flags are probably not suitable for production.
			Flags = maps:get(tracer_flags, Opts, [
				send, 'receive', call, return_to,
				procs, ports, monotonic_timestamp,
				%% The set_on_spawn flag is necessary to catch events
				%% from request processes.
				set_on_spawn
			]),
			erlang:trace(self(), true, [{tracer, TracerPid}|Flags]),
			ok;
		_ ->
			ok
	end.

%% Tracer process.

-spec tracer_process(_, _, _) -> no_return().
tracer_process(StreamID, Req=#{pid := Parent}, Opts=#{tracer_callback := Fun}) ->
	%% This is necessary because otherwise the tracer could stop
	%% before it has finished processing the events in its queue.
	process_flag(trap_exit, true),
	State = Fun(init, {StreamID, Req, Opts}),
	tracer_loop(Parent, Opts, State).

tracer_loop(Parent, Opts=#{tracer_callback := Fun}, State0) ->
	receive
		Msg when element(1, Msg) =:= trace; element(1, Msg) =:= trace_ts ->
			State = Fun(Msg, State0),
			tracer_loop(Parent, Opts, State);
		{'EXIT', Parent, Reason} ->
			tracer_terminate(Reason, Opts, State0);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {Opts, State0});
		Msg ->
			cowboy:log(warning, "~p: Tracer process received stray message ~9999p~n",
				[?MODULE, Msg], Opts),
			tracer_loop(Parent, Opts, State0)
	end.

-spec tracer_terminate(_, _, _) -> no_return().
tracer_terminate(Reason, #{tracer_callback := Fun}, State) ->
	_ = Fun(terminate, State),
	exit(Reason).

%% System callbacks.

-spec system_continue(pid(), _, {cowboy:opts(), any()}) -> no_return().
system_continue(Parent, _, {Opts, State}) ->
	tracer_loop(Parent, Opts, State).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, {Opts, State}) ->
	tracer_terminate(Reason, Opts, State).

-spec system_code_change(Misc, _, _, _) -> {ok, Misc} when Misc::any().
system_code_change(Misc, _, _, _) ->
	{ok, Misc}.
