%% Copyright (c) 2015-2024, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_stream).

-type state() :: any().
-type human_reason() :: atom().

-type streamid() :: any().
-export_type([streamid/0]).

-type fin() :: fin | nofin.
-export_type([fin/0]).

%% @todo Perhaps it makes more sense to have resp_body in this module?

-type resp_command()
	:: {response, cowboy:http_status(), cowboy:http_headers(), cowboy_req:resp_body()}.
-export_type([resp_command/0]).

-type commands() :: [{inform, cowboy:http_status(), cowboy:http_headers()}
	| resp_command()
	| {headers, cowboy:http_status(), cowboy:http_headers()}
	| {data, fin(), cowboy_req:resp_body()}
	| {trailers, cowboy:http_headers()}
	| {push, binary(), binary(), binary(), inet:port_number(),
		binary(), binary(), cowboy:http_headers()}
	| {flow, pos_integer()}
	| {spawn, pid(), timeout()}
	| {error_response, cowboy:http_status(), cowboy:http_headers(), iodata()}
	| {switch_protocol, cowboy:http_headers(), module(), state()}
	| {internal_error, any(), human_reason()}
	| {set_options, map()}
	| {log, logger:level(), io:format(), list()}
	| stop].
-export_type([commands/0]).

-type reason() :: normal | switch_protocol
	| {internal_error, timeout | {error | exit | throw, any()}, human_reason()}
	| {socket_error, closed | atom(), human_reason()}
	| {stream_error, cow_http2:error(), human_reason()}
	| {connection_error, cow_http2:error(), human_reason()}
	| {stop, cow_http2:frame() | {exit, any()}, human_reason()}.
-export_type([reason/0]).

-type partial_req() :: map(). %% @todo Take what's in cowboy_req with everything? optional.
-export_type([partial_req/0]).

-callback init(streamid(), cowboy_req:req(), cowboy:opts()) -> {commands(), state()}.
-callback data(streamid(), fin(), binary(), State) -> {commands(), State} when State::state().
-callback info(streamid(), any(), State) -> {commands(), State} when State::state().
-callback terminate(streamid(), reason(), state()) -> any().
-callback early_error(streamid(), reason(), partial_req(), Resp, cowboy:opts())
	-> Resp when Resp::resp_command().

%% @todo To optimize the number of active timers we could have a command
%% that enables a timeout that is called in the absence of any other call,
%% similar to what gen_server does. However the nice thing about this is
%% that the connection process can keep a single timer around (the same
%% one that would be used to detect half-closed sockets) and use this
%% timer and other events to trigger the timeout in streams at their
%% intended time.
%%
%% This same timer can be used to try and send PING frames to help detect
%% that the connection is indeed unresponsive.

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).
-export([make_error_log/5]).

%% Note that this and other functions in this module do NOT catch
%% exceptions. We want the exception to go all the way down to the
%% protocol code.
%%
%% OK the failure scenario is not so clear. The problem is
%% that the failure at any point in init/3 will result in the
%% corresponding state being lost. I am unfortunately not
%% confident we can do anything about this. If the crashing
%% handler just created a process, we'll never know about it.
%% Therefore at this time I choose to leave all failure handling
%% to the protocol process.
%%
%% Note that a failure in init/3 will result in terminate/3
%% NOT being called. This is because the state is not available.

-spec init(streamid(), cowboy_req:req(), cowboy:opts())
	-> {commands(), {module(), state()} | undefined}.
init(StreamID, Req, Opts) ->
	case maps:get(stream_handlers, Opts, [cowboy_stream_h]) of
		[] ->
			{[], undefined};
		[Handler|Tail] ->
			%% We call the next handler and remove it from the list of
			%% stream handlers. This means that handlers that run after
			%% it have no knowledge it exists. Should user require this
			%% knowledge they can just define a separate option that will
			%% be left untouched.
			{Commands, State} = Handler:init(StreamID, Req, Opts#{stream_handlers => Tail}),
			{Commands, {Handler, State}}
	end.

-spec data(streamid(), fin(), binary(), {Handler, State} | undefined)
	-> {commands(), {Handler, State} | undefined}
	when Handler::module(), State::state().
data(_, _, _, undefined) ->
	{[], undefined};
data(StreamID, IsFin, Data, {Handler, State0}) ->
	{Commands, State} = Handler:data(StreamID, IsFin, Data, State0),
	{Commands, {Handler, State}}.

-spec info(streamid(), any(), {Handler, State} | undefined)
	-> {commands(), {Handler, State} | undefined}
	when Handler::module(), State::state().
info(_, _, undefined) ->
	{[], undefined};
info(StreamID, Info, {Handler, State0}) ->
	{Commands, State} = Handler:info(StreamID, Info, State0),
	{Commands, {Handler, State}}.

-spec terminate(streamid(), reason(), {module(), state()} | undefined) -> ok.
terminate(_, _, undefined) ->
	ok;
terminate(StreamID, Reason, {Handler, State}) ->
	_ = Handler:terminate(StreamID, Reason, State),
	ok.

-spec early_error(streamid(), reason(), partial_req(), Resp, cowboy:opts())
	-> Resp when Resp::resp_command().
early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
	case maps:get(stream_handlers, Opts, [cowboy_stream_h]) of
		[] ->
			Resp;
		[Handler|Tail] ->
			%% This is the same behavior as in init/3.
			Handler:early_error(StreamID, Reason,
				PartialReq, Resp, Opts#{stream_handlers => Tail})
	end.

-spec make_error_log(init | data | info | terminate | early_error,
	list(), error | exit | throw, any(), list())
	-> {log, error, string(), list()}.
make_error_log(init, [StreamID, Req, Opts], Class, Exception, Stacktrace) ->
	{log, error,
		"Unhandled exception ~p:~p in cowboy_stream:init(~p, Req, Opts)~n"
		"Stacktrace: ~p~n"
		"Req: ~p~n"
		"Opts: ~p~n",
		[Class, Exception, StreamID, Stacktrace, Req, Opts]};
make_error_log(data, [StreamID, IsFin, Data, State], Class, Exception, Stacktrace) ->
	{log, error,
		"Unhandled exception ~p:~p in cowboy_stream:data(~p, ~p, Data, State)~n"
		"Stacktrace: ~p~n"
		"Data: ~p~n"
		"State: ~p~n",
		[Class, Exception, StreamID, IsFin, Stacktrace, Data, State]};
make_error_log(info, [StreamID, Msg, State], Class, Exception, Stacktrace) ->
	{log, error,
		"Unhandled exception ~p:~p in cowboy_stream:info(~p, Msg, State)~n"
		"Stacktrace: ~p~n"
		"Msg: ~p~n"
		"State: ~p~n",
		[Class, Exception, StreamID, Stacktrace, Msg, State]};
make_error_log(terminate, [StreamID, Reason, State], Class, Exception, Stacktrace) ->
	{log, error,
		"Unhandled exception ~p:~p in cowboy_stream:terminate(~p, Reason, State)~n"
		"Stacktrace: ~p~n"
		"Reason: ~p~n"
		"State: ~p~n",
		[Class, Exception, StreamID, Stacktrace, Reason, State]};
make_error_log(early_error, [StreamID, Reason, PartialReq, Resp, Opts],
		Class, Exception, Stacktrace) ->
	{log, error,
		"Unhandled exception ~p:~p in cowboy_stream:early_error(~p, Reason, PartialReq, Resp, Opts)~n"
		"Stacktrace: ~p~n"
		"Reason: ~p~n"
		"PartialReq: ~p~n"
		"Resp: ~p~n"
		"Opts: ~p~n",
		[Class, Exception, StreamID, Stacktrace, Reason, PartialReq, Resp, Opts]}.
