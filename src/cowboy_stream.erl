%% Copyright (c) 2015, Loïc Hoguin <essen@ninenines.eu>
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

-type streamid() :: any().
-type fin() :: fin | nofin.
-type headers() :: map(). %% @todo cowboy:http_headers() when they're maps

-type status_code() :: 100..999. %% @todo cowboy:http_status() when not binary
-type state() :: any().

-type commands() :: [{response, fin(), status_code(), headers()}
	| {data, fin(), iodata()}
	| {promise, binary(), binary(), binary(), binary(), headers()}
	| {flow, auto | integer()}
	| {spawn, pid()}
	| {upgrade, module(), state()}].

-type human_reason() :: atom().
-type reason() :: [{internal_error, timeout | {error | exit | throw, any()}, human_reason()}
	| {socket_error, closed | atom(), human_reason()}
	| {stream_error, cow_http2:error_reason(), human_reason()}
	| {connection_error, cow_http2:error_reason(), human_reason()}
	| {stop, cow_http2:frame(), human_reason()}].

-callback init(streamid(), fin(), binary(), binary(), binary(), binary(),
	headers(), cowboy:opts()) -> {commands(), state()}.
-callback data(streamid(), fin(), binary(), State) -> {commands(), State} when State::state().
-callback info(streamid(), any(), state()) -> {commands(), State} when State::state().
-callback terminate(streamid(), reason(), state()) -> any().

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
