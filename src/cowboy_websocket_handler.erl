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

-module(cowboy_websocket_handler).

-type opts() :: any().
-type state() :: any().
-type terminate_reason() :: {normal, shutdown}
	| {normal, timeout}
	| {error, closed}
	| {remote, closed}
	| {remote, cowboy_websocket:close_code(), binary()}
	| {error, badencoding}
	| {error, badframe}
	| {error, atom()}.

-callback websocket_init(atom(), Req, opts())
	-> {ok, Req, state()}
	| {ok, Req, state(), hibernate}
	| {ok, Req, state(), timeout()}
	| {ok, Req, state(), timeout(), hibernate}
	| {shutdown, Req}
	when Req::cowboy_req:req().
-callback websocket_handle({text | binary | ping | pong, binary()}, Req, State)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State}
	| {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State, hibernate}
	| {shutdown, Req, State}
	when Req::cowboy_req:req(), State::state().
-callback websocket_info(any(), Req, State)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State}
	| {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State, hibernate}
	| {shutdown, Req, State}
	when Req::cowboy_req:req(), State::state().
-callback websocket_terminate(terminate_reason(), cowboy_req:req(), state())
	-> ok.
