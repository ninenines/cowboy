%% Copyright (c) 2011-2013, Loïc Hoguin <essen@ninenines.eu>
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

%% @doc Handler for HTTP WebSocket requests.
%%
%% WebSocket handlers must implement five callbacks: <em>init/3</em>,
%% <em>websocket_init/3</em>, <em>websocket_handle/3</em>,
%% <em>websocket_info/3</em> and <em>websocket_terminate/3</em>.
%% These callbacks will only be called if the connection is upgraded
%% to WebSocket in the HTTP handler's <em>init/3</em> callback.
%% They are then called in that order, although <em>websocket_handle/3</em>
%% will be called for each packet received, and <em>websocket_info</em>
%% for each message received.
%%
%% <em>websocket_init/3</em> is meant for initialization. It receives
%% information about the transport and protocol used, along with the handler
%% options from the dispatch list. You can define a request-wide state here.
%% If you are going to want to compact the request, you should probably do it
%% here.
%%
%% <em>websocket_handle/3</em> receives the data from the socket. It can reply
%% something, do nothing or close the connection.
%%
%% <em>websocket_info/3</em> receives messages sent to the process. It has
%% the same reply format as <em>websocket_handle/3</em> described above. Note
%% that unlike in a <em>gen_server</em>, when <em>websocket_info/3</em>
%% replies something, it is always to the socket, not to the process that
%% originated the message.
%%
%% <em>websocket_terminate/3</em> is meant for cleaning up. It also receives
%% the request and the state previously defined, along with a reason for
%% termination.
%%
%% All of <em>websocket_init/3</em>, <em>websocket_handle/3</em> and
%% <em>websocket_info/3</em> can decide to hibernate the process by adding
%% an extra element to the returned tuple, containing the atom
%% <em>hibernate</em>. Doing so helps save memory and improve CPU usage.
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
