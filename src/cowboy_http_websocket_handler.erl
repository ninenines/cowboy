%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
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
%% WebSocket handlers must implement three callbacks: <em>websocket_init/3</em>,
%% <em>websocket_handle/3</em> and <em>websocket_terminate/3</em>. These
%% callbacks will only be called if the connection is upgraded to WebSocket
%% in the HTTP handler's <em>init/3</em> callback. They are then called in that
%% order, although <em>websocket_handle/3</em> will be called multiple time,
%% one time for each message or packet received.
%%
%% <em>websocket_init/3</em> is meant for initialization. It receives
%% information about the transport and protocol used, along with the handler
%% options from the dispatch list. You can define a request-wide state here.
%% If you are going to want to compact the request, you should probably do it
%% here.
%%
%% <em>websocket_handle/3</em> receives messages sent to the process and
%% also the data sent to the socket. In the later case the information is
%% given as a tuple <em>{websocket, Data}</em>. It can reply something, do
%% nothing or close the connection. You can choose to hibernate the process
%% by returning <em>hibernate</em> to save memory and CPU.
%%
%% <em>websocket_terminate/3</em> is meant for cleaning up. It also receives
%% the request and the state previously defined, along with a reason for
%% termination.
-module(cowboy_http_websocket_handler).

-export([behaviour_info/1]).

%% @private
-spec behaviour_info(_) -> undefined | [{websocket_handle, 3}
	| {websocket_init, 3} | {websocket_terminate, 3}, ...].
behaviour_info(callbacks) ->
	[{websocket_init, 3}, {websocket_handle, 3}, {websocket_terminate, 3}];
behaviour_info(_Other) ->
	undefined.
