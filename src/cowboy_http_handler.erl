%% Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>
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

%% @doc Handler for HTTP requests.
%%
%% HTTP handlers must implement three callbacks: <em>init/3</em>,
%% <em>handle/2</em> and <em>terminate/2</em>, called one after another in
%% that order.
%%
%% <em>init/3</em> is meant for initialization. It receives information about
%% the transport and protocol used, along with the handler options from the
%% dispatch list, and allows you to upgrade the protocol if needed. You can
%% define a request-wide state here.
%%
%% <em>handle/2</em> is meant for handling the request. It receives the
%% request and the state previously defined.
%%
%% <em>terminate/2</em> is meant for cleaning up. It also receives the
%% request and the state previously defined.
%%
%% You do not have to read the request body or even send a reply if you do
%% not need to. Cowboy will properly handle these cases and clean-up afterwards.
%% In doubt it'll simply close the connection.
%%
%% Note that when upgrading the connection to WebSocket you do not need to
%% define the <em>handle/2</em> and <em>terminate/2</em> callbacks.
-module(cowboy_http_handler).

-export([behaviour_info/1]).

%% @private
-spec behaviour_info(_)
	-> undefined | [{handle, 2} | {init, 3} | {terminate, 2}, ...].
behaviour_info(callbacks) ->
	[{init, 3}, {handle, 2}, {terminate, 2}];
behaviour_info(_Other) ->
	undefined.
