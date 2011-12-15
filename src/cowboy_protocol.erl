%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
%% Copyright (c) 2011, Michiel Hakvoort <michiel@hakvoort.it>
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

%% @doc Cowboy protocol.
%%
%% A Cowboy protocol must implement one callback: <em>start_link/4</em>.
%%
%% <em>start_link/4</em> is meant for the initialization of the
%% protocol process.
%% It receives the pid to the listener's gen_server, the client socket,
%% the module name of the chosen transport and the options defined when
%% starting the listener. The <em>start_link/4</em> function must follow
%% the supervisor start function specification.
%%
%% After initializing your protocol, it is recommended to call the
%% function cowboy:accept_ack/1 with the ListenerPid as argument,
%% as it will ensure Cowboy has been able to fully initialize the socket.
%% Anything you do past this point is up to you!
%%
%% If you need to change some socket options, like enabling raw mode
%% for example, you can call the <em>Transport:setopts/2</em> function.
%% It is the protocol's responsability to manage the socket usage,
%% there should be no need for an user to specify that kind of options
%% while starting a listener.
%%
%% You should definitely look at the cowboy_http_protocol module for
%% a great example of fast request handling if you need to.
%% Otherwise it's probably safe to use <code>{active, once}</code> mode
%% and handle everything as it comes.
%%
%% Note that while you technically can run a protocol handler directly
%% as a gen_server or a gen_fsm, it's probably not a good idea,
%% as the only call you'll ever receive from Cowboy is the
%% <em>start_link/4</em> call. On the other hand, feel free to write
%% a very basic protocol handler which then forwards requests to a
%% gen_server or gen_fsm. By doing so however you must take care to
%% supervise their processes as Cowboy only knows about the protocol
%% handler itself.
-module(cowboy_protocol).

-export([behaviour_info/1]).

%% @private
-spec behaviour_info(_)
	-> undefined | [{start_link, 4}, ...].
behaviour_info(callbacks) ->
	[{start_link, 4}];
behaviour_info(_Other) ->
	undefined.
