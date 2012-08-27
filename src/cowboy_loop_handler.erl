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

%% @doc Behaviour for long-lived HTTP handlers.
%%
%% <em>init/3</em> allows you to initialize a state for all subsequent
%% callbacks, and indicate to Cowboy whether you accept to handle the
%% request or want to shutdown without handling it, in which case the
%% receive loop and <em>info/3</em> calls will simply be skipped.
%%
%% <em>info/3</em> allows you to handle the messages this process will
%% receive. It receives the message and the state previously defined.
%% It can decide to stop the receive loop or continue receiving.
%%
%% <em>terminate/2</em> allows you to clean up. It receives the state
%% previously defined.
%%
%% There is no required operation to perform in any of these callbacks
%% other than returning the proper values. Make sure you always return
%% the last modified Req so that Cowboy has the up to date information
%% about the request.
%%
%% It is recommended to use hibernate if this process is not going to
%% receive a lot of messages. It is also recommended to use a timeout
%% value so that the connection gets closed after a long period of
%% inactivity.
-module(cowboy_loop_handler).

-type opts() :: any().
-type state() :: any().

-callback init({atom(), http}, Req, opts())
	-> {ok, Req, state()}
	| {loop, Req, state()}
	| {loop, Req, state(), hibernate}
	| {loop, Req, state(), timeout()}
	| {loop, Req, state(), timeout(), hibernate}
	| {shutdown, Req, state()}
	| {upgrade, protocol, module()}
	when Req::cowboy_req:req().
-callback info(any(), Req, State)
	-> {ok, Req, State}
	| {loop, Req, State}
	| {loop, Req, State, hibernate}
	when Req::cowboy_req:req(), State::state().
-callback terminate(cowboy_req:req(), state()) -> ok.
