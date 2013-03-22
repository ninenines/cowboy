Websocket handlers
==================

Purpose
-------

Websocket is an extension to HTTP to emulate plain TCP connections
between the user's browser and the server. Requests that are upgraded
are then handled by websocket handlers.

Both sides of the socket can send data at any time asynchronously.

Websocket is an IETF standard. Cowboy supports the standard and all
the drafts that were previously implemented by browsers. Websocket
is implemented by most browsers today, although for backward
compatibility reasons a solution like [Bullet](https://github.com/extend/bullet)
might be preferred.

Usage
-----

Websocket handlers are a bridge between the client and your system.
They can receive data from the client, through `websocket_handle/3`,
or from the system, through `websocket_info/3`. It is up to the
handler to decide to process this data, and optionally send a reply
to the client.

The first thing to do to be able to handle websockets is to tell
Cowboy that it should upgrade the connection to use the Websocket
protocol, as follow.

``` erlang
init({tcp, http}, Req, Opts) ->
    {upgrade, protocol, cowboy_websocket}.
```

Cowboy will then switch the protocol and call `websocket_init`,
followed by zero or more calls to `websocket_handle` and
`websocket_info`. Then, when the connection is shutting down,
`websocket_terminate` will be called.

The following handler sends a message every second. It also echoes
back what it receives.

``` erlang
-module(my_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, Req, Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(TransportName, Req, _Opts) ->
    erlang:start_timer(1000, self(), <<"Hello!">>),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    erlang:start_timer(1000, self(), <<"How' you doin'?">>),
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
```
