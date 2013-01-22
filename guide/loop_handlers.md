Loop handlers
=============

Purpose
-------

Loop handlers are a special kind of HTTP handlers used when the
response can not be sent right away. The handler enters instead
a receive loop waiting for the right message before it can send
a response.

They are most useful when performing long-polling operations or
when using server-sent events.

While the same can be accomplished using plain HTTP handlers,
it is recommended to use loop handlers because they are well-tested
and allow using built-in features like hibernation and timeouts.

Usage
-----

Loop handlers are used for requests where a response might not
be immediately available, but where you would like to keep the
connection open for a while in case the response arrives. The
most known example of such practice is known as long-polling.

Loop handlers can also be used for requests where a response is
partially available and you need to stream the response body
while the connection is open. The most known example of such
practice is known as server-sent events.

Loop handlers essentially wait for one or more Erlang messages
and feed these messages to the `info/3` callback. It also features
the `init/3` and `terminate/3` callbacks which work the same as
for plain HTTP handlers.

The following handler waits for a message `{reply, Body}` before
sending a response. If this message doesn't arrive within 60
seconds, it gives up and a `204 No Content` will be replied.
It also hibernates the process to save memory while waiting for
this message.

``` erlang
-module(my_loop_handler).
-behaviour(cowboy_loop_handler).

-export([init/3]).
-export([info/3]).
-export([terminate/3]).

init({tcp, http}, Req, Opts) ->
    {loop, Req, undefined_state, 60000, hibernate}.

info({reply, Body}, Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [], Body, Req),
    {ok, Req2, State};
info(Message, Req, State) ->
    {loop, Req, State, hibernate}.

terminate(Reason, Req, State) ->
    ok.
```
