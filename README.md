Cowboy
======

Cowboy is a small, fast and modular HTTP server written in Erlang.

Goals
-----

Cowboy aims to provide the following advantages:

* **Small** codebase.
* Damn **fast**.
* **Modular**: transport, protocol and handlers are replaceable. (see below)
* Easy to **embed** inside another application.
* Selectively **dispatch** requests to handlers, allowing you to send some
  requests to your embedded code and others to a FastCGI application in
  PHP or Ruby.
* No parameterized module. No process dictionary. **Clean** Erlang code.

The server is currently in early development stage. Comments, suggestions are
more than welcome. To contribute, either open bug reports, or fork the project
and send us pull requests with new or improved functionality. Of course you
might want to discuss your plans with us before you do any serious work so
we can share ideas and save everyone time.

Embedding Cowboy
----------------

* Add Cowboy as a rebar or agner dependency to your application.
* Start Cowboy and add one or more listeners.
* Write handlers.

Getting Started
---------------

Cowboy can be started and stopped like any other application. However, the
Cowboy application does not start any listener, those must be started manually.

A listener is a special kind of supervisor that handles a pool of acceptor
processes. It also manages all its associated request processes. This allows
you to shutdown all processes related to a listener by stopping the supervisor.

An acceptor simply accepts connections and forwards them to a protocol module,
for example HTTP. You must thus define the transport and protocol module to
use for the listener, their options and the number of acceptors in the pool
before you can start a listener supervisor.

For HTTP applications the transport can be either TCP or SSL for HTTP and
HTTPS respectively. On the other hand, the protocol is of course HTTP.

You can start and stop listeners by calling cowboy:start_listener and
cowboy:stop_listener respectively. It is your responsability to give each
listener a unique name.

Code speaks more than words:

``` erlang
application:start(cowboy),
Dispatch = [
    %% {Host, list({Path, Handler, Opts})}
    {'_', [{'_', my_handler, []}]}
],
%% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
cowboy:start_listener(http, 100,
    cowboy_tcp_transport, [{port, 8080}],
    cowboy_http_protocol, [{dispatch, Dispatch}]
).
```

You must also write the `my_handler` module to process requests. You can
use one of the predefined handlers or write your own. An hello world HTTP
handler could be written like this:

``` erlang
-module(my_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [], "Hello World!", Req),
    {ok, Req2, State}.

terminate(Req, State) ->
    ok.
```
