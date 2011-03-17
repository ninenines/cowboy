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

* Add Cowboy as a rebar dependency to your application.
* Start Cowboy and add one or more listeners.
* Write handlers.

Starting and stopping
---------------------

Cowboy can be started and stopped like any other application. However the
Cowboy application do not start any listener, those must be started manually.

A listener is a special kind of supervisor that handles a pool of acceptor
processes. An acceptor simply accept connections and forward them to a
protocol module, for example HTTP. You must thus define the transport and
protocol module to use for the listener, their options and the number of
acceptors in the pool before you can start a listener supervisor.

For HTTP applications the transport can be either TCP or SSL for HTTP and
HTTPS respectively. On the other hand, the protocol is of course HTTP.

Code speaks more than words:

    application:start(cowboy),
    Dispatch = [
        %% Host, Path, Handler, Opts
        {'_', '_', my_handler, []}
    ],
    %% NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy_listener_sup:start_link(100,
        cowboy_tcp_transport, [{port, 8080}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ).

You must also write the `my_handler` module to process requests. You can
use one of the predefined handlers or write your own. An hello world HTTP
handler could be written like this:

    -module(my_handler).
    -export([init/2, handle/2]).

    init(Req, Opts) ->
        {ok, undefined}.

    handle(Req, State) ->
        {reply, 200, [], "Hello World!"}.
