Introduction
============

Purpose
-------

Cowboy is a small, fast and modular HTTP server written in Erlang.

Cowboy aims to provide a complete HTTP stack, including its derivatives
SPDY, Websocket and REST. Cowboy currently supports HTTP/1.0, HTTP/1.1,
Websocket (all implemented drafts + standard) and Webmachine-based REST.

Cowboy is a high quality project. It has a small code base, is very
efficient (both in latency and memory use) and can easily be embedded
in another application.

Cowboy is clean Erlang code. It bans the use of parameterized modules
and the process dictionary. It includes documentation and typespecs
for all public interfaces.

Prerequisites
-------------

It is assumed the developer already knows Erlang and has basic knowledge
about the HTTP protocol.

In order to run the examples available in this user guide, you will need
Erlang and rebar installed and in your $PATH.

Please see the [rebar repository](https://github.com/basho/rebar) for
downloading and building instructions. Please look up the environment
variables documentation of your system for details on how to update the
$PATH information.

Supported platforms
-------------------

Cowboy is tested and supported on Linux.

Cowboy has been reported to work on other platforms, but we make no
guarantee that the experience will be safe and smooth. You are advised
to perform the necessary testing and security audits prior to deploying
on other platforms.

Cowboy is developed for Erlang R15B+.

Cowboy may be compiled on earlier Erlang versions with small source code
modifications but there is no guarantee that it will work as expected.

Conventions
-----------

In the HTTP protocol, the method name is case sensitive. All standard
method names are uppercase.

Header names are case insensitive. Cowboy converts all the request
header names to lowercase, and expects your application to provide
lowercase header names in the response.

Getting started
---------------

Cowboy does nothing by default.

Cowboy requires the `crypto` and `ranch` applications to be started.

``` erlang
ok = application:start(crypto).
ok = application:start(ranch).
ok = application:start(cowboy).
```

Cowboy uses Ranch for handling the connections and provides convenience
functions to start Ranch listeners.

The `cowboy:start_http/4` function starts a listener for HTTP connections
using the TCP transport. The `cowboy:start_https/4` function starts a
listener for HTTPS connections using the SSL transport.

Listeners are a group of processes that are used to accept and manage
connections. The processes used specifically for accepting connections
are called acceptors. The number of acceptor processes is unrelated to
the maximum number of connections Cowboy can handle. Please refer to
the Ranch guide for in-depth information.

Listeners are named. They spawn a given number of acceptors, listen for
connections using the given transport options and pass along the protocol
options to the connection processes. The protocol options must include
the dispatch list for routing requests to handlers.

The dispatch list is explained in greater details in the Routing section
of the guide.

``` erlang
Dispatch = cowboy_router:compile([
    %% {URIHost, list({URIPath, Handler, Opts})}
    {'_', [{'_', my_handler, []}]}
]),
%% Name, NbAcceptors, TransOpts, ProtoOpts
cowboy:start_http(my_http_listener, 100,
    [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
).
```

Cowboy features many kinds of handlers. It has plain HTTP handlers, loop
handlers, Websocket handlers, REST handlers and static handlers. Their
usage is documented in the respective sections of the guide.

Most applications use the plain HTTP handler, which has three callback
functions: init/3, handle/2 and terminate/3. You can find more information
about the arguments and possible return values of these callbacks in the
HTTP handlers section of this guide. Following is an example of a simple
HTTP handler module.

``` erlang
-module(my_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init({tcp, http}, Req, Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [], <<"Hello World!">>, Req),
    {ok, Req2, State}.

terminate(Reason, Req, State) ->
    ok.
```

The `Req` variable above is the Req object, which allows the developer
to obtain information about the request and to perform a reply. Its usage
is explained in its respective section of the guide.
