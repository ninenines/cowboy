Getting started
===============

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
the [Ranch guide](http://ninenines.eu/docs/en/ranch/HEAD/guide/toc)
for in-depth information.

Listeners are named. They spawn a given number of acceptors, listen for
connections using the given transport options and pass along the protocol
options to the connection processes. The protocol options must include
the dispatch list for routing requests to handlers.

The dispatch list is explained in greater details in the
[Routing](routing.md) chapter.

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

Cowboy features many kinds of handlers. For this simple example,
we will just use the plain HTTP handler, which has three callback
functions: init/3, handle/2 and terminate/3. You can find more information
about the arguments and possible return values of these callbacks in the
[cowboy_http_handler function reference](http://ninenines.eu/docs/en/cowboy/HEAD/manual/cowboy_http_handler).
Here is an example of a simple HTTP handler module.

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
is explained in the [cowboy_req function reference](http://ninenines.eu/docs/en/cowboy/HEAD/manual/cowboy_req).

You can find many examples in the `examples/` directory of the
Cowboy repository. A more complete "Hello world" example can be
found in the `examples/hello_world/` directory.
