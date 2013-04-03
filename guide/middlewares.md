Middlewares
===========

Purpose
-------

Cowboy delegates the request processing to middleware components.
By default, two middlewares are defined, for the routing and handling
of the request, as is detailed in most of this guide.

Middlewares give you complete control over how requests are to be
processed. You can add your own middlewares to the mix or completely
change the chain of middlewares as needed.

Cowboy will execute all middlewares in the given order, unless one
of them decides to stop processing.

Usage
-----

Middlewares only need to implement a single callback: `execute/2`.
It is defined in the `cowboy_middleware` behavior.

This callback has two arguments. The first is the `Req` object.
The second is the environment.

Middlewares can return one of four different values:
 *  `{ok, Req, Env}` to continue the request processing
 *  `{suspend, Module, Function, Args}` to hibernate
 *  `{halt, Req}` to stop processing and move on to the next request
 *  `{error, StatusCode, Req}` to reply an error and close the socket

Of note is that when hibernating, processing will resume on the given
MFA, discarding all previous stacktrace. Make sure you keep the `Req`
and `Env` in the arguments of this MFA for later use.

If an error happens during middleware processing, Cowboy will not try
to send an error back to the socket, the process will just crash. It
is up to the middleware to make sure that a reply is sent if something
goes wrong.

Configuration
-------------

The middleware environment is defined as the `env` protocol option.
In the previous chapters we saw it briefly when we needed to pass
the routing information. It is a list of tuples with the first
element being an atom and the second any Erlang term.

Two values in the environment are reserved:
 *  `listener` contains the name of the listener
 *  `result` contains the result of the processing

The `listener` value is always defined. The `result` value can be
set by any middleware. If set to anything other than `ok`, Cowboy
will not process any subsequent requests on this connection.

The middlewares that come with Cowboy may define or require other
environment values to perform.

You can update the environment by calling the `cowboy:set_env/3`
convenience function, adding or replacing a value in the environment.

Routing middleware
------------------

The routing middleware requires the `dispatch` value. If routing
succeeds, it will put the handler name and options in the `handler`
and `handler_opts` values of the environment, respectively.

Handler middleware
------------------

The handler middleware requires the `handler` and `handler_opts`
values. It puts the result of the request handling into `result`.
