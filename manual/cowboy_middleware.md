cowboy_middleware
=================

The `cowboy_middleware` behaviour defines the interface used
by Cowboy middleware modules.

Middlewares process the request sequentially in the order they
are configured.

Types
-----

### env() = [{atom(), any()}]

> The environment variable.
>
> One is created for every request. It is passed to each
> middleware module executed and subsequently returned,
> optionally with its contents modified.

Callbacks
---------

### execute(Req, Env)
	-> {ok, Req, Env}
	| {suspend, Module, Function, Args}
	| {halt, Req}
	| {error, StatusCode, Req}

> Types:
>  *  Req = cowboy_req:req()
>  *  Env = env()
>  *  Module = module()
>  *  Function = atom()
>  *  Args = [any()]
>  *  StatusCode = cowboy:http_status()
>
> Execute the middleware.
>
> The `ok` return value indicates that everything went well
> and that Cowboy should continue processing the request. A
> response may or may not have been sent.
>
> The `suspend` return value will hibernate the process until
> an Erlang message is received. Note that when resuming, any
> previous stacktrace information will be gone.
>
> The `halt` return value stops Cowboy from doing any further
> processing of the request, even if there are middlewares
> that haven't been executed yet. The connection may be left
> open to receive more requests from the client.
>
> The `error` return value sends an error response identified
> by the `StatusCode` and then proceeds to terminate the
> connection. Middlewares that haven't been executed yet
> will not be called.
