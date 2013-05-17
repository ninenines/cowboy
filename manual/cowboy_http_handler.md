cowboy_http_handler
===================

The `cowboy_http_handler` behaviour defines the interface used
by plain HTTP handlers.

Unless noted otherwise, the callbacks will be executed sequentially.

Types
-----

None.

Callbacks
---------

### init({TransportName, ProtocolName}, Req, Opts)
	-> {ok, Req, State} | {shutdown, Req, State}

> Types:
>  *  TransportName = tcp | ssl | atom()
>  *  ProtocolName = http | atom()
>  *  Req = cowboy_req:req()
>  *  Opts = any()
>  *  State = any()
>
> Initialize the state for this request.
>
> The `shutdown` return value can be used to skip the `handle/2`
> call entirely.

### handle(Req, State) -> {ok, Req, State}

> Types:
>  *  Req = cowboy_req:req()
>  *  State = any()
>
> Handle the request.
>
> This callback is where the request is handled and a response
> should be sent. If a response is not sent, Cowboy will send
> a `204 No Content` response automatically.

### terminate(Reason, Req, State) -> ok

> Types:
>  *  Reason = {normal, shutdown} | {error, atom()}
>  *  Req = cowboy_req:req()
>  *  State = any()
>
> Perform any necessary cleanup of the state.
>
> This callback should release any resource currently in use,
> clear any active timer and reset the process to its original
> state, as it might be reused for future requests sent on the
> same connection. Typical plain HTTP handlers rarely need to
> use it.
