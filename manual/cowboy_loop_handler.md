cowboy_loop_handler
===================

The `cowboy_loop_handler` behaviour defines the interface used
by HTTP handlers that do not send a response directly, instead
requiring a receive loop to process Erlang messages.

This interface is best fit for long-polling types of requests.

The `init/3` callback will always be called, followed by zero
or more calls to `info/3`. The `terminate/3` will always be
called last.

Types
-----

None.

Callbacks
---------

### init({TransportName, ProtocolName}, Req, Opts)
	-> {loop, Req, State}
	| {loop, Req, State, hibernate}
	| {loop, Req, State, Timeout}
	| {loop, Req, State, Timeout, hibernate}
	| {shutdown, Req, State}

> Types:
>  *  TransportName = tcp | ssl | atom()
>  *  ProtocolName = http | atom()
>  *  Req = cowboy_req:req()
>  *  Opts = any()
>  *  State = any()
>  *  Timeout = timeout()
>
> Initialize the state for this request.
>
> This callback will typically be used to register this process
> to an event manager or a message queue in order to receive
> the messages the handler wants to process.
>
> The receive loop will run for a duration of up to `Timeout`
> milliseconds after it last received data from the socket,
> at which point it will stop and send a `204 No Content` reply.
> By default this value is set to `infinity`. It is recommended
> to either set this value or ensure by any other mechanism
> that the handler will be closed after a certain period of
> inactivity.
>
> The `hibernate` option will hibernate the process until it
> starts receiving messages.
>
> The `shutdown` return value can be used to skip the receive
> loop entirely.

### info(Info, Req, State) -> {ok, Req, State} | {loop, Req, State}
	| {loop, Req, State, hibernate}

> Types:
>  *  Info = any()
>  *  Req = cowboy_req:req()
>  *  State = any()
>
> Handle the Erlang message received.
>
> This function will be called every time an Erlang message
> has been received. The message can be any Erlang term.
>
> The `ok` return value can be used to stop the receive loop,
> typically because a response has been sent.
>
> The `hibernate` option will hibernate the process until
> it receives another message.

### terminate(Reason, Req, State) -> ok

> Types:
>  *  Reason = {normal, shutdown} | {normal, timeout} | {error, closed} | {error, overflow} | {error, atom()}
>  *  Req = cowboy_req:req()
>  *  State = any()
>
> Perform any necessary cleanup of the state.
>
> This callback will typically unregister from any event manager
> or message queue it registered to in `init/3`.
>
> This callback should release any resource currently in use,
> clear any active timer and reset the process to its original
> state, as it might be reused for future requests sent on the
> same connection.
