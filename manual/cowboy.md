cowboy
======

The `cowboy` module provides convenience functions for
manipulating Ranch listeners.

Types
-----

### http_headers() = [{binary(), iodata()}]

> HTTP headers as a list of key/values.

### http_status() = non_neg_integer() | binary()

> HTTP status.
>
> A binary status can be used to set a custom message.

### http_version() = 'HTTP/1.1' | 'HTTP/1.0'

> HTTP version.

### onrequest_fun() = fun((cowboy_req:req()) -> cowboy_req:req())

> Fun called immediately after receiving a request.
>
> It can perform any operation on the `Req` object, including
> reading the request body or replying. If a reply is sent,
> the processing of the request ends here, before any middleware
> is executed.

### onresponse_fun() = fun((http_status(), http_headers(),
	iodata(), cowboy_req:req()) -> cowboy_req:req())

> Fun called immediately before sending the response.
>
> It can perform any operation on the `Req` object, including
> reading the request body or replying. If a reply is sent, it
> overrides the reply initially sent. The callback will not be
> called again for the new reply.

Exports
-------

### start_http(Ref, NbAcceptors, TransOpts, ProtoOpts) -> {ok, pid()}

> Types:
>  *  Ref = ranch:ref()
>  *  NbAcceptors = non_neg_integer()
>  *  TransOpts = ranch_tcp:opts()
>  *  ProtoOpts = cowboy_protocol:opts()
>
> Start listening for HTTP connections. Returns the pid for this
> listener's supervisor.

### start_https(Ref, NbAcceptors, TransOpts, ProtoOpts) -> {ok, pid()}

> Types:
>  *  Ref = ranch:ref()
>  *  NbAcceptors = non_neg_integer()
>  *  TransOpts = ranch_ssl:opts()
>  *  ProtoOpts = cowboy_protocol:opts()
>
> Start listening for HTTPS connections. Returns the pid for this
> listener's supervisor.

### start_spdy(Ref, NbAcceptors, TransOpts, ProtoOpts) -> {ok, pid()}

> Types:
>  *  Ref = ranch:ref()
>  *  NbAcceptors = non_neg_integer()
>  *  TransOpts = ranch_ssl:opts()
>  *  ProtoOpts = cowboy_spdy:opts()
>
> Start listening for SPDY connections. Returns the pid for this
> listener's supervisor.

### stop_listener(Ref) -> ok

> Types:
>  *  Ref = ranch:ref()
>
> Stop a previously started listener.

### set_env(Ref, Name, Value) -> ok

> Types:
>  *  Ref = ranch:ref()
>  *  Name = atom()
>  *  Value = any()
>
> Set or update an environment value for an already running listener.
> This will take effect on all subsequent connections.

See also
--------

The [Ranch guide](http://ninenines.eu/docs/en/ranch/HEAD/guide)
provides detailed information about how listeners work.
