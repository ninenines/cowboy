The Req object
==============

The Req object is this variable that you will use to obtain
information about a request, read the body of the request
and send a response.

A special variable
------------------

While we call it an "object", it is not an object in the
OOP sense of the term. In fact it is completely opaque
to you and the only way you can perform operations using
it is by calling the functions from the `cowboy_req`
module.

Almost all the calls to the `cowboy_req` module will
return an updated request object. Just like you would
keep the updated `State` variable in a gen_server,
you MUST keep the updated `Req` variable in a Cowboy
handler. Cowboy will use this object to know whether
a response has been sent when the handler has finished
executing.

The Req object allows accessing both immutable and
mutable state. This means that calling some of the
functions twice will not produce the same result.
For example, when streaming the request body, the
function will return the body by chunks, one at a
time, until there is none left.

It also caches the result of operations performed
on the immutable state. That means that some calls
will give a result much faster when called many times.

Overview of the cowboy_req interface
------------------------------------

The `cowboy_req` interface is divided in four groups
of functions, each having a well defined return type
signature common to the entire group.

The first group, access functions, will always return
`{Value, Req}`. The group includes all the following
functions: `binding/{2,3}`, `bindings/1`, `body_length/1`,
`cookie/{2,3}`, `cookies/1`, `header/{2,3}`, `headers/1`,
`host/1`, `host_info/1`, `host_url/1`, `meta/{2,3}`,
`method/1`, `path/1`, `path_info/1`, `peer/1`, `port/1`,
`qs/1`, `qs_val/{2,3}`, `qs_vals/1`, `url/1`, `version/1`.

The second group, question functions, will always return
a `boolean()`. The group includes the following three
functions: `has_body/1`, `has_resp_body/1`, `has_resp_header/2`.

The third group contains the functions that manipulate
the socket or perform operations that may legitimately fail.
They may return `{Result, Req}`, `{Result, Value, Req}`
or `{error, atom()}`. This includes the following functions:
`body/{1,2}`, `body_qs/{1,2}`, `chunked_reply/{2,3}`,
`init_stream/4`, `parse_header/{2,3}`, `reply/{2,3,4}`,
`skip_body/1`, `stream_body/{1,2}`. Finally, the group
also includes the `chunk/2` function which always returns
`ok`.

The final group modifies the Req object, so it always return
a new `Req`. It includes the following functions: `compact/1`,
`delete_resp_header/2`, `set_meta/3`, `set_resp_body/2`,
`set_resp_body_fun/{2,3}`, `set_resp_cookie/4`, `set_resp_header/3`.

This chapter covers most of the first group, plus a few other
functions. The next few chapters cover cookies handling, reading
the request body and sending a response.

Request
-------

When a client performs a request, it first sends a few required
values. They are sent differently depending on the protocol
being used, but the intent is the same. They indicate to the
server the type of action it wants to do and how to locate
the resource to perform it on.

The method identifies the action. Standard methods include
GET, HEAD, OPTIONS, PATCH, POST, PUT, DELETE. Method names
are case sensitive.

``` erlang
{Method, Req2} = cowboy_req:method(Req).
```

The host, port and path parts of the URL identify the resource
being accessed. The host and port information may not be
available if the client uses HTTP/1.0.

``` erlang
{Host, Req2} = cowboy_req:host(Req),
{Port, Req3} = cowboy_req:port(Req2),
{Path, Req4} = cowboy_req:path(Req3).
```

The version used by the client can of course also be obtained.

``` erlang
{Version, Req2} = cowboy_req:version(Req).
```

Do note however that clients claiming to implement one version
of the protocol does not mean they implement it fully, or even
properly.

Bindings
--------

After routing the request, bindings are available. Bindings
are these parts of the host or path that you chose to extract
when defining the routes of your application.

You can fetch a single binding. The value will be `undefined`
if the binding doesn't exist.

``` erlang
{Binding, Req2} = cowboy_req:binding(my_binding, Req).
```

If you need a different value when the binding doesn't exist,
you can change the default.

``` erlang
{Binding, Req2} = cowboy_req:binding(my_binding, Req, 42).
```

You can also obtain all bindings in one call. They will be
returned as a list of key/value tuples.

``` erlang
{AllBindings, Req2} = cowboy_req:bindings(Req).
```

If you used `...` at the beginning of the route's pattern
for the host, you can retrieve the matched part of the host.
The value will be `undefined` otherwise.

``` erlang
{HostInfo, Req2} = cowboy_req:host_info(Req).
```

Similarly, if you used `...` at the end of the route's
pattern for the path, you can retrieve the matched part,
or get `undefined` otherwise.

``` erlang
{PathInfo, Req2} = cowboy_req:path_info(Req).
```

Query string
------------

The query string can be obtained directly.

``` erlang
{Qs, Req2} = cowboy_req:qs(Req).
```

You can also requests only one value.

``` erlang
{QsVal, Req2} = cowboy_req:qs_val(<<"lang">>, Req).
```

If that value is optional, you can define a default to simplify
your task.

``` erlang
{QsVal, Req2} = cowboy_req:qs_val(<<"lang">>, Req, <<"en">>).
```

Finally, you can obtain all query string values.

``` erlang
{AllValues, Req2} = cowboy_req:qs_vals(Req).
```

Request URL
-----------

You can reconstruct the full URL of the resource.

``` erlang
{URL, Req2} = cowboy_req:url(Req).
```

You can also obtain only the base of the URL, excluding the
path and query string.

``` erlang
{BaseURL, Req2} = cowboy_req:host_url(Req).
```

Headers
-------

Cowboy allows you to obtain the header values as string,
or parsed into a more meaningful representation.

This will get the string value of a header.

``` erlang
{HeaderVal, Req2} = cowboy_req:header(<<"content-type">>, Req).
```

You can of course set a default in case the header is missing.

``` erlang
{HeaderVal, Req2}
    = cowboy_req:header(<<"content-type">>, Req, <<"text/plain">>).
```

And also obtain all headers.

``` erlang
{AllHeaders, Req2} = cowboy_req:headers(Req).
```

To parse the previous header, simply call `parse_header/{2,3}`
where you would call `header/{2,3}` otherwise. Note that the
return value changes and includes the result of the operation
as the first element of the returned tuple. A successful parse
returns `ok`.

``` erlang
{ok, ParsedVal, Req2} = cowboy_req:parse_header(<<"content-type">>, Req).
```

When Cowboy doesn't know how to parse the given header, the
result of the operation will be `undefined` and the string value
will be returned instead.

``` erlang
{undefined, HeaderVal, Req2}
    = cowboy_req:parse_header(<<"unicorn-header">>, Req).
```

When parsing fails, `{error, Reason}` is returned instead.

You can of course define a default value. Note that the default
value you specify here is the parsed value you'd like to get
by default.

``` erlang
{ok, ParsedVal, Req2}
    = cowboy_req:parse_header(<<"content-type">>, Req,
    {<<"text">>, <<"plain">>, []}).
```

The list of known headers and default values is defined in the
manual. Also note that the result of parsing is cached, so
calling this function multiple times for the same values will
not have a significant performance impact.

Meta
----

Cowboy will sometimes associate some meta information with
the request. Built-in meta values are listed in the manual
for their respective modules.

This will get a meta value. The returned value will be `undefined`
if it isn't defined.

``` erlang
{MetaVal, Req2} = cowboy_req:meta(websocket_version, Req).
```

You can change the default value if needed.

``` erlang
{MetaVal, Req2} = cowboy_req:meta(websocket_version, Req, 13).
```

You can also define your own meta values. The name must be
an `atom()`.

``` erlang
Req2 = cowboy_req:set_meta(the_answer, 42, Req).
```

Peer
----

You can obtain the peer address and port number. This is
not necessarily the actual IP and port of the client, but
rather the one of the machine that connected to the server.

``` erlang
{{IP, Port}, Req2} = cowboy_req:peer(Req).
```

Reducing the memory footprint
-----------------------------

When you are done reading information from the request object
and know you are not going to access it anymore, for example
when using long-polling or Websocket, you can use the `compact/1`
function to remove most of the data from the request object and
free memory.

``` erlang
Req2 = cowboy_req:compact(Req).
```

You will still be able to send a reply if needed.
