Request object
==============

Purpose
-------

The request object is a special variable that can be used
to interact with a request, extracting information from it
or modifying it, and sending a response.

It's a special variable because it contains both immutable
and mutable state. This means that some operations performed
on the request object will always return the same result,
while others will not. For example, obtaining request headers
can be repeated safely. Obtaining the request body can only
be done once, as it is read directly from the socket.

With few exceptions, all calls to the `cowboy_req` module
will return an updated request object. You MUST use the new
request object instead of the old one for all subsequent
operations.

Request
-------

Cowboy allows you to retrieve a lot of information about
the request. All these calls return a `{Value, Req}` tuple,
with `Value` the requested value and `Req` the updated
request object.

The following access functions are defined in `cowboy_req`:

 *  `method/1`: the request method (`<<"GET">>`, `<<"POST">>`...)
 *  `version/1`: the HTTP version (`{1,0}` or `{1,1}`)
 *  `peer/1`: the peer address and port number
 *  `host/1`: the hostname requested
 *  `host_info/1`: the result of the `[...]` match on the host
 *  `port/1`: the port number used for the connection
 *  `path/1`: the path requested
 *  `path_info/1`: the result of the `[...]` match on the path
 *  `qs/1`: the entire query string unmodified
 *  `qs_val/{2,3}`: the value for the requested query string key
 *  `qs_vals/1`: all key/values found in the query string
 *  `fragment/1`: the fragment part of the URL (e.g. `#nav-links`)
 *  `host_url/1`: the requested URL without the path, qs and fragment
 *  `url/1`: the requested URL
 *  `binding/{2,3}`: the value for the requested binding found during routing
 *  `bindings/1`: all key/values found during routing
 *  `header/{2,3}`: the value for the requested header name
 *  `headers/1`: all headers name/value
 *  `cookie/{2,3}`: the value for the requested cookie name
 *  `cookies/1`: all cookies name/value
 *  `meta/{2,3}`: the meta information for the requested key

All the functions above that can take two or three arguments
take an optional third argument for the default value if
none is found. Otherwise it will return `undefined`.

In addition, Cowboy allows you to parse headers using the
`parse_header/{2,3}` function, which takes a header name
as lowercase binary, the request object, and an optional
default value. It returns `{ok, ParsedValue, Req}` if it
could be parsed, `{undefined, RawValue, Req}` if Cowboy
doesn't know this header, and `{error, badarg}` if Cowboy
encountered an error while trying to parse it.

Finally, Cowboy allows you to set request meta information
using the `set_meta/3` function, which takes a name, a value
and the request object and returns the latter modified.

Request body
------------

Cowboy will not read the request body until you ask it to.
If you don't, then Cowboy will simply discard it. It will
not take extra memory space until you start reading it.

Cowboy has a few utility functions for dealing with the
request body.

The function `has_body/1` will return whether the request
contains a body. Note that some clients may not send the
right headers while still sending a body, but as Cowboy has
no way of detecting it this function will return `false`.

The function `body_length/1` retrieves the size of the
request body. If the body is compressed, the value returned
here is the compressed size. If a `Transfer-Encoding` header
was passed in the request, then Cowboy will return a size
of `undefined`, as it has no way of knowing it.

If you know the request contains a body, and that it is
within 8MB (for `body/1`) or 16KB (for `body_qs/1`) bytes,
then you can read it directly with either `body/1` or `body_qs/1`.
If you want to override the default size limits of `body/1`
or `body_qs/1`, you can pass the maximum body length byte
size as first parameter to `body/2` and `body_qs/2` or pass
atom `infinity` to ignore size limits.

If the request contains bigger body than allowed default sizes
or supplied maximum body length, `body/1`, `body/2`, `body_qs/1`
and `body_qs/2` will return `{error, badlength}`. If the request
contains chunked body, `body/1`, `body/2`, `body_qs/1`
and `body_qs/2` will return `{error, chunked}`.
If you get either of the above two errors, you will want to
handle the body of the request using `stream_body/1`,
`stream_body/2` and `skip_body/1`, with the streaming process
optionally initialized using `init_stream/4`.

Multipart request body
----------------------

Cowboy provides facilities for dealing with multipart bodies.
They are typically used for uploading files. You can use two
functions to process these bodies, `multipart_data/1` and
`multipart_skip/1`.

Response
--------

You can send a response by calling the `reply/{2,3,4}` function.
It takes the status code for the response (usually `200`),
an optional list of headers, an optional body and the request
object.

The following snippet sends a simple response with no headers
specified but with a body.

``` erlang
{ok, Req2} = cowboy_req:reply(200, [], "Hello world!", Req).
```

If this is the only line in your handler then make sure to return
the `Req2` variable to Cowboy so it can know you replied.

If you want to send HTML you'll need to specify the `Content-Type`
header so the client can properly interpret it.

``` erlang
{ok, Req2} = cowboy_req:reply(200,
    [{<<"content-type">>, <<"text/html">>}],
	"<html><head>Hello world!</head><body><p>Hats off!</p></body></html>",
	Req).
```

You only need to make sure to follow conventions and to use a
lowercase header name.

Chunked response
----------------

You can also send chunked responses using `chunked_reply/{2,3}`.
Chunked responses allow you to send the body in chunks of various
sizes. It is the recommended way of performing streaming if the
client supports it.

You must first initiate the response by calling the aforementioned
function, then you can call `chunk/2` as many times as needed.
The following snippet sends a body in three chunks.

``` erlang
{ok, Req2} = cowboy_req:chunked_reply(200, Req),
ok = cowboy_req:chunk("Hello...", Req2),
ok = cowboy_req:chunk("chunked...", Req2),
ok = cowboy_req:chunk("world!!", Req2).
```

As you can see the call to `chunk/2` does not return a modified
request object. It may return an error, however, so you should
make sure that you match the return value on `ok`.

Response preconfiguration
-------------------------

Cowboy allows you to set response cookies, headers or body
in advance without having to send the response at the same time.
Then, when you decide to send it, all these informations will be
built into the resulting response.

Some of the functions available for this purpose also give you
additional functionality, like `set_resp_cookie/4` which will build
the appropriate `Set-Cookie` header, or `set_resp_body_fun/{2,3}`
which allows you to stream the response body.

Note that any value given directly to `reply/{2,3,4}` will
override all preset values. This means for example that you
can set a default body and then override it when you decide
to send a reply.

Closing the connection
----------------------

HTTP/1.1 keep-alive allows clients to send more than one request
on the same connection. This can be useful for speeding up the
loading of webpages, but is not required. You can tell Cowboy
explicitly that you want to close the connection by setting the
`Connection` header to `close`.

``` erlang
{ok, Req2} = cowboy_req:reply(200,
    [{<<"connection">>, <<"close">>}],
    Req).
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
