Reading the request body
========================

The Req object also allows you to read the request body.

Because the request body can be of any size, all body
reading operations will only work once, as Cowboy will
not cache the result of these operations.

Cowboy will not attempt to read the body until you do.
If handler execution ends without reading it, Cowboy
will simply skip it.

Check for request body
----------------------

You can check whether a body was sent with the request.

``` erlang
cowboy_req:has_body(Req).
```

It will return `true` if there is a request body, and
`false` otherwise.

Note that it is generally safe to assume that a body is
sent for `POST`, `PUT` and `PATCH` requests, without
having to explicitly check for it.

Request body length
-------------------

You can obtain the body length if it was sent with the
request.

``` erlang
{Length, Req2} = cowboy_req:body_length(Req).
```

The value returned will be `undefined` if the length
couldn't be figured out from the request headers. If
there's a body but no length is given, this means that
the chunked transfer-encoding was used. You can read
chunked bodies by using the stream functions.

Reading the body
----------------

If a content-length header was sent with the request,
you can read the whole body directly.

``` erlang
{ok, Body, Req2} = cowboy_req:body(Req).
```

If no content-length header is available, Cowboy will
return the `{error, chunked}` tuple. You will need to
stream the request body instead.

By default, Cowboy will reject all body sizes above 8MB,
to prevent an attacker from needlessly filling up memory.
You can override this limit however.

``` erlang
{ok, Body, Req2} = cowboy_req:body(100000000, Req).
```

You can also disable it.

``` erlang
{ok, Body, Req2} = cowboy_req:body(infinity, Req).
```

It is recommended that you do not disable it for public
facing websites.

Reading a body sent from an HTML form
-------------------------------------

You can directly obtain a list of key/value pairs if the
body was sent using the application/x-www-form-urlencoded
content-type.

``` erlang
{ok, KeyValues, Req2} = cowboy_req:body_qs(Req).
```

You can then retrieve an individual value from that list.

``` erlang
{_, Lang} = lists:keyfind(lang, 1, KeyValues).
```

You should not attempt to match on the list as the order
of the values is undefined.

By default Cowboy will reject bodies with a size above
16KB when using this function. You can override this limit.

``` erlang
{ok, KeyValues, Req2} = cowboy_req:body_qs(500000, Req).
```

You can also disable it by passing the atom `infinity`,
although it is not recommended.

Streaming the body
------------------

You can stream the request body by chunks.

``` erlang
{ok, Chunk, Req2} = cowboy_req:stream_body(Req).
```

By default, Cowboy will attempt to read chunks of up to
1MB in size. The chunks returned by this function will
often be smaller, however. You can also change this limit.

``` erlang
{ok, Chunk, Req2} = cowboy_req:stream_body(500000, Req).
```

When Cowboy finishes reading the body, any subsequent call
will return `{done, Req2}`. You can thus write a recursive
function to read the whole body and perform an action on
all chunks, for example printing them to the console.

``` erlang
body_to_console(Req) ->
    case cowboy_req:stream_body(Req) of
        {ok, Chunk, Req2} ->
            io:format("~s", [Chunk]),
            body_to_console(Req2);
        {done, Req2} ->
            Req2
    end.
```

Advanced streaming
------------------

Cowboy will by default decode the chunked transfer-encoding
if any. It will not decode any content-encoding by default.

Before starting to stream, you can configure the functions
that will be used for decoding both transfer-encoding and
content-encoding.

``` erlang
{ok, Req2} = cowboy_req:init_stream(fun transfer_decode/2,
    TransferStartState, fun content_decode/1, Req).
```

Note that you do not need to call this function generally,
as Cowboy will happily initialize the stream on its own.

Skipping the body
-----------------

If you do not need the body, or if you started streaming
the body but do not need the rest of it, you can skip it.

``` erlang
{ok, Req2} = cowboy_req:skip_body(Req).
```

You do not have to call this function though, as Cowboy will
do it automatically when handler execution ends.
