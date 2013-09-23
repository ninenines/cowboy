Sending a response
==================

The Req object also allows you to send a response.

You can only send one response. Any other attempt will
trigger a crash. The response may be sent in one go or
with its body streamed by chunks of arbitrary size.

You can also set headers or the response body in advance
and Cowboy will use them when you finally do reply.

Reply
-----

You can send a reply with no particular headers or body.
Cowboy will make sure to send the mandatory headers with
the response.

``` erlang
{ok, Req2} = cowboy_req:reply(200, Req).
```

You can define headers to be sent with the response. Note
that header names must be lowercase. Again, Cowboy will
make sure to send the mandatory headers with the response.

``` erlang
{ok, Req2} = cowboy_req:reply(303, [
    {<<"location">>, <<"http://ninenines.eu">>}
], Req).
```

You can override headers that Cowboy would send otherwise.
Any header set by the user will be used over the ones set
by Cowboy. For example, you can advertise yourself as a
different server.

``` erlang
{ok, Req2} = cowboy_req:reply(200, [
    {<<"server">>, <<"yaws">>}
], Req).
```

We also saw earlier how to force close the connection by
overriding the connection header.

Finally, you can also send a body with the response. Cowboy
will automatically set the content-length header if you do.
We recommend that you set the content-type header so the
client may know how to read the body.

``` erlang
{ok, Req2} = cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/plain">>
], "Hello world!", Req).
```

Here is the same example but sending HTML this time.

``` erlang
{ok, Req2} = cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/html">>}
], "<html><head>Hello world!</head><body><p>Hats off!</p></body></html>", Req).
```

Note that the reply is sent immediately.

Chunked reply
-------------

You can also stream the response body. First, you need to
initiate the reply by sending the response status code.
Then you can send the body in chunks of arbitrary size.

``` erlang
{ok, Req2} = cowboy_req:chunked_reply(200, Req),
ok = cowboy_req:chunk("Hello...", Req2),
ok = cowboy_req:chunk("chunked...", Req2),
ok = cowboy_req:chunk("world!!", Req2).
```

You should make sure to match on `ok` as an error may be
returned.

While it is possible to send a chunked response without
a content-type header, it is still recommended. You can
set this header or any other just like for normal replies.

``` erlang
{ok, Req2} = cowboy_req:chunked_reply(200, [
    {<<"content-type">>, <<"text/html">>}
], Req),
ok = cowboy_req:chunk("<html><head>Hello world!</head>", Req2),
ok = cowboy_req:chunk("<body><p>Hats off!</p></body></html>", Req2).
```

Note that the reply and each chunk following it are sent
immediately.

Preset response headers
-----------------------

You can define response headers in advance. They will be
merged into the headers given in the reply call. Headers
in the reply call override preset response headers which
override the default Cowboy headers.

``` erlang
Req2 = cowboy_req:set_resp_header(<<"allow">>, "GET", Req).
```

You can check if a response header has already been set.
This will only check the response headers that you set,
and not the ones Cowboy will add when actually sending
the reply.

``` erlang
cowboy_req:has_resp_header(<<"allow">>, Req).
```

It will return `true` if the header is defined, and `false`
otherwise.

Finally, you can also delete a preset response header if
needed. If you do, it will not be sent.

``` erlang
Req2 = cowboy_req:delete_resp_header(<<"allow">>, Req).
```

Preset response body
--------------------

You can set the response body in advance. Note that this
body will be ignored if you then choose to send a chunked
reply, or if you send a reply with an explicit body.

``` erlang
Req2 = cowboy_req:set_resp_body("Hello world!", Req).
```

You can also set a fun that will be called when it is time
to send the body. There are three different ways of doing
that.

If you know the length of the body that needs to be sent,
you should specify it, as it will help clients determine
the remaining download time and allow them to inform the
user.

``` erlang
F = fun (Socket, Transport) ->
    Transport:send(Socket, "Hello world!")
end,
Req2 = cowboy_req:set_resp_body_fun(12, F, Req).
```

If you do not know the length of the body, you should use
a chunked response body fun instead.

``` erlang
F = fun (SendChunk) ->
    Body = lists:duplicate(random:uniform(1024, $a)),
    SendChunk(Body)
end,
Req2 = cowboy_req:set_resp_body_fun(chunked, F, Req).
```

Finally, you can also send data on the socket directly,
without knowing the length in advance. Cowboy may be
forced to close the connection at the end of the response
though depending on the protocol capabilities.

``` erlang
F = fun (Socket, Transport) ->
    Body = lists:duplicate(random:uniform(1024, $a)),
    Transport:send(Socket, Body)
end,
Req2 = cowboy_req:set_resp_body_fun(F, Req).
```

Sending files
-------------

You can send files directly from disk without having to
read them. Cowboy will use the `sendfile` syscall when
possible, which means that the file is sent to the socket
directly from the kernel, which is a lot more performant
than doing it from userland.

Again, it is recommended to set the size of the file if it
can be known in advance.

``` erlang
F = fun (Socket, Transport) ->
    Transport:sendfile(Socket, "priv/styles.css")
end,
Req2 = cowboy_req:set_resp_body_fun(FileSize, F, Req).
```

Please see the Ranch guide for more information about
sending files.
