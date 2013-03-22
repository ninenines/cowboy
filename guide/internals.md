Internals
=========

Architecture
------------

Cowboy is a lightweight HTTP server.

It is built on top of Ranch. Please see the Ranch guide for more
informations.

It uses only one process per connection. The process where your
code runs is the process controlling the socket. Using one process
instead of two allows for lower memory usage.

It uses binaries. Binaries are more efficient than lists for
representing strings because they take less memory space. Processing
performance can vary depending on the operation. Binaries are known
for generally getting a great boost if the code is compiled natively.
Please see the HiPE documentation for more details.

Because querying for the current date and time can be expensive,
Cowboy generates one `Date` header value every second, shares it
to all other processes, which then simply copy it in the response.
This allows compliance with HTTP/1.1 with no actual performance loss.

One process for many requests
-----------------------------

As previously mentioned, Cowboy only use one process per connection.
Because there can be more than one request per connection with the
keepalive feature of HTTP/1.1, that means the same process will be
used to handle many requests.

Because of this, you are expected to make sure your process cleans
up before terminating the handling of the current request. This may
include cleaning up the process dictionary, timers, monitoring and
more.

Lowercase header names
----------------------

For consistency reasons it has been chosen to convert all header names
to lowercase binary strings. This prevents the programmer from making
case mistakes, and is possible because header names are case insensitive.

This works fine for the large majority of clients. However, some badly
implemented clients, especially ones found in corporate code or closed
source products, may not handle header names in a case insensitive manner.
This means that when Cowboy returns lowercase header names, these clients
will not find the headers they are looking for.

A simple way to solve this is to create an `onresponse` hook that will
format the header names with the expected case.

``` erlang
capitalize_hook(Status, Headers, Body, Req) ->
    Headers2 = [{cowboy_bstr:capitalize_token(N), V}
        || {N, V} <- Headers],
    {ok, Req2} = cowboy_req:reply(Status, Headers2, Body, Req),
    Req2.
```

Improving performance
---------------------

By default the maximum number of active connections is set to a
generally accepted big enough number. This is meant to prevent having
too many processes performing potentially heavy work and slowing
everything else down, or taking up all the memory.

Disabling this feature, by setting the `{max_connections, infinity}`
protocol option, would give you greater performance when you are
only processing short-lived requests.

Another option is to define platform-specific socket options that
are known to improve their efficiency.

Please see the Ranch guide for more information.
