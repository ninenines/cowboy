Internals
=========

Architecture
------------

@todo Describe.

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

Efficiency considerations
-------------------------

@todo Mention that you need to cleanup in terminate especially if you
used the process dictionary, started timers, started monitoring...
