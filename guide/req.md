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

All calls to the `cowboy_req` module will return an updated
request object. You MUST use the new request object instead
of the old one for all subsequent operations.

Request
-------

@todo Describe.

Request body
------------

@todo Describe.

Response
--------

@todo Describe.
