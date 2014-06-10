ROADMAP
=======

This document explains in as much details as possible the
list of planned changes and work to be done on the Cowboy
server. It is intended to be exhaustive but some elements
might still be missing.

1.0 (R16 and R17)
-----------------

We are now in the final push to Cowboy 1.0. Further changes
are expected to be bug fixes and documentation improvements.

2.0 (R17 and R18)
-----------------

 *  HTTP/2.0

 *  Websocket permessage deflate compression

 *  Better cowboy_req usability

    The number one usability concern with Cowboy today is
    the need to keep the Req object. Most functions in
    cowboy_req don't actually modify it and probably never
    will. This change will make sure that only the required
    function return a new Req.

    At the same time, some of the functions that cache their
    results will stop to do so. This will save memory and
    allow us to not modify the Req.

 *  Start experimenting with maps.

Under consideration
-------------------

 *  Convenience API for extracting query string and body
    information, similar to PHP's $_GET, $_POST and $_FILES
