ROADMAP
=======

This document explains in as much details as possible the
list of planned changes and work to be done on the Cowboy
server. It is intended to be exhaustive but some elements
might still be missing.

All the following items must be done before Cowboy 1.0 is
released.

 *  Parse support for all standard HTTP/1.1 headers

 *  Support for multipart requests and responses

 *  Convenience API for extracting query string and body
    information, similar to PHP's $_GET, $_POST and $_FILES

 *  Add Range support to REST

 *  SPDY support

    We are only interested in supporting existing
    implementations, not the full protocol, as this
    protocol has been abandoned in favor of HTTP/2.0

 *  Complete the user guide

The following items pertain to Ranch, but are equally important.

 *  Resizing the acceptor pool

    We should be able to add more acceptors to a pool but also
    to remove some of them as needed

 *  Add Transport:secure/0

    Currently Cowboy checks if a connection is secure by
    checking if its name is 'ssl'. This isn't a very modular
    solution,  adding an API function that returns whether
    a connection is secure would fix that issue
