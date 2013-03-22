ROADMAP
=======

This document explains in as much details as possible the
list of planned changes and work to be done on the Cowboy
server. It is non-exhaustive and subject to change. Items
are not ordered.

 *  Add and improve examples

 *  Improve user guide

    We need feedback to improve the guide.

 *  Add and improve tests

    Amongst the areas less tested there is protocol upgrades
    and the REST handler.

    While eunit and ct tests are fine, some parts of the
    code could benefit from PropEr tests.

 *  Continuous performance testing

    Initially dubbed the Horse project, Cowboy could benefit
    from a continuous performance testing tool that would
    allow us to easily compare the impact of the changes we
    are introducing, similar to what the Phoronix test suite
    allows.

    Depending on the test it may be interesting to compare
    Cowboy to other servers and eventually take ideas from
    the servers that outperform Cowboy for the task being tested.

 *  Full HTTP/1.1 support

 *  Improved HTTP/1.0 support

    Most of the work on Cowboy has been done with HTTP/1.1
    in mind. But there is still a need for HTTP/1.0 code in
    Cowboy. The server code should be reviewed and tested
    to ensure compatibility with remaining HTTP/1.0 products.

 *  Continue improving the REST API

 *  SPDY support

The following items pertain to Ranch.

 *  Resizing the acceptor pool

    We should be able to add more acceptors to a pool but also
    to remove some of them as needed.

 *  Add Transport:secure/0

    Currently Cowboy checks if a connection is secure by
    checking if its name is 'ssl'. This isn't a very modular
    solution,  adding an API function that returns whether
    a connection is secure would fix that issue.
