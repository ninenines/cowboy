ROADMAP
=======

This document explains in as much details as possible the
list of planned changes and work to be done on the Cowboy
server. It is non-exhaustive and subject to change. Items
are not ordered.

*   Write more, better examples.

    The first step would be to port misultin's examples
    to Cowboy. Then these examples could be completed with
    examples for Cowboy specific features.

    The extend/cowboy_examples is to be used for this. As
    it is a separate repository, we can organize the file
    structure as appropriate. Ideally we would have one
    complete example per folder.

    Examples should be commented. They may or may not be
    used for writing the user guides.

*   Write user guides.

    We currently have good API documentation, but no step
    by step user guides.

*   Write more, better tests.

    Amongst the areas less tested there is protocol upgrades
    and the REST handler.

    Current tests should be completed with unit tests
    where applicable. We should probably also test the
    dependencies used, like erlang:decode_packet/3.

    While eunit and ct tests are fine, some parts of the
    code could benefit from PropEr tests.

*   Continuous performance testing.

    Initially dubbed the Horse project, Cowboy could benefit
    from a continuous performance testing tool that would
    allow us to easily compare the impact of the changes we
    are introducing, similar to what the Phoronix test suite
    allows.

    Depending on the test it may be interesting to compare
    Cowboy to other servers and eventually take ideas from
    the servers that outperform Cowboy for the task being tested.

*   Improve HTTP/1.0 support.

    Most of the work on Cowboy has been done with HTTP/1.1
    in mind. But there is still a need for HTTP/1.0 code in
    Cowboy. The server code should be reviewed and tested
    to ensure compatibility with remaining HTTP/1.0 products.

*   Complete the work on Websockets.

    Now that the Autobahn test suite is available (make inttests),
    we have a definite way to know whether Cowboy's implementation
    of Websockets is right. The work can thus be completed. The
    remaining task is proper UTF8 handling.

*   SPDY support.

    While SPDY probably won't be added directly to Cowboy, work
    has been started on making Cowboy use SPDY.

*   Transport upgrades.

    Some protocols allow an upgrade from TCP to SSL without
    closing the connection. This is currently not possible
    through the Cowboy API.

*   Resizing the acceptor pool.

    We should be able to add more acceptors to a pool but also
    to remove some of them as needed.

*   Simplified dispatch list.

    For convenience purposes, the dispatch list should allow
    lists instead of binaries. The lists can be converted to
    binary by Cowboy at listener initialization.

    There has also been discussion on allowing the dispatch
    list to be hierarchical.

*   Add Transport:secure/0.

    Currently Cowboy checks if a connection is secure by
    checking if its name is 'ssl'. This isn't a very modular
    solution,  adding an API function that returns whether
    a connection is secure would fix that issue.
