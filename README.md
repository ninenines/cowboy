Cowboy
======

Cowboy is a small, fast and modular HTTP server written in Erlang.

Goals
-----

Cowboy aims to provide a **complete** HTTP stack in a **small** code base.
It is optimized for **low latency** and **low memory usage**, in part
because it uses **binary strings**.

Cowboy provides **routing** capabilities, selectively dispatching requests
to handlers written in Erlang.

Because it uses Ranch for managing connections, Cowboy can easily be
**embedded** in any other application.

No parameterized module. No process dictionary. **Clean** Erlang code.

Sponsors
--------

The SPDY implementation was sponsored by
[LeoFS Cloud Storage](http://www.leofs.org).

The project is currently sponsored by
[Kato.im](https://kato.im).

Getting Started
---------------

 *  [Read the guide](http://ninenines.eu/docs/en/cowboy/HEAD/guide)
 *  [Check the manual](http://ninenines.eu/docs/en/cowboy/HEAD/manual)
 *  Look at the examples in the `examples/` directory

Support
-------

 *  Official IRC Channel: #ninenines on irc.freenode.net
 *  [Mailing Lists](http://lists.ninenines.eu)
 *  [Commercial Support](http://ninenines.eu/support)
