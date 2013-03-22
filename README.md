Cowboy
======

Cowboy is a small, fast and modular HTTP server written in Erlang.

Goals
-----

Cowboy aims to provide a **complete** HTTP stack in a **small** code base.
It is optimized for **low latency** and **low memory usage**, in parts
because it uses **binary strings**.

Cowboy provides **routing** capabilities, selectively dispatching requests
to handlers written in Erlang.

Because it uses Ranch for managing connections, Cowboy can easily be
**embedded** in any other application.

No parameterized module. No process dictionary. **Clean** Erlang code.

Getting Started
---------------

 *  [Read the guide](http://ninenines.eu/docs/en/cowboy/HEAD/guide/introduction)
 *  Look at the examples in the `examples/` directory
 *  Build API documentation with `make docs`; open `doc/index.html`

Support
-------

 *  Official IRC Channel: #ninenines on irc.freenode.net
 *  [Mailing Lists](http://lists.ninenines.eu)
 *  [Commercial Support](http://ninenines.eu/support)
