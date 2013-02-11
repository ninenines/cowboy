REST handlers
=============

Purpose
-------

REST is a set of constraints that, when applied to HTTP, dictates how
resources must behave. It is the recommended way to handle requests
with Cowboy.

REST is implemented in Cowboy as a protocol upgrade. Once upgraded,
the request is handled as a state machine with many optional callbacks
describing the resource and modifying the machine's behavior.

As the REST handler is still subject to change, the documentation is
still thin. This state of affair will be improved in the coming weeks.

Flow diagram
------------

Not done yet. Feel free to use the one that is currently being worked on.

 *  https://github.com/extend/cowboy/pull/364

Callbacks
---------

Please see the Webmachine documentation at this time.

Usage
-----

Please see the examples at this time.
