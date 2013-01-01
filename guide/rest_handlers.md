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

Flow diagram
------------

@todo Add the beautiful flow diagram here.

Callbacks
---------

@todo Describe the callbacks.

Usage
-----

@todo Explain how to use them.
