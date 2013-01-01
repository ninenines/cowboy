Loop handlers
=============

Purpose
-------

Loop handlers are a special kind of HTTP handlers used when the
response can not be sent right away. The handler enters instead
a receive loop waiting for the right message before it can send
a response.

They are most useful when performing long-polling operations or
when using server-sent events.

Callbacks
---------

@todo Describe the callbacks.

Usage
-----

@todo Explain how to use them.
