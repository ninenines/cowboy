The Websocket protocol
======================

This chapter explains what Websocket is and why it is
a vital component of soft realtime Web applications.

Description
-----------

Websocket is an extension to HTTP that emulates plain TCP
connections between the client, typically a Web browser,
and the server. It uses the HTTP Upgrade mechanism to
establish the connection.

Websocket connections are asynchronous, unlike HTTP. This
means that not only can the client send frames to the server
at any time, but the server can also send frames to the client
without the client initiating anything other than the
Websocket connection itself. This allows the server to push
data to the client directly.

Websocket is an IETF standard. Cowboy supports the standard
and all drafts that were previously implemented by browsers,
excluding the initial flawed draft sometimes known as
"version 0".

Implementation
--------------

Cowboy implements Websocket as a protocol upgrade. Once the
upgrade is performed from the `init/3` callback, Cowboy
switches to Websocket. Please consult the next chapter for
more information on initiating and handling Websocket
connections.

The implementation of Websocket in Cowboy is validated using
the Autobahn test suite, which is an extensive suite of tests
covering all aspects of the protocol. Cowboy passes the
suite with 100% success, including all optional tests.

Cowboy's Websocket implementation also includes the
x-webkit-deflate-frame compression draft which is being used
by some browsers to reduce the size of data being transmitted.
Cowboy will automatically use compression as long as the
`compress` protocol option is set when starting the listener.
