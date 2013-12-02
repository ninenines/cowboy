The Cowboy Application
======================

Small, fast, modular HTTP server.

Dependencies
------------

The `cowboy` application uses the Erlang applications `ranch`
for listening and accepting TCP connections, `crypto` for
establishing Websocket connections, and `cowlib` for parsing and
building messages for Web protocols. These dependencies must
be loaded for the `cowboy` application to work. In an embedded
environment this means that they need to be started with the
`application:start/{1,2}` function before the `cowboy`
application is started.

The `cowboy` application also uses the Erlang applications
`asn1`, `public_key` and `ssl` when listening for HTTPS connections.
These are started automatically if they weren't before.

Environment
-----------

The `cowboy` application does not define any application
environment configuration parameters.
