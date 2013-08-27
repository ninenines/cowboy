cowboy_websocket
================

The `cowboy_websocket` module implements the Websocket protocol.

The callbacks for websocket handlers are defined in the manual
for the `cowboy_websocket_handler` behaviour.

Types
-----

### close_code() = 1000..4999

> Reason for closing the connection.

### frame() = close | ping | pong
	| {text | binary | close | ping | pong, iodata()}
	| {close, close_code(), iodata()}

> Frames that can be sent to the client.

Meta values
-----------

### websocket_compress

> Type: true | false
>
> Whether a websocket compression extension in in use.

### websocket_version

> Type: 7 | 8 | 13
>
> The version of the Websocket protocol being used.

Exports
-------

None.
