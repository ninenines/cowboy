Handling Websocket connections
==============================

A special handler is required for handling Websocket connections.
Websocket handlers allow you to initialize the connection,
handle incoming frames from the socket, handle incoming Erlang
messages and then clean up on termination.

Websocket handlers essentially act as a bridge between the client
and the Erlang system. They will typically do little more than
socket communication and decoding/encoding of frames.

Initialization
--------------

First, the `init/3` callback is called. This callback is common
to all handlers. To establish a Websocket connection, this function
must return an `upgrade` tuple.

``` erlang
init(_, Req, Opts) ->
	{upgrade, protocol, cowboy_websocket}.
```

It is also possible to return an update Req object and options
using the longer form of this tuple.

``` erlang
init(_Type, Req, Opts) ->
	{upgrade, protocol, cowboy_websocket, Req, Opts}.
```

Upon receiving this tuple, Cowboy will switch to the code
that handles Websocket connections. It does not immediately
perform the handshake however. First, it calls the `websocket_init/3`
callback.

This function must be used to initialize the state, and can
also be used to register the process, start a timer, etc.
As long as the function returns an `ok` tuple, then Cowboy
performs the Websocket handshake.

``` erlang
websocket_init(_Type, Req, _Opts) ->
    {ok, Req, #state{}}.
```

A `shutdown` tuple can be returned to refuse to perform the
handshake. When doing so, Cowboy will send a `400 Bad Request`
response to the client and close the connection.

``` erlang
websocket_init(_Type, Req, _Opts) ->
	{shutdown, Req}.
```

It is also possible to perform a `cowboy_req:reply/{2,3,4}`
before returning a `shutdown` tuple, allowing you to override
the response sent back to the client.

Note that browser support for handling Websocket connection
failures may vary.

If the sec-websocket-protocol header was sent with the request
for establishing a Websocket connection, then the Websocket
handler *must* select one of these subprotocol and send it
back to the client, otherwise the client might decide to close
the connection, assuming no correct subprotocol was found.

``` erlang
websocket_init(_Type, Req, _Opts) ->
	case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
		{ok, undefined, Req2} ->
			{ok, Req, #state{}};
		{ok, Subprotocols, Req2} ->
			case lists:keymember(<<"mychat2">>, 1, Subprotocols) of
				true ->
					Req3 = cowboy:set_resp_header(<<"sec-websocket-protocol">>,
						<<"mychat2">>, Req2),
					{ok, Req3, #state{}};
				false ->
					{shutdown, Req2}
			end
	end.
```

It is not recommended to wait too long inside the `websocket_init/3`
function. Any extra initialization may be done after returning by
sending yourself a message before doing anything. Any message sent
to `self()` from `websocket_init/3` is guaranteed to arrive before
any frames from the client.

It is also very easy to ensure that this message arrives before
any message from other processes by sending it before registering
or enabling timers.

``` erlang
websocket_init(_Type, Req, _Opts) ->
	self() ! post_init,
	%% Register process here...
	{ok, Req, #state{}}.

websocket_info(post_init, Req, State) ->
	%% Perform post_init initialization here...
	{ok, Req, State}.
```

Handling frames from the client
-------------------------------

Cowboy will call `websocket_handle/3` whenever a text, binary,
ping or pong frame arrives from the client. Note that in the
case of ping and pong frames, no action is expected as Cowboy
automatically replies to ping frames.

The handler can decide to send frames to the socket, shutdown
or just continue without sending anything.

The following snippet echoes back any text frame received and
ignores all others.

``` erlang
websocket_handle(Frame = {text, _}, Req, State) ->
	{reply, Frame, Req, State};
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.
```

Handling Erlang messages
------------------------

Cowboy will call `websocket_info/3` whenever an Erlang message
arrives.

The handler can decide to send frames to the socket, shutdown
or just continue without sending anything.

The following snippet forwards any `log` message to the socket
and ignores all others.

``` erlang
websocket_info({log, Text}, Req, State) ->
	{reply, {text, Text}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.
```

Sending frames to the socket
----------------------------

Cowboy allows sending either a single frame or a list of
frames to the socket. Any frame can be sent: text, binary, ping,
pong or close frames.

The following example sends three frames using a single `reply`
tuple.

``` erlang
websocket_info(hello_world, Req, State) ->
	{reply, [
		{text, "Hello"},
		{text, <<"world!">>},
		{binary, <<0:8000>>}
	], Req, State};
%% More websocket_info/3 clauses here...
```

Note that the payload for text and binary frames is of type
`iodata()`, meaning it can be either a `binary()` or an
`iolist()`.

Sending a `close` frame will immediately initiate the closing
of the Websocket connection. Be aware that any additional
frames sent by the client or any Erlang messages waiting to
be received will not be processed. Also note that when replying
a list of frames that includes close, any frame found after the
close frame will not be sent.

Ping and timeout
----------------

The biggest performance improvement you can do when dealing
with a huge number of Websocket connections is to reduce the
number of timers that are started on the server. A common use
of timers when dealing with connections is for sending a ping
every once in a while. This should be done exclusively on the
client side. Indeed, a server handling one million Websocket
connections will perform a lot better when it doesn't have to
handle one million extra timers too!

Cowboy will automatically respond to ping frames sent by the
client. It will still forward the frame to the handler for
informative purpose, but no further action is required.

Cowboy can be configured to automatically close the Websocket
connection when no data arrives on the socket. It is highly
recommended to configure a timeout for it, as otherwise you
may end up with zombie "half-connected" sockets that may
leave the process alive forever.

A good timeout value is 60 seconds.

``` erlang
websocket_init(_Type, Req, _Opts) ->
	{ok, Req, #state{}, 60000}.
```

This value cannot be changed once it is set. It defaults to
`infinity`.

Hibernate
---------

Most tuples returned from handler callbacks can include an
extra value `hibernate`. After doing any necessary operations
following the return of the callback, Cowboy will hibernate
the process.

It is highly recommended to hibernate processes that do not
handle much traffic. It is a good idea to hibernate all
connections by default and investigate only when you start
noticing increased CPU usage.

Supporting older browsers
-------------------------

Unfortunately Websocket is a relatively recent technology,
which means that not all browsers support it. A library like
[Bullet](https://github.com/extend/bullet) can be used to
emulate Websocket connections on older browsers.
