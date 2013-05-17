cowboy_websocket_handler
========================

The `cowboy_websocket_handler` behaviour defines the interface used
by Websocket handlers.

The `init/3` and `websocket_init/3` callbacks will always be called,
followed by zero or more calls to `websocket_handle/3` and
`websocket_info/3`. The `websocket_terminate/3` will always
be called last.

Types
-----

None.

Callbacks
---------

### init({TransportName, ProtocolName}, Req, Opts)
	-> {upgrade, protocol, cowboy_websocket}
	| {upgrade, protocol, cowboy_websocket, Req, Opts}

> Types:
>  *  TransportName = tcp | ssl | atom()
>  *  ProtocolName = http | atom()
>  *  Req = cowboy_req:req()
>  *  Opts = any()
>
> Upgrade the protocol to `cowboy_websocket`.

### websocket_init(TransportName, Req, Opts)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {ok, Req, State, Timeout}
	| {ok, Req, State, Timeout, hibernate}
	| {shutdown, Req}

> Types:
>  *  TransportName = tcp | ssl | atom()
>  *  Req = cowboy_req:req()
>  *  Opts = any()
>  *  State = any()
>  *  Timeout = timeout()
>
> Initialize the state for this session.
>
> This function is called before the upgrade to Websocket occurs.
> It can be used to negotiate Websocket protocol extensions
> with the client. It will typically be used to register this process
> to an event manager or a message queue in order to receive
> the messages the handler wants to process.
>
> The connection will stay up for a duration of up to `Timeout`
> milliseconds after it last received data from the socket,
> at which point it will stop and close the connection.
> By default this value is set to `infinity`. It is recommended
> to either set this value or ensure by any other mechanism
> that the handler will be closed after a certain period of
> inactivity.
>
> The `hibernate` option will hibernate the process until it
> starts receiving either data from the Websocket connection
> or Erlang messages.
>
> The `shutdown` return value can be used to close the connection
> before upgrading to Websocket.

### websocket_handle(InFrame, Req, State)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {reply, OutFrame | [OutFrame], Req, State}
	| {reply, OutFrame | [OutFrame], Req, State, hibernate}
	| {shutdown, Req, State}

> Types:
>  *  InFrame = {text | binary | ping | pong, binary()}
>  *  Req = cowboy_req:req()
>  *  State = any()
>  *  OutFrame = cowboy_websocket:frame()
>
> Handle the data received from the Websocket connection.
>
> This function will be called every time data is received
> from the Websocket connection.
>
> The `shutdown` return value can be used to close the
> connection. A close reply will also result in the connection
> being closed.
>
> The `hibernate` option will hibernate the process until
> it receives new data from the Websocket connection or an
> Erlang message.

### websocket_info(Info, Req, State)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {reply, OutFrame | [OutFrame], Req, State}
	| {reply, OutFrame | [OutFrame], Req, State, hibernate}
	| {shutdown, Req, State}

> Types:
>  *  Info = any()
>  *  Req = cowboy_req:req()
>  *  State = any()
>  *  OutFrame = cowboy_websocket:frame()
>
> Handle the Erlang message received.
>
> This function will be called every time an Erlang message
> has been received. The message can be any Erlang term.
>
> The `shutdown` return value can be used to close the
> connection. A close reply will also result in the connection
> being closed.
>
> The `hibernate` option will hibernate the process until
> it receives another message or new data from the Websocket
> connection.

### websocket_terminate(Reason, Req, State) -> ok

> Types:
>  *  Reason = {normal, shutdown | timeout} | {remote, closed} | {remote, cowboy_websocket:close_code(), binary()} | {error, badencoding | badframe | closed | atom()}
>  *  Req = cowboy_req:req()
>  *  State = any()
>
> Perform any necessary cleanup of the state.
>
> The connection will be closed and the process stopped right
> after this call.
