cowboy_req
==========

The `cowboy_req` module provides functions to access, manipulate
and respond to requests.

The functions in this module follow patterns for their return types,
based on the kind of function.

 *  access: `{Value, Req}`
 *  action: `{Result, Req} | {Result, Value, Req} | {error, atom()}`
 *  modification: `Req`
 *  question: `boolean()`

The only exception is the `chunk/2` function which may return `ok`.

Whenever `Req` is returned, you must use this returned value and
ignore any previous you may have had. This value contains various
state informations which are necessary for Cowboy to do some lazy
evaluation or cache results where appropriate.

Types
-----

### cookie_opts() = [{max_age, non_neg_integer()}
	| {domain, binary()} | {path, binary()}
	| {secure, boolean()} | {http_only, boolean()}]

> Cookie options.

### req() - opaque to the user

> The `Req` object.
>
> All functions in this module receive a `Req` as argument,
> and most of them return a new object labelled `Req2` in
> the function descriptions below.

Request related exports
-----------------------

### binding(Name, Req) -> binding(Name, Req, undefined)
### binding(Name, Req, Default) -> {Value, Req2}

> Types:
>  *  Name = atom()
>  *  Default = any()
>  *  Value = binary() | Default
>
> Return the value for the given binding.

### bindings(Req) -> {[{Name, Value}], Req2}

> Types:
>  *  Name = atom()
>  *  Value = binary()
>
> Return all bindings.

### cookie(Name, Req) -> cookie(Name, Req, undefined)
### cookie(Name, Req, Default) -> {Value, Req2}

> Types:
>  *  Name = binary()
>  *  Default = any()
>  *  Value = binary() | Default
>
> Return the value for the given cookie.
>
> Cookie names are case sensitive.

### cookies(Req) -> {[{Name, Value}], Req2}

> Types:
>  *  Name = binary()
>  *  Value = binary()
>
> Return all cookies.

### header(Name, Req) -> header(Name, Req, undefined)
### header(Name, Req, Default) -> {Value, Req2}

> Types:
>  *  Name = binary()
>  *  Default = any()
>  *  Value = binary() | Default
>
> Return the value for the given header.
>
> While header names are case insensitive, this function expects
> the name to be a lowercase binary.

### headers(Req) -> {Headers, Req2}

> Types:
>  *  Headers = cowboy:http_headers()
>
> Return all headers.

### host(Req) -> {Host, Req2}

> Types:
>  *  Host = binary()
>
> Return the requested host.

### host_info(Req) -> {HostInfo, Req2}

> Types:
>  *  HostInfo = cowboy_router:tokens() | undefined
>
> Return the extra tokens from matching against `...` during routing.

### host_url(Req) -> {HostURL, Req2}

> Types:
>  *  HostURL = binary() | undefined
>
> Return the requested URL excluding the path component.
>
> This function will always return `undefined` until the
> `cowboy_router` middleware has been executed. This includes
> the `onrequest` hook.

### meta(Name, Req) -> meta(Name, Req, undefined)
### meta(Name, Req, Default) -> {Value, Req2}

> Types:
>  *  Name = atom()
>  *  Default = any()
>  *  Value = any()
>
> Return metadata about the request.

### method(Req) -> {Method, Req2}

> Types:
>  *  Method = binary()
>
> Return the method.
>
> Methods are case sensitive. Standard methods are always uppercase.

### parse_header(Name, Req) ->
### parse_header(Name, Req, Default) -> {ok, ParsedValue, Req2}
	| {undefined, Value, Req2} | {error, badarg}

> Types:
>  *  Name = binary()
>  *  Default = any()
>  *  ParsedValue - see below
>  *  Value = any()
>
> Parse the given header.
>
> While header names are case insensitive, this function expects
> the name to be a lowercase binary.
>
> The `parse_header/2` function will call `parser_header/3` with a
> different default value depending on the header being parsed. The
> following table summarizes the default values used.
>
> | Header name       | Default value      |
> | ----------------- | ------------------ |
> | transfer-encoding | `[<<"identity">>]` |
> | Any other header  | `undefined`        |
>
> The parsed value differs depending on the header being parsed. The
> following table summarizes the different types returned.
>
> | Header name            | Type                                              |
> | ---------------------- | ------------------------------------------------- |
> | accept                 | `[{{Type, SubType, Params}, Quality, AcceptExt}]` |
> | accept-charset         | `[{Charset, Quality}]`                            |
> | accept-encoding        | `[{Encoding, Quality}]`                           |
> | accept-language        | `[{LanguageTag, Quality}]`                        |
> | authorization          | `{AuthType, Credentials}`                         |
> | content-length         | `non_neg_integer()`                               |
> | content-type           | `{Type, SubType, ContentTypeParams}`              |
> | cookie                 | `[{binary(), binary()}]`                          |
> | expect                 | `[Expect | {Expect, ExpectValue, Params}]`        |
> | if-match               | `'*' | [{weak | strong, OpaqueTag}]`              |
> | if-modified-since      | `calendar:datetime()`                             |
> | if-none-match          | `'*' | [{weak | strong, OpaqueTag}]`              |
> | if-unmodified-since    | `calendar:datetime()`                             |
> | range                  | `{Unit, [Range]}`                                 |
> | sec-websocket-protocol | `[binary()]`                                      |
> | transfer-encoding      | `[binary()]`                                      |
> | upgrade                | `[binary()]`                                      |
> | x-forwarded-for        | `[binary()]`                                      |
>
> Types for the above table:
>  *  Type = SubType = Charset = Encoding = LanguageTag = binary()
>  *  AuthType = Expect = OpaqueTag = Unit = binary()
>  *  Params = ContentTypeParams = [{binary(), binary()}]
>  *  Quality = 0..1000
>  *  AcceptExt = [{binary(), binary()} | binary()]
>  *  Credentials - see below
>  *  Range = {non_neg_integer(), non_neg_integer() | infinity} | neg_integer()
>
> The cookie names and values, the values of the sec-websocket-protocol
> and x-forwarded-for headers, the values in `AcceptExt` and `Params`,
> the authorization `Credentials`, the `ExpectValue` and `OpaqueTag`
> are case sensitive. All values in `ContentTypeParams` are case sensitive
> except the value of the charset parameter, which is case insensitive.
> All other values are case insensitive and will be returned as lowercase.
>
> The headers accept, accept-encoding and cookie headers can return
> an empty list. Others will return `{error, badarg}` if the header
> value is empty.
>
> The authorization header parsing code currently only supports basic
> HTTP authentication. The `Credentials` type is thus `{Username, Password}`
> with `Username` and `Password` being `binary()`.
>
> The range header value `Range` can take three forms:
>  *  `{From, To}`: from `From` to `To` units
>  *  `{From, infinity}`: everything after `From` units
>  *  `-Final`: the final `Final` units
>
> An `undefined` tuple will be returned if Cowboy doesn't know how
> to parse the requested header.

### path(Req) -> {Path, Req2}

> Types:
>  *  Path = binary()
>
> Return the requested path.

### path_info(Req) -> {PathInfo, Req2}

> Types:
>  *  PathInfo = cowboy_router:tokens() | undefined
>
> Return the extra tokens from matching against `...` during routing.

### peer(Req) -> {Peer, Req2}

> Types:
>  *  Peer = {inet:ip_address(), inet:port_number()}
>
> Return the client's IP address and port number.

### port(Req) -> {Port, Req2}

> Types:
>  *  Port = inet:port_number()
>
> Return the request's port.
>
> The port returned by this function is obtained by parsing
> the host header. It may be different than the actual port
> the client used to connect to the Cowboy server.

### qs(Req) -> {QueryString, Req2}

> Types:
>  *  QueryString = binary()
>
> Return the request's query string.

### qs_val(Name, Req) -> qs_val(Name, Req, undefined)
### qs_val(Name, Req, Default) -> {Value, Req2}

> Types:
>  *  Name = binary()
>  *  Default = any()
>  *  Value = binary() | true
>
> Return a value from the request's query string.
>
> The value `true` will be returned when the name was found
> in the query string without an associated value.

### qs_vals(Req) -> {[{Name, Value}], Req2}

> Types:
>  *  Name = binary()
>  *  Value = binary() | true
>
> Return the request's query string as a list of tuples.
>
> The value `true` will be returned when a name was found
> in the query string without an associated value.

### set_meta(Name, Value, Req) -> Req2

> Types:
>  *  Name = atom()
>  *  Value = any()
>
> Set metadata about the request.
>
> An existing value will be overwritten.

### url(Req) -> {URL, Req2}

> Types:
>  *  URL = binary() | undefined
>
> Return the requested URL.
>
> This function will always return `undefined` until the
> `cowboy_router` middleware has been executed. This includes
> the `onrequest` hook.

### version(Req) -> {Version, Req2}

> Types:
>  *  Version = cowboy:http_version()
>
> Return the HTTP version used for this request.

Request body related exports
----------------------------

### body(Req) -> body(8000000, Req)
### body(MaxLength, Req) -> {ok, Data, Req2} | {error, Reason}

> Types:
>  *  MaxLength = non_neg_integer() | infinity
>  *  Data = binary()
>  *  Reason = chunked | badlength | atom()
>
> Return the request body.
>
> This function will return `{error, chunked}` if the request
> body was sent using the chunked transfer-encoding. It will
> also return `{error, badlength}` if the length of the body
> exceeds the given `MaxLength`, which is 8MB by default.

### body_length(Req) -> {Length, Req2}

> Types:
>  *  Length = non_neg_integer() | undefined
>
> Return the length of the request body.
>
> The length will only be returned if the request does not
> use any transfer-encoding and if the content-length header
> is present.

### body_qs(Req) -> body_qs(16000, Req)
### body_qs(MaxLength, Req) -> {ok, [{Name, Value}], Req2} | {error, Reason}

> Types:
>  *  MaxLength = non_neg_integer() | infinity
>  *  Name = binary()
>  *  Value = binary() | true
>  *  Reason = chunked | badlength | atom()
>
> Return the request body as a list of tuples.
>
> This function will parse the body assuming the content-type
> application/x-www-form-urlencoded, commonly used for the
> query string.
>
> This function will return `{error, chunked}` if the request
> body was sent using the chunked transfer-encoding. It will
> also return `{error, badlength}` if the length of the body
> exceeds the given `MaxLength`, which is 16KB by default.

### has_body(Req) -> boolean()

> Return whether the request has a body.

### init_stream(TransferDecode, TransferState, ContentDecode, Req) -> {ok, Req2}

> Types:
>  *  TransferDecode = fun((Encoded, TransferState) -> OK | More | Done | {error, Reason})
>  *  Encoded = Decoded = Rest = binary()
>  *  TransferState = any()
>  *  OK = {ok, Decoded, Rest, TransferState}
>  *  More = more | {more, Length, Decoded, TransferState}
>  *  Done = {done, TotalLength, Rest} | {done, Decoded, TotalLength, Rest}
>  *  Length = TotalLength = non_neg_integer()
>  *  ContentDecode = fun((Encoded) -> {ok, Decoded} | {error, Reason})
>  *  Reason = atom()
>
> Initialize streaming of the request body.
>
> This function can be used to specify what function to use
> for decoding the request body, generally specified in the
> transfer-encoding and content-encoding request headers.
>
> Cowboy will properly handle chunked transfer-encoding by
> default. You do not need to call this function if you do
> not need to decode other encodings, `stream_body/{1,2}`
> will perform all the required initialization when it is
> called the first time.

### skip_body(Req) -> {ok, Req2} | {error, Reason}

> Types:
>  *  Reason = atom()
>
> Skip the request body.
>
> This function will skip the body even if it was partially
> read before.

### stream_body(Req) -> stream_body(1000000, Req)
### stream_body(MaxSegmentSize, Req) -> {ok, Data, Req2}
	| {done, Req2} | {error, Reason}

> Types:
>  *  MaxSegmentSize = non_neg_integer()
>  *  Data = binary()
>  *  Reason = atom()
>
> Stream the request body.
>
> This function will return a segment of the request body
> with a size of up to `MaxSegmentSize`, or 1MB by default.
> This function can be called repeatedly until a `done` tuple
> is returned, indicating the body has been fully received.
>
> Cowboy will properly handle chunked transfer-encoding by
> default. If any other transfer-encoding or content-encoding
> has been used for the request, custom decoding functions
> can be used. They must be specified using `init_stream/4`.
>
> After the body has been streamed fully, Cowboy will remove
> the transfer-encoding header from the `Req` object, and add
> the content-length header if it wasn't already there.

Response related exports
------------------------

### chunk(Data, Req) -> ok | {error, Reason}

> Types:
>  *  Data = iodata()
>  *  Reason = atom()
>
> Send a chunk of data.
>
> This function should be called as many times as needed
> to send data chunks after calling `chunked_reply/{2,3}`.
>
> When the method is HEAD, no data will actually be sent.
>
> If the request uses HTTP/1.0, the data is sent directly
> without wrapping it in an HTTP/1.1 chunk, providing
> compatibility with older clients.

### chunked_reply(StatusCode, Req) -> chunked_reply(StatusCode, [], Req)
### chunked_reply(StatusCode, Headers, Req) -> {ok, Req2}

> Types:
>  *  StatusCode = cowboy:http_status()
>  *  Headers = cowboy:http_headers()
>
> Send a response using chunked transfer-encoding.
>
> This function effectively sends the response status line
> and headers to the client.
>
> This function will not send any body set previously. After
> this call the handler must use the `chunk/2` function
> repeatedly to send the body in as many chunks as needed.
>
> If the request uses HTTP/1.0, the data is sent directly
> without wrapping it in an HTTP/1.1 chunk, providing
> compatibility with older clients.

### delete_resp_header(Name, Req) -> Req2

> Types:
>  *  Name = binary()
>
> Delete the given response header.
>
> While header names are case insensitive, this function expects
> the name to be a lowercase binary.

### has_resp_body(Req) -> boolean()

> Return whether a response body has been set.
>
> This function will return false if a response body has
> been set with a length of 0.

### has_resp_header(Name, Req) -> boolean()

> Types:
>  *  Name = binary()
>
> Return whether the given response header has been set.
>
> While header names are case insensitive, this function expects
> the name to be a lowercase binary.

### reply(StatusCode, Req) -> reply(StatusCode, [], Req)
### reply(StatusCode, Headers, Req) - see below
### reply(StatusCode, Headers, Body, Req) -> {ok, Req2}

> Types:
>  *  StatusCode = cowboy:http_status()
>  *  Headers = cowboy:http_headers()
>  *  Body = iodata()
>
> Send a response.
>
> This function effectively sends the response status line,
> headers and body to the client, in a single send function
> call.
>
> The `reply/2` and `reply/3` functions will send the body
> set previously, if any. The `reply/4` function overrides
> any body set previously and sends `Body` instead.
>
> If a body function was set, and `reply/2` or `reply/3` was
> used, it will be called before returning.
>
> No more data can be sent to the client after this function
> returns.

### set_resp_body(Body, Req) -> Req2

> Types:
>  *  Body = iodata()
>
> Set a response body.
>
> This body will not be sent if `chunked_reply/{2,3}` or
> `reply/4` is used, as they override it.

### set_resp_body_fun(Fun, Req) -> Req2
### set_resp_body_fun(Length, Fun, Req) -> Req2

> Types:
>  *  Fun = fun((Socket, Transport) -> ok)
>  *  Socket = inet:socket()
>  *  Transport = module()
>  *  Length = non_neg_integer()
>
> Set a fun for sending the response body.
>
> If a `Length` is provided, it will be sent in the
> content-length header in the response. It is recommended
> to set the length if it can be known in advance.
>
> This function will only be called if the response is sent
> using the `reply/2` or `reply/3` function.
>
> The fun will receive the Ranch `Socket` and `Transport` as
> arguments. Only send and sendfile operations are supported.

### set_resp_body_fun(chunked, Fun, Req) -> Req2

> Types:
>  *  Fun = fun((ChunkFun) -> ok)
>  *  ChunkFun = fun((iodata()) -> ok | {error, atom()})
>
> Set a fun for sending the response body using chunked transfer-encoding.
>
> This function will only be called if the response is sent
> using the `reply/2` or `reply/3` function.
>
> The fun will receive another fun as argument. This fun is to
> be used to send chunks in a similar way to the `chunk/2` function,
> except the fun only takes one argument, the data to be sent in
> the chunk.

### set_resp_cookie(Name, Value, Opts, Req) -> Req2

> Types:
>  *  Name = iodata()
>  *  Value = iodata()
>  *  Opts = cookie_opts()
>
> Set a cookie in the response.
>
> Cookie names are case sensitive.

### set_resp_header(Name, Value, Req) -> Req2

> Types:
>  *  Name = binary()
>  *  Value = iodata()
>
> Set a response header.
>
> You should use `set_resp_cookie/4` instead of this function
> to set cookies.

Misc. exports
-------------

### compact(Req) -> Req2

> Remove any non-essential data from the `Req` object.
>
> Long-lived connections usually only need to manipulate the
> `Req` object at initialization. Compacting allows saving up
> memory by discarding extraneous information.
