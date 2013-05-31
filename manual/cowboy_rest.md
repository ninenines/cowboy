cowboy_rest
===========

The `cowboy_rest` module implements REST semantics on top of
the HTTP protocol.

This module cannot be described as a behaviour due to most of
the callbacks it defines being optional. It has the same
semantics as a behaviour otherwise.

The only mandatory callback is `init/3`, needed to perform
the protocol upgrade.

Types
-----

None.

Meta values
-----------

### charset

> Type: binary()
>
> Negotiated charset.
>
> This value may not be defined if no charset was negotiated.

### language

> Type: binary()
>
> Negotiated language.
>
> This value may not be defined if no language was negotiated.

### media_type

> Type: {binary(), binary(), '*' | [{binary(), binary()}]}
>
> Negotiated media-type.
>
> The media-type is the content-type, excluding the charset.
>
> This value is always defined after the call to
> `content_types_provided/2`.

Callbacks
---------

### init({TransportName, ProtocolName}, Req, Opts)
	-> {upgrade, protocol, cowboy_rest}
	| {upgrade, protocol, cowboy_rest, Req, Opts}

> Types:
>  *  TransportName = tcp | ssl | atom()
>  *  ProtocolName = http | atom()
>  *  Req = cowboy_req:req()
>  *  Opts = any()
>
> Upgrade the protocol to `cowboy_rest`.
>
> This is the only mandatory callback.

### rest_init(Req, Opts) -> {ok, Req, State}

> Types:
>  *  Req = cowboy_req:req()
>  *  Opts = any()
>  *  State = any()
>
> Initialize the state for this request.

### rest_terminate(Req, State) -> ok

> Types:
>  *  Req = cowboy_req:req()
>  *  State = any()
>
> Perform any necessary cleanup of the state.
>
> This callback should release any resource currently in use,
> clear any active timer and reset the process to its original
> state, as it might be reused for future requests sent on the
> same connection.

### Callback(Req, State) -> {Value, Req, State} | {halt, Req, State}

> Types:
>  *  Callback - one of the REST callbacks described below
>  *  Req = cowboy_req:req()
>  *  State = any()
>  *  Value - see the REST callbacks description below
>
> Please see the REST callbacks description below for details
> on the `Value` type, the default value if the callback is
> not defined, and more general information on when the
> callback is called and what its intended use is.
>
> The `halt` tuple can be returned to stop REST processing.
> It is up to the resource code to send a reply before that,
> otherwise a `204 No Content` will be sent.

REST callbacks description
--------------------------

### allowed_methods

>  *  Methods: all
>  *  Value type: [binary()]
>  *  Default value: [<<"GET">>, <<"HEAD">>, <<"OPTIONS">>]
>
> Return the list of allowed methods.
>
> Methods are case sensitive. Standard methods are always uppercase.

### allow_missing_post

>  *  Methods: POST
>  *  Value type: boolean()
>  *  Default value: true
>
> Return whether POST is allowed when the resource doesn't exist.
>
> Returning `true` here means that a new resource will be
> created. The URL to the created resource should also be
> returned from the `AcceptResource` callback.

### charsets_provided

>  *  Methods: GET, HEAD, POST, PUT, PATCH, DELETE
>  *  Value type: [binary()]
>  *  Skip to the next step if undefined
>
> Return the list of charsets the resource provides.
>
> The list must be ordered in order of preference.
>
> If the accept-charset header was not sent, the first charset
> in the list will be selected. Otherwise Cowboy will select
> the most appropriate charset from the list.
>
> The chosen charset will be set in the `Req` object as the meta
> value `charset`.
>
> While charsets are case insensitive, this callback is expected
> to return them as lowercase binary.

### content_types_accepted

>  *  Methods: POST, PUT, PATCH
>  *  No default
>
> Types:
>  *  Value = [{binary() | {Type, SubType, Params}, AcceptResource}]
>  *  Type = SubType = binary()
>  *  Params = '*' | [{binary(), binary()}]
>  *  AcceptResource = atom()
>
> Return the list of content-types the resource accepts.
>
> The list must be ordered in order of preference.
>
> Each content-type can be given either as a binary string or as
> a tuple containing the type, subtype and parameters.
>
> Cowboy will select the most appropriate content-type from the list.
> If any parameter is acceptable, then the tuple form should be used
> with parameters set to `'*'`. If the parameters value is set to `[]`
> only content-type values with no parameters will be accepted. All
> parameter values are treated in a case sensitive manner except the
> `charset` parameter, if present, which is case insensitive.
>
> This function will be called for POST, PUT and PATCH requests.
> It is entirely possible to define different callbacks for different
> methods if the handling of the request differs. Simply verify
> what the method is with `cowboy_req:method/1` and return a
> different list for each methods.
>
> The `AcceptResource` value is the name of the callback that will
> be called if the content-type matches. It is defined as follow.
>
>  *  Value type: true | {true, URL} | false
>  *  No default
>
> Process the request body.
>
> This function should create or update the resource with the
> information contained in the request body. This information
> may be full or partial depending on the request method.
>
> If the request body was processed successfully, `true` or
> `{true, URL}` may be returned. If an URL is provided, the
> response will redirect the client to the location of the
> resource.
>
> If a response body must be sent, the appropriate media-type, charset
> and language can be retrieved using the `cowboy_req:meta/{2,3}`
> functions. The respective keys are `media_type`, `charset`
> and `language`. The body can be set using `cowboy_req:set_resp_body/2`.

### content_types_provided

>  *  Methods: GET, HEAD
>  *  Default value: [{{<<"text">>, <<"html">>, '*'}, to_html}]
>
> Types:
>  *  Value = [{binary() | {Type, SubType, Params}, ProvideResource}]
>  *  Type = SubType = binary()
>  *  Params = '*' | [{binary(), binary()}]
>  *  ProvideResource = atom()
>
> Return the list of content-types the resource provides.
>
> The list must be ordered in order of preference.
>
> Each content-type can be given either as a binary string or as
> a tuple containing the type, subtype and parameters.
>
> Cowboy will select the most appropriate content-type from the list.
> If any parameter is acceptable, then the tuple form should be used
> with parameters set to `'*'`. If the parameters value is set to `[]`
> only content-type values with no parameters will be accepted. All
> parameter values are treated in a case sensitive manner except the
> `charset` parameter, if present, which is case insensitive.
>
> The `ProvideResource` value is the name of the callback that will
> be called if the content-type matches. It is defined as follow.
>
>  *  Value type: iodata() | {stream, Fun} | {stream, Len, Fun} | {chunked, ChunkedFun}
>  *  No default
>
> Return the response body.
>
> The response body may be provided directly or through a fun.
> If a fun tuple is returned, the appropriate `set_resp_body_fun`
> function will be called. Please refer to the documentation for
> these functions for more information about the types.
>
> The call to this callback happens a good time after the call to
> `content_types_provided/2`, when it is time to start rendering
> the response body.

### delete_completed

>  *  Methods: DELETE
>  *  Value type: boolean()
>  *  Default value: true
>
> Return whether the delete action has been completed.
>
> This function should return `false` if there is no guarantee
> that the resource gets deleted immediately from the system,
> including from any internal cache.
>
> When this function returns `false`, a `202 Accepted`
> response will be sent instead of a `200 OK` or `204 No Content`.

### delete_resource

>  *  Methods: DELETE
>  *  Value type: boolean()
>  *  Default value: false
>
> Delete the resource.
>
> The value returned indicates if the action was successful,
> regardless of whether the resource is immediately deleted
> from the system.

### expires

>  *  Methods: GET, HEAD
>  *  Value type: calendar:datetime() | undefined
>  *  Default value: undefined
>
> Return the date of expiration of the resource.
>
> This date will be sent as the value of the expires header.

### forbidden

>  *  Methods: all
>  *  Value type: boolean()
>  *  Default value: false
>
> Return whether access to the resource is forbidden.
>
> A `403 Forbidden` response will be sent if this
> function returns `true`. This status code means that
> access is forbidden regardless of authentication,
> and that the request shouldn't be repeated.

### generate_etag

>  *  Methods: GET, HEAD, POST, PUT, PATCH, DELETE
>  *  Value type: binary() | {weak | strong, binary()}
>  *  Default value: undefined
>
> Return the entity tag of the resource.
>
> This value will be sent as the value of the etag header.
>
> If a binary is returned, then the value will be parsed
> to the tuple form automatically. The value must be in
> the same format as the etag header, including quotes.

### is_authorized

>  *  Methods: all
>  *  Value type: true | {false, AuthHeader}
>  *  Default value: true
>
> Return whether the user is authorized to perform the action.
>
> This function should be used to perform any necessary
> authentication of the user before attempting to perform
> any action on the resource.
>
> If the authentication fails, the value returned will be sent
> as the value for the www-authenticate header in the
> `401 Unauthorized` response.

### is_conflict

>  *  Methods: PUT
>  *  Value type: boolean()
>  *  Default value: false
>
> Return whether the put action results in a conflict.
>
> A `409 Conflict` response will be sent if this function
> returns `true`.

### known_content_type

>  *  Methods: all
>  *  Value type: boolean()
>  *  Default value: true
>
> Return whether the content-type is known.
>
> This function determines if the server understands the
> content-type, regardless of its use by the resource.

### known_methods

>  *  Methods: all
>  *  Value type: [binary()]
>  *  Default value: [<<"GET">>, <<"HEAD">>, <<"POST">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>]
>
> Return the list of known methods.
>
> The full list of methods known by the server should be
> returned, regardless of their use in the resource.
>
> The default value lists the methods Cowboy knows and
> implement in `cowboy_rest`.
>
> Methods are case sensitive. Standard methods are always uppercase.

### languages_provided

>  *  Methods: GET, HEAD, POST, PUT, PATCH, DELETE
>  *  Value type: [binary()]
>  *  Skip to the next step if undefined
>
> Return the list of languages the resource provides.
>
> The list must be ordered in order of preference.
>
> If the accept-language header was not sent, the first language
> in the list will be selected. Otherwise Cowboy will select
> the most appropriate language from the list.
>
> The chosen language will be set in the `Req` object as the meta
> value `language`.
>
> While languages are case insensitive, this callback is expected
> to return them as lowercase binary.

### last_modified

>  *  Methods: GET, HEAD, POST, PUT, PATCH, DELETE
>  *  Value type: calendar:datetime()
>  *  Default value: undefined
>
> Return the date of last modification of the resource.
>
> This date will be used to test against the if-modified-since
> and if-unmodified-since headers, and sent as the last-modified
> header in the response of GET and HEAD requests.

### malformed_request

>  *  Methods: all
>  *  Value type: boolean()
>  *  Default value: false
>
> Return whether the request is malformed.
>
> Cowboy has already performed all the necessary checks
> by the time this function is called, so few resources
> are expected to implement it.
>
> The check is to be done on the request itself, not on
> the request body, which is processed later.

### moved_permanently

>  *  Methods: GET, HEAD, POST, PUT, PATCH, DELETE
>  *  Value type: {true, URL} | false
>  *  Default value: false
>
> Return whether the resource was permanently moved.
>
> If it was, its new URL is also returned and sent in the
> location header in the response.

### moved_temporarily

>  *  Methods: GET, HEAD, POST, PATCH, DELETE
>  *  Value type: {true, URL} | false
>  *  Default value: false
>
> Return whether the resource was temporarily moved.
>
> If it was, its new URL is also returned and sent in the
> location header in the response.

### multiple_choices

>  *  Methods: GET, HEAD, POST, PUT, PATCH, DELETE
>  *  Value type: boolean()
>  *  Default value: false
>
> Return whether there are multiple representations of the resource.
>
> This function should be used to inform the client if there
> are different representations of the resource, for example
> different content-type. If this function returns `true`,
> the response body should include information about these
> different representations using `cowboy_req:set_resp_body/2`.
> The content-type of the response should be the one previously
> negociated and that can be obtained by calling
> `cowboy_req:meta(media_type, Req)`.

### options

>  *  Methods: OPTIONS
>  *  Value type: ok
>  *  Default value: ok
>
> Handle a request for information.
>
> The response should inform the client the communication
> options available for this resource.
>
> By default, Cowboy will send a `200 OK` response with the
> allow header set.

### previously_existed

>  *  Methods: GET, HEAD, POST, PATCH, DELETE
>  *  Value type: boolean()
>  *  Default value: false
>
> Return whether the resource existed previously.

### resource_exists

>  *  Methods: GET, HEAD, POST, PUT, PATCH, DELETE
>  *  Value type: boolean()
>  *  Default value: true
>
> Return whether the resource exists.
>
> If it exists, conditional headers will be tested before
> attempting to perform the action. Otherwise, Cowboy will
> check if the resource previously existed first.

### service_available

>  *  Methods: all
>  *  Value type: boolean()
>  *  Default value: true
>
> Return whether the service is available.
>
> This function can be used to test that all relevant backend
> systems are up and able to handle requests.
>
> A `503 Service Unavailable` response will be sent if this
> function returns `false`.

### uri_too_long

>  *  Methods: all
>  *  Value type: boolean()
>  *  Default value: false
>
> Return whether the requested URI is too long.
>
> Cowboy has already performed all the necessary checks
> by the time this function is called, so few resources
> are expected to implement it.
>
> A `414 Request-URI Too Long` response will be sent if this
> function returns `true`.

### valid_content_headers

>  *  Methods: all
>  *  Value type: boolean()
>  *  Default value: true
>
> Return whether the content-* headers are valid.
>
> This also applies to the transfer-encoding header. This
> function must return `false` for any unknown content-*
> headers, or if the headers can't be understood. The
> function `cowboy_req:parse_header/2` can be used to
> quickly check the headers can be parsed.
>
> A `501 Not Implemented` response will be sent if this
> function returns `false`.

### valid_entity_length

>  *  Methods: all
>  *  Value type: boolean()
>  *  Default value: true
>
> Return whether the request body length is within acceptable boundaries.
>
> A `413 Request Entity Too Large` response will be sent if this
> function returns `false`.

### variances

>  *  Methods: GET, HEAD, POST, PUT, PATCH, DELETE
>  *  Value type: [binary()]
>  *  Default value: []
>
> Return the list of headers that affect the representation of the resource.
>
> These request headers return the same resource but with different
> parameters, like another language or a different content-type.
>
> Cowboy will automatically add the accept, accept-language and
> accept-charset headers to the list if the respective functions
> were defined in the resource.
>
> This operation is performed right before the `resource_exists/2`
> callback. All responses past that point will contain the vary
> header which holds this list.
