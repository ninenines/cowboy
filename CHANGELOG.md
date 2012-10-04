CHANGELOG
=========

next
----

*   This release drops R14 compatibility

*   Add CONTRIBUTING.md file

*   Use Ranch for connection handling

    To start listeners you can now use cowboy:start_http/4 for HTTP,
    and cowboy:start_https/4 for HTTPS. The proper transport and
    protocol modules will be used.

*   Shorten the name of many modules

    * cowboy_http_protocol becomes cowboy_protocol.
    * cowboy_http_req becomes cowboy_req.
    * cowboy_http_rest becomes cowboy_rest.
    * cowboy_http_static becomes cowboy_static.
    * cowboy_http_websocket becomes cowboy_websocket.

*   Introduce the cowboy_req:req() opaque type

    The include/http.hrl file was removed. Users are expected to use
    the cowboy_req API to access or modify the Req object.

    This required a lot of changes so cleanup and optimization were
    performed where possible.

*   Add many cowboy_req functions

    * cowboy_req:delete_resp_header/2 deletes a previously set resp header.
    * cowboy_req:set_meta/3 sets metadata in the Req object.
    * cowboy_req:to_list/1 converts the Req object to a list of key/values.
    * cowboy_req:fragment/1 returns the request URL fragment.
    * cowboy_req:host_url/1 returns the request URL without the path or qs.
    * cowboy_req:url/1 returns the full request URL.

*   Rename or drop many cowboy_req functions

    * Replace cowboy_req:host/1 with cowboy_req:raw_host/1.
    * Replace cowboy_req:path/1 with cowboy_req:raw_path/1.
    * cowboy_req:raw_qs/1 becomes cowboy_req:qs/1.

*   Change the signature of many cowboy_req functions

    * parse_header now returns {ok, any(), Req} instead of {any(), Req}.
    * body_qs now returns {ok, QsVals, Req} instead of {QsVals, Req}.
    * multipart_data now returns {headers, Headers, Req} instead of
      {{headers, Headers}, Req} and {body, Body, Req} instead of
      {{body, Body}, Req}.
    * set_resp_* functions now return Req instead of {ok, Req}.

*   Fix consistency issues caused by erlang:decode_packet/3

    * The method is now always a case sensitive binary string.
    * Note that standard method names are uppercase (e.g. <<"GET">>).
    * Header names are now always lowercase binary string.

*   The max_line_length cowboy_protocol option was replaced by 3 new options:

    * max_request_line_length, defaults to 4096 bytes
    * max_header_name_length, defaults to 64 bytes
    * max_header_value_length, defaults to 4096 bytes

*   Add max_headers option, limiting the number of headers; defaults to 100

*   Use -callback in behaviours

*   Add cowboy_protocol:onrequest_fun/0 and :onresponse_fun/0 types

*   Add the body data to onresponse_fun/0 callback

*   Remove the urldecode cowboy_protocol option

*   Isolate multipart from body reading to fix an issue

*   Change a websocket error from {error, protocol} to {error, badframe}

*   Avoid a duplicate HTTP reply in cowboy_websocket:upgrade_error/1

*   Many, many optimizations for the most critical code path

0.6.1
-----

*   Add hello_world, rest_hello_world, chunked_hello_world,
    echo_get, echo_post and static examples.

*   Add support for the "Expect: 100-continue" header.

*   Keep the original 'Host' header value instead of modifying it.

*   Fix use of parsed headers cache.

*   REST: fix the matching of charsets.

*   REST: allow <<"type/subtype">> format for content_types_accepted.

*   Improve typespecs.

0.6.0
-----

*   Add multipart support

*   Add chunked transfer decoding support

    Done by reworking the body reading API. Now all the body
    reading goes through the cowboy_http_req:stream_body/1
    function. This function takes care of handling both the
    Transfer-Encoding and the Content-Encoding, returning
    properly decoded data ready for consumption.

*   Add fragmented websocket messages support

    Properly tested by the addition of the Autobahn websocket
    test suite to our toolbox. All tests pass except a few
    related to UTF-8 handling, as Cowboy does no checks on that
    end at this point.

*   Add 'onrequest' and 'onresponse' hooks

    The first can be used for all the special cases you may have
    that can't be dealt with otherwise. It's also pretty good for
    writing access logs or rewriting URLs.

    The second can be used for logging errors or replacing error
    pages, amongst others.

*   Add cowboy:get_protocol_options/1 and cowboy:set_protocol_options/2

    These functions allow for retrieving a listener's protocol options,
    and for modifying them while the listener is running. This is
    most useful to upgrade the dispatch list. The upgrade applies
    to all the future connections.

*   Add the sockname/1 function to TCP and SSL transports

*   Improve SSL transport support

    Add support for specifying the ciphers. Add CA support. Make
    specifying the password optional.

*   Add new HTTP status codes from RFC 6585

*   Add a 'file' option to cowboy_http_static

    This allows for mapping /folder/ paths to a /folder/index.html file.

*   Add the '*' catch all Content-Type for REST

*   Add {halt, Req, State} as a possible return value for REST

*   Add absolute URI support for requests

*   Add cowboy_http:x_www_form_urlencoded/2

*   Various REST bug fixes

*   Do not send chunked replies for HTTP/1.0 connections

*   Fix a DST bug in the cookies code

*   Fix a bug with setting cookie values containing slashes

*   Fix a small timer leak when using loop/websocket timeouts

*   Make charset and media type parsing more relaxed

    This is to accomodate some widely used broken clients.

*   Make error messages more readable

*   Fix and improve type specifications

*   Fix a bug preventing documentation from being generated

*   Small improvements to the documentation

*   Rework the HTTP test suite

    The suite now uses an integrated Cowboy HTTP client. The client
    is currently experimental and shouldn't be used.

*   Add many many tests.

0.4.0
-----

*   Set the cowboy_listener process priority to high

    As it is the central process used by all incoming requests
    we need to set its priority to high to avoid timeouts that
    would happen otherwise when reaching a huge number of
    concurrent requests.

*   Add cowboy:child_spec/6 for embedding in other applications

*   Add cowboy_http_rest, an experimental REST protocol support

    Based on the Webmachine diagram and documentation. It is a
    new implementation, not a port, therefore a few changes have
    been made. However all the callback names are the same and
    should behave similarly to Webmachine.

    There is currently no documentation other than the Webmachine
    resource documentation and the comments found in cowboy_http_rest,
    which itself should be fairly easy to read and understand.

*   Add cowboy_http_static, an experimental static file handler

    Makes use of the aforementioned REST protocol support to
    deliver files with proper content type and cache headers.

    Note that this uses the new file:sendfile support when
    appropriate, which currently requires the VM to be started
    with the +A option defined, else errors may randomly appear.

*   Add cowboy_bstr module for binary strings related functions

*   Add cowboy_http module for HTTP parsing functions

    This module so far contains various functions for HTTP header
    parsing along with URL encoding and decoding.

*   Remove quoted from the default dependencies

    This should make Cowboy much easier to compile and use by default.
    It is of course still possible to use quoted as your URL decoding
    library in Cowboy thanks to the newly added urldecode option.

*   Fix supervisor spec for non dynamic modules to allow upgrades to complete

*   Add cowboy:accept_ack/1 for a cleaner handling of the shoot message

    Before, when the listener accepted a connection, the newly created
    process was waiting for a message containing the atom 'shoot' before
    proceeding. This has been replaced by the cowboy:accept_ack/1 function.

    This function should be used where 'shoot' was received because the
    contents of the message have changed (and could change again in the
    distant future).

*   Update binary parsing expressions to avoid hype crashes

    More specifically, /bits was replaced by /binary.

*   Rename the type cowboy_dispatcher:path_tokens/0 to tokens/0

*   Remove the cowboy_clock:date/0, time/0 and datetime/0 types

    The calendar module exports those same types properly since R14B04.

*   Add cacertfile configuration option to cowboy_ssl_transport

*   Add cowboy_protocol behaviour

*   Remove -Wbehaviours dialyzer option unavailable in R15B

*   Many tests and specs improvements

### cowboy_http_req

*   Fix a crash when reading the request body

*   Add parse_header/2 and parse_header/3

    The following headers can now be semantically parsed: Connection, Accept,
    Accept-Charset, Accept-Encoding, Accept-Language, Content-Length,
    Content-Type, If-Match, If-None-Match, If-Modified-Since,
    If-Unmodified-Since, Upgrade

*   Add set_resp_header/3, set_resp_cookie/4 and set_resp_body/2

    These functions allow handlers to set response headers and body
    without having to reply directly.

*   Add set_resp_body_fun/3

    This function allows handlers to stream the body of the response
    using the given fun. The size of the response must be known beforehand.

*   Add transport/1 to obtain the transport and socket for the request

    This allows handlers to have low-level socket access in those cases
    where they do need it, like when streaming a response body with
    set_resp_body_fun/3.

*   Add peer_addr/1

    This function tries to guess the real peer IP based on the HTTP
    headers received.

*   Add meta/2 and meta/3 to save useful protocol information

    Currently used to save the Websocket protocol version currently used,
    and to save request information in the REST protocol handler.

*   Add reply/2 and reply/3 aliases to reply/4

*   Add upgrade_reply/3 for protocol upgrades

### cowboy_http_protocol

*   Add the {urldecode, fun urldecode/2} option

    Added when quoted was removed from the default build. Can be used to
    tell Cowboy to use quoted or any other URL decoding routine.

*   Add the max_keepalive option

*   Add the max_line_length option

*   Allow HTTP handlers to stop during init/3

    To do so they can return {shutdown, Req, State}.

*   Add loops support in HTTP handlers for proper long-polling support

    A loop can be entered by returning either of {loop, Req, State},
    {loop, Req, State, hibernate}, {loop, Req, State, Timeout} or
    {loop, Req, State, Timeout, hibernate} from init/3.

    Loops are useful when we cannot reply immediately and instead
    are waiting for an Erlang message to be able to complete the request,
    as would typically be done for long-polling.

    Loop support in the protocol means that timeouts and hibernating
    are well tested and handled so you can use those options without
    worrying. It is recommended to set the timeout option.

    When a loop is started, handle/2 will never be called so it does
    not need to be defined. When the request process receives an Erlang
    message, it will call the info/3 function with the message as the
    first argument.

    Like in OTP, you do need to set timeout and hibernate again when
    returning from info/3 to enable them until the next call.

*   Fix the sending of 500 errors when handlers crash

    Now we send an error response when no response has been sent,
    and do nothing more than close the connection if anything
    did get sent.

*   Fix a crash when the server is sent HTTP responses

*   Fix HTTP timeouts handling when the Request-Line wasn't received

*   Fix the handling of the max number of empty lines between requests

*   Fix the handling of HEAD requests

*   Fix HTTP/1.0 Host header handling

*   Reply status 400 if we receive an unexpected value or error for headers

*   Properly close when the application sends "Connection: close" header

*   Close HTTP connections on all errors

*   Improve the error message for HTTP handlers

### cowboy_http_websocket

*   Add websocket support for all versions up to RFC 6455

    Support isn't perfect yet according to the specifications, but
    is working against all currently known client implementations.

*   Allow websocket_init/3 to return with the hibernate option set

*   Add {shutdown, Req} return value to websocket_init/3 to fail an upgrade

*   Fix websocket timeout handling

*   Fix error messages: wrong callback name was reported on error

*   Fix byte-by-byte websocket handling

*   Fix an issue when using hixie-76 with certain proxies

*   Fix a crash in the hixie-76 handshake

*   Fix the handshake when SSL is used on port 443

*   Fix a crash in the handshake when cowboy_http_req:compact/1 is used

*   Fix handshake when a query string is present

*   Fix a crash when the Upgrade header contains more than one token

0.2.0
-----

*   Initial release.
