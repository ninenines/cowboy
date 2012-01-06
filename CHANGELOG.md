CHANGELOG
=========

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
