ROADMAP
=======

This document explains in as much details as possible the
list of planned changes and work to be done on the Cowboy
server. It is intended to be exhaustive but some elements
might still be missing.

2.0 (R17 and R18)
-----------------

The main features that will be added to Cowboy 2.0 are support
for HTTP/2.0 and Websocket permessage deflate compression.

A complete analysis of the httpbis set of specifications
will be performed and extensive tests will be written to
ensure maximum compatibility.

A number of backward incompatible changes are planned. These
changes are individually small, but together should result
in a large improvement in usability.

### cowboy_req

The interface of `cowboy_req` will be largely changed. The
number one complaint about Cowboy today is that you have
to keep track of the Req whenever you do anything. The new
interface will minimize that.

All functions will return a single term, excluding the body
reading functions `body/{1,2}`, `body_qs/{1,2}`, `part/{1,2}`,
`part_body/{1,2}`.

Of the functions returning a single term, some of them will
return a Req object. This includes the functions that already
return Req: `compact/1`, `delete_resp_header/2`, `set_meta/3`,
`set_resp_body/2`, `set_resp_body_fun/{2,3}`, `set_resp_cookie/4`,
`set_resp_header/3`, and adds the `chunked_reply/{2,3}` and
`reply/{2,3,4}` functions to the list.

Of note is that this will allow chaining all the response
functions if that's what you fancy.

The `parse_header/{2,3}` function will now only return the
parsed header value, and crash on error. It will also not
cache the parsed value anymore, except for headers that Cowboy
requires, like the connection header.

It is unsure what will become of the `qs_val`, `qs_vals`,
`cookie` and `cookies` functions. The main idea at this point
is to replace them with a `parse_qs/2` and `parse_cookies/2`
that would return the parsed list, and let the user decide
how to access it.

### init/terminate unification

The first argument of the `init/3` function is too rarely used.
It will be removed.

The return value of the `init/2` function will become
`{http, Req, State} | {loop, Req, State} | {Module, Req, State}`
with `Module` being `cowboy_rest`, `cowboy_websocket` or a
user provided module.

The `rest_init` and `websocket_init` callbacks will be removed
as they become unnecessary with the new `init/2` interface.

Similarly, the `rest_terminate` and `websocket_terminate`
callbacks will be removed in favor of a unified `terminate/3`.

The `terminate/3` callback will become optional.

### Middlewares

The error tuple return value brings little value compared to
the halt tuple. The error tuple will therefore be removed.

### Hooks

The `onrequest` hook will be removed. It can easily be replaced
by a middleware.

The interface of the `onresponse` hook will change. There has
been a number of issues and added complexity with the current
interface that warrant fixing. The main problem is that the
hook may be used to change the reply, by calling the reply
function again, forcing us to be careful not to reprocess
everything again.

To fix that, we will cut the reply mechanism in two steps,
one that is basically some preprocessing of the response
header to follow the protocol requirements, and then the
actual response. The `onresponse` hook will fit in the
middle, being called from the first step and calling the
second step itself.

If a body streaming function is provided, the hook will
also receive it (unlike today). It will not be able to
inspect its contents however.

This should greatly simplify the code and allow users to
do any operation they wish.

### Low-level interface documented

A special chapter of the manual will document a low-level
interface that may be used in middlewares or hooks (but
nowhere else). This includes the Req access and update
functions and the new response function described above.

### REST

The `known_content_type` callback has no purpose, so it
is going to be removed.

The documentation for all REST callbacks will be updated
to describe whether they can have side effects. This will
allows us to build introspection tools on top of a working
REST API.

Range support will be added.

Under consideration
-------------------

 *  Convenience API for extracting query string and body
    information, similar to PHP's $_GET, $_POST and $_FILES
