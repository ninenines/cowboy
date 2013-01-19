Request object
==============

Purpose
-------

The request object is a special variable that can be used
to interact with a request, extracting information from it
or modifying it, and sending a response.

It's a special variable because it contains both immutable
and mutable state. This means that some operations performed
on the request object will always return the same result,
while others will not. For example, obtaining request headers
can be repeated safely. Obtaining the request body can only
be done once, as it is read directly from the socket.

With few exceptions, all calls to the `cowboy_req` module
will return an updated request object. You MUST use the new
request object instead of the old one for all subsequent
operations.

Request
-------

Cowboy allows you to retrieve a lot of information about
the request. All these calls return a `{Value, Req}` tuple,
with `Value` the requested value and `Req` the updated
request object.

The following access functions are defined in `cowboy_req`:

 *  `method/1`: the request method (`<<"GET">>`, `<<"POST">>`...)
 *  `version/1`: the HTTP version (`{1,0}` or `{1,1}`)
 *  `peer/1`: the peer address and port number
 *  `peer_addr/1`: the peer address guessed using the request headers
 *  `host/1`: the hostname requested
 *  `host_info/1`: the result of the `[...]` match on the host
 *  `port/1`: the port number used for the connection
 *  `path/1`: the path requested
 *  `path_info/1`: the result of the `[...]` match on the path
 *  `qs/1`: the entire query string unmodified
 *  `qs_val/{2,3}`: the value for the requested query string key
 *  `qs_vals/1`: all key/values found in the query string
 *  `fragment/1`: the fragment part of the URL (e.g. `#nav-links`)
 *  `host_url/1`: the requested URL without the path, qs and fragment
 *  `url/1`: the requested URL
 *  `binding/{2,3}`: the value for the requested binding found during routing
 *  `bindings/1`: all key/values found during routing
 *  `header/{2,3}`: the value for the requested header name
 *  `headers/1`: all headers name/value
 *  `cookie/{2,3}`: the value for the requested cookie name
 *  `cookies/1`: all cookies name/value
 *  `meta/{2,3}`: the meta information for the requested key

All the functions above that can take two or three arguments
take an optional third argument for the default value if
none is found. Otherwise it will return `undefined`.

In addition, Cowboy allows you to parse headers using the
`parse_header/{2,3}` function, which takes a header name
as lowercase binary, the request object, and an optional
default value. It returns `{ok, ParsedValue, Req}` if it
could be parsed, `{undefined, RawValue, Req}` if Cowboy
doesn't know this header, and `{error, badarg}` if Cowboy
encountered an error while trying to parse it.

Finally, Cowboy allows you to set request meta information
using the `set_meta/3` function, which takes a name, a value
and the request object and returns the latter modified.

Request body
------------

@todo Describe.

Response
--------

@todo Describe.
