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

### New cowboy_req function

A convenience function will be added to more easily process
HTML forms that were sent using POST. These forms have two
main ways of being submitted: as a form urlencoded format,
or as multipart. The latter case may also be used to submit
files, therefore we also need a way to handle this. We also
need to take into account that we are reading from the socket
and can't take as much a shortcut as with `match_qs/2`.

The function will work similarly to other body reading functions,
in that it may require more than one call to obtain everything.
In this case there would be two return values: the `ok` return
with the map filled with key/value pairs, ending the body reading;
and the `file` return that informs the caller that a file has been
provided and the caller must handle it. If the caller calls the
function again without doing anything, the part is just skipped,
like what `part/1` is doing. If a file is the last input from
the form then a subsequent call will return an `ok` return with
an empty map.

The interface would look as follow:

```
match_body(Fields, Req) -> match_body(Fields, Req, [])
match_body(Fields, Req, Opts)
	-> {ok, Map, Req}
	| {file, FieldName, Filename, CType, CTransferEncoding, Map, Req}
	when Req::req()
```

It would be up to the caller to decide what to do with the
maps returned. Fields are in order so the map returned may
be empty if the form starts with a file, or may only
contain the values before the file input if this one is
in the middle of the form. It is of course possible to
merge all maps returned into one though that should not
be needed very often.

It is also possible to switch from this function to only
multipart functions if the function returns a `file` tuple,
as this function is a higher level interface that simply
calls the multipart functions when the request body is
in multipart format.

### Hooks

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

### Loop

We probably want to send something other than 500 when the
max data read value has been reached. This happens when the
client is misbehaving and is not a server error, but rather
a safeguard.

### REST

The documentation for all REST callbacks will be updated
to describe whether they can have side effects. This will
allows us to build introspection tools on top of a working
REST API.

Range support will be added.
