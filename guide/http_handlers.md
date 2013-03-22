HTTP handlers
=============

Purpose
-------

HTTP handlers are the simplest Cowboy module to handle a request.

Usage
-----

You need to implement three callbacks for HTTP handlers. The first,
`init/3`, is common to all handlers. It receives three arguments:
a tuple containing the transport and protocol in use, the `Req` object
and the handler options you defined in the routes. In the context of
HTTP handlers this should be used for any initialization needs. For
example you can initialize here the `State` variable that will be
passed to the following functions.

The second callback, `handle/2`, is where most of your code should
be. It receives two arguments: the `Req` object and the `State`
previously defined. As the name explains, this is where you handle
the request.

The last callback, `terminate/3`, will be empty most of the time.
It receives three arguments: the `Reason` for termination, the `Req`
object and the `State` previously defined. This callback should be
used strictly for cleaning up. Replying using the `Req` is disallowed.
If you used the process dictionary, timers, monitors then you most
likely want to stop them in this callback, as Cowboy might end up
reusing this process for subsequent requests. Please see the
Internals chapter for more information.

Of course the general advice is to not use the process dictionary,
and that any operation requiring reception of messages should be
done in a loop handler, documented in its own chapter.
