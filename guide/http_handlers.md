HTTP handlers
=============

Purpose
-------

HTTP handlers are the simplest Cowboy module to handle a request.

Usage
-----

You need to implement three callbacks for HTTP handlers. The first,
`init/3`, is common to all handlers. In the context of HTTP handlers
this should be used for any initialization needs.

The second callback, `handle/2`, is where most of your code should
be. As the name explains, this is where you handle the request.

The last callback, `terminate/3`, will be empty most of the time.
It's used for any needed cleanup. If you used the process dictionary,
timers, monitors then you most likely want to stop them in this
callback, as Cowboy might end up reusing this process for subsequent
requests. Please see the Internals chapter for more information.

Of course the general advice is to not use the process dictionary,
and that any operation requiring reception of messages should be
done in a loop handler, documented in its own chapter.
