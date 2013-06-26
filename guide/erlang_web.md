Erlang and the Web
==================

The Web is concurrent
---------------------

When you access a website there is little concurrency
involved. A few connections are opened and requests
are sent through these connections. Then the web page
is displayed on your screen. Your browser will only
open up to 4 or 8 connections to the server, depending
on your settings. This isn't much.

But think about it. You are not the only one accessing
the server at the same time. There can be hundreds, if
not thousands, if not millions of connections to the
same server at the same time.

Even today a lot of systems used in production haven't
solved the C10K problem (ten thousand concurrent connections).
And the ones who did are trying hard to get to the next
step, C100K, and are pretty far from it.

Erlang meanwhile has no problem handling millions of
connections. At the time of writing there are application
servers written in Erlang that can handle more than two
million connections on a single server in a real production
application, with spare memory and CPU!

The Web is concurrent, and Erlang is a language designed
for concurrency, so it is a perfect match.

Of course, various platforms need to scale beyond a few
million connections. This is where Erlang's built-in
distribution mechanisms come in. If one server isn't
enough, add more! Erlang allows you to use the same code
for talking to local processes or to processes in other
parts of your cluster, which means you can scale very
quickly if the need arises.

The Web has large userbases, and the Erlang platform was
designed to work in a distributed setting, so it is a
perfect match.

Or is it? Surely you can find solutions to handle that many
concurrent connections with your favorite language... But all
these solutions will break down in the next few years. Why?
Firstly because servers don't get any more powerful, they
instead get a lot more cores and memory. This is only useful
if your application can use them properly, and Erlang is
light-years away from anything else in that area. Secondly,
today your computer and your phone are online, tomorrow your
watch, goggles, bike, car, fridge and tons of other devices
will also connect to various applications on the Internet.

Only Erlang is prepared to deal with what's coming.

The Web is soft real time
-------------------------

What does soft real time mean, you ask? It means we want the
operations done as quickly as possible, and in the case of
web applications, it means we want the data propagated fast.

In comparison, hard real time has a similar meaning, but also
has a hard time constraint, for example an operation needs to
be done in under N milliseconds otherwise the system fails
entirely.

Users aren't that needy yet, they just want to get access
to their content in a reasonable delay, and they want the
actions they make to register at most a few seconds after
they submitted them, otherwise they'll start worrying about
whether it successfully went through.

The Web is soft real time because taking longer to perform an
operation would be seen as bad quality of service.

Erlang is a soft real time system. It will always run
processes fairly, a little at a time, switching to another
process after a while and preventing a single process to
steal resources from all others. This means that Erlang
can guarantee stable low latency of operations.

Erlang provides the guarantees that the soft real time Web
requires.

The Web is asynchronous
-----------------------

Long ago, the Web was synchronous because HTTP was synchronous.
You fired a request, and then waited for a response. Not anymore.
It all began when XmlHttpRequest started being used. It allowed
the client to perform asynchronous calls to the server.

Then Websocket appeared and allowed both the server and the client
to send data to the other endpoint completely asynchronously. The
data is contained within frames and no response is necessary.

Erlang processes work the same. They send each other data contained
within messages and then continue running without needing a response.
They tend to spend most of their time inactive, waiting for a new
message, and the Erlang VM happily activate them when one is received.

It is therefore quite easy to imagine Erlang being good at receiving
Websocket frames, which may come in at unpredictable times, pass the
data to the responsible processes which are always ready waiting for
new messages, and perform the operations required by only activating
the required parts of the system.

The more recent Web technologies, like Websocket of course, but also
SPDY and HTTP/2.0, are all fully asynchronous protocols. The concept
of requests and responses is retained of course, but anything could
be sent in between, by both the client or the browser, and the
responses could also be received in a completely different order.

Erlang is by nature asynchronous and really good at it thanks to the
great engineering that has been done in the VM over the years. It's
only natural that it's so good at dealing with the asynchronous Web.

The Web is omnipresent
----------------------

The Web has taken a very important part of our lives. We're
connected at all times, when we're on our phone, using our computer,
passing time using a tablet while in the bathroom... And this
isn't going to slow down, every single device at home or on us
will be connected.

All these devices are always connected. And with the number of
alternatives to give you access to the content you seek, users
tend to not stick around when problems arise. Users today want
their applications to be always available and if it's having
too many issues they just move on.

Despite this, when developers choose a product to use for building
web applications, their only concern seem to be "Is it fast?",
and they look around for synthetic benchmarks showing which one
is the fastest at sending "Hello world" with only a handful
concurrent connections. Web benchmarks haven't been representative
of reality in a long time, and are drifting further away as
time goes on.

What developers should really ask themselves is "Can I service
all my users with no interruption?" and they'd find that they have
two choices. They can either hope for the best, or they can use
Erlang.

Erlang is built for fault tolerance. When writing code in any other
language, you have to check all the return values and act accordingly
to avoid any unforeseen issues. If you're lucky, you won't miss
anything important. When writing Erlang code, you can just check
the success condition and ignore all errors. If an error happen,
the Erlang process crashes and is then restarted by a special
process called a supervisor.

The Erlang developer thus has no need to fear about unhandled
errors, and can focus on handling only the errors that should
give some feedback to the user and let the system take care of
the rest. This also has the advantage of allowing him to write
a lot less code, and letting him sleep at night.

Erlang's fault tolerance oriented design is the first piece of
what makes it the best choice for the omnipresent, always available
Web.

The second piece is Erlang's built-in distribution. Distribution
is a key part of building a fault tolerant system, because it
allows you to handle bigger failures, like a whole server going
down, or even a data center entirely.

Fault tolerance and distribution are important today, and will be
vital in the future of the Web. Erlang is ready.

Erlang is the ideal platform for the Web
----------------------------------------

Erlang provides all the important features that the Web requires
or will require in the near future. Erlang is a perfect match
for the Web, and it only makes sense to use it to build web
applications.
