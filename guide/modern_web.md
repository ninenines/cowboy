The modern Web
==============

Let's take a look at various technologies from the beginnings
of the Web up to this day, and get a preview of what's
coming next.

Cowboy is compatible with all the technology cited in this
chapter except of course HTTP/2.0 which has no implementation
in the wild at the time of writing.

The prehistoric Web
-------------------

HTTP was initially created to serve HTML pages and only
had the GET method for retrieving them. This initial
version is documented and is sometimes called HTTP/0.9.
HTTP/1.0 defined the GET, HEAD and POST methods, and
was able to send data with POST requests.

HTTP/1.0 works in a very simple way. A TCP connection
is first established to the server. Then a request is
sent. Then the server sends a response back and closes
the connection.

Suffice to say, HTTP/1.0 is not very efficient. Opening
a TCP connection takes some time, and pages containing
many assets load much slower than they could because of
this.

Most improvements done in recent years focused on reducing
this load time and reducing the latency of the requests.

HTTP/1.1
--------

HTTP/1.1 quickly followed and added a keep-alive mechanism
to allow using the same connection for many requests, as
well as streaming capabilities, allowing an endpoint to send
a body in well defined chunks.

HTTP/1.1 defines the OPTIONS, GET, HEAD, POST, PUT, DELETE,
TRACE and CONNECT methods. The PATCH method was added in more
recent years. It also improves the caching capabilities with
the introduction of many headers.

HTTP/1.1 still works like HTTP/1.0 does, except the connection
can be kept alive for subsequent requests. This however allows
clients to perform what is called as pipelining: sending many
requests in a row, and then processing the responses which will
be received in the same order as the requests.

REST
----

The design of HTTP/1.1 was influenced by the REST architectural
style. REST, or REpresentational State Transfer, is a style of
architecture for loosely connected distributed systems.

REST defines constraints that systems must obey to in order to
be RESTful. A system which doesn't follow all the constraints
cannot be considered RESTful.

REST is a client-server architecture with a clean separation
of concerns between the client and the server. They communicate
by referencing resources. Resources can be identified, but
also manipulated. A resource representation has a media type
and information about whether it can be cached and how. Hypermedia
determines how resources are related and how they can be used.
REST is also stateless. All requests contain the complete
information necessary to perform the action.

HTTP/1.1 defines all the methods, headers and semantics required
to implement RESTful systems.

REST is most often used when designing web application APIs
which are generally meant to be used by executable code directly.

XmlHttpRequest
--------------

Also know as AJAX, this technology allows Javascript code running
on a web page to perform asynchronous requests to the server.
This is what started the move from static websites to dynamic
web applications.

XmlHttpRequest still performs HTTP requests under the hood,
and then waits for a response, but the Javascript code can
continue to run until the response arrives. It will then receive
the response through a callback previously defined.

This is of course still requests initiated by the client,
the server still had no way of pushing data to the client
on its own, so new technology appeared to allow that.

Long-polling
------------

Polling was a technique used to overcome the fact that the server
cannot push data directly to the client. Therefore the client had
to repeatedly create a connection, make a request, get a response,
then try again a few seconds later. This is overly expensive and
adds an additional delay before the client receives the data.

Polling was necessary to implement message queues and other
similar mechanisms, where a user must be informed of something
when it happens, rather than when he refreshes the page next.
A typical example would be a chat application.

Long-polling was created to reduce the server load by creating
less connections, but also to improve latency by getting the
response back to the client as soon as it becomes available
on the server.

Long-polling works in a similar manner to polling, except the
request will not get a response immediately. Instead the server
leaves it open until it has a response to send. After getting
the response, the client creates a new request and gets back
to waiting.

You probably guessed by now that long-polling is a hack, and
like most hacks it can suffer from unforeseen issues, in this
case it doesn't always play well with proxies.

HTML5
-----

HTML5 is, of course, the HTML version after HTML4. But HTML5
emerged to solve a specific problem: dynamic web applications.

HTML was initially created to write web pages which compose
a website. But soon people and companies wanted to use HTML
to write more and more complex websites, eventually known as
web applications. They are for example your news reader, your
email client in the browser, or your video streaming website.

Because HTML wasn't enough, they started using proprietary
solutions, often implemented using plug-ins. This wasn't
perfect of course, but worked well enough for most people.

However, the needs for a standard solution eventually became
apparent. The browser needed to be able to play media natively.
It needed to be able to draw anything. It needed an efficient
way of streaming events to the server, but also receiving
events from the server.

The solution went on to become HTML5. At the time of writing
it is being standardized.

EventSource
-----------

EventSource, sometimes also called Server-Sent Events, is a
technology allowing servers to push data to HTML5 applications.

EventSource is one-way communication channel from the server
to the client. The client has no means to talk to the server
other than by using HTTP requests.

It consists of a Javascript object allowing setting up an
EventSource connection to the server, and a very small protocol
for sending events to the client on top of the HTTP/1.1
connection.

EventSource is a lightweight solution that only works for
UTF-8 encoded text data. Binary data and text data encoded
differently are not allowed by the protocol. A heavier but
more generic approach can be found in Websocket.

Websocket
---------

Websocket is a protocol built on top of HTTP/1.1 that provides
a two-ways communication channel between the client and the
server. Communication is asynchronous and can occur concurrently.

It consists of a Javascript object allowing setting up a
Websocket connection to the server, and a binary based
protocol for sending data to the server or the client.

Websocket connections can transfer either UTF-8 encoded text
data or binary data. The protocol also includes support for
implementing a ping/pong mechanism, allowing the server and
the client to have more confidence that the connection is still
alive.

A Websocket connection can be used to transfer any kind of data,
small or big, text or binary. Because of this Websocket is
sometimes used for communication between systems.

SPDY
----

SPDY is an attempt to reduce page loading time by opening a
single connection per server, keeping it open for subsequent
requests, and also by compressing the HTTP headers to reduce
the size of requests.

SPDY is compatible with HTTP/1.1 semantics, and is actually
just a different way of performing HTTP requests and responses,
by using binary frames instead of a text-based protocol.
SPDY also allows the server to send extra responses following
a request. This is meant to allow sending the resources
associated with the request before the client requests them,
saving latency when loading websites.

SPDY is an experiment that has proven successful and is used
as the basis for the HTTP/2.0 standard.

Browsers make use of TLS Next Protocol Negotiation to upgrade
to a SPDY connection seamlessly if the protocol supports it.

The protocol itself has a few shortcomings which are being
fixed in HTTP/2.0.

HTTP/2.0
--------

HTTP/2.0 is the long-awaited update to the HTTP/1.1 protocol.
It is based on SPDY although a lot has been improved at the
time of writing.

HTTP/2.0 is an asynchronous two-ways communication channel
between two endpoints.

It is planned to be ready late 2014.
