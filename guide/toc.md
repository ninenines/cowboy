Cowboy User Guide
=================

The Cowboy User Guide explores the modern Web and how to make
best use of Cowboy for writing powerful web applications.

Introducing Cowboy
------------------

 *  [Introduction](introduction.md)
 *  [The modern Web](modern_web.md)
 *  [Erlang and the Web](erlang_web.md)
 *  [Erlang for beginners](erlang_beginners.md)
 *  [Getting started](getting_started.md)

HTTP
----

 *  [The life of a request](http_req_life.md)
 *  [Routing](routing.md)
 *  [Handling plain HTTP requests](http_handlers.md)
 *  [The Req object](req.md)
 *  [Reading the request body](req_body.md)
 *  [Sending a response](resp.md)
 *  [Using cookies](cookies.md)

Static files
------------

 *  [Static handler](static_handlers.md)
 *  Distributed CDN solutions
 *  Efficiently serving files

REST
----

 *  REST principles
 *  Media types explained
 *  HTTP caching
 *  [Handling REST requests](rest_handlers.md)
 *  HEAD/GET requests flowchart
 *  POST/PUT/PATCH requests flowchart
 *  DELETE requests flowchart
 *  OPTIONS requests flowchart
 *  Designing a REST API

Multipart
---------

 *  Understanding multipart
 *  Multipart requests
 *  Multipart responses

Server push technologies
------------------------

 *  Push technologies
 *  [Using loop handlers for server push](loop_handlers.md)
 *  CORS

Using Websocket
---------------

 *  The Websocket protocol
 *  [Handling Websocket connections](ws_handlers.md)

Advanced HTTP
-------------

 *  Authentication
 *  Sessions

Advanced Cowboy usage
---------------------

 *  Optimization guide
 *  [Hooks](hooks.md)
 *  [Middlewares](middlewares.md)
 *  Access and error logs
 *  Handling broken clients
   *  HTTP header names
   *  HTTP/1.1 streaming not chunked

Old guide misc
--------------

This section will be removed as content is moved into other chapters.

 *  [Handlers](handlers.md)
 *  [Internals](internals.md)
 *  [Resources](resources.md)
