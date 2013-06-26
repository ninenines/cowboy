Cowboy User Guide
=================

The Cowboy User Guide explores the modern Web and how to make
best use of Cowboy for writing powerful web applications.

Introducing Cowboy
------------------

 *  [Introduction](introduction.md)
   *  Purpose
   *  Prerequisites
   *  Supported platforms
   *  Conventions
 *  [The modern Web](modern_web.md)
   *  The prehistoric Web
   *  HTTP/1.1
   *  REST
   *  Long-polling
   *  HTML5
   *  EventSource
   *  Websocket
   *  SPDY
   *  HTTP/2.0
 *  [Erlang and the Web](erlang_web.md)
   *  The Web is concurrent
   *  The Web is soft real time
   *  The Web is asynchronous
   *  The Web is omnipresent
   *  Erlang is the ideal platform for the Web
 *  [Erlang for beginners](erlang_beginners.md)
 *  [Getting started](getting_started.md)

Using Cowboy
------------

 *  [Routing](routing.md)
   *  Purpose
   *  Structure
   *  Match syntax
   *  Constraints
   *  Compilation
   *  Live update
 *  [Handlers](handlers.md)
   *  Purpose
   *  Protocol upgrades
   *  Custom protocol upgrades
 *  [HTTP handlers](http_handlers.md)
   *  Purpose
   *  Usage
 *  [Loop handlers](loop_handlers.md)
   *  Purpose
   *  Usage
 *  [Websocket handlers](ws_handlers.md)
   *  Purpose
   *  Usage
 *  [REST handlers](rest_handlers.md)
   *  Purpose
   *  Usage
   *  Flow diagram
   *  Methods
   *  Callbacks
   *  Meta data
   *  Response headers
 *  [Static handlers](static_handlers.md)
   *  Purpose
   *  Usage
   *  MIME type
 *  [Request object](req.md)
   *  Purpose
   *  Request
   *  Request body
   *  Multipart request body
   *  Response
   *  Chunked response
   *  Response preconfiguration
   *  Closing the connection
   *  Reducing the memory footprint
 *  [Hooks](hooks.md)
   *  On request
   *  On response
 *  [Middlewares](middlewares.md)
   *  Purpose
   *  Usage
   *  Configuration
   *  Routing middleware
   *  Handler middleware
 *  [Internals](internals.md)
   *  Architecture
   *  One process for many requests
   *  Lowercase header names
   *  Improving performance
 *  [Resources](resources.md)
   *  Frameworks
   *  Helper libraries
   *  Articles
