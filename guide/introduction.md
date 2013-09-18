Introduction
============

Cowboy is a small, fast and modular HTTP server written in Erlang.

Cowboy aims to provide a complete HTTP stack, including its derivatives
SPDY, Websocket and REST. Cowboy currently supports HTTP/1.0, HTTP/1.1,
Websocket (all implemented drafts + standard) and Webmachine-based REST.

Cowboy is a high quality project. It has a small code base, is very
efficient (both in latency and memory use) and can easily be embedded
in another application.

Cowboy is clean Erlang code. It includes hundreds of tests and its code
is fully compliant with the Dialyzer. It is also well documented and
features both a Function Reference and a User Guide.

Prerequisites
-------------

No Erlang knowledge is required for reading this guide. The reader will
be introduced to Erlang concepts and redirected to reference material
whenever necessary.

Knowledge of the HTTP protocol is recommended but not required, as it
will be detailed throughout the guide.

Supported platforms
-------------------

Cowboy is tested and supported on Linux.

Cowboy has been reported to work on other platforms, but we make no
guarantee that the experience will be safe and smooth. You are advised
to perform the necessary testing and security audits prior to deploying
on other platforms.

Cowboy is developed for Erlang R15B+.

Cowboy may be compiled on earlier Erlang versions with small source code
modifications but there is no guarantee that it will work as expected.

Conventions
-----------

In the HTTP protocol, the method name is case sensitive. All standard
method names are uppercase.

Header names are case insensitive. Cowboy converts all the request
header names to lowercase, and expects your application to provide
lowercase header names in the response.

The same applies to any other case insensitive value.
