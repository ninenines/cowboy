Cowboy Session Cookie
=====================

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

This application provides an example of a session cookie that is loaded and
updated on every request. The cookie data is stored in Erlang's external term
format and verified with a hash of the cookie data and its timeout salted with
an application secret. This means that cookie data should not contain sensitive
information, but it is tamper-evident and will be rejected after the timeout
has been reached (for the example's sake, this has be set to two minutes).
Advanced techniques like encryption and compression are left as exercises for
the reader.

Example
--------

Open http://localhost:8080/ in your favorite browser.
