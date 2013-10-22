Error hook example
==================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/error_hook_example console
```

Then point your browser at [http://localhost:8080](http://localhost:8080).

Example output
--------------

Not found:

``` bash
$ curl -i http://localhost:8080
HTTP/1.1 404 Not Found
connection: keep-alive
server: Cowboy
date: Wed, 27 Feb 2013 23:32:55 GMT
content-length: 56

404 Not Found: "/" is not the path you are looking for.
```

Bad request:

``` bash
$ telnet localhost 8080
Trying ::1...
Connection failed: Connection refused
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
bad
HTTP/1.1 400 Bad Request
connection: close
server: Cowboy
date: Sun, 08 Sep 2013 09:29:27 GMT
content-length: 15

HTTP Error 400
Connection closed by foreign host.
```
