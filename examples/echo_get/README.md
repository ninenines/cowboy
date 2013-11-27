GET parameter echo example
==========================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/get_echo_example console
```

Then point your browser at
[http://localhost:8080/?echo=hello](http://localhost:8080/?echo=hello).
You can replace the `echo` parameter with another to check
that the handler is echoing it back properly.

Example output
--------------

``` bash
$ curl -i "http://localhost:8080/?echo=saymyname"
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:09:04 GMT
content-length: 9
content-type: text/plain; charset=utf-8

saymyname
```
