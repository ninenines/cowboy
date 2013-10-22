POST parameter echo example
===========================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/echo_post_example console
```

As this example echoes a POST parameter, it is a little more
complex to test. Some browsers feature tools that allow you
to perform one such request, or you can use the command line
tool `curl` as we will demonstrate.

Example output
--------------

``` bash
$ curl -i -d echo=echomeplz http://localhost:8080
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:12:36 GMT
content-length: 9
content-type: text/plain; charset=utf-8

echomeplz
```
