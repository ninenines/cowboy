Chunked hello world example
===========================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/chunked_hello_world_example console
```

Then point your browser at [http://localhost:8080](http://localhost:8080),
or use `curl` to see the chunks arriving one at a time every second.

Example output
--------------

``` bash
$ time curl -i http://localhost:8080
HTTP/1.1 200 OK
transfer-encoding: chunked
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:24:16 GMT

Hello
World
Chunked!
curl -i http://localhost:8080  0.01s user 0.00s system 0% cpu 2.015 total
```
