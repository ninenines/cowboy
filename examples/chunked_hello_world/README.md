Cowboy Chunked Hello World
==========================

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

Then run the given command or point your browser to the indicated URL.

Example
-------

```bash
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
