Cowboy Static Files Server
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

Cowboy will serve all the files you put in the priv/ directory.
You can replace the filename given in the example URL with the
one of a file you added to this directory to receive that file.

Example
-------

Show that the file is returned as an octet-stream

``` bash
$ curl -i http://localhost:8080/test.txt
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:19:40 GMT
content-length: 52
Content-Type: application/octet-stream
Last-Modified: Fri, 28 Sep 2012 04:01:20 GMT

If you read this then the static file server works!
```

Finally download and cat the file to verify

``` bash
$ curl -sLO http://localhost:8080/test.txt
$ cat test.txt
If you read this then the static file server works!
```
