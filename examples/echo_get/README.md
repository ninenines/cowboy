Cowboy GET Echo
===============

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

Then point your browser to the indicated URL. You can change
the GET parameter to check that the handler is echoing properly.

Example
-------

``` bash
$ curl -i "http://localhost:8080/?echo=saymyname"
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:09:04 GMT
content-length: 9
Content-Encoding: utf-8

saymyname
```
