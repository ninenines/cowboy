Cowboy regexp router
=================

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

Then point your browser to the indicated URL.


Example
-------------------

``` bash
$ curl -i http://localhost:8080/
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Thu, 15 Aug 2013 08:05:01 GMT
content-length: 15

Hello everyone!

$ curl -i http://localhost:8080/hello/Jack
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Thu, 15 Aug 2013 08:05:21 GMT
content-length: 12

Hello, Jack!

$ curl -i http://localhost:8080/hello/Tom
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Thu, 15 Aug 2013 08:05:41 GMT
content-length: 16

Hello, dear Tom!

```
