Cowboy Error Hook
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
-------

``` bash
$ curl -i http://localhost:8080
HTTP/1.1 404 Not Found
connection: keep-alive
server: Cowboy
date: Wed, 27 Feb 2013 23:32:55 GMT
content-length: 56

404 Not Found: "/" is not the path you are looking for.
```
