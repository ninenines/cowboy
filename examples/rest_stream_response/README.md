Cowboy REST Streaming Responses
===============================

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

This example simulates streaming a large amount of data from a data store one
record at a time in CSV format. It also uses a constraint to ensure that the
last segment of the route is an integer.

Examples
--------

### Get records with a field 2 value of 1

``` bash
$ curl -i localhost:8080
HTTP/1.1 200 OK
transfer-encoding: identity
server: Cowboy
date: Sun, 10 Feb 2013 19:32:16 GMT
connection: close
content-type: text/csv

DBUZGQ0C,1,28
BgoQAxMV,1,6
DAYEFxER,1,18
...
```

### Get records with a field 2 value of 4

``` bash
$ curl -i localhost:8080/4
HTTP/1.1 200 OK
transfer-encoding: identity
server: Cowboy
date: Sun, 10 Feb 2013 19:34:31 GMT
connection: close
content-type: text/csv

ABcFDxcE,4,42
DgYQCgEE,4,5
CA8BBhYD,4,10
...
```

### Get a 404

``` bash
$ curl -i localhost:8080/foo
HTTP/1.1 404 Not Found
connection: keep-alive
server: Cowboy
date: Sun, 10 Feb 2013 19:36:16 GMT
content-length: 0
```
