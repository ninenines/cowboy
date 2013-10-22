REST streaming example
======================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/rest_stream_response_example console
```

Then point your browser at [http://localhost:8080](http://localhost:8080).

About
-----

This example simulates streaming a large amount of data from a data store one
record at a time in CSV format. It also uses a constraint to ensure that the
last segment of the route is an integer.

Example output
--------------

Fetch records with the second field with value 1:

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

Fetch records with the second field with value 4:

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

Fail to use a proper integer and get an error:

``` bash
$ curl -i localhost:8080/foo
HTTP/1.1 404 Not Found
connection: keep-alive
server: Cowboy
date: Sun, 10 Feb 2013 19:36:16 GMT
content-length: 0

```
