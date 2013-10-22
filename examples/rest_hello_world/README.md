REST hello world example
========================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/hello_world_example console
```

Then point your browser at [http://localhost:8080](http://localhost:8080).

Example output
--------------

Request HTML:

``` bash
$ curl -i http://localhost:8080
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:15:52 GMT
content-length: 136
content-type: text/html
vary: Accept

<html>
<head>
  <meta charset="utf-8">
  <title>REST Hello World!</title>
</head>
<body>
  <p>REST Hello World as HTML!</p>
</body>
</html>
```

Request JSON:

``` bash
$ curl -i -H "Accept: application/json" http://localhost:8080
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:16:46 GMT
content-length: 24
content-type: application/json
vary: Accept

{"rest": "Hello World!"}
```

Request plain text:

``` bash
$ curl -i -H "Accept: text/plain" http://localhost:8080
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:18:35 GMT
content-length: 25
content-type: text/plain
vary: Accept

REST Hello World as text!
```

Request a non acceptable content-type:

``` bash
$ curl -i -H "Accept: text/css" http://localhost:8080
HTTP/1.1 406 Not Acceptable
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:18:51 GMT
content-length: 0

```
