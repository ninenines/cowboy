Cowboy Rest Hello World
=======================

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

Then run any given command or point your browser to the indicated URL.

Examples
--------

### Get HTML

``` bash
$ curl -i http://localhost:8080
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:15:52 GMT
content-length: 136
Content-Type: text/html
Vary: Accept

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

### Get JSON

``` bash
$ curl -i -H "Accept: application/json" http://localhost:8080
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:16:46 GMT
content-length: 24
Content-Type: application/json
Vary: Accept

{"rest": "Hello World!"}
```

### Get text

``` bash
$ curl -i -H "Accept: text/plain" http://localhost:8080
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:18:35 GMT
content-length: 25
Content-Type: text/plain
Vary: Accept

REST Hello World as text!
```

### Get a 406
``` bash
$ curl -i -H "Accept: text/css" http://localhost:8080
HTTP/1.1 406 Not Acceptable
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:18:51 GMT
content-length: 0

```
