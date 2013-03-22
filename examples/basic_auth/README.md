Cowboy Basic Authorization Rest Hello World
===========================================

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

### Get 401
``` bash
$ curl -i http://localhost:8080
HTTP/1.1 401 Unauthorized
connection: keep-alive
server: Cowboy
date: Sun, 20 Jan 2013 14:10:27 GMT
content-length: 0
www-authenticate: Restricted
```

### Get 200
``` bash
$ curl -i -u "Alladin:open sesame" http://localhost:8080
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Sun, 20 Jan 2013 14:11:12 GMT
content-length: 16
content-type: text/plain

Hello, Alladin!
```
