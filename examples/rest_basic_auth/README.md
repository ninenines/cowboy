Basic authorization example using REST
======================================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/rest_basic_auth_example console
```

Then point your browser at [http://localhost:8080](http://localhost:8080).

Example output
--------------

Request with no authentication:

``` bash
$ curl -i http://localhost:8080
HTTP/1.1 401 Unauthorized
connection: keep-alive
server: Cowboy
date: Sun, 20 Jan 2013 14:10:27 GMT
content-length: 0
www-authenticate: Basic realm="cowboy"
```

Request with authentication:

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
