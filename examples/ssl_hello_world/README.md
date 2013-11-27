Hello world example
===================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/ssl_hello_world_example console
```

Then point your browser at [http://localhost:8443](http://localhost:8443).
You will need to temporarily trust the root certificate authority,
which can also be found in `priv/ssl/cowboy-ca.crt`.

Example output
--------------

``` bash
$ curl --cacert priv/ssl/cowboy-ca.crt -i https://localhost:8443
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:10:25 GMT
content-length: 12

Hello world!
```
