Cowboy Hello World
==================

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

Then point your browser to the indicated URL. You will need to temporarily
trust the root certificate authority in `priv/ssl/cowboy-ca.crt`.

Example
-------

``` bash
$ curl --cacert priv/ssl/cowboy-ca.crt -i https://localhost:8443
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:10:25 GMT
content-length: 12

Hello world!
```
