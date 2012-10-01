Cowboy POST Echo
================

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

Then point your browser to the indicated URL. You can change
the GET parameter to check that the handler is echoing properly.

Then run the following command, replacing STRING_TO_ECHO by the
string you want to echo. Check the ```curl_post.sh``` file for details.

```
./curl_post.sh STRING_TO_ECHO
```

Example
-------

``` bash
$ curl -i -d echo=echomeplz http://localhost:8080
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:12:36 GMT
content-length: 9
Content-Encoding: utf-8

echomeplz
```
