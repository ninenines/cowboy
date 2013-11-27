Static file handler example
===========================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/static_world_example console
```

The example will serve all the files found in the `priv`
directory. For example:

 *  [Plain text file](http://localhost:8080/test.txt)
 *  [HTML5 video demo](http://localhost:8080/video.html)

Example output
--------------

``` bash
$ curl -i http://localhost:8080/test.txt
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Mon, 09 Sep 2013 13:49:50 GMT
content-length: 52
content-type: text/plain
last-modified: Fri, 18 Jan 2013 16:33:31 GMT

If you read this then the static file server works!
```
