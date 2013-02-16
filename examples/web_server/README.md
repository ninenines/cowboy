Cowboy Static File Handler with Index Support
=============================================

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

Cowboy will serve all the files you put in the priv/ directory. You can replace
the filename given in the example URL with the one of a file you added to this
directory to receive that file. A middleware has been added that will re-route
the request to a different handler if the requested path is a directory.

Example
-------

Point your browser to http://localhost:8080 to see the contents of `priv/`. You
can click on a link to see that file. If HTML is not preferred, the contents of
a directory will be listed as a JSON array (e.g. with `curl
http://localhost:8080`).
