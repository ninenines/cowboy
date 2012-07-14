Cowboy Static Files Server
==========================

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

Cowboy will serve all the files you put in the priv/ directory.
You can replace the filename given in the example URL with the
one of a file you added to this directory to receive that file.
