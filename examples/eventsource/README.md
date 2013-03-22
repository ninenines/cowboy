Cowboy EventSource
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

Uses Cowboy's loop functionality to continuously send events to the browser.

Example
-------

Point your browser to http://localhost:8080 to see EventSource in action with
any modern browser (not IE).
