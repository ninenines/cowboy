#!/bin/sh
erl -pa ebin deps/*/ebin -s ssl_hello_world \
	-eval "io:format(\"Point your browser at https://localhost:8443~n\")."
