#!/bin/sh
erl -pa ebin deps/*/ebin -s web_server \
	-eval "io:format(\"Point your browser at http://localhost:8080/~n\")."
