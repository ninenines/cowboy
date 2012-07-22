#!/bin/sh
erl -pa ebin deps/*/ebin -s chunked_hello_world \
	-eval "io:format(\"Run: curl -i http://localhost:8080~n\")."
