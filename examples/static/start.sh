#!/bin/sh
erl -pa ebin deps/*/ebin -s static \
	-eval "io:format(\"Point your browser at http://localhost:8080/test.txt~n\")."
