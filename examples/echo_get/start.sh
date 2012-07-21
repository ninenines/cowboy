#!/bin/sh
erl -pa ebin deps/*/ebin -s echo_get \
	-eval "io:format(\"Point your browser at http://localhost:8080/?echo=test~n\")."
