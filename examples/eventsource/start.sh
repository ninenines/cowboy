#!/bin/sh
erl -pa ebin deps/*/ebin -s eventsource \
	-eval "io:format(\"Point your browser at http://localhost:8080/~n\")."
