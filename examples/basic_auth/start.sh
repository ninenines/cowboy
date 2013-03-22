#!/bin/sh
erl -pa ebin deps/*/ebin -s basic_auth \
	-eval "io:format(\"Get 401: curl -i http://localhost:8080~n\")." \
	-eval "io:format(\"Get 200: curl -i -u \\\"Alladin:open sesame\\\" http://localhost:8080~n\")."
