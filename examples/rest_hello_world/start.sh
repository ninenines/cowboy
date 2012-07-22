#!/bin/sh
erl -pa ebin deps/*/ebin -s rest_hello_world \
	-eval "io:format(\"Get HTML: curl -i http://localhost:8080~n\")." \
	-eval "io:format(\"Get JSON: curl -i -H \\\"Accept: application/json\\\" http://localhost:8080~n\")." \
	-eval "io:format(\"Get text: curl -i -H \\\"Accept: text/plain\\\" http://localhost:8080~n\")." \
	-eval "io:format(\"Get a 406: curl -i -H \\\"Accept: text/css\\\" http://localhost:8080~n\")."
