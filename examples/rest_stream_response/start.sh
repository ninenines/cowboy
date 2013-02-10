#!/bin/sh
erl -pa ebin deps/*/ebin -s rest_stream_response \
	-eval "io:format(\"Streaming results: curl -i http://localhost:8080~n\")."
