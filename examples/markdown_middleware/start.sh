#!/bin/sh
erl -pa ebin deps/*/ebin -s markdown_middleware \
	-eval "io:format(\"Point your browser at http://localhost:8080/video.html~n\")."
