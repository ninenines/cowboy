#!/bin/sh
erl -pa ebin deps/*/ebin -s echo_post \
	-eval "io:format(\"Run ./curl_post.sh STRING_TO_ECHO\")."
