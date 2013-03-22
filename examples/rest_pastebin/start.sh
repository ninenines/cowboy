#!/bin/sh
erl -pa ebin deps/*/ebin -s rest_pastebin \
	-eval "io:format(\"Upload: echo foo | curl -i --data-urlencode paste@- localhost:8080~n\")." \
	-eval "io:format(\"Get: curl <value of the location header>~n\")." \
	-eval "io:format(\"Get with highlighting: curl <location>?lang=<language>~n\")." \
	-eval "io:format(\"To get html, point your browser to http://localhost:8080~n\")."
