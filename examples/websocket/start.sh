#!/bin/sh
erl -pa ebin deps/*/ebin -s websocket \
    -eval "io:format(\"ws://localhost:8080/websocket \
            ~nPoint your browser at http://localhost:8080/ to use a simple websocket client~n\")."

