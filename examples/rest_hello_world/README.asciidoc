= REST hello world example

To try this example, you need GNU `make` and `git` in your PATH.

To build and run the example, use the following command:

[source,bash]
$ make run

Then point your browser to http://localhost:8080

== HTTP/1.1 example output

Request HTML:

[source,bash]
----
$ curl -i http://localhost:8080
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:15:52 GMT
content-length: 136
content-type: text/html
vary: Accept

<html>
<head>
  <meta charset="utf-8">
  <title>REST Hello World!</title>
</head>
<body>
  <p>REST Hello World as HTML!</p>
</body>
</html>
----

Request JSON:

[source,bash]
----
$ curl -i -H "Accept: application/json" http://localhost:8080
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:16:46 GMT
content-length: 24
content-type: application/json
vary: Accept

{"rest": "Hello World!"}
----

Request plain text:

[source,bash]
----
$ curl -i -H "Accept: text/plain" http://localhost:8080
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:18:35 GMT
content-length: 25
content-type: text/plain
vary: Accept

REST Hello World as text!
----

Request a non acceptable content-type:

[source,bash]
----
$ curl -i -H "Accept: text/css" http://localhost:8080
HTTP/1.1 406 Not Acceptable
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:18:51 GMT
content-length: 0

----

== HTTP/2 example output

Request HTML:

[source,bash]
----
$ nghttp -v http://localhost:8080
[  0.000] Connected
[  0.000] send SETTINGS frame <length=12, flags=0x00, stream_id=0>
          (niv=2)
          [SETTINGS_MAX_CONCURRENT_STREAMS(0x03):100]
          [SETTINGS_INITIAL_WINDOW_SIZE(0x04):65535]
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=3>
          (dep_stream_id=0, weight=201, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=5>
          (dep_stream_id=0, weight=101, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=7>
          (dep_stream_id=0, weight=1, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=9>
          (dep_stream_id=7, weight=1, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=11>
          (dep_stream_id=3, weight=1, exclusive=0)
[  0.000] send HEADERS frame <length=38, flags=0x25, stream_id=13>
          ; END_STREAM | END_HEADERS | PRIORITY
          (padlen=0, dep_stream_id=11, weight=16, exclusive=0)
          ; Open new stream
          :method: GET
          :path: /
          :scheme: http
          :authority: localhost:8080
          accept: */*
          accept-encoding: gzip, deflate
          user-agent: nghttp2/1.7.1
[  0.000] recv SETTINGS frame <length=0, flags=0x00, stream_id=0>
          (niv=0)
[  0.000] send SETTINGS frame <length=0, flags=0x01, stream_id=0>
          ; ACK
          (niv=0)
[  0.000] recv SETTINGS frame <length=0, flags=0x01, stream_id=0>
          ; ACK
          (niv=0)
[  0.001] recv (stream_id=13) :status: 200
[  0.001] recv (stream_id=13) content-length: 136
[  0.001] recv (stream_id=13) content-type: text/html
[  0.001] recv (stream_id=13) date: Thu, 09 Jun 2016 14:28:50 GMT
[  0.001] recv (stream_id=13) server: Cowboy
[  0.001] recv (stream_id=13) vary: accept
[  0.001] recv HEADERS frame <length=52, flags=0x04, stream_id=13>
          ; END_HEADERS
          (padlen=0)
          ; First response header
<html>
<head>
	<meta charset="utf-8">
	<title>REST Hello World!</title>
</head>
<body>
	<p>REST Hello World as HTML!</p>
</body>
</html>[  0.001] recv DATA frame <length=136, flags=0x01, stream_id=13>
          ; END_STREAM
[  0.001] send GOAWAY frame <length=8, flags=0x00, stream_id=0>
          (last_stream_id=0, error_code=NO_ERROR(0x00), opaque_data(0)=[])
----

Request JSON:

[source,bash]
----
$ nghttp -v -H "accept: application/json" http://localhost:8080
[  0.000] Connected
[  0.000] send SETTINGS frame <length=12, flags=0x00, stream_id=0>
          (niv=2)
          [SETTINGS_MAX_CONCURRENT_STREAMS(0x03):100]
          [SETTINGS_INITIAL_WINDOW_SIZE(0x04):65535]
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=3>
          (dep_stream_id=0, weight=201, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=5>
          (dep_stream_id=0, weight=101, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=7>
          (dep_stream_id=0, weight=1, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=9>
          (dep_stream_id=7, weight=1, exclusive=0)
[  0.001] send PRIORITY frame <length=5, flags=0x00, stream_id=11>
          (dep_stream_id=3, weight=1, exclusive=0)
[  0.001] send HEADERS frame <length=46, flags=0x25, stream_id=13>
          ; END_STREAM | END_HEADERS | PRIORITY
          (padlen=0, dep_stream_id=11, weight=16, exclusive=0)
          ; Open new stream
          :method: GET
          :path: /
          :scheme: http
          :authority: localhost:8080
          accept: application/json
          accept-encoding: gzip, deflate
          user-agent: nghttp2/1.7.1
[  0.001] recv SETTINGS frame <length=0, flags=0x00, stream_id=0>
          (niv=0)
[  0.001] send SETTINGS frame <length=0, flags=0x01, stream_id=0>
          ; ACK
          (niv=0)
[  0.001] recv SETTINGS frame <length=0, flags=0x01, stream_id=0>
          ; ACK
          (niv=0)
[  0.001] recv (stream_id=13) :status: 200
[  0.001] recv (stream_id=13) content-length: 24
[  0.001] recv (stream_id=13) content-type: application/json
[  0.001] recv (stream_id=13) date: Thu, 09 Jun 2016 14:29:00 GMT
[  0.001] recv (stream_id=13) server: Cowboy
[  0.001] recv (stream_id=13) vary: accept
[  0.001] recv HEADERS frame <length=55, flags=0x04, stream_id=13>
          ; END_HEADERS
          (padlen=0)
          ; First response header
{"rest": "Hello World!"}[  0.002] recv DATA frame <length=24, flags=0x01, stream_id=13>
          ; END_STREAM
[  0.002] send GOAWAY frame <length=8, flags=0x00, stream_id=0>
          (last_stream_id=0, error_code=NO_ERROR(0x00), opaque_data(0)=[])
----

Request plain text:

[source,bash]
----
$ nghttp -v -H "accept: text/plain" http://localhost:8080
[  0.000] Connected
[  0.000] send SETTINGS frame <length=12, flags=0x00, stream_id=0>
          (niv=2)
          [SETTINGS_MAX_CONCURRENT_STREAMS(0x03):100]
          [SETTINGS_INITIAL_WINDOW_SIZE(0x04):65535]
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=3>
          (dep_stream_id=0, weight=201, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=5>
          (dep_stream_id=0, weight=101, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=7>
          (dep_stream_id=0, weight=1, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=9>
          (dep_stream_id=7, weight=1, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=11>
          (dep_stream_id=3, weight=1, exclusive=0)
[  0.000] send HEADERS frame <length=42, flags=0x25, stream_id=13>
          ; END_STREAM | END_HEADERS | PRIORITY
          (padlen=0, dep_stream_id=11, weight=16, exclusive=0)
          ; Open new stream
          :method: GET
          :path: /
          :scheme: http
          :authority: localhost:8080
          accept: text/plain
          accept-encoding: gzip, deflate
          user-agent: nghttp2/1.7.1
[  0.000] recv SETTINGS frame <length=0, flags=0x00, stream_id=0>
          (niv=0)
[  0.000] send SETTINGS frame <length=0, flags=0x01, stream_id=0>
          ; ACK
          (niv=0)
[  0.000] recv SETTINGS frame <length=0, flags=0x01, stream_id=0>
          ; ACK
          (niv=0)
[  0.000] recv (stream_id=13) :status: 200
[  0.000] recv (stream_id=13) content-length: 25
[  0.000] recv (stream_id=13) content-type: text/plain
[  0.000] recv (stream_id=13) date: Thu, 09 Jun 2016 14:28:25 GMT
[  0.000] recv (stream_id=13) server: Cowboy
[  0.000] recv (stream_id=13) vary: accept
[  0.000] recv HEADERS frame <length=51, flags=0x04, stream_id=13>
          ; END_HEADERS
          (padlen=0)
          ; First response header
REST Hello World as text![  0.000] recv DATA frame <length=25, flags=0x01, stream_id=13>
          ; END_STREAM
[  0.000] send GOAWAY frame <length=8, flags=0x00, stream_id=0>
          (last_stream_id=0, error_code=NO_ERROR(0x00), opaque_data(0)=[])
----

Request a non acceptable content-type:

[source,bash]
----
$ nghttp -v -H "accept: text/css" http://localhost:8080
[  0.000] Connected
[  0.000] send SETTINGS frame <length=12, flags=0x00, stream_id=0>
          (niv=2)
          [SETTINGS_MAX_CONCURRENT_STREAMS(0x03):100]
          [SETTINGS_INITIAL_WINDOW_SIZE(0x04):65535]
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=3>
          (dep_stream_id=0, weight=201, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=5>
          (dep_stream_id=0, weight=101, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=7>
          (dep_stream_id=0, weight=1, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=9>
          (dep_stream_id=7, weight=1, exclusive=0)
[  0.000] send PRIORITY frame <length=5, flags=0x00, stream_id=11>
          (dep_stream_id=3, weight=1, exclusive=0)
[  0.000] send HEADERS frame <length=41, flags=0x25, stream_id=13>
          ; END_STREAM | END_HEADERS | PRIORITY
          (padlen=0, dep_stream_id=11, weight=16, exclusive=0)
          ; Open new stream
          :method: GET
          :path: /
          :scheme: http
          :authority: localhost:8080
          accept: text/css
          accept-encoding: gzip, deflate
          user-agent: nghttp2/1.7.1
[  0.007] recv SETTINGS frame <length=0, flags=0x00, stream_id=0>
          (niv=0)
[  0.007] recv SETTINGS frame <length=0, flags=0x01, stream_id=0>
          ; ACK
          (niv=0)
[  0.007] send SETTINGS frame <length=0, flags=0x01, stream_id=0>
          ; ACK
          (niv=0)
[  0.021] recv (stream_id=13) :status: 406
[  0.021] recv (stream_id=13) content-length: 0
[  0.021] recv (stream_id=13) date: Thu, 09 Jun 2016 14:29:15 GMT
[  0.021] recv (stream_id=13) server: Cowboy
[  0.021] recv HEADERS frame <length=39, flags=0x04, stream_id=13>
          ; END_HEADERS
          (padlen=0)
          ; First response header
[  0.021] recv DATA frame <length=0, flags=0x01, stream_id=13>
          ; END_STREAM
[  0.021] send GOAWAY frame <length=8, flags=0x00, stream_id=0>
          (last_stream_id=0, error_code=NO_ERROR(0x00), opaque_data(0)=[])
----
