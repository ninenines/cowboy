%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(http_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
	init_per_group/2, end_per_group/2]). %% ct.
-export([chunked_response/1, headers_dupe/1, headers_huge/1,
	keepalive_nl/1, nc_rand/1, pipeline/1, raw/1,
	ws0/1, ws8/1, ws_timeout_hibernate/1]). %% http.
-export([http_200/1, http_404/1]). %% http and https.
-export([http_10_hostless/1]). %% misc.

%% ct.

all() ->
	[{group, http}, {group, https}, {group, misc}].

groups() ->
	BaseTests = [http_200, http_404],
	[{http, [], [chunked_response, headers_dupe, headers_huge,
		keepalive_nl, nc_rand, pipeline, raw,
		ws0, ws8, ws_timeout_hibernate] ++ BaseTests},
	{https, [], BaseTests}, {misc, [], [http_10_hostless]}].

init_per_suite(Config) ->
	application:start(inets),
	application:start(cowboy),
	Config.

end_per_suite(_Config) ->
	application:stop(cowboy),
	application:stop(inets),
	ok.

init_per_group(http, Config) ->
	Port = 33080,
	cowboy:start_listener(http, 100,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [{dispatch, init_http_dispatch()}]
	),
	[{scheme, "http"}, {port, Port}|Config];
init_per_group(https, Config) ->
	Port = 33081,
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	DataDir = ?config(data_dir, Config),
	cowboy:start_listener(https, 100,
		cowboy_ssl_transport, [
			{port, Port}, {certfile, DataDir ++ "cert.pem"},
			{keyfile, DataDir ++ "key.pem"}, {password, "cowboy"}],
		cowboy_http_protocol, [{dispatch, init_https_dispatch()}]
	),
	[{scheme, "https"}, {port, Port}|Config];
init_per_group(misc, Config) ->
	Port = 33082,
	cowboy:start_listener(misc, 100,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [{dispatch, [{'_', [
			{[], http_handler, []}
	]}]}]),
	[{port, Port}|Config].

end_per_group(https, _Config) ->
	cowboy:stop_listener(https),
	application:stop(ssl),
	application:stop(public_key),
	application:stop(crypto),
	ok;
end_per_group(Listener, _Config) ->
	cowboy:stop_listener(Listener),
	ok.

%% Dispatch configuration.

init_http_dispatch() ->
	[
		{[<<"localhost">>], [
			{[<<"chunked_response">>], chunked_handler, []},
			{[<<"websocket">>], websocket_handler, []},
			{[<<"ws_timeout_hibernate">>], ws_timeout_hibernate_handler, []},
			{[<<"headers">>, <<"dupe">>], http_handler,
				[{headers, [{<<"Connection">>, <<"close">>}]}]},
			{[], http_handler, []}
		]}
	].

init_https_dispatch() ->
	init_http_dispatch().

%% http.

chunked_response(Config) ->
	{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, "chunked_handler\r\nworks fine!"}} =
		httpc:request(build_url("/chunked_response", Config)).

headers_dupe(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, "GET /headers/dupe HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: keep-alive\r\n\r\n"),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{_Start, _Length} = binary:match(Data, <<"Connection: close">>),
	nomatch = binary:match(Data, <<"Connection: keep-alive">>),
	ok = gen_tcp:close(Socket).

headers_huge(Config) ->
	Cookie = lists:flatten(["whatever_man_biiiiiiiiiiiig_cookie_me_want_77="
		"Wed Apr 06 2011 10:38:52 GMT-0500 (CDT)" || _N <- lists:seq(1, 1000)]),
	{_Packet, 200} = raw_req(["GET / HTTP/1.0\r\nHost: localhost\r\n"
		"Set-Cookie: ", Cookie, "\r\n\r\n"], Config).

keepalive_nl(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = keepalive_nl_loop(Socket, 100),
	ok = gen_tcp:close(Socket).

keepalive_nl_loop(_Socket, 0) ->
	ok;
keepalive_nl_loop(Socket, N) ->
	ok = gen_tcp:send(Socket, "GET / HTTP/1.1\r\n"
		"Host: localhost\r\nConnection: keep-alive\r\n\r\n"),
	{ok, Data} = gen_tcp:recv(Socket, 0, 6000),
	{0, 12} = binary:match(Data, <<"HTTP/1.1 200">>),
	nomatch = binary:match(Data, <<"Connection: close">>),
	ok = gen_tcp:send(Socket, "\r\n"), %% extra nl
	keepalive_nl_loop(Socket, N - 1).

nc_rand(Config) ->
	Cat = os:find_executable("cat"),
	Nc = os:find_executable("nc"),
	case {Cat, Nc} of
		{false, _} ->
			{skip, {notfound, cat}};
		{_, false} ->
			{skip, {notfound, nc}};
		_Good ->
			%% Throw garbage at the server then check if it's still up.
			{port, Port} = lists:keyfind(port, 1, Config),
			[nc_rand_run(Port) || _N <- lists:seq(1, 100)],
			Packet = "GET / HTTP/1.0\r\nHost: localhost\r\n\r\n",
			{Packet, 200} = raw_req(Packet, Config)
	end.

nc_rand_run(Port) ->
	os:cmd("cat /dev/urandom | nc localhost " ++ integer_to_list(Port)).

pipeline(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket,
		"GET / HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n"
		"GET / HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n"
		"GET / HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n"
		"GET / HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n"
		"GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"),
	Data = pipeline_recv(Socket, <<>>),
	Reqs = binary:split(Data, << "\r\n\r\nhttp_handler" >>, [global, trim]),
	5 = length(Reqs),
	pipeline_check(Reqs).

pipeline_check([]) ->
	ok;
pipeline_check([Req|Tail]) ->
	<< "HTTP/1.1 200", _Rest/bits >> = Req,
	pipeline_check(Tail).

pipeline_recv(Socket, SoFar) ->
	case gen_tcp:recv(Socket, 0, 6000) of
		{ok, Data} ->
			pipeline_recv(Socket, << SoFar/binary, Data/binary >>);
		{error, closed} ->
			ok = gen_tcp:close(Socket),
			SoFar
	end.

raw_req(Packet, Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, Packet),
	{ok, << "HTTP/1.1 ", Str:24/bits, _Rest/bits >>}
		= gen_tcp:recv(Socket, 0, 6000),
	gen_tcp:close(Socket),
	{Packet, list_to_integer(binary_to_list(Str))}.

raw(Config) ->
	Tests = [
		{"\r\n\r\n\r\n\r\n\r\nGET / HTTP/1.1\r\nHost: localhost\r\n\r\n", 200},
		{"\n", 400},
		{"Garbage\r\n\r\n", 400},
		{"\r\n\r\n\r\n\r\n\r\n\r\n", 400},
		{"GET / HTTP/1.1\r\nHost: dev-extend.eu\r\n\r\n", 400},
		{"", 408},
		{"\r\n", 408},
		{"\r\n\r\n", 408},
		{"GET / HTTP/1.1", 408},
		{"GET / HTTP/1.1\r\n", 408},
		{"GET / HTTP/1.1\r\nHost: localhost", 408},
		{"GET / HTTP/1.1\r\nHost: localhost\r\n", 408},
		{"GET / HTTP/1.1\r\nHost: localhost\r\n\r", 408},
		{"GET http://localhost/ HTTP/1.1\r\n\r\n", 501},
		{"GET / HTTP/1.2\r\nHost: localhost\r\n\r\n", 505}
	],
	[{Packet, StatusCode} = raw_req(Packet, Config)
		|| {Packet, StatusCode} <- Tests].

ws0(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /websocket HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: WebSocket\r\n"
		"Origin: http://localhost\r\n"
		"Sec-Websocket-Key1: Y\" 4 1Lj!957b8@0H756!i\r\n"
		"Sec-Websocket-Key2: 1711 M;4\\74  80<6\r\n"
		"\r\n", <<15,245,8,18,2,204,133,33>>]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "WebSocket Protocol Handshake"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, Body] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "WebSocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-location", "ws://localhost/websocket"}
		= lists:keyfind("sec-websocket-location", 1, Headers),
	{"sec-websocket-origin", "http://localhost"}
		= lists:keyfind("sec-websocket-origin", 1, Headers),
	<<169,244,191,103,146,33,149,59,74,104,67,5,99,118,171,236>> = Body,
	ok = gen_tcp:send(Socket, << 0, "client_msg", 255 >>),
	{ok, << 0, "client_msg", 255 >>} = gen_tcp:recv(Socket, 0, 6000),
	{ok, << 0, "websocket_init", 255 >>} = gen_tcp:recv(Socket, 0, 6000),
	{ok, << 0, "websocket_handle", 255 >>} = gen_tcp:recv(Socket, 0, 6000),
	{ok, << 0, "websocket_handle", 255 >>} = gen_tcp:recv(Socket, 0, 6000),
	{ok, << 0, "websocket_handle", 255 >>} = gen_tcp:recv(Socket, 0, 6000),
	ok = gen_tcp:send(Socket, << 255, 0 >>),
	{ok, << 255, 0 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws8(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /websocket HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	ok = gen_tcp:send(Socket, << 16#81, 16#85, 16#37, 16#fa, 16#21, 16#3d,
		16#7f, 16#9f, 16#4d, 16#51, 16#58 >>),
	{ok, << 1:1, 0:3, 1:4, 0:1, 5:7, "Hello" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 14:7, "websocket_init" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 9:4, 0:8 >>), %% ping
	{ok, << 1:1, 0:3, 10:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000), %% pong
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 0:8 >>), %% close
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_timeout_hibernate(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_timeout_hibernate HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

websocket_headers({ok, http_eoh, Rest}, Acc) ->
	[Acc, Rest];
websocket_headers({ok, {http_header, _I, Key, _R, Value}, Rest}, Acc) ->
	F = fun(S) when is_atom(S) -> S; (S) -> string:to_lower(S) end,
	websocket_headers(erlang:decode_packet(httph, Rest, []),
		[{F(Key), Value}|Acc]).

%% http and https.

build_url(Path, Config) ->
	{scheme, Scheme} = lists:keyfind(scheme, 1, Config),
	{port, Port} = lists:keyfind(port, 1, Config),
	Scheme ++ "://localhost:" ++ integer_to_list(Port) ++ Path.

http_200(Config) ->
	{ok, {{"HTTP/1.1", 200, "OK"}, _Headers, "http_handler"}} =
		httpc:request(build_url("/", Config)).

http_404(Config) ->
	{ok, {{"HTTP/1.1", 404, "Not Found"}, _Headers, _Body}} =
		httpc:request(build_url("/not/found", Config)).

%% misc.

http_10_hostless(Config) ->
	Packet = "GET / HTTP/1.0\r\n\r\n",
	{Packet, 200} = raw_req(Packet, Config).
