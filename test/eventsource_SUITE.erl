%% Copyright (c) 2013, Dave Peticolas <dave@krondo.com>
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

-module(eventsource_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([eventsource_init/1]).
-export([eventsource_last_event_id/1]).
-export([eventsource_retry/1]).
-export([eventsource_comment/1]).
-export([eventsource_event/1]).

%% ct.

all() ->
	[{group, eventsource}].

groups() ->
	BaseTests = [
		eventsource_init,
		eventsource_last_event_id,
		eventsource_retry,
		eventsource_comment,
		eventsource_event
	],
	[{eventsource, [parallel], BaseTests}].

init_per_suite(Config) ->
	application:start(crypto),
	application:start(ranch),
	application:start(cowboy),
	Config.

end_per_suite(_Config) ->
	application:stop(cowboy),
	application:stop(ranch),
	application:stop(crypto),
	ok.

init_per_group(eventsource, Config) ->
	cowboy:start_http(eventsource, 100, [{port, 0}], [
		{env, [{dispatch, init_dispatch()}]}
	]),
	Port = ranch:get_port(eventsource),
	[{port, Port}|Config].

end_per_group(Listener, _Config) ->
	cowboy:stop_listener(Listener),
	ok.

%% Dispatch configuration.

init_dispatch() ->
	cowboy_router:compile([
		{"localhost", [
			{"/eventsource_init", eventsource_handler, [quit]},
			{"/eventsource_last_event_id", eventsource_handler,
				[last_event_id, quit]},
			{"/eventsource_retry", eventsource_handler,
				[{send, {retry, 1000}}, quit]},
			{"/eventsource_comment", eventsource_handler,
				[{send, {comment, <<"comment">>}}, quit]},
			{"/eventsource_event", eventsource_handler,
				[{send, {event, [{id, <<"id">>}, {type, <<"type">>},
					{data, <<"line 1\nline 2">>}]}}, quit]}
		]}
	]).

eventsource_init(Config) ->
	%% cowboy_eventsource:init/1 sends the correct header
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket,
		"GET /eventsource_init HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{ok, Response} = recv(Socket),
	{ok, {http_response, {1, 1}, 200, _}, Rest}
		= erlang:decode_packet(http, Response, []),
	[Headers, <<"0\r\n\r\n">>]
		= headers(erlang:decode_packet(httph, Rest, []), []),
	{'Content-Type', "text/event-stream"}
		= lists:keyfind('Content-Type', 1, Headers).

eventsource_last_event_id(Config) ->
	%% cowboy_eventsource:last_event_id/1 retrieves the correct header
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket,
		"GET /eventsource_last_event_id HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Last-Event-ID: lastid\r\n"
		"\r\n"),
	{ok, Response} = recv(Socket),
	{ok, {http_response, {1, 1}, 200, _}, Rest}
		= erlang:decode_packet(http, Response, []),
	[_, Rest2] = headers(erlang:decode_packet(httph, Rest, []), []),
	<<"E\r\ndata: lastid\n\n\r\n0\r\n\r\n">> = Rest2.

eventsource_retry(Config) ->
	%% cowboy_eventsource:send/2 sends correct retry frames
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket,
		"GET /eventsource_retry HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{ok, Response} = recv(Socket),
	{ok, {http_response, {1, 1}, 200, _}, Rest}
		= erlang:decode_packet(http, Response, []),
	[_, Rest2] = headers(erlang:decode_packet(httph, Rest, []), []),
	<<"C\r\nretry: 1000\n\r\n0\r\n\r\n">> = Rest2.

eventsource_comment(Config) ->
	%% cowboy_eventsource:send/2 sends correct comment frames
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket,
		"GET /eventsource_comment HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{ok, Response} = recv(Socket),
	{ok, {http_response, {1, 1}, 200, _}, Rest}
		= erlang:decode_packet(http, Response, []),
	[_, Rest2] = headers(erlang:decode_packet(httph, Rest, []), []),
	<<"A\r\n: comment\n\r\n0\r\n\r\n">> = Rest2.

eventsource_event(Config) ->
	%% cowboy_eventsource:send/2 sends correct event frames
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket,
		"GET /eventsource_event HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{ok, Response} = recv(Socket),
	{ok, {http_response, {1, 1}, 200, _}, Rest}
		= erlang:decode_packet(http, Response, []),
	[_, Rest2] = headers(erlang:decode_packet(httph, Rest, []), []),
	<<"2E\r\nid: id\nevent: type\ndata: line 1\n",
		"data: line 2\n\n\r\n0\r\n\r\n">> = Rest2.

%% Internal.

headers({ok, http_eoh, Rest}, Acc) ->
	[Acc, Rest];
headers({ok, {http_header, _I, Key, _R, Value}, Rest}, Acc) ->
	F = fun(S) when is_atom(S) -> S; (S) -> string:to_lower(S) end,
	headers(erlang:decode_packet(httph, Rest, []),
		[{F(Key), Value}|Acc]).

recv(Socket) ->
	recv(Socket, <<>>).

recv(Socket, Accum) ->
	case gen_tcp:recv(Socket, 0, 6000) of
		{error, closed} ->
			{ok, Accum};
		{ok, Data} ->
			recv(Socket, <<Accum/binary, Data/binary>>)
	end.
