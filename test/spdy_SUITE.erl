%% Copyright (c) 2013, Loïc Hoguin <essen@ninenines.eu>
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

-module(spdy_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("../src/cowboy_spdy.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([check_status/1]).

%% ct.

all() ->
	[{group, spdy}].

groups() ->
	[{spdy, [], [
		check_status
	]}].

init_per_suite(Config) ->
	application:start(crypto),
	application:start(ranch),
	application:start(cowboy),
	application:start(public_key),
	application:start(ssl),
	Dir = ?config(priv_dir, Config) ++ "/static",
	ct_helper:create_static_dir(Dir),
	[{static_dir, Dir}|Config].

end_per_suite(Config) ->
	Dir = ?config(static_dir, Config),
	ct_helper:delete_static_dir(Dir),
	application:stop(ssl),
	application:stop(public_key),
	application:stop(cowboy),
	application:stop(ranch),
	application:stop(crypto),
	ok.

init_per_group(Name, Config) ->
	{_, Cert, Key} = ct_helper:make_certs(),
	Opts = [{cert, Cert}, {key, Key}],
	{ok, _} = cowboy:start_spdy(Name, 100, Opts ++ [{port, 0}], [
		{env, [{dispatch, init_dispatch(Config)}]}
	]),
	Port = ranch:get_port(Name),
	[{port, Port}|Config].

end_per_group(Name, _) ->
	cowboy:stop_listener(Name),
	ok.

%% Dispatch configuration.

init_dispatch(Config) ->
	cowboy_router:compile([
		{"localhost", [
			{"/static/[...]", cowboy_static,
				[{directory, ?config(static_dir, Config)},
				 {mimetypes, [{<<".css">>, [<<"text/css">>]}]}]},
			{"/chunked", http_chunked, []},
			{"/", http_handler, []}
		]}
	]).

%% Convenience functions.

quick_get(Host, Path, ExpectedFlags, Config) ->
	{_, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = ssl:connect("localhost", Port, [
		binary, {active, false},
		{client_preferred_next_protocols, client, [<<"spdy/3">>]}
	]),
	{Zdef, Zinf} = zlib_init(),
	ReqHeaders = headers_encode(Zdef, [
		{<<":method">>, <<"GET">>},
		{<<":path">>, list_to_binary(Path)},
		{<<":version">>, <<"HTTP/1.1">>},
		{<<":host">>, list_to_binary(Host)},
		{<<":scheme">>, <<"https">>}
	]),
	ReqLength = 10 + byte_size(ReqHeaders),
	StreamID = 1,
	ok = ssl:send(Socket, << 1:1, 3:15, 1:16, 0:8, ReqLength:24,
		0:1, StreamID:31, 0:1, 0:31, 0:3, 0:5, 0:8, ReqHeaders/binary >>),
	{ok, Packet} = ssl:recv(Socket, 0, 1000),
	<< 1:1, 3:15, 2:16, Flags:8, RespLength:24,
		_:1, StreamID:31, RespHeaders/bits >> = Packet,
	Flags = ExpectedFlags,
	RespLength = 4 + byte_size(RespHeaders),
	[<< NbHeaders:32, Rest/bits >>] = try
		zlib:inflate(Zinf, RespHeaders)
	catch _:_ ->
		ok = zlib:inflateSetDictionary(Zinf, ?ZDICT),
		zlib:inflate(Zinf, <<>>)
	end,
	RespHeaders2 = headers_decode(Zinf, Rest, []),
	NbHeaders = length(RespHeaders2),
	{_, << Status:3/binary, _/bits >>}
		= lists:keyfind(<<":status">>, 1, RespHeaders2),
	StatusCode = list_to_integer(binary_to_list(Status)),
	ok = ssl:close(Socket),
	zlib_terminate(Zdef, Zinf),
	{StatusCode, RespHeaders2}.

zlib_init() ->
	Zdef = zlib:open(),
	ok = zlib:deflateInit(Zdef),
	_ = zlib:deflateSetDictionary(Zdef, ?ZDICT),
	Zinf = zlib:open(),
	ok = zlib:inflateInit(Zinf),
	{Zdef, Zinf}.

zlib_terminate(Zdef, Zinf) ->
	zlib:close(Zdef),
	zlib:close(Zinf).

headers_encode(Zdef, Headers) ->
	NbHeaders = length(Headers),
	Headers2 = << << (begin
		SizeN = byte_size(N),
		SizeV = byte_size(V),
		<< SizeN:32, N/binary, SizeV:32, V/binary >>
	end)/binary >> || {N, V} <- Headers >>,
	Headers3 = << NbHeaders:32, Headers2/binary >>,
	iolist_to_binary(zlib:deflate(Zdef, Headers3, full)).

headers_decode(_, <<>>, Acc) ->
	lists:reverse(Acc);
headers_decode(Zinf, << SizeN:32, Rest/bits >>, Acc) ->
	<< Name:SizeN/binary, SizeV:32, Rest2/bits >> = Rest,
	<< Value:SizeV/binary, Rest3/bits >> = Rest2,
	headers_decode(Zinf, Rest3, [{Name, Value}|Acc]).

%% Tests.

check_status(Config) ->
	Tests = [
		{200, nofin, "localhost", "/"},
		{200, nofin, "localhost", "/chunked"},
		{200, nofin, "localhost", "/static/style.css"},
		{400, fin, "bad-host", "/"},
		{400, fin, "localhost", "bad-path"},
		{404, fin, "localhost", "/this/path/does/not/exist"}
	],
	_ = [{Status, Fin, Host, Path} = begin
		RespFlags = case Fin of fin -> 1; nofin -> 0 end,
		{Ret, _} = quick_get(Host, Path, RespFlags, Config),
		{Ret, Fin, Host, Path}
	end || {Status, Fin, Host, Path} <- Tests].
