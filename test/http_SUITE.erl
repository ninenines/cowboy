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
-export([http_200/1, http_404/1]). %% Common tests for http and https.

%% ct.

all() ->
	[{group, http}, {group, https}].

groups() ->
	BaseTests = [http_200, http_404],
	[{http, [], BaseTests},
	{https, [], BaseTests}].

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
	[{scheme, "https"}, {port, Port}|Config].

end_per_group(http, _Config) ->
	cowboy:stop_listener(http),
	ok;
end_per_group(https, _Config) ->
	cowboy:stop_listener(https),
	application:stop(ssl),
	application:stop(public_key),
	application:stop(crypto),
	ok.

%% Dispatch configuration.

init_http_dispatch() ->
	[
		{["localhost"], [{[], http_handler, []}]}
	].

init_https_dispatch() ->
	init_http_dispatch().

%% Common tests for http and https.

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
