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

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([check_status/1]).
-export([echo_body/1]).
-export([echo_body_multi/1]).

%% ct.

all() ->
	[{group, spdy}].

groups() ->
	[{spdy, [], [
		check_status,
		echo_body,
		echo_body_multi
	]}].

init_per_suite(Config) ->
	application:start(crypto),
	application:start(cowlib),
	application:start(ranch),
	application:start(cowboy),
	application:start(asn1),
	application:start(public_key),
	application:start(ssl),
	application:start(gun),
	Dir = ?config(priv_dir, Config) ++ "/static",
	ct_helper:create_static_dir(Dir),
	[{static_dir, Dir}|Config].

end_per_suite(Config) ->
	Dir = ?config(static_dir, Config),
	ct_helper:delete_static_dir(Dir),
	application:stop(gun),
	application:stop(ssl),
	application:stop(public_key),
	application:stop(asn1),
	application:stop(cowboy),
	application:stop(ranch),
	application:stop(cowlib),
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
				{dir, ?config(static_dir, Config)}},
			{"/echo/body", http_echo_body, []},
			{"/chunked", http_chunked, []},
			{"/", http_handler, []}
		]}
	]).

%% Convenience functions.

quick_get(Pid, Host, Path) ->
	MRef = monitor(process, Pid),
	StreamRef = gun:get(Pid, Path, [{":host", Host}]),
	receive
		{'DOWN', MRef, _, _, Reason} ->
			error(Reason);
		{gun_response, Pid, StreamRef, IsFin,
				<< Status:3/binary, _/bits >>, Headers} ->
			{IsFin, binary_to_integer(Status), Headers}
	after 1000 ->
		error(timeout)
	end.

%% Tests.

check_status(Config) ->
	{_, Port} = lists:keyfind(port, 1, Config),
	{ok, Pid} = gun:open("localhost", Port),
	Tests = [
		{200, nofin, "localhost", "/"},
		{200, nofin, "localhost", "/chunked"},
		{200, nofin, "localhost", "/static/style.css"},
		{400, fin, "bad-host", "/"},
		{400, fin, "localhost", "bad-path"},
		{404, fin, "localhost", "/this/path/does/not/exist"}
	],
	_ = [{Status, Fin, Host, Path} = begin
		{IsFin, Ret, _} = quick_get(Pid, Host, Path),
		{Ret, IsFin, Host, Path}
	end || {Status, Fin, Host, Path} <- Tests],
	gun:close(Pid).

echo_body(Config) ->
	{_, Port} = lists:keyfind(port, 1, Config),
	{ok, Pid} = gun:open("localhost", Port),
	MRef = monitor(process, Pid),
	Body = << 0:800000 >>,
	StreamRef = gun:post(Pid, "/echo/body", [
		{<<"content-length">>, integer_to_list(byte_size(Body))},
		{<<"content-type">>, "application/octet-stream"}
	], Body),
	receive
		{'DOWN', MRef, _, _, Reason} ->
			error(Reason);
		{gun_response, Pid, StreamRef, nofin, << "200", _/bits >>, _} ->
			ok
	after 1000 ->
		error(response_timeout)
	end,
	receive
		{'DOWN', MRef, _, _, Reason2} ->
			error({gun_data, Reason2});
		{gun_data, Pid, StreamRef, fin, Body} ->
			ok
	after 1000 ->
		error(data_timeout)
	end,
	gun:close(Pid).

echo_body_multi(Config) ->
	{_, Port} = lists:keyfind(port, 1, Config),
	{ok, Pid} = gun:open("localhost", Port),
	MRef = monitor(process, Pid),
	BodyChunk = << 0:80000 >>,
	StreamRef = gun:post(Pid, "/echo/body", [
		{<<"content-length">>, integer_to_list(byte_size(BodyChunk) * 10)},
		{<<"content-type">>, "application/octet-stream"}
	]),
	_ = [gun:data(Pid, StreamRef, nofin, BodyChunk) || _ <- lists:seq(1, 9)],
	gun:data(Pid, StreamRef, fin, BodyChunk),
	receive
		{'DOWN', MRef, _, _, Reason} ->
			error(Reason);
		{gun_response, Pid, StreamRef, nofin, << "200", _/bits >>, _} ->
			ok
	after 1000 ->
		error(response_timeout)
	end,
	receive
		{'DOWN', MRef, _, _, Reason2} ->
			error({gun_data, Reason2});
		{gun_data, Pid, StreamRef, fin, << 0:800000 >>} ->
			ok
	after 1000 ->
		error(data_timeout)
	end,
	gun:close(Pid).
