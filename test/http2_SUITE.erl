%% Copyright (c) 2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(http2_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

all() -> [{group, clear}].

groups() -> [{clear, [parallel], ct_helper:all(?MODULE)}].

init_routes(_) -> [
	{"localhost", [
		{"/", hello_h, []},
		{"/resp_iolist_body", resp_iolist_body_h, []}
	]}
].

%% Do a prior knowledge handshake (function copied from rfc7540_SUITE).
do_handshake(Config) ->
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a valid preface.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% Send the SETTINGS ack.
	ok = gen_tcp:send(Socket, cow_http2:settings_ack()),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, Socket}.

inactivity_timeout(Config) ->
	doc("Terminate when the inactivity timeout is reached"),
	ProtoOpts = #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))},
		inactivity_timeout => 1000
	},
	{ok, _} = cowboy:start_clear(inactivity_timeout, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(inactivity_timeout),
	{ok, Socket} = do_handshake([{port, Port}|Config]),
	receive after 1000 -> ok end,
	%% Receive a GOAWAY frame back with an INTERNAL_ERROR.
	{ok, << _:24, 7:8, _:72, 2:32 >>} = gen_tcp:recv(Socket, 17, 1000),
	ok.

resp_iolist_body(Config) ->
	doc("Regression test when response bodies are iolists that "
		"include improper lists, empty lists and empty binaries. "
		"The original issue failed to split the body into frames properly."),
	ProtoOpts = #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))}
	},
	{ok, _} = cowboy:start_clear(resp_iolist_body, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(resp_iolist_body),
	ConnPid = gun_open([{type, tcp}, {protocol, http2}, {port, Port}|Config]),
	Ref = gun:get(ConnPid, "/resp_iolist_body"),
	{response, nofin, 200, RespHeaders} = gun:await(ConnPid, Ref),
	{_, BinLen} = lists:keyfind(<<"content-length">>, 1, RespHeaders),
	Len = binary_to_integer(BinLen),
	{ok, RespBody} = gun:await_body(ConnPid, Ref),
	Len = iolist_size(RespBody),
	gun:close(ConnPid).
