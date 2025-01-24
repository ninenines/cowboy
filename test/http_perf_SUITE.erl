%% Copyright (c) 2025, Loïc Hoguin <essen@ninenines.eu>
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

-module(http_perf_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

%% ct.

all() ->
	%% @todo Enable HTTP/3 for this test suite.
	cowboy_test:common_all() -- [{group, h3}, {group, h3_compress}].

groups() ->
	cowboy_test:common_groups(ct_helper:all(?MODULE), no_parallel).

init_per_suite(Config) ->
	do_log("", []),
	%% Optionally enable `perf` for the current node.
%	spawn(fun() -> ct:pal(os:cmd("perf record -g -F 9999 -o /tmp/ws_perf.data -p " ++ os:getpid() ++ " -- sleep 60")) end),
	Config.

end_per_suite(_) ->
	ok.

init_per_group(Name, Config) ->
	[{group, Name}|cowboy_test:init_common_groups(Name, Config, ?MODULE, #{
		%% HTTP/1.1
		max_keepalive => infinity,
		%% HTTP/2
		max_received_frame_rate => {10_000_000, 1}
	})].

end_per_group(Name, _) ->
	do_log("", []),
	cowboy_test:stop_group(Name).

%% Routes.

init_dispatch(_) ->
	cowboy_router:compile([{'_', [
		{"/", hello_h, []}
	]}]).

%% Tests.

stream_h_hello_1(Config) ->
	doc("Stream handler Hello World; 10K requests per 1 client."),
	do_stream_h_hello(Config, 1).

stream_h_hello_10(Config) ->
	doc("Stream handler Hello World; 10K requests per 10 clients."),
	do_stream_h_hello(Config, 10).

do_stream_h_hello(Config, NumClients) ->
	Ref = config(ref, Config),
	ProtoOpts = ranch:get_protocol_options(Ref),
	StreamHandlers = case ProtoOpts of
		#{stream_handlers := _} -> [cowboy_compress_h, stream_hello_h];
		_ -> [stream_hello_h]
	end,
	ranch:set_protocol_options(Ref, ProtoOpts#{
		env => #{},
		stream_handlers => StreamHandlers
	}),
	do_bench_get(?FUNCTION_NAME, "/", #{}, NumClients, 10000, Config),
	ranch:set_protocol_options(Ref, ProtoOpts).

plain_h_hello_1(Config) ->
	doc("Plain HTTP handler Hello World; 10K requests per 1 client."),
	do_bench_get(?FUNCTION_NAME, "/", #{}, 1, 10000, Config).

plain_h_hello_10(Config) ->
	doc("Plain HTTP handler Hello World; 10K requests per 10 clients."),
	do_bench_get(?FUNCTION_NAME, "/", #{}, 10, 10000, Config).

%% Internal.

do_bench_get(What, Path, Headers, NumClients, NumRuns, Config) ->
	Clients = [spawn_link(?MODULE, do_bench_proc, [self(), What, Path, Headers, NumRuns, Config])
		|| _ <- lists:seq(1, NumClients)],
	_ = [receive {What, ready} -> ok end || _ <- Clients],
	{Time, _} = timer:tc(?MODULE, do_bench_get1, [What, Clients]),
	do_log("~32s: ~8bµs ~8.1freqs/s", [
		[atom_to_list(config(group, Config)), $., atom_to_list(What)],
		Time,
		(NumClients * NumRuns) / Time * 1_000_000]),
	ok.

do_bench_get1(What, Clients) ->
	_ = [ClientPid ! {What, go} || ClientPid <- Clients],
	_ = [receive {What, done} -> ok end || _ <- Clients],
	ok.

do_bench_proc(Parent, What, Path, Headers0, NumRuns, Config) ->
	ConnPid = gun_open(Config),
	Headers = Headers0#{<<"accept-encoding">> => <<"gzip">>},
	Parent ! {What, ready},
	receive {What, go} -> ok end,
	do_bench_run(ConnPid, Path, Headers, NumRuns),
	Parent ! {What, done},
	gun:close(ConnPid).

do_bench_run(_, _, _, 0) ->
	ok;
do_bench_run(ConnPid, Path, Headers, Num) ->
	Ref = gun:request(ConnPid, <<"GET">>, Path, Headers, <<>>),
	{response, IsFin, 200, _RespHeaders} = gun:await(ConnPid, Ref, infinity),
	{ok, _} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref, infinity);
		fin -> {ok, <<>>}
	end,
	do_bench_run(ConnPid, Path, Headers, Num - 1).

do_log(Str, Args) ->
	ct:log(Str, Args),
	io:format(ct_default_gl, Str ++ "~n", Args).
