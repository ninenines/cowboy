%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
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

-module(ws_perf_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/2]).
-import(cowboy_test, [gun_down/1]).

%% ct.

all() ->
	[{group, binary}, {group, ascii}, {group, mixed}, {group, japanese}].

groups() ->
	CommonGroups = cowboy_test:common_groups(ct_helper:all(?MODULE), no_parallel),
	SubGroups = [G || G = {GN, _, _} <- CommonGroups,
		GN =:= http orelse GN =:= h2c orelse GN =:= http_compress orelse GN =:= h2c_compress],
	[
		{binary, [], SubGroups},
		{ascii, [], SubGroups},
		{mixed, [], SubGroups},
		{japanese, [], SubGroups}
	].

init_per_suite(Config) ->
	%% Optionally enable `perf` for the current node.
%	spawn(fun() -> ct:pal(os:cmd("perf record -g -F 9999 -o /tmp/ws_perf.data -p " ++ os:getpid() ++ " -- sleep 60")) end),
	Config.

end_per_suite(_Config) ->
	ok.

init_per_group(Name, Config) when Name =:= http; Name =:= http_compress ->
	init_info(Name, Config),
	cowboy_test:init_common_groups(Name, Config, ?MODULE);
init_per_group(Name, Config) when Name =:= h2c; Name =:= h2c_compress ->
	init_info(Name, Config),
	{Flavor, Opts} = case Name of
		h2c -> {vanilla, #{}};
		h2c_compress -> {compress, #{stream_handlers => [cowboy_compress_h, cowboy_stream_h]}}
	end,
	Config1 = cowboy_test:init_http(Name, Opts#{
		connection_window_margin_size => 64*1024,
		enable_connect_protocol => true,
		env => #{dispatch => init_dispatch(Config)},
		max_frame_size_sent => 64*1024,
		max_frame_size_received => 16384 * 1024 - 1,
		max_received_frame_rate => {10_000_000, 1},
		stream_window_data_threshold => 1024,
		stream_window_margin_size => 64*1024
	}, [{flavor, Flavor}|Config]),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2});
init_per_group(ascii, Config) ->
	init_text_data("ascii.txt", Config);
init_per_group(mixed, Config) ->
	init_text_data("grok_segond.txt", Config);
init_per_group(japanese, Config) ->
	init_text_data("japanese.txt", Config);
init_per_group(binary, Config) ->
	[{frame_type, binary}|Config].

init_info(Name, Config) ->
	DataInfo = case config(frame_type, Config) of
		text -> config(text_data_filename, Config);
		binary -> binary
	end,
	ConnInfo = case Name of
		http -> "cleartext HTTP/1.1";
		http_compress -> "cleartext HTTP/1.1 with compression";
		h2c -> "cleartext HTTP/2";
		h2c_compress -> "cleartext HTTP/2 with compression"
	end,
	ct:pal("Websocket over ~s (~s)", [ConnInfo, DataInfo]).

init_text_data(Filename, Config) ->
	{ok, Text} = file:read_file(filename:join(config(data_dir, Config), Filename)),
	[
		{frame_type, text},
		{text_data, Text},
		{text_data_filename, Filename}
	|Config].

end_per_group(Name, _Config) ->
	cowboy_test:stop_group(Name).

%% Dispatch configuration.

init_dispatch(_Config) ->
	cowboy_router:compile([
		{"localhost", [
			{"/ws_echo", ws_echo, []},
			{"/ws_ignore", ws_ignore, []}
		]}
	]).

%% Support functions for testing using Gun.

do_gun_open_ws(Path, Config) ->
	ConnPid = gun_open(Config, #{
		http2_opts => #{
			connection_window_margin_size => 64*1024,
			max_frame_size_sent => 64*1024,
			max_frame_size_received => 16384 * 1024 - 1,
			notify_settings_changed => true,
			stream_window_data_threshold => 1024,
			stream_window_margin_size => 64*1024
		},
		tcp_opts => [{nodelay, true}],
		ws_opts => #{compress => config(flavor, Config) =:= compress}
	}),
	case config(protocol, Config) of
		http -> ok;
		http2 ->
			{notify, settings_changed, #{enable_connect_protocol := true}}
				= gun:await(ConnPid, undefined) %% @todo Maybe have a gun:await/1?
	end,
	StreamRef = gun:ws_upgrade(ConnPid, Path),
	receive
		{gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _} ->
			{ok, ConnPid, StreamRef};
		{gun_response, ConnPid, _, _, Status, Headers} ->
			exit({ws_upgrade_failed, Status, Headers});
		{gun_error, ConnPid, StreamRef, Reason} ->
			exit({ws_upgrade_failed, Reason})
	after 1000 ->
		error(timeout)
	end.

receive_ws(ConnPid, StreamRef) ->
	receive
		{gun_ws, ConnPid, StreamRef, Frame} ->
			{ok, Frame}
	after 30000 ->
		{error, timeout}
	end.

%% Tests.

echo_1_00064KiB(Config) ->
	doc("Send and receive a 64KiB frame."),
	do_echo(Config, echo_1, 1, 64 * 1024).

echo_1_00256KiB(Config) ->
	doc("Send and receive a 256KiB frame."),
	do_echo(Config, echo_1, 1, 256 * 1024).

echo_1_01024KiB(Config) ->
	doc("Send and receive a 1024KiB frame."),
	do_echo(Config, echo_1, 1, 1024 * 1024).

echo_1_04096KiB(Config) ->
	doc("Send and receive a 4096KiB frame."),
	do_echo(Config, echo_1, 1, 4096 * 1024).

%% Minus one because frames can only get so big.
echo_1_16384KiB(Config) ->
	doc("Send and receive a 16384KiB - 1 frame."),
	do_echo(Config, echo_1, 1, 16384 * 1024 - 1).

echo_N_00000B(Config) ->
	doc("Send and receive a 0B frame 1000 times."),
	do_echo(Config, echo_N, 1000, 0).

echo_N_00256B(Config) ->
	doc("Send and receive a 256B frame 1000 times."),
	do_echo(Config, echo_N, 1000, 256).

echo_N_01024B(Config) ->
	doc("Send and receive a 1024B frame 1000 times."),
	do_echo(Config, echo_N, 1000, 1024).

echo_N_04096B(Config) ->
	doc("Send and receive a 4096B frame 1000 times."),
	do_echo(Config, echo_N, 1000, 4096).

echo_N_16384B(Config) ->
	doc("Send and receive a 16384B frame 1000 times."),
	do_echo(Config, echo_N, 1000, 16384).

%echo_N_16384B_10K(Config) ->
%	doc("Send and receive a 16384B frame 10000 times."),
%	do_echo(Config, echo_N, 10000, 16384).

do_echo(Config, What, Num, FrameSize) ->
	{ok, ConnPid, StreamRef} = do_gun_open_ws("/ws_echo", Config),
	FrameType = config(frame_type, Config),
	FrameData = case FrameType of
		text -> do_text_data(Config, FrameSize);
		binary -> rand:bytes(FrameSize)
	end,
	%% Heat up the processes before doing the real run.
%	do_echo_loop(ConnPid, StreamRef, Num, FrameType, FrameData),
	{Time, _} = timer:tc(?MODULE, do_echo_loop, [ConnPid, StreamRef, Num, FrameType, FrameData]),
	do_log("~-6s ~-6s ~6s: ~8bµs", [What, FrameType, do_format_size(FrameSize), Time]),
	gun:ws_send(ConnPid, StreamRef, close),
	{ok, close} = receive_ws(ConnPid, StreamRef),
	gun_down(ConnPid).

do_echo_loop(_, _, 0, _, _) ->
	ok;
do_echo_loop(ConnPid, StreamRef, Num, FrameType, FrameData) ->
	gun:ws_send(ConnPid, StreamRef, {FrameType, FrameData}),
	{ok, {FrameType, FrameData}} = receive_ws(ConnPid, StreamRef),
	do_echo_loop(ConnPid, StreamRef, Num - 1, FrameType, FrameData).

send_1_00064KiB(Config) ->
	doc("Send a 64KiB frame."),
	do_send(Config, send_1, 1, 64 * 1024).

send_1_00256KiB(Config) ->
	doc("Send a 256KiB frame."),
	do_send(Config, send_1, 1, 256 * 1024).

send_1_01024KiB(Config) ->
	doc("Send a 1024KiB frame."),
	do_send(Config, send_1, 1, 1024 * 1024).

send_1_04096KiB(Config) ->
	doc("Send a 4096KiB frame."),
	do_send(Config, send_1, 1, 4096 * 1024).

%% Minus one because frames can only get so big.
send_1_16384KiB(Config) ->
	doc("Send a 16384KiB - 1 frame."),
	do_send(Config, send_1, 1, 16384 * 1024 - 1).

send_N_00000B(Config) ->
	doc("Send a 0B frame 10000 times."),
	do_send(Config, send_N, 10000, 0).

send_N_00256B(Config) ->
	doc("Send a 256B frame 10000 times."),
	do_send(Config, send_N, 10000, 256).

send_N_01024B(Config) ->
	doc("Send a 1024B frame 10000 times."),
	do_send(Config, send_N, 10000, 1024).

send_N_04096B(Config) ->
	doc("Send a 4096B frame 10000 times."),
	do_send(Config, send_N, 10000, 4096).

send_N_16384B(Config) ->
	doc("Send a 16384B frame 10000 times."),
	do_send(Config, send_N, 10000, 16384).

%send_N_16384B_10K(Config) ->
%	doc("Send and receive a 16384B frame 10000 times."),
%	do_send(Config, send_N, 10000, 16384).

do_send(Config, What, Num, FrameSize) ->
	{ok, ConnPid, StreamRef} = do_gun_open_ws("/ws_ignore", Config),
	FrameType = config(frame_type, Config),
	FrameData = case FrameType of
		text -> do_text_data(Config, FrameSize);
		binary -> rand:bytes(FrameSize)
	end,
	%% Heat up the processes before doing the real run.
%	do_send_loop(ConnPid, StreamRef, Num, FrameType, FrameData),
	{Time, _} = timer:tc(?MODULE, do_send_loop, [ConnPid, StreamRef, Num, FrameType, FrameData]),
	do_log("~-6s ~-6s ~6s: ~8bµs", [What, FrameType, do_format_size(FrameSize), Time]),
	gun:ws_send(ConnPid, StreamRef, close),
	{ok, close} = receive_ws(ConnPid, StreamRef),
	gun_down(ConnPid).

do_send_loop(ConnPid, StreamRef, 0, _, _) ->
	gun:ws_send(ConnPid, StreamRef, {text, <<"CHECK">>}),
	{ok, {text, <<"CHECK">>}} = receive_ws(ConnPid, StreamRef),
	ok;
do_send_loop(ConnPid, StreamRef, Num, FrameType, FrameData) ->
	gun:ws_send(ConnPid, StreamRef, {FrameType, FrameData}),
	do_send_loop(ConnPid, StreamRef, Num - 1, FrameType, FrameData).

%% Internal.

do_text_data(Config, FrameSize) ->
	do_text_data1(config(text_data, Config), FrameSize).

do_text_data1(LargeText, FrameSize) when byte_size(LargeText) >= FrameSize ->
	binary:part(LargeText, 0, FrameSize);
do_text_data1(LargeText, FrameSize) ->
	do_text_data1(<<LargeText/binary, LargeText/binary>>, FrameSize).

do_format_size(Size) when Size < 1024 ->
	integer_to_list(Size) ++ "B";
do_format_size(Size) when Size < (1024*1024) ->
	integer_to_list(Size div 1024) ++ "KiB";
do_format_size(Size) ->
	integer_to_list(Size div (1024*1024)) ++ "MiB".

do_log(Str, Args) ->
	ct:log(Str, Args),
	io:format(ct_default_gl, Str ++ "~n", Args).
