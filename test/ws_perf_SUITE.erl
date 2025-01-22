%% Copyright (c) 2024, Loïc Hoguin <essen@ninenines.eu>
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
			{"/ws_echo", ws_echo, []}
		]}
	]).

%% Support functions for testing using Gun.

do_gun_open_ws(Config) ->
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
	StreamRef = gun:ws_upgrade(ConnPid, "/ws_echo"),
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

one_00064KiB(Config) ->
	doc("Send and receive a 64KiB frame."),
	do_full(Config, one, 1, 64 * 1024).

one_00256KiB(Config) ->
	doc("Send and receive a 256KiB frame."),
	do_full(Config, one, 1, 256 * 1024).

one_01024KiB(Config) ->
	doc("Send and receive a 1024KiB frame."),
	do_full(Config, one, 1, 1024 * 1024).

one_04096KiB(Config) ->
	doc("Send and receive a 4096KiB frame."),
	do_full(Config, one, 1, 4096 * 1024).

%% Minus one because frames can only get so big.
one_16384KiB(Config) ->
	doc("Send and receive a 16384KiB - 1 frame."),
	do_full(Config, one, 1, 16384 * 1024 - 1).

repeat_00000B(Config) ->
	doc("Send and receive a 0B frame 1000 times."),
	do_full(Config, repeat, 1000, 0).

repeat_00256B(Config) ->
	doc("Send and receive a 256B frame 1000 times."),
	do_full(Config, repeat, 1000, 256).

repeat_01024B(Config) ->
	doc("Send and receive a 1024B frame 1000 times."),
	do_full(Config, repeat, 1000, 1024).

repeat_04096B(Config) ->
	doc("Send and receive a 4096B frame 1000 times."),
	do_full(Config, repeat, 1000, 4096).

repeat_16384B(Config) ->
	doc("Send and receive a 16384B frame 1000 times."),
	do_full(Config, repeat, 1000, 16384).

%repeat_16384B_10K(Config) ->
%	doc("Send and receive a 16384B frame 10000 times."),
%	do_full(Config, repeat, 10000, 16384).

do_full(Config, What, Num, FrameSize) ->
	{ok, ConnPid, StreamRef} = do_gun_open_ws(Config),
	FrameType = config(frame_type, Config),
	FrameData = case FrameType of
		text -> do_text_data(Config, FrameSize);
		binary -> rand:bytes(FrameSize)
	end,
	%% Heat up the processes before doing the real run.
%	do_full1(ConnPid, StreamRef, Num, FrameType, FrameData),
	{Time, _} = timer:tc(?MODULE, do_full1, [ConnPid, StreamRef, Num, FrameType, FrameData]),
	do_log("~-6s ~-6s ~6s: ~8bµs", [What, FrameType, do_format_size(FrameSize), Time]),
	gun:ws_send(ConnPid, StreamRef, close),
	{ok, close} = receive_ws(ConnPid, StreamRef),
	gun_down(ConnPid).

do_full1(_, _, 0, _, _) ->
	ok;
do_full1(ConnPid, StreamRef, Num, FrameType, FrameData) ->
	gun:ws_send(ConnPid, StreamRef, {FrameType, FrameData}),
	{ok, {FrameType, FrameData}} = receive_ws(ConnPid, StreamRef),
	do_full1(ConnPid, StreamRef, Num - 1, FrameType, FrameData).

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
