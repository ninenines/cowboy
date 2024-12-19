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
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [gun_down/1]).

%% ct.

all() ->
	ct_helper:all(?MODULE).

init_per_suite(Config0) ->
	Config = cowboy_test:init_http(?MODULE, #{
		env => #{dispatch => init_dispatch()}
	}, Config0),
	{ok, LargeText} = file:read_file(filename:join(config(data_dir, Config), "grok_segond.txt")),
	[{large_text, LargeText}|Config].

end_per_suite(_Config) ->
	ok.

%% Support functions for testing using Gun.

do_gun_open_ws(Config) ->
	ConnPid = gun_open(Config),
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
	after 1000 ->
		{error, timeout}
	end.

%% Dispatch configuration.

init_dispatch() ->
	cowboy_router:compile([
		{"localhost", [
			{"/ws_echo", ws_echo, []}
		]}
	]).

%% Tests.

one_binary_00064KiB(Config) ->
	doc("Send and receive a 64KiB binary frame."),
	do_full(Config, one, 1, binary, 64 * 1024).

one_binary_00256KiB(Config) ->
	doc("Send and receive a 256KiB binary frame."),
	do_full(Config, one, 1, binary, 256 * 1024).

one_binary_01024KiB(Config) ->
	doc("Send and receive a 1024KiB binary frame."),
	do_full(Config, one, 1, binary, 1024 * 1024).

one_binary_04096KiB(Config) ->
	doc("Send and receive a 4096KiB binary frame."),
	do_full(Config, one, 1, binary, 4096 * 1024).

one_binary_16384KiB(Config) ->
	doc("Send and receive a 16384KiB binary frame."),
	do_full(Config, one, 1, binary, 16384 * 1024).

one_text_00064KiB(Config) ->
	doc("Send and receive a 64KiB text frame."),
	do_full(Config, one, 1, text, 64 * 1024).

one_text_00256KiB(Config) ->
	doc("Send and receive a 256KiB text frame."),
	do_full(Config, one, 1, text, 256 * 1024).

one_text_01024KiB(Config) ->
	doc("Send and receive a 1024KiB text frame."),
	do_full(Config, one, 1, text, 1024 * 1024).

one_text_04096KiB(Config) ->
	doc("Send and receive a 4096KiB text frame."),
	do_full(Config, one, 1, text, 4096 * 1024).

one_text_16384KiB(Config) ->
	doc("Send and receive a 16384KiB text frame."),
	do_full(Config, one, 1, text, 16384 * 1024).

repeat_binary_00000B(Config) ->
	doc("Send and receive a 0B binary frame 1000 times."),
	do_full(Config, repeat, 1000, binary, 0).

repeat_binary_00256B(Config) ->
	doc("Send and receive a 256B binary frame 1000 times."),
	do_full(Config, repeat, 1000, binary, 256).

repeat_binary_01024B(Config) ->
	doc("Send and receive a 1024B binary frame 1000 times."),
	do_full(Config, repeat, 1000, binary, 1024).

repeat_binary_04096B(Config) ->
	doc("Send and receive a 4096B binary frame 1000 times."),
	do_full(Config, repeat, 1000, binary, 4096).

repeat_binary_16384B(Config) ->
	doc("Send and receive a 16384B binary frame 1000 times."),
	do_full(Config, repeat, 1000, binary, 16384).

repeat_text_00000B(Config) ->
	doc("Send and receive a 0B text frame 1000 times."),
	do_full(Config, repeat, 1000, text, 0).

repeat_text_00256B(Config) ->
	doc("Send and receive a 256B text frame 1000 times."),
	do_full(Config, repeat, 1000, text, 256).

repeat_text_01024B(Config) ->
	doc("Send and receive a 1024B text frame 1000 times."),
	do_full(Config, repeat, 1000, text, 1024).

repeat_text_04096B(Config) ->
	doc("Send and receive a 4096B text frame 1000 times."),
	do_full(Config, repeat, 1000, text, 4096).

repeat_text_16384B(Config) ->
	doc("Send and receive a 16384B text frame 1000 times."),
	do_full(Config, repeat, 1000, text, 16384).

do_full(Config, What, Num, FrameType, FrameSize) ->
	{ok, ConnPid, StreamRef} = do_gun_open_ws(Config),
	FrameData = case FrameType of
		text -> do_text_data(Config, FrameSize);
		binary -> rand:bytes(FrameSize)
	end,
	{Time, _} = timer:tc(?MODULE, do_full1, [ConnPid, StreamRef, Num, FrameType, FrameData]),
	do_log("~-6s ~-6s ~6s: ~7bµs", [What, FrameType, do_format_size(FrameSize), Time]),
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
	do_text_data1(config(large_text, Config), FrameSize).

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
