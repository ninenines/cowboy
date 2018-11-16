%% Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(ws_handler_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [gun_down/1]).

%% ct.

all() ->
	[{group, ws}, {group, ws_hibernate}].

%% @todo Test against HTTP/2 too.
groups() ->
	AllTests = ct_helper:all(?MODULE),
	[{ws, [parallel], AllTests}, {ws_hibernate, [parallel], AllTests}].

init_per_group(Name, Config) ->
	cowboy_test:init_http(Name, #{
		env => #{dispatch => init_dispatch(Name)}
	}, Config).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

%% Dispatch configuration.

init_dispatch(Name) ->
	RunOrHibernate = case Name of
		ws -> run;
		ws_hibernate -> hibernate
	end,
	cowboy_router:compile([{'_', [
		{"/init", ws_init_commands_h, RunOrHibernate},
		{"/handle", ws_handle_commands_h, RunOrHibernate},
		{"/info", ws_info_commands_h, RunOrHibernate},
		{"/active", ws_active_commands_h, RunOrHibernate},
		{"/deflate", ws_deflate_commands_h, RunOrHibernate},
		{"/set_options", ws_set_options_commands_h, RunOrHibernate}
	]}]).

%% Support functions for testing using Gun.

gun_open_ws(Config, Path, Commands) ->
	ConnPid = gun_open(Config),
	StreamRef = gun:ws_upgrade(ConnPid, Path, [
		{<<"x-commands">>, base64:encode(term_to_binary(Commands))}
	]),
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

ensure_handle_is_called(ConnPid, "/handle") ->
	gun:ws_send(ConnPid, {text, <<"Necessary to trigger websocket_handle/2.">>});
ensure_handle_is_called(_, _) ->
	ok.

%% Tests.

websocket_init_nothing(Config) ->
	doc("Nothing happens when websocket_init/1 returns no commands."),
	do_nothing(Config, "/init").

websocket_handle_nothing(Config) ->
	doc("Nothing happens when websocket_handle/2 returns no commands."),
	do_nothing(Config, "/handle").

websocket_info_nothing(Config) ->
	doc("Nothing happens when websocket_info/2 returns no commands."),
	do_nothing(Config, "/info").

do_nothing(Config, Path) ->
	{ok, ConnPid, StreamRef} = gun_open_ws(Config, Path, []),
	ensure_handle_is_called(ConnPid, Path),
	{error, timeout} = receive_ws(ConnPid, StreamRef),
	ok.

websocket_init_invalid(Config) ->
	doc("The connection must be closed when websocket_init/1 returns an invalid command."),
	do_invalid(Config, "/init").

websocket_handle_invalid(Config) ->
	doc("The connection must be closed when websocket_handle/2 returns an invalid command."),
	do_invalid(Config, "/init").

websocket_info_invalid(Config) ->
	doc("The connection must be closed when websocket_info/2 returns an invalid command."),
	do_invalid(Config, "/info").

do_invalid(Config, Path) ->
	{ok, ConnPid, _} = gun_open_ws(Config, Path, bad),
	ensure_handle_is_called(ConnPid, Path),
	gun_down(ConnPid).

websocket_init_one_frame(Config) ->
	doc("A single frame is received when websocket_init/1 returns it as a command."),
	do_one_frame(Config, "/init").

websocket_handle_one_frame(Config) ->
	doc("A single frame is received when websocket_handle/2 returns it as a command."),
	do_one_frame(Config, "/handle").

websocket_info_one_frame(Config) ->
	doc("A single frame is received when websocket_info/2 returns it as a command."),
	do_one_frame(Config, "/info").

do_one_frame(Config, Path) ->
	{ok, ConnPid, StreamRef} = gun_open_ws(Config, Path, [
		{text, <<"One frame!">>}
	]),
	ensure_handle_is_called(ConnPid, Path),
	{ok, {text, <<"One frame!">>}} = receive_ws(ConnPid, StreamRef),
	ok.

websocket_init_many_frames(Config) ->
	doc("Multiple frames are received when websocket_init/1 returns them as commands."),
	do_many_frames(Config, "/init").

websocket_handle_many_frames(Config) ->
	doc("Multiple frames are received when websocket_handle/2 returns them as commands."),
	do_many_frames(Config, "/handle").

websocket_info_many_frames(Config) ->
	doc("Multiple frames are received when websocket_info/2 returns them as commands."),
	do_many_frames(Config, "/info").

do_many_frames(Config, Path) ->
	{ok, ConnPid, StreamRef} = gun_open_ws(Config, Path, [
		{text, <<"One frame!">>},
		{binary, <<"Two frames!">>}
	]),
	ensure_handle_is_called(ConnPid, Path),
	{ok, {text, <<"One frame!">>}} = receive_ws(ConnPid, StreamRef),
	{ok, {binary, <<"Two frames!">>}} = receive_ws(ConnPid, StreamRef),
	ok.

websocket_init_close_frame(Config) ->
	doc("A single close frame is received when websocket_init/1 returns it as a command."),
	do_close_frame(Config, "/init").

websocket_handle_close_frame(Config) ->
	doc("A single close frame is received when websocket_handle/2 returns it as a command."),
	do_close_frame(Config, "/handle").

websocket_info_close_frame(Config) ->
	doc("A single close frame is received when websocket_info/2 returns it as a command."),
	do_close_frame(Config, "/info").

do_close_frame(Config, Path) ->
	{ok, ConnPid, StreamRef} = gun_open_ws(Config, Path, [close]),
	ensure_handle_is_called(ConnPid, Path),
	{ok, close} = receive_ws(ConnPid, StreamRef),
	gun_down(ConnPid).

websocket_init_many_frames_then_close_frame(Config) ->
	doc("Multiple frames are received followed by a close frame "
		"when websocket_init/1 returns them as commands."),
	do_many_frames_then_close_frame(Config, "/init").

websocket_handle_many_frames_then_close_frame(Config) ->
	doc("Multiple frames are received followed by a close frame "
		"when websocket_handle/2 returns them as commands."),
	do_many_frames_then_close_frame(Config, "/handle").

websocket_info_many_frames_then_close_frame(Config) ->
	doc("Multiple frames are received followed by a close frame "
		"when websocket_info/2 returns them as commands."),
	do_many_frames_then_close_frame(Config, "/info").

do_many_frames_then_close_frame(Config, Path) ->
	{ok, ConnPid, StreamRef} = gun_open_ws(Config, Path, [
		{text, <<"One frame!">>},
		{binary, <<"Two frames!">>},
		close
	]),
	ensure_handle_is_called(ConnPid, Path),
	{ok, {text, <<"One frame!">>}} = receive_ws(ConnPid, StreamRef),
	{ok, {binary, <<"Two frames!">>}} = receive_ws(ConnPid, StreamRef),
	{ok, close} = receive_ws(ConnPid, StreamRef),
	gun_down(ConnPid).

websocket_active_false(Config) ->
	doc("The {active, false} command stops receiving data from the socket. "
		"The {active, true} command reenables it."),
	{ok, ConnPid, StreamRef} = gun_open_ws(Config, "/active", []),
	gun:ws_send(ConnPid, {text, <<"Not received until the handler enables active again.">>}),
	{error, timeout} = receive_ws(ConnPid, StreamRef),
	{ok, {text, <<"Not received until the handler enables active again.">>}}
		= receive_ws(ConnPid, StreamRef),
	ok.

websocket_deflate_false(Config) ->
	doc("The {deflate, false} command temporarily disables compression. "
		"The {deflate, true} command reenables it."),
	%% We disable context takeover so that the compressed data
	%% does not change across all frames.
	{ok, Socket, Headers} = ws_SUITE:do_handshake("/deflate",
		"Sec-WebSocket-Extensions: permessage-deflate; server_no_context_takeover\r\n", Config),
	{_, "permessage-deflate; server_no_context_takeover"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers),
	%% The handler receives a compressed "Hello" frame and
	%% sends back a compressed or uncompressed echo intermittently.
	Mask = 16#11223344,
	CompressedHello = <<242, 72, 205, 201, 201, 7, 0>>,
	MaskedHello = ws_SUITE:do_mask(CompressedHello, Mask, <<>>),
	%% First echo is compressed.
	ok = gen_tcp:send(Socket, <<1:1, 1:1, 0:2, 1:4, 1:1, 7:7, Mask:32, MaskedHello/binary>>),
	{ok, <<1:1, 1:1, 0:2, 1:4, 0:1, 7:7, CompressedHello/binary>>} = gen_tcp:recv(Socket, 0, 6000),
	%% Second echo is not compressed when it is received back.
	ok = gen_tcp:send(Socket, <<1:1, 1:1, 0:2, 1:4, 1:1, 7:7, Mask:32, MaskedHello/binary>>),
	{ok, <<1:1, 0:3, 1:4, 0:1, 5:7, "Hello">>} = gen_tcp:recv(Socket, 0, 6000),
	%% Third echo is compressed again.
	ok = gen_tcp:send(Socket, <<1:1, 1:1, 0:2, 1:4, 1:1, 7:7, Mask:32, MaskedHello/binary>>),
	{ok, <<1:1, 1:1, 0:2, 1:4, 0:1, 7:7, CompressedHello/binary>>} = gen_tcp:recv(Socket, 0, 6000),
	%% Client-initiated close.
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>),
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

websocket_deflate_ignore_if_not_negotiated(Config) ->
	doc("The {deflate, boolean()} commands are ignored "
		"when compression was not negotiated."),
	{ok, ConnPid, StreamRef} = gun_open_ws(Config, "/deflate", []),
	_ = [begin
		gun:ws_send(ConnPid, {text, <<"Hello.">>}),
		{ok, {text, <<"Hello.">>}} = receive_ws(ConnPid, StreamRef)
	end || _ <- lists:seq(1, 10)],
	ok.

websocket_set_options_idle_timeout(Config) ->
	doc("The idle_timeout option can be modified using the "
		"command {set_options, Opts} at runtime."),
	ConnPid = gun_open(Config),
	StreamRef = gun:ws_upgrade(ConnPid, "/set_options"),
	receive
		{gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _} ->
			ok;
		{gun_response, ConnPid, _, _, Status, Headers} ->
			exit({ws_upgrade_failed, Status, Headers});
		{gun_error, ConnPid, StreamRef, Reason} ->
			exit({ws_upgrade_failed, Reason})
	after 1000 ->
		error(timeout)
	end,
	%% We don't send anything for a short while and confirm
	%% that idle_timeout does not trigger.
	{error, timeout} = gun:await(ConnPid, StreamRef, 2000),
	%% Trigger the change in idle_timeout and confirm that
	%% the connection gets closed soon after.
	gun:ws_send(ConnPid, {text, <<"idle_timeout_short">>}),
	receive
		{gun_down, ConnPid, _, _, _, _} ->
			ok
	after 2000 ->
		error(timeout)
	end.
