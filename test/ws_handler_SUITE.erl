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

-module(ws_handler_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [gun_open/2]).
-import(cowboy_test, [gun_down/1]).

%% ct.

all() ->
	[
		{group, h1},
		{group, h1_hibernate},
		{group, h2},
		{group, h2_relay}
	].

%% @todo Test against HTTP/2 too.
groups() ->
	AllTests = ct_helper:all(?MODULE),
	[
		{h1, [parallel], AllTests},
		{h1_hibernate, [parallel], AllTests},
		%% The websocket_deflate_false test isn't compatible with HTTP/2.
		{h2, [parallel], AllTests -- [websocket_deflate_false]},
		{h2_relay, [parallel], AllTests -- [websocket_deflate_false]}
	].

init_per_group(Name, Config)
		when Name =:= h1; Name =:= h1_hibernate ->
	cowboy_test:init_http(Name, #{
		env => #{dispatch => init_dispatch(Name)}
	}, Config);
init_per_group(Name, Config)
		when Name =:= h2; Name =:= h2_relay ->
	cowboy_test:init_http2(Name, #{
		enable_connect_protocol => true,
		env => #{dispatch => init_dispatch(Name)}
	}, Config).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

%% Dispatch configuration.

init_dispatch(Name) ->
	InitialState = case Name of
		h1_hibernate -> #{run_or_hibernate => hibernate};
		h2_relay -> #{run_or_hibernate => run, data_delivery => relay};
		_ -> #{run_or_hibernate => run}
	end,
	cowboy_router:compile([{'_', [
		{"/init", ws_init_commands_h, InitialState},
		{"/handle", ws_handle_commands_h, InitialState},
		{"/info", ws_info_commands_h, InitialState},
		{"/trap_exit", ws_init_h, InitialState},
		{"/active", ws_active_commands_h, InitialState},
		{"/deflate", ws_deflate_commands_h, InitialState},
		{"/set_options", ws_set_options_commands_h, InitialState},
		{"/shutdown_reason", ws_shutdown_reason_commands_h, InitialState},
		{"/terminate", ws_terminate_h, InitialState}
	]}]).

%% Support functions for testing using Gun.

gun_open_ws(Config, Path, Commands) ->
	ConnPid = gun_open(Config, #{http2_opts => #{notify_settings_changed => true}}),
	do_await_enable_connect_protocol(config(protocol, Config), ConnPid),
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

do_await_enable_connect_protocol(http, _) ->
	ok;
do_await_enable_connect_protocol(http2, ConnPid) ->
	{notify, settings_changed, #{enable_connect_protocol := true}}
		= gun:await(ConnPid, undefined), %% @todo Maybe have a gun:await/1?
	ok.

receive_ws(ConnPid, StreamRef) ->
	receive
		{gun_ws, ConnPid, StreamRef, Frame} ->
			{ok, Frame}
	after 1000 ->
		{error, timeout}
	end.

ensure_handle_is_called(ConnPid, StreamRef, "/handle") ->
	gun:ws_send(ConnPid, StreamRef, {text, <<"Necessary to trigger websocket_handle/2.">>});
ensure_handle_is_called(_, _, _) ->
	ok.

do_receive(Tag) ->
	receive
		Msg when element(1, Msg) =:= Tag ->
			Msg
	after 1000 ->
		ct:pal("do_receive(~p): ~p", [Tag, process_info(self(), messages)]),
		error(timeout)
	end.

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
	ensure_handle_is_called(ConnPid, StreamRef, Path),
	{error, timeout} = receive_ws(ConnPid, StreamRef),
	gun:close(ConnPid).

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
	{ok, ConnPid, StreamRef} = gun_open_ws(Config, Path, bad),
	ensure_handle_is_called(ConnPid, StreamRef, Path),
	case config(protocol, Config) of
		%% HTTP/1.1 closes the connection.
		http -> gun_down(ConnPid);
		%% HTTP/2 terminates the stream.
		http2 ->
			receive {gun_error, ConnPid, StreamRef, {stream_error, internal_error, _}} -> ok
			after 500 -> error(timeout) end
	end.

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
	ensure_handle_is_called(ConnPid, StreamRef, Path),
	{ok, {text, <<"One frame!">>}} = receive_ws(ConnPid, StreamRef),
	gun:close(ConnPid).

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
	ensure_handle_is_called(ConnPid, StreamRef, Path),
	{ok, {text, <<"One frame!">>}} = receive_ws(ConnPid, StreamRef),
	{ok, {binary, <<"Two frames!">>}} = receive_ws(ConnPid, StreamRef),
	gun:close(ConnPid).

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
	ensure_handle_is_called(ConnPid, StreamRef, Path),
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
	ensure_handle_is_called(ConnPid, StreamRef, Path),
	{ok, {text, <<"One frame!">>}} = receive_ws(ConnPid, StreamRef),
	{ok, {binary, <<"Two frames!">>}} = receive_ws(ConnPid, StreamRef),
	{ok, close} = receive_ws(ConnPid, StreamRef),
	gun_down(ConnPid).

websocket_init_trap_exit_false(Config) ->
	doc("The trap_exit process flag must be set back to false before "
		"the connection is taken over by Websocket."),
	{ok, ConnPid, StreamRef} = gun_open_ws(Config, "/trap_exit?reply_trap_exit", []),
	{ok, {text, <<"trap_exit: false">>}} = receive_ws(ConnPid, StreamRef),
	ok.

websocket_active_false(Config) ->
	doc("The {active, false} command stops receiving data from the socket. "
		"The {active, true} command reenables it."),
	{ok, ConnPid, StreamRef} = gun_open_ws(Config, "/active", []),
	%% We must exhaust the HTTP/2 flow control window
	%% otherwise the frame will be received even if active mode is disabled.
	gun:ws_send(ConnPid, StreamRef, {binary, <<0:100000/unit:8>>}),
	gun:ws_send(ConnPid, StreamRef, {text, <<"Not received until the handler enables active again.">>}),
	{error, timeout} = receive_ws(ConnPid, StreamRef),
	{ok, {binary, _}} = receive_ws(ConnPid, StreamRef),
	{ok, {text, <<"Not received until the handler enables active again.">>}}
		= receive_ws(ConnPid, StreamRef),
	gun:close(ConnPid).

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
		gun:ws_send(ConnPid, StreamRef, {text, <<"Hello.">>}),
		{ok, {text, <<"Hello.">>}} = receive_ws(ConnPid, StreamRef)
	end || _ <- lists:seq(1, 10)],
	gun:close(ConnPid).

websocket_set_options_idle_timeout(Config) ->
	doc("The idle_timeout option can be modified using the "
		"command {set_options, Opts} at runtime."),
	ConnPid = gun_open(Config, #{http2_opts => #{notify_settings_changed => true}}),
	do_await_enable_connect_protocol(config(protocol, Config), ConnPid),
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
	gun:ws_send(ConnPid, StreamRef, {text, <<"idle_timeout_short">>}),
	receive
		{gun_down, ConnPid, _, _, _} ->
			ok
	after 2000 ->
		error(timeout)
	end.

websocket_set_options_max_frame_size(Config) ->
	doc("The max_frame_size option can be modified using the "
		"command {set_options, Opts} at runtime."),
	ConnPid = gun_open(Config, #{http2_opts => #{notify_settings_changed => true}}),
	do_await_enable_connect_protocol(config(protocol, Config), ConnPid),
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
	%% We first send a 1MB frame to confirm that yes, we can
	%% send a frame that large. The default max_frame_size is infinity.
	gun:ws_send(ConnPid, StreamRef, {binary, <<0:8000000>>}),
	{ws, {binary, <<0:8000000>>}} = gun:await(ConnPid, StreamRef),
	%% Trigger the change in max_frame_size. From now on we will
	%% only allow frames of up to 1000 bytes.
	gun:ws_send(ConnPid, StreamRef, {text, <<"max_frame_size_small">>}),
	%% Confirm that we can send frames of up to 1000 bytes.
	gun:ws_send(ConnPid, StreamRef, {binary, <<0:8000>>}),
	{ws, {binary, <<0:8000>>}} = gun:await(ConnPid, StreamRef),
	%% Confirm that sending frames larger than 1000 bytes
	%% results in the closing of the connection.
	gun:ws_send(ConnPid, StreamRef, {binary, <<0:8008>>}),
	receive
		{gun_down, ConnPid, _, _, _} ->
			ok
	after 2000 ->
		error(timeout)
	end.

websocket_shutdown_reason(Config) ->
	doc("The command {shutdown_reason, any()} can be used to "
		"change the shutdown reason of a Websocket connection."),
	ConnPid = gun_open(Config, #{http2_opts => #{notify_settings_changed => true}}),
	do_await_enable_connect_protocol(config(protocol, Config), ConnPid),
	StreamRef = gun:ws_upgrade(ConnPid, "/shutdown_reason", [
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	{upgrade, [<<"websocket">>], _} = gun:await(ConnPid, StreamRef),
	WsPid = receive {ws_pid, P} -> P after 1000 -> error(timeout) end,
	MRef = monitor(process, WsPid),
	WsPid ! {self(), {?MODULE, ?FUNCTION_NAME}},
	receive
		{'DOWN', MRef, process, WsPid, {shutdown, {?MODULE, ?FUNCTION_NAME}}} ->
			ok
	after 1000 ->
		error(timeout)
	end.

websocket_terminate_close_normal(Config) ->
	doc("Receiving a close frame results in a terminate/3 call. "
		"The Req object is kept in a more compact form by default."),
	ConnPid = gun_open(Config, #{http2_opts => #{notify_settings_changed => true}}),
	do_await_enable_connect_protocol(config(protocol, Config), ConnPid),
	StreamRef = gun:ws_upgrade(ConnPid, "/terminate", [
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	{upgrade, [<<"websocket">>], _} = gun:await(ConnPid, StreamRef),
	{ws_pid, WsPid} = do_receive(ws_pid),
	MRef = monitor(process, WsPid),
	gun:ws_send(ConnPid, StreamRef, close),
	{terminate, remote, Req} = do_receive(terminate),
	{'DOWN', MRef, process, WsPid, normal} = do_receive('DOWN'),
	%% Confirm terminate/3 was called with a compacted Req.
	true = maps:is_key(path, Req),
	false = maps:is_key(headers, Req),
	ok.

websocket_terminate_close_reason(Config) ->
	doc("Receiving a close frame results in a terminate/3 call. "
		"The Req object is kept in a more compact form by default."),
	ConnPid = gun_open(Config, #{http2_opts => #{notify_settings_changed => true}}),
	do_await_enable_connect_protocol(config(protocol, Config), ConnPid),
	StreamRef = gun:ws_upgrade(ConnPid, "/terminate", [
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	{upgrade, [<<"websocket">>], _} = gun:await(ConnPid, StreamRef),
	{ws_pid, WsPid} = do_receive(ws_pid),
	MRef = monitor(process, WsPid),
	gun:ws_send(ConnPid, StreamRef, {close, 4000, <<"test-close">>}),
	{terminate, {remote, 4000, <<"test-close">>}, Req} = do_receive(terminate),
	{'DOWN', MRef, process, WsPid, normal} = do_receive('DOWN'),
	%% Confirm terminate/3 was called with a compacted Req.
	true = maps:is_key(path, Req),
	false = maps:is_key(headers, Req),
	ok.

websocket_terminate_socket_close(Config) ->
	doc("The socket getting closed results in a terminate/3 call. "
		"The Req object is kept in a more compact form by default."),
	Protocol = config(protocol, Config),
	ConnPid = gun_open(Config, #{http2_opts => #{notify_settings_changed => true}}),
	do_await_enable_connect_protocol(Protocol, ConnPid),
	StreamRef = gun:ws_upgrade(ConnPid, "/terminate", [
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	{upgrade, [<<"websocket">>], _} = gun:await(ConnPid, StreamRef),
	{ws_pid, WsPid} = do_receive(ws_pid),
	MRef = monitor(process, WsPid),
	gun:close(ConnPid),
	%% Terminate reasons differ depending on the protocol.
	{terminate, Reason, Req} = do_receive(terminate),
	case Reason of
		{error, closed} when Protocol =:= http -> ok;
		shutdown when Protocol =:= http2 -> ok
	end,
	{'DOWN', MRef, process, WsPid, normal} = do_receive('DOWN'),
	%% Confirm terminate/3 was called with a compacted Req.
	true = maps:is_key(path, Req),
	false = maps:is_key(headers, Req),
	ok.

websocket_terminate_req_filter(Config) ->
	doc("Receiving a close frame results in a terminate/3 call. "
		"A function can be given to filter the Req object."),
	ConnPid = gun_open(Config, #{http2_opts => #{notify_settings_changed => true}}),
	do_await_enable_connect_protocol(config(protocol, Config), ConnPid),
	StreamRef = gun:ws_upgrade(ConnPid, "/terminate?req_filter", [
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	{upgrade, [<<"websocket">>], _} = gun:await(ConnPid, StreamRef),
	{ws_pid, WsPid} = do_receive(ws_pid),
	MRef = monitor(process, WsPid),
	gun:ws_send(ConnPid, StreamRef, close),
	{terminate, remote, Req} = do_receive(terminate),
	{'DOWN', MRef, process, WsPid, normal} = do_receive('DOWN'),
	%% Confirm terminate/3 was called with a filtered Req.
	filtered = Req,
	ok.
