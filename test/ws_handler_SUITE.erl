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

init_per_testcase(Name, Config) when
		Name =:= websocket_data_delivery_n;
		Name =:= websocket_data_delivery_n_passive;
		Name =:= websocket_data_delivery_n_empty;
		Name =:= websocket_data_delivery_n_late_ack;
		Name =:= websocket_data_delivery_n_conn_window;
		Name =:= websocket_data_delivery_n_late_active ->
	case config(ref, Config) of
		h2_relay -> Config;
		_ -> {skip, "HTTP/2 relay only."}
	end;
init_per_testcase(_, Config) ->
	Config.

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
		{"/delivery_n/:n", ws_delivery_n_h, InitialState},
		{"/delivery_n/:n/:mode", ws_delivery_n_h, InitialState},
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
	case config(protocol, Config) of
		http -> gun_down(ConnPid);
		http2 -> ok
	end.

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
	case config(protocol, Config) of
		http -> gun_down(ConnPid);
		http2 -> ok
	end.

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

websocket_data_delivery_n(Config) ->
	doc("HTTP/2 relay delivers DATA frames received after data_delivery_n."),
	%% The next frame is delivered while the handler is blocked.
	do_data_delivery_n(Config, 1, 1),
	%% Both following frames are delivered. The pause does not hold them.
	do_data_delivery_n(Config, 2, 2),
	ok.

do_data_delivery_n(Config, N, ExpectInMailbox) ->
	{ok, ConnPid, StreamRef} = gun_open_ws(Config,
		"/delivery_n/" ++ integer_to_list(N), []),
	gun:ws_send(ConnPid, StreamRef, {binary, term_to_binary({self(), ExpectInMailbox})}),
	WsPid = receive
		{blocked, Pid} ->
			Pid
	after 1000 ->
		error(timeout)
	end,
	%% Forwarded while the handler is blocked, so not echoed yet.
	_ = [gun:ws_send(ConnPid, StreamRef, {text, <<"x">>})
		|| _ <- lists:seq(1, ExpectInMailbox)],
	{error, timeout} = receive
		{gun_ws, ConnPid, StreamRef, Frame} ->
			{ok, Frame}
	after 1000 ->
		{error, timeout}
	end,
	WsPid ! release,
	{ok, {text, <<"ok">>}} = receive_ws(ConnPid, StreamRef),
	_ = [{ok, {text, <<"x">>}} = receive_ws(ConnPid, StreamRef)
		|| _ <- lists:seq(1, ExpectInMailbox)],
	gun:close(ConnPid).

websocket_data_delivery_n_passive(Config) ->
	doc("A DATA frame already in the window is delivered while active is false."),
	{ok, ConnPid, StreamRef} = gun_open_ws(Config, "/delivery_n/1/passive", []),
	{ok, {text, PidBin}} = receive_ws(ConnPid, StreamRef),
	WsPid = list_to_pid(binary_to_list(PidBin)),
	gun:ws_send(ConnPid, StreamRef, {text, <<"one">>}),
	{ok, {text, <<"one">>}} = receive_ws(ConnPid, StreamRef),
	%% Already allowed by the stream window, so it is delivered
	%% while active is false. Active still controls the window.
	gun:ws_send(ConnPid, StreamRef, {text, <<"two">>}),
	{ok, {text, <<"two">>}} = receive_ws(ConnPid, StreamRef),
	WsPid ! go_active,
	gun:close(ConnPid).

websocket_data_delivery_n_empty(Config) ->
	doc("Empty DATA frames are not relayed."),
	%% N=2: an empty frame before the pause is not delivered
	%% and does not consume a credit.
	CreditSocket = do_h2_ws(Config, <<"/delivery_n/2/empty">>),
	ok = ssl:send(CreditSocket, cow_http2:data(1, nofin,
		do_ws_masked(2, term_to_binary({self(), before})))),
	CreditPid = receive
		{blocked, Pid0, _} ->
			Pid0
	after 1000 ->
		error(timeout)
	end,
	ok = ssl:send(CreditSocket, cow_http2:data(1, nofin, <<>>)),
	ok = ssl:send(CreditSocket, cow_http2:ping(1)),
	ok = do_recv_ping_ack(CreditSocket),
	CreditPid ! release,
	[<<"ok">>] = do_recv_ws_payloads(CreditSocket, 1),
	ssl:close(CreditSocket),
	%% An empty END_STREAM is not relayed either.
	FinSocket = do_h2_ws(Config, <<"/delivery_n/1/empty">>),
	ok = ssl:send(FinSocket, cow_http2:data(1, nofin,
		do_ws_masked(2, term_to_binary({self(), fin})))),
	FinPid = receive
		{blocked, Pid1, _} ->
			Pid1
	after 1000 ->
		error(timeout)
	end,
	ok = ssl:send(FinSocket, cow_http2:data(1, fin, <<>>)),
	ok = ssl:send(FinSocket, cow_http2:ping(2)),
	ok = do_recv_ping_ack(FinSocket),
	FinPid ! release,
	[<<"ok">>] = do_recv_ws_payloads(FinSocket, 1),
	ssl:close(FinSocket),
	%% Empty nofin frames must not grow the connection. A later
	%% non-empty frame is delivered while waiting.
	Socket = do_h2_ws(Config, <<"/delivery_n/1/empty">>),
	ok = ssl:send(Socket, cow_http2:data(1, nofin,
		do_ws_masked(2, term_to_binary({self(), forward})))),
	{FloodPid, ConnPid} = receive
		{blocked, Pid2, Conn} ->
			{Pid2, Conn}
	after 1000 ->
		error(timeout)
	end,
	erlang:garbage_collect(ConnPid),
	{memory, Before} = process_info(ConnPid, memory),
	ok = ssl:send(Socket, lists:duplicate(2500, cow_http2:data(1, nofin, <<>>))),
	ok = ssl:send(Socket, cow_http2:ping(2)),
	ok = do_recv_ping_ack(Socket),
	erlang:garbage_collect(ConnPid),
	{memory, After} = process_info(ConnPid, memory),
	%% 2500 queued frames grew the connection by about 200KB.
	case After - Before < 32768 of
		true ->
			ok;
		false ->
			error({connection_memory_grew, Before, After})
	end,
	ok = ssl:send(Socket, cow_http2:data(1, nofin, do_ws_masked(1, <<"x">>))),
	ok = ssl:send(Socket, cow_http2:ping(3)),
	ok = do_recv_ping_ack(Socket),
	FloodPid ! release,
	[<<"ok">>, <<"x">>] = do_recv_ws_payloads(Socket, 2),
	ssl:close(Socket).

websocket_data_delivery_n_late_ack(Config) ->
	doc("A late acknowledgement does not crash the HTTP/2 connection."),
	%% The client resets the stream before the session acknowledges.
	ResetSocket = do_h2_ws(Config, <<"/delivery_n/1/empty">>),
	ok = ssl:send(ResetSocket, cow_http2:data(1, nofin,
		do_ws_masked(2, term_to_binary({self(), nofin})))),
	ResetConn = receive
		{blocked, _, Pid1} ->
			Pid1
	after 1000 ->
		error(timeout)
	end,
	ok = ssl:send(ResetSocket, cow_http2:rst_stream(1, cancel)),
	ok = ssl:send(ResetSocket, cow_http2:ping(1)),
	ok = do_recv_ping_ack(ResetSocket),
	ResetConn ! {'$cowboy_relay_command', {ResetConn, 1}, ack},
	ok = ssl:send(ResetSocket, cow_http2:ping(2)),
	ok = do_recv_ping_ack(ResetSocket),
	ssl:close(ResetSocket),
	%% The session acknowledges twice. The second time nothing is waiting.
	Socket = do_h2_ws(Config, <<"/delivery_n/1/empty">>),
	ok = ssl:send(Socket, cow_http2:data(1, nofin,
		do_ws_masked(2, term_to_binary({self(), nofin})))),
	{WsPid, ConnPid} = receive
		{blocked, Pid2, Pid3} ->
			{Pid2, Pid3}
	after 1000 ->
		error(timeout)
	end,
	WsPid ! release,
	[<<"ok">>] = do_recv_ws_payloads(Socket, 1),
	ConnPid ! {'$cowboy_relay_command', {ConnPid, 1}, ack},
	ok = ssl:send(Socket, cow_http2:ping(3)),
	ok = do_recv_ping_ack(Socket),
	ssl:close(Socket).

websocket_data_delivery_n_conn_window(Config) ->
	doc("Pausing one relay stream does not freeze the connection receive window."),
	Socket = do_h2_ws(Config, <<"/delivery_n/1/empty">>),
	ok = ssl:send(Socket, cow_http2:data(1, nofin,
		do_ws_masked(2, term_to_binary({self(), nofin})))),
	WsPid = receive
		{blocked, Pid, _} ->
			Pid
	after 1000 ->
		error(timeout)
	end,
	%% Connection window moves. The stream window stays where it is.
	{ok, <<4:24, 8:8, 0:8, 0:32, _:32>>} = ssl:recv(Socket, 13, 1000),
	{error, timeout} = ssl:recv(Socket, 1, 200),
	WsPid ! release,
	ssl:close(Socket).

websocket_data_delivery_n_late_active(Config) ->
	doc("A late active or passive command does not crash the HTTP/2 connection."),
	ok = do_late_relay_command(Config, active),
	ok = do_late_relay_command(Config, passive).

do_late_relay_command(Config, Command) ->
	Socket = do_h2_ws(Config, <<"/delivery_n/1/empty">>),
	ok = ssl:send(Socket, cow_http2:data(1, nofin,
		do_ws_masked(2, term_to_binary({self(), nofin})))),
	ConnPid = receive
		{blocked, _, Pid} ->
			Pid
	after 1000 ->
		error(timeout)
	end,
	ok = ssl:send(Socket, cow_http2:rst_stream(1, cancel)),
	ok = ssl:send(Socket, cow_http2:ping(1)),
	ok = do_recv_ping_ack(Socket),
	ConnPid ! {'$cowboy_relay_command', {ConnPid, 1}, Command},
	ok = ssl:send(Socket, cow_http2:ping(2)),
	ok = do_recv_ping_ack(Socket),
	ssl:close(Socket).

%% HTTP/2 Websocket over TLS, the way h2_relay listens.
do_h2_ws(Config, Path) ->
	TlsOpts = config(opts, Config),
	{ok, Socket} = ssl:connect("localhost", config(port, Config),
		[{alpn_advertised_protocols, [<<"h2">>]}, binary, {active, false}|TlsOpts]),
	{ok, <<"h2">>} = ssl:negotiated_protocol(Socket),
	ok = ssl:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	{ok, << Len:24 >>} = ssl:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = ssl:recv(Socket, 6 + Len, 1000),
	ok = ssl:send(Socket, cow_http2:settings_ack()),
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = ssl:recv(Socket, 9, 1000),
	{ReqHeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"CONNECT">>},
		{<<":protocol">>, <<"websocket">>},
		{<<":scheme">>, <<"https">>},
		{<<":path">>, Path},
		{<<":authority">>, <<"localhost">>},
		{<<"sec-websocket-version">>, <<"13">>}
	]),
	ok = ssl:send(Socket, cow_http2:headers(1, nofin, ReqHeadersBlock)),
	{ok, << Len1:24, 1:8, _:8, 1:32 >>} = ssl:recv(Socket, 9, 1000),
	{ok, RespHeadersBlock} = ssl:recv(Socket, Len1, 1000),
	{RespHeaders, _} = cow_hpack:decode(RespHeadersBlock),
	{_, <<"200">>} = lists:keyfind(<<":status">>, 1, RespHeaders),
	Socket.

do_ws_masked(Opcode, Payload) ->
	Mask = 16#37fa213d,
	Masked = ws_SUITE:do_mask(Payload, Mask, <<>>),
	Len = byte_size(Payload),
	true = Len < 126,
	<<1:1, 0:3, Opcode:4, 1:1, Len:7, Mask:32, Masked/binary>>.

do_recv_ping_ack(Socket) ->
	{ok, <<Len:24, Type:8, Flags:8, _:32>>} = ssl:recv(Socket, 9, 2000),
	_ = case Len of
		0 -> ok;
		_ -> {ok, _} = ssl:recv(Socket, Len, 2000)
	end,
	case {Type, Flags} of
		{6, 1} -> ok;
		_ -> do_recv_ping_ack(Socket)
	end.

do_recv_ws_payloads(Socket, N) ->
	do_recv_ws_payloads(Socket, N, []).

do_recv_ws_payloads(_, 0, Acc) ->
	lists:reverse(Acc);
do_recv_ws_payloads(Socket, N, Acc) ->
	{ok, <<Len:24, Type:8, _:8, _:32>>} = ssl:recv(Socket, 9, 2000),
	Payload = case Len of
		0 -> <<>>;
		_ ->
			{ok, Data} = ssl:recv(Socket, Len, 2000),
			Data
	end,
	case Type of
		0 ->
			<<1:1, 0:3, _:4, 0:1, PLen:7, Bin:PLen/binary>> = Payload,
			do_recv_ws_payloads(Socket, N - 1, [Bin|Acc]);
		_ ->
			do_recv_ws_payloads(Socket, N, Acc)
	end.

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
	case config(protocol, Config) of
		http ->
			receive
				{gun_down, ConnPid, _, _, _} ->
					ok
			after 2000 ->
				error(timeout)
			end;
		http2 ->
			ok
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
	case config(protocol, Config) of
		http ->
			receive
				{gun_down, ConnPid, _, _, _} ->
					ok
			after 2000 ->
				error(timeout)
			end;
		http2 ->
			ok
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
