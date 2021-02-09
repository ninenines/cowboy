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
-import(ct_helper, [get_remote_pid_tcp/1]).
-import(cowboy_test, [gun_open/1]).

all() -> [{group, clear}].

groups() -> [{clear, [parallel], ct_helper:all(?MODULE)}].

init_dispatch(_) ->
	cowboy_router:compile([{"localhost", [
		{"/", hello_h, []},
		{"/echo/:key", echo_h, []},
		{"/resp_iolist_body", resp_iolist_body_h, []},
		{"/streamed_result/:n/:interval", streamed_result_h, []}
	]}]).

%% Do a prior knowledge handshake (function originally copied from rfc7540_SUITE).
do_handshake(Config) ->
	do_handshake(#{}, Config).

do_handshake(Settings, Config) ->
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config), [binary, {active, false}]),
	%% Send a valid preface.
	ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(Settings)]),
	%% Receive the server preface.
	{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
	{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
	%% Send the SETTINGS ack.
	ok = gen_tcp:send(Socket, cow_http2:settings_ack()),
	%% Receive the SETTINGS ack.
	{ok, << 0:24, 4:8, 1:8, 0:32 >>} = gen_tcp:recv(Socket, 9, 1000),
	{ok, Socket}.

idle_timeout(Config) ->
	doc("Terminate when the idle timeout is reached."),
	ProtoOpts = #{
		env => #{dispatch => init_dispatch(Config)},
		idle_timeout => 1000
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		{ok, Socket} = do_handshake([{port, Port}|Config]),
		timer:sleep(1000),
		%% Receive a GOAWAY frame back with NO_ERROR.
		{ok, << _:24, 7:8, _:72, 0:32 >>} = gen_tcp:recv(Socket, 17, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

idle_timeout_infinity(Config) ->
	doc("Ensure the idle_timeout option accepts the infinity value."),
	ProtoOpts = #{
		env => #{dispatch => init_dispatch(Config)},
		idle_timeout => infinity
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		{ok, Socket} = do_handshake([{port, Port}|Config]),
		timer:sleep(1000),
		%% Don't receive a GOAWAY frame.
		{error, timeout} = gen_tcp:recv(Socket, 17, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

idle_timeout_reset_on_data(Config) ->
	doc("Terminate when the idle timeout is reached."),
	ProtoOpts = #{
		env => #{dispatch => init_dispatch(Config)},
		idle_timeout => 1000
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		{ok, Socket} = do_handshake([{port, Port}|Config]),
		%% We wait a little, send a PING, receive a PING ack.
		{error, timeout} = gen_tcp:recv(Socket, 17, 500),
		ok = gen_tcp:send(Socket, cow_http2:ping(0)),
		{ok, <<8:24, 6:8, 0:7, 1:1, 0:96>>} = gen_tcp:recv(Socket, 17, 1000),
		%% Again.
		{error, timeout} = gen_tcp:recv(Socket, 17, 500),
		ok = gen_tcp:send(Socket, cow_http2:ping(0)),
		{ok, <<8:24, 6:8, 0:7, 1:1, 0:96>>} = gen_tcp:recv(Socket, 17, 1000),
		%% And one more time.
		{error, timeout} = gen_tcp:recv(Socket, 17, 500),
		ok = gen_tcp:send(Socket, cow_http2:ping(0)),
		{ok, <<8:24, 6:8, 0:7, 1:1, 0:96>>} = gen_tcp:recv(Socket, 17, 1000),
		%% The connection goes away soon after we stop sending data.
		timer:sleep(1000),
		{ok, << _:24, 7:8, _:72, 0:32 >>} = gen_tcp:recv(Socket, 17, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

inactivity_timeout(Config) ->
	doc("Terminate when the inactivity timeout is reached."),
	ProtoOpts = #{
		env => #{dispatch => init_dispatch(Config)},
		inactivity_timeout => 1000
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		{ok, Socket} = do_handshake([{port, Port}|Config]),
		receive after 1000 -> ok end,
		%% Receive a GOAWAY frame back with an INTERNAL_ERROR.
		{ok, << _:24, 7:8, _:72, 2:32 >>} = gen_tcp:recv(Socket, 17, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

initial_connection_window_size(Config) ->
	doc("Confirm a WINDOW_UPDATE frame is sent when the configured "
		"connection window is larger than the default."),
	ConfiguredSize = 100000,
	ProtoOpts = #{
		env => #{dispatch => init_dispatch(Config)},
		initial_connection_window_size => ConfiguredSize
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}]),
		%% Send a valid preface.
		ok = gen_tcp:send(Socket, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
		%% Receive the server preface.
		{ok, << Len:24 >>} = gen_tcp:recv(Socket, 3, 1000),
		{ok, << 4:8, 0:40, _:Len/binary >>} = gen_tcp:recv(Socket, 6 + Len, 1000),
		%% Receive a WINDOW_UPDATE frame incrementing the connection window to 100000.
		{ok, <<4:24, 8:8, 0:41, Size:31>>} = gen_tcp:recv(Socket, 13, 1000),
		ConfiguredSize = Size + 65535
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

max_frame_size_sent(Config) ->
	doc("Confirm that frames sent by Cowboy are limited in size "
		"by the max_frame_size_sent configuration value."),
	MaxFrameSize = 20000,
	ProtoOpts = #{
		env => #{dispatch => init_dispatch(Config)},
		max_frame_size_sent => MaxFrameSize
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		{ok, Socket} = do_handshake(#{max_frame_size => MaxFrameSize + 10000},
			[{port, Port}|Config]),
		%% Send a request with a 30000 bytes body.
		{HeadersBlock, _} = cow_hpack:encode([
			{<<":method">>, <<"POST">>},
			{<<":scheme">>, <<"http">>},
			{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
			{<<":path">>, <<"/echo/read_body">>}
		]),
		ok = gen_tcp:send(Socket, [
			cow_http2:headers(1, nofin, HeadersBlock),
			cow_http2:data(1, nofin, <<0:16384/unit:8>>),
			cow_http2:data(1, fin, <<0:13616/unit:8>>)
		]),
		%% Receive a HEADERS frame as a response.
		{ok, << SkipLen:24, 1:8, _:8, 1:32 >>} = case gen_tcp:recv(Socket, 9, 1000) of
			%% We received a WINDOW_UPDATE first. Skip it and the next.
			{ok, <<4:24, 8:8, 0:40>>} ->
				{ok, _} = gen_tcp:recv(Socket, 4 + 13, 1000),
				gen_tcp:recv(Socket, 9, 1000);
			Res ->
				Res
		end,
		{ok, _} = gen_tcp:recv(Socket, SkipLen, 6000),
		%% The DATA frames following must have lengths of 20000
		%% and then 10000 due to the limit.
		{ok, <<20000:24, 0:8, _:40, _:20000/unit:8>>} = gen_tcp:recv(Socket, 20009, 6000),
		{ok, <<10000:24, 0:8, _:40, _:10000/unit:8>>} = gen_tcp:recv(Socket, 10009, 6000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

persistent_term_router(Config) ->
	doc("The router can retrieve the routes from persistent_term storage."),
	case erlang:function_exported(persistent_term, get, 1) of
		true -> do_persistent_term_router(Config);
		false -> {skip, "This test uses the persistent_term functionality added in Erlang/OTP 21.2."}
	end.

do_persistent_term_router(Config) ->
	persistent_term:put(?FUNCTION_NAME, init_dispatch(Config)),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => {persistent_term, ?FUNCTION_NAME}}
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http2}, {port, Port}|Config]),
		{ok, http2} = gun:await_up(ConnPid),
		StreamRef = gun:get(ConnPid, "/"),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef),
		gun:close(ConnPid)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

preface_timeout_infinity(Config) ->
	doc("Ensure infinity for preface_timeout is accepted."),
	ProtoOpts = #{
		env => #{dispatch => init_dispatch(Config)},
		preface_timeout => infinity
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		{ok, Socket} = do_handshake([{port, Port}|Config]),
		Pid = get_remote_pid_tcp(Socket),
		Ref = erlang:monitor(process, Pid),
		receive
			{'DOWN', Ref, process, Pid, Reason} ->
				error(Reason)
		after 1000 ->
			ok
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

resp_iolist_body(Config) ->
	doc("Regression test when response bodies are iolists that "
		"include improper lists, empty lists and empty binaries. "
		"The original issue failed to split the body into frames properly."),
	ProtoOpts = #{
		env => #{dispatch => init_dispatch(Config)}
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http2}, {port, Port}|Config]),
		Ref = gun:get(ConnPid, "/resp_iolist_body"),
		{response, nofin, 200, RespHeaders} = gun:await(ConnPid, Ref),
		{_, BinLen} = lists:keyfind(<<"content-length">>, 1, RespHeaders),
		Len = binary_to_integer(BinLen),
		{ok, RespBody} = gun:await_body(ConnPid, Ref),
		Len = iolist_size(RespBody),
		gun:close(ConnPid)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

settings_timeout_infinity(Config) ->
	doc("Ensure infinity for settings_timeout is accepted."),
	ProtoOpts = #{
		env => #{dispatch => init_dispatch(Config)},
		settings_timeout => infinity
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		{ok, Socket} = do_handshake([{port, Port}|Config]),
		Pid = get_remote_pid_tcp(Socket),
		Ref = erlang:monitor(process, Pid),
		receive
			{'DOWN', Ref, process, Pid, Reason} ->
				error(Reason)
		after 1000 ->
			ok
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

graceful_shutdown_connection(Config) ->
	doc("Check that ongoing requests are handled before gracefully shutting down a connection."),
	Dispatch = cowboy_router:compile([{"localhost", [
		{"/delay_hello", delay_hello_h,
			#{delay => 500, notify_received => self()}}
	]}]),
	ProtoOpts = #{
		env => #{dispatch => Dispatch}
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http2}, {port, Port}|Config]),
		Ref = gun:get(ConnPid, "/delay_hello"),
		%% Make sure the request is received.
		receive {request_received, <<"/delay_hello">>} -> ok end,
		%% Tell the connection to shutdown while the handler is working.
		[CowboyConnPid] = ranch:procs(?FUNCTION_NAME, connections),
		monitor(process, CowboyConnPid),
		ok = sys:terminate(CowboyConnPid, goaway),
		%% Check that the response is sent to the client before the
		%% connection goes down.
		{response, nofin, 200, _RespHeaders} = gun:await(ConnPid, Ref),
		{ok, RespBody} = gun:await_body(ConnPid, Ref),
		<<"Hello world!">> = iolist_to_binary(RespBody),
		%% Check that the connection is gone soon afterwards. (The exit
		%% reason is supposed to be 'goaway' as passed to
		%% sys:terminate/2, but it is {shutdown, closed}.)
		receive
			{'DOWN', _, process, CowboyConnPid, _Reason} ->
				ok
		end,
		[] = ranch:procs(?FUNCTION_NAME, connections),
		gun:close(ConnPid)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

graceful_shutdown_timeout(Config) ->
	doc("Check that a connection is closed when gracefully shutting down times out."),
	Dispatch = cowboy_router:compile([{"localhost", [
		{"/long_delay_hello", delay_hello_h,
			#{delay => 10000, notify_received => self()}}
	]}]),
	ProtoOpts = #{
		env => #{dispatch => Dispatch},
		goaway_initial_timeout => 200,
		goaway_complete_timeout => 500
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http2}, {port, Port}|Config]),
		Ref = gun:get(ConnPid, "/long_delay_hello"),
		%% Make sure the request is received.
		receive {request_received, <<"/long_delay_hello">>} -> ok end,
		%% Tell the connection to shutdown while the handler is working.
		[CowboyConnPid] = ranch:procs(?FUNCTION_NAME, connections),
		monitor(process, CowboyConnPid),
		ok = sys:terminate(CowboyConnPid, goaway),
		%% Check that connection didn't wait for the slow handler.
		{error, {stream_error, closed}} = gun:await(ConnPid, Ref),
		%% Check that the connection is gone. (The exit reason is
		%% supposed to be 'goaway' as passed to sys:terminate/2, but it
		%% is {shutdown, {stop, {exit, goaway}, 'Graceful shutdown timed
		%% out.'}}.)
		receive
			{'DOWN', _, process, CowboyConnPid, _Reason} ->
				ok
		after 100 ->
		       error(still_alive)
		end,
		[] = ranch:procs(?FUNCTION_NAME, connections),
		gun:close(ConnPid)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

graceful_shutdown_listener(Config) ->
	doc("Check that connections are shut down gracefully when stopping a listener."),
	Dispatch = cowboy_router:compile([{"localhost", [
		{"/delay_hello", delay_hello_h,
			#{delay => 500, notify_received => self()}}
	]}]),
	ProtoOpts = #{
		env => #{dispatch => Dispatch}
	},
	{ok, Listener} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	ConnPid = gun_open([{type, tcp}, {protocol, http2}, {port, Port}|Config]),
	Ref = gun:get(ConnPid, "/delay_hello"),
	%% Shutdown listener while the handlers are working.
	receive {request_received, <<"/delay_hello">>} -> ok end,
	ListenerMonitorRef = monitor(process, Listener),
	ok = cowboy:stop_listener(?FUNCTION_NAME),
	receive
		{'DOWN', ListenerMonitorRef, process, Listener, _Reason} ->
			ok
	end,
	%% Check that the request is handled before shutting down.
	{response, nofin, 200, _RespHeaders} = gun:await(ConnPid, Ref),
	{ok, RespBody} = gun:await_body(ConnPid, Ref),
	<<"Hello world!">> = iolist_to_binary(RespBody),
	gun:close(ConnPid).

graceful_shutdown_listener_timeout(Config) ->
	doc("Check that connections are shut down when gracefully stopping a listener times out."),
	Dispatch = cowboy_router:compile([{"localhost", [
		{"/long_delay_hello", delay_hello_h,
			#{delay => 10000, notify_received => self()}}
	]}]),
	ProtoOpts = #{
		env => #{dispatch => Dispatch},
		goaway_initial_timeout => 200,
		goaway_complete_timeout => 500
	},
	{ok, Listener} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	ConnPid = gun_open([{type, tcp}, {protocol, http2}, {port, Port}|Config]),
	Ref = gun:get(ConnPid, "/long_delay_hello"),
	%% Shutdown listener while the handlers are working.
	receive {request_received, <<"/long_delay_hello">>} -> ok end,
	ListenerMonitorRef = monitor(process, Listener),
	ok = cowboy:stop_listener(?FUNCTION_NAME),
	receive
		{'DOWN', ListenerMonitorRef, process, Listener, _Reason} ->
			ok
	end,
	%% Check that the slow request is aborted.
	{error, {stream_error, closed}} = gun:await(ConnPid, Ref),
	gun:close(ConnPid).

idle_timeout_on_send(Config) ->
	doc("The idle timeout is not reset by default by sending."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		idle_timeout => 1000
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http2}, {port, Port}|Config]),
		{ok, http2} = gun:await_up(ConnPid),
		#{socket := Socket} = gun:info(ConnPid),
		Pid = get_remote_pid_tcp(Socket),
		StreamRef = gun:get(ConnPid, "/streamed_result/10/250"),
		Ref = erlang:monitor(process, Pid),
		receive
			{gun_response, ConnPid, StreamRef, nofin, _Status, _Headers} ->
				idle_timeout_recv_loop(Ref, Pid, ConnPid, StreamRef, false)
		after 2000 ->
		      error(timeout)
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

idle_timeout_reset_on_send(Config) ->
	doc("The idle timeout can be made to reset by sending."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		idle_timeout => 1000,
		reset_idle_on_send => true
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http2}, {port, Port}|Config]),
		{ok, http2} = gun:await_up(ConnPid),
		#{socket := Socket} = gun:info(ConnPid),
		Pid = get_remote_pid_tcp(Socket),
		StreamRef = gun:get(ConnPid, "/streamed_result/10/250"),
		Ref = erlang:monitor(process, Pid),
		receive
			{gun_response, ConnPid, StreamRef, nofin, _Status, _Headers} ->
				idle_timeout_recv_loop(Ref, Pid, ConnPid, StreamRef, true)
		after 2000 ->
		      error(timeout)
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

idle_timeout_recv_loop(Ref, Pid, ConnPid, StreamRef, ExpectCompletion) ->
	receive
		{gun_data, ConnPid, StreamRef, nofin, _Data} ->
			idle_timeout_recv_loop(Ref, Pid, ConnPid, StreamRef, ExpectCompletion);
		{gun_data, ConnPid, StreamRef, fin, _Data} when ExpectCompletion ->
			ok;
		{gun_data, ConnPid, StreamRef, fin, _Data} ->
			error(completed);
		{'DOWN', Ref, process, Pid, _} when ExpectCompletion ->
			error(exited);
		{'DOWN', Ref, process, Pid, _} ->
			 ok
	after 2000 ->
	      error(timeout)
	end.
