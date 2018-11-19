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

-module(stream_handler_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [gun_down/1]).

%% ct.

all() ->
	cowboy_test:common_all().

groups() ->
	cowboy_test:common_groups(ct_helper:all(?MODULE)).

%% We set this module as a logger in order to silence expected errors.
init_per_group(Name = http, Config) ->
	cowboy_test:init_http(Name, #{
		logger => ?MODULE,
		stream_handlers => [stream_handler_h]
	}, Config);
init_per_group(Name = https, Config) ->
	cowboy_test:init_https(Name, #{
		logger => ?MODULE,
		stream_handlers => [stream_handler_h]
	}, Config);
init_per_group(Name = h2, Config) ->
	cowboy_test:init_http2(Name, #{
		logger => ?MODULE,
		stream_handlers => [stream_handler_h]
	}, Config);
init_per_group(Name = h2c, Config) ->
	Config1 = cowboy_test:init_http(Name, #{
		logger => ?MODULE,
		stream_handlers => [stream_handler_h]
	}, Config),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2});
init_per_group(Name = http_compress, Config) ->
	cowboy_test:init_http(Name, #{
		logger => ?MODULE,
		stream_handlers => [cowboy_compress_h, stream_handler_h]
	}, Config);
init_per_group(Name = https_compress, Config) ->
	cowboy_test:init_https(Name, #{
		logger => ?MODULE,
		stream_handlers => [cowboy_compress_h, stream_handler_h]
	}, Config);
init_per_group(Name = h2_compress, Config) ->
	cowboy_test:init_http2(Name, #{
		logger => ?MODULE,
		stream_handlers => [cowboy_compress_h, stream_handler_h]
	}, Config);
init_per_group(Name = h2c_compress, Config) ->
	Config1 = cowboy_test:init_http(Name, #{
		logger => ?MODULE,
		stream_handlers => [cowboy_compress_h, stream_handler_h]
	}, Config),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2}).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

%% Logger function silencing the expected crashes.

error("Unhandled exception " ++ _, [error, crash|_]) ->
	ok;
error(Format, Args) ->
	error_logger:error_msg(Format, Args).

%% Tests.

crash_in_init(Config) ->
	doc("Confirm an error is sent when a stream handler crashes in init/3."),
	Self = self(),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"crash_in_init">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	%% Confirm terminate/3 is NOT called. We have no state to give to it.
	receive {Self, Pid, terminate, _, _, _} -> error(terminate) after 1000 -> ok end,
	%% Receive a 500 error response.
	case gun:await(ConnPid, Ref) of
		{response, fin, 500, _} -> ok;
		{error, {stream_error, internal_error, _}} -> ok
	end.

crash_in_data(Config) ->
	doc("Confirm an error is sent when a stream handler crashes in data/4."),
	Self = self(),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"content-length">>, <<"6">>},
		{<<"x-test-case">>, <<"crash_in_data">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	%% Send data to make the stream handler crash.
	gun:data(ConnPid, Ref, fin, <<"Hello!">>),
	%% Confirm terminate/3 is called, indicating the stream ended.
	receive {Self, Pid, terminate, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% Receive a 500 error response.
	case gun:await(ConnPid, Ref) of
		{response, fin, 500, _} -> ok;
		{error, {stream_error, internal_error, _}} -> ok
	end.

crash_in_info(Config) ->
	doc("Confirm an error is sent when a stream handler crashes in info/3."),
	Self = self(),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"crash_in_info">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	%% Send a message to make the stream handler crash.
	Pid ! {{Pid, 1}, crash},
	%% Confirm terminate/3 is called, indicating the stream ended.
	receive {Self, Pid, terminate, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% Receive a 500 error response.
	case gun:await(ConnPid, Ref) of
		{response, fin, 500, _} -> ok;
		{error, {stream_error, internal_error, _}} -> ok
	end.

crash_in_terminate(Config) ->
	doc("Confirm the state is correct when a stream handler crashes in terminate/3."),
	Self = self(),
	ConnPid = gun_open(Config),
	%% Do a first request.
	Ref1 = gun:get(ConnPid, "/hello_world", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"crash_in_terminate">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	%% Confirm terminate/3 is called.
	receive {Self, Pid, terminate, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% Receive the response.
	{response, nofin, 200, _} = gun:await(ConnPid, Ref1),
	{ok, <<"Hello world!">>} = gun:await_body(ConnPid, Ref1),
	%% Do a second request to make sure the connection state is still good.
	Ref2 = gun:get(ConnPid, "/hello_world", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"crash_in_terminate">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called. The pid shouldn't change.
	receive {Self, Pid, init, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% Confirm terminate/3 is called.
	receive {Self, Pid, terminate, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% Receive the second response.
	{response, nofin, 200, _} = gun:await(ConnPid, Ref2),
	{ok, <<"Hello world!">>} = gun:await_body(ConnPid, Ref2),
	ok.

crash_in_early_error(Config) ->
	case config(protocol, Config) of
		http -> do_crash_in_early_error(Config);
		http2 -> doc("The callback early_error/5 is not currently used for HTTP/2.")
	end.

do_crash_in_early_error(Config) ->
	doc("Confirm an error is sent when a stream handler crashes in early_error/5."
		"The connection is kept open by Cowboy."),
	Self = self(),
	ConnPid = gun_open(Config),
	Ref1 = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"crash_in_early_error">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	%% Confirm terminate/3 is NOT called. We have no state to give to it.
	receive {Self, Pid, terminate, _, _, _} -> error(terminate) after 1000 -> ok end,
	%% Confirm early_error/5 is called.
	receive {Self, Pid, early_error, _, _, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% Receive a 500 error response.
	{response, fin, 500, _} = gun:await(ConnPid, Ref1),
	%% This error is not fatal. We should be able to repeat it on the same connection.
	Ref2 = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"crash_in_early_error">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called.
	receive {Self, Pid, init, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% Confirm terminate/3 is NOT called. We have no state to give to it.
	receive {Self, Pid, terminate, _, _, _} -> error(terminate) after 1000 -> ok end,
	%% Confirm early_error/5 is called.
	receive {Self, Pid, early_error, _, _, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% Receive a 500 error response.
	{response, fin, 500, _} = gun:await(ConnPid, Ref2),
	ok.

crash_in_early_error_fatal(Config) ->
	case config(protocol, Config) of
		http -> do_crash_in_early_error_fatal(Config);
		http2 -> doc("The callback early_error/5 is not currently used for HTTP/2.")
	end.

do_crash_in_early_error_fatal(Config) ->
	doc("Confirm an error is sent when a stream handler crashes in early_error/5."
		"The error was fatal and the connection is closed by Cowboy."),
	Self = self(),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"host">>, <<"host:port">>},
		{<<"x-test-case">>, <<"crash_in_early_error_fatal">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is NOT called. The error occurs before we reach this step.
	receive {Self, _, init, _, _, _} -> error(init) after 1000 -> ok end,
	%% Confirm terminate/3 is NOT called. We have no state to give to it.
	receive {Self, _, terminate, _, _, _} -> error(terminate) after 1000 -> ok end,
	%% Confirm early_error/5 is called.
	receive {Self, _, early_error, _, _, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% Receive a 400 error response. We do not send a 500 when
	%% early_error/5 crashes, we send the original error.
	{response, fin, 400, _} = gun:await(ConnPid, Ref),
	%% Confirm the connection gets closed.
	gun_down(ConnPid).

set_options_ignore_unknown(Config) ->
	doc("Confirm that unknown options are ignored when using the set_options commands."),
	Self = self(),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"set_options_ignore_unknown">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	%% Confirm terminate/3 is called, indicating the stream ended.
	receive {Self, Pid, terminate, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% Confirm the response is sent.
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, _} = gun:await_body(ConnPid, Ref),
	ok.

shutdown_on_stream_stop(Config) ->
	doc("Confirm supervised processes are shutdown when stopping the stream."),
	Self = self(),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"shutdown_on_stream_stop">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	%% Receive the pid of the newly started process and monitor it.
	Spawn = receive {Self, Pid, spawned, S} -> S after 1000 -> error(timeout) end,
	MRef = monitor(process, Spawn),
	Spawn ! {Self, ready},
	%% Confirm terminate/3 is called, indicating the stream ended.
	receive {Self, Pid, terminate, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% We should receive a DOWN message soon after (or before) because the stream
	%% handler is stopping the stream immediately after the process started.
	receive {'DOWN', MRef, process, Spawn, shutdown} -> ok after 1000 -> error(timeout) end,
	%% The response is still sent.
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, _} = gun:await_body(ConnPid, Ref),
	ok.

shutdown_on_socket_close(Config) ->
	doc("Confirm supervised processes are shutdown when the socket closes."),
	Self = self(),
	ConnPid = gun_open(Config),
	_ = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"shutdown_on_socket_close">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	%% Receive the pid of the newly started process and monitor it.
	Spawn = receive {Self, Pid, spawned, S} -> S after 1000 -> error(timeout) end,
	MRef = monitor(process, Spawn),
	Spawn ! {Self, ready},
	%% Close the socket.
	ok = gun:close(ConnPid),
	%% Confirm terminate/3 is called, indicating the stream ended.
	receive {Self, Pid, terminate, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% Confirm we receive a DOWN message for the child process.
	receive {'DOWN', MRef, process, Spawn, shutdown} -> ok after 1000 -> error(timeout) end,
	ok.

shutdown_timeout_on_stream_stop(Config) ->
	doc("Confirm supervised processes are killed "
		"when the shutdown timeout triggers after stopping the stream."),
	Self = self(),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"shutdown_timeout_on_stream_stop">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	%% Receive the pid of the newly started process and monitor it.
	Spawn = receive {Self, Pid, spawned, S} -> S after 1000 -> error(timeout) end,
	MRef = monitor(process, Spawn),
	Spawn ! {Self, ready},
	%% Confirm terminate/3 is called, indicating the stream ended.
	receive {Self, Pid, terminate, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% We should NOT receive a DOWN message immediately.
	receive {'DOWN', MRef, process, Spawn, killed} -> error(killed) after 1500 -> ok end,
	%% We should received it now.
	receive {'DOWN', MRef, process, Spawn, killed} -> ok after 1000 -> error(timeout) end,
	%% The response is still sent.
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, _} = gun:await_body(ConnPid, Ref),
	ok.

shutdown_timeout_on_socket_close(Config) ->
	doc("Confirm supervised processes are killed "
		"when the shutdown timeout triggers after the socket has closed."),
	Self = self(),
	ConnPid = gun_open(Config),
	_ = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"shutdown_timeout_on_socket_close">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	%% Receive the pid of the newly started process and monitor it.
	Spawn = receive {Self, Pid, spawned, S} -> S after 1000 -> error(timeout) end,
	MRef = monitor(process, Spawn),
	Spawn ! {Self, ready},
	%% Close the socket.
	ok = gun:close(ConnPid),
	%% Confirm terminate/3 is called, indicating the stream ended.
	receive {Self, Pid, terminate, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% We should NOT receive a DOWN message immediately.
	receive {'DOWN', MRef, process, Spawn, killed} -> error(killed) after 1500 -> ok end,
	%% We should received it now.
	receive {'DOWN', MRef, process, Spawn, killed} -> ok after 1000 -> error(timeout) end,
	ok.

switch_protocol_after_headers(Config) ->
	case config(protocol, Config) of
		http -> do_switch_protocol_after_response(
			<<"switch_protocol_after_headers">>, Config);
		http2 -> doc("The switch_protocol command is not currently supported for HTTP/2.")
	end.

switch_protocol_after_headers_data(Config) ->
	case config(protocol, Config) of
		http -> do_switch_protocol_after_response(
			<<"switch_protocol_after_headers_data">>, Config);
		http2 -> doc("The switch_protocol command is not currently supported for HTTP/2.")
	end.

switch_protocol_after_response(Config) ->
	case config(protocol, Config) of
		http -> do_switch_protocol_after_response(
			<<"switch_protocol_after_response">>, Config);
		http2 -> doc("The switch_protocol command is not currently supported for HTTP/2.")
	end.

do_switch_protocol_after_response(TestCase, Config) ->
	doc("The 101 informational response must not be sent when a response "
		"has already been sent before the switch_protocol is returned."),
	Self = self(),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, TestCase},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called and receive the response.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	Gzipped =
		lists:keyfind(<<"content-encoding">>, 1, Headers)
			=:= {<<"content-encoding">>, <<"gzip">>},
	case TestCase of
		<<"switch_protocol_after_headers">> ->
			ok;
		_ ->
			<<"{}">> = case gun:await_body(ConnPid, Ref) of
				{ok, Body} when Gzipped ->
					zlib:gunzip(Body);
				{ok, Body} ->
					Body
			end,
			ok
	end,
	{error, _} = gun:await(ConnPid, Ref),
	%% Confirm terminate/3 is called.
	receive {Self, Pid, terminate, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% Confirm takeover/7 is called.
	receive {Self, Pid, takeover, _, _, _, _, _, _, _} -> ok after 1000 -> error(timeout) end,
	ok.

terminate_on_socket_close(Config) ->
	doc("Confirm terminate/3 is called when the socket gets closed brutally."),
	Self = self(),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"terminate_on_socket_close">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called and receive the beginning of the response.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	%% Close the socket.
	ok = gun:close(ConnPid),
	%% Confirm terminate/3 is called.
	receive {Self, Pid, terminate, _, _, _} -> ok after 1000 -> error(timeout) end,
	ok.

terminate_on_stop(Config) ->
	doc("Confirm terminate/3 is called after stop is returned."),
	Self = self(),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"terminate_on_stop">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called and receive the response.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	%% Confirm the stream is still alive even though we
	%% received the response fully, and tell it to stop.
	Pid ! {{Pid, 1}, please_stop},
	receive {Self, Pid, info, _, please_stop, _} -> ok after 1000 -> error(timeout) end,
	%% Confirm terminate/3 is called.
	receive {Self, Pid, terminate, _, _, _} -> ok after 1000 -> error(timeout) end,
	ok.

terminate_on_switch_protocol(Config) ->
	case config(protocol, Config) of
		http -> do_terminate_on_switch_protocol(Config);
		http2 -> doc("The switch_protocol command is not currently supported for HTTP/2.")
	end.

do_terminate_on_switch_protocol(Config) ->
	doc("Confirm terminate/3 is called after switch_protocol is returned."),
	Self = self(),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"terminate_on_switch_protocol">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called and receive the response.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	{inform, 101, _} = gun:await(ConnPid, Ref),
	%% Confirm terminate/3 is called.
	receive {Self, Pid, terminate, _, _, _} -> ok after 1000 -> error(timeout) end,
	%% Confirm takeover/7 is called.
	receive {Self, Pid, takeover, _, _, _, _, _, _, _} -> ok after 1000 -> error(timeout) end,
	ok.
