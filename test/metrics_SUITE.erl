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

-module(metrics_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [gun_down/1]).
-import(cowboy_test, [raw_open/1]).
-import(cowboy_test, [raw_send/2]).
-import(cowboy_test, [raw_recv_head/1]).

%% ct.

all() ->
	cowboy_test:common_all().

groups() ->
	cowboy_test:common_groups(ct_helper:all(?MODULE)).

init_per_group(Name = http, Config) ->
	cowboy_test:init_http(Name, init_plain_opts(Config), Config);
init_per_group(Name = https, Config) ->
	cowboy_test:init_http(Name, init_plain_opts(Config), Config);
init_per_group(Name = h2, Config) ->
	cowboy_test:init_http2(Name, init_plain_opts(Config), Config);
init_per_group(Name = h2c, Config) ->
	Config1 = cowboy_test:init_http(Name, init_plain_opts(Config), Config),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2});
init_per_group(Name = http_compress, Config) ->
	cowboy_test:init_http(Name, init_compress_opts(Config), Config);
init_per_group(Name = https_compress, Config) ->
	cowboy_test:init_http(Name, init_compress_opts(Config), Config);
init_per_group(Name = h2_compress, Config) ->
	cowboy_test:init_http2(Name, init_compress_opts(Config), Config);
init_per_group(Name = h2c_compress, Config) ->
	Config1 = cowboy_test:init_http(Name, init_compress_opts(Config), Config),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2}).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

init_plain_opts(Config) ->
	#{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))},
		metrics_callback => do_metrics_callback(),
		stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
	}.

init_compress_opts(Config) ->
	#{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))},
		metrics_callback => do_metrics_callback(),
		stream_handlers => [cowboy_metrics_h, cowboy_compress_h, cowboy_stream_h]
	}.

init_routes(_) -> [
	{"localhost", [
		{"/", hello_h, []},
		{"/crash/no_reply", crash_h, no_reply},
		{"/crash/reply", crash_h, reply},
		{"/default", default_h, []},
		{"/full/:key", echo_h, []},
		{"/resp/:key[/:arg]", resp_h, []},
		{"/ws_echo", ws_echo, []}
	]}
].

do_metrics_callback() ->
	fun(Metrics) ->
		Pid = case Metrics of
			#{req := #{headers := #{<<"x-test-pid">> := P}}} ->
				list_to_pid(binary_to_list(P));
			#{partial_req := #{headers := #{<<"x-test-pid">> := P}}} ->
				list_to_pid(binary_to_list(P));
			_ ->
				whereis(early_error_metrics)
		end,
		Pid ! {metrics, self(), Metrics},
		ok
	end.

%% Tests.

hello_world(Config) ->
	doc("Confirm metrics are correct for a normal GET request."),
	do_get("/", Config).

do_get(Path, Config) ->
	%% Perform a GET request.
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	{response, nofin, 200, RespHeaders} = gun:await(ConnPid, Ref),
	{ok, RespBody} = gun:await_body(ConnPid, Ref),
	gun:close(ConnPid),
	%% Receive the metrics and validate them.
	receive
		{metrics, From, Metrics} ->
			%% Ensure the timestamps are in the expected order.
			#{
				req_start := ReqStart, req_end := ReqEnd,
				resp_start := RespStart, resp_end := RespEnd
			} = Metrics,
			true = (ReqStart =< RespStart)
				and (RespStart =< RespEnd)
				and (RespEnd =< ReqEnd),
			%% We didn't send a body.
			#{
				req_body_start := undefined,
				req_body_end := undefined,
				req_body_length := 0
			} = Metrics,
			%% We got a 200 response with a body.
			#{
				resp_status := 200,
				resp_headers := ExpectedRespHeaders,
				resp_body_length := RespBodyLen
			} = Metrics,
			%% The transfer-encoding header is hidden from stream handlers.
			ExpectedRespHeaders = maps:remove(<<"transfer-encoding">>,
				maps:from_list(RespHeaders)),
			true = byte_size(RespBody) > 0,
			true = RespBodyLen > 0,
			%% The request process executed normally.
			#{procs := Procs} = Metrics,
			[{_, #{
				spawn := ProcSpawn,
				exit := ProcExit,
				reason := normal
			}}] = maps:to_list(Procs),
			true = ProcSpawn =< ProcExit,
			%% Confirm other metadata are as expected.
			#{
				ref := _,
				pid := From,
				streamid := 1,
				reason := normal,
				req := #{},
				informational := []
			} = Metrics,
			%% All good!
			ok
	after 1000 ->
		error(timeout)
	end.

post_body(Config) ->
	doc("Confirm metrics are correct for a normal POST request."),
	%% Perform a POST request.
	ConnPid = gun_open(Config),
	Body = <<0:8000000>>,
	Ref = gun:post(ConnPid, "/full/read_body", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-pid">>, pid_to_list(self())}
	], Body),
	{response, nofin, 200, RespHeaders} = gun:await(ConnPid, Ref),
	{ok, RespBody} = gun:await_body(ConnPid, Ref),
	gun:close(ConnPid),
	%% Receive the metrics and validate them.
	receive
		{metrics, From, Metrics} ->
			%% Ensure the timestamps are in the expected order.
			#{
				req_start := ReqStart, req_end := ReqEnd,
				resp_start := RespStart, resp_end := RespEnd
			} = Metrics,
			true = (ReqStart =< RespStart)
				and (RespStart =< RespEnd)
				and (RespEnd =< ReqEnd),
			%% We didn't send a body.
			#{
				req_body_start := ReqBodyStart,
				req_body_end := ReqBodyEnd,
				req_body_length := ReqBodyLen
			} = Metrics,
			true = ReqBodyStart =< ReqBodyEnd,
			ReqBodyLen = byte_size(Body),
			%% We got a 200 response with a body.
			#{
				resp_status := 200,
				resp_headers := ExpectedRespHeaders,
				resp_body_length := RespBodyLen
			} = Metrics,
			ExpectedRespHeaders = maps:from_list(RespHeaders),
			true = byte_size(RespBody) > 0,
			true = RespBodyLen > 0,
			%% The request process executed normally.
			#{procs := Procs} = Metrics,
			[{_, #{
				spawn := ProcSpawn,
				exit := ProcExit,
				reason := normal
			}}] = maps:to_list(Procs),
			true = ProcSpawn =< ProcExit,
			%% Confirm other metadata are as expected.
			#{
				ref := _,
				pid := From,
				streamid := 1,
				reason := normal,
				req := #{},
				informational := []
			} = Metrics,
			%% All good!
			ok
	after 1000 ->
		error(timeout)
	end.

no_resp_body(Config) ->
	doc("Confirm metrics are correct for a default 204 response to a GET request."),
	%% Perform a GET request.
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/default", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	{response, fin, 204, RespHeaders} = gun:await(ConnPid, Ref),
	gun:close(ConnPid),
	%% Receive the metrics and validate them.
	receive
		{metrics, From, Metrics} ->
			%% Ensure the timestamps are in the expected order.
			#{
				req_start := ReqStart, req_end := ReqEnd,
				resp_start := RespStart, resp_end := RespEnd
			} = Metrics,
			true = (ReqStart =< RespStart)
				and (RespStart =< RespEnd)
				and (RespEnd =< ReqEnd),
			%% We didn't send a body.
			#{
				req_body_start := undefined,
				req_body_end := undefined,
				req_body_length := 0
			} = Metrics,
			%% We got a 200 response with a body.
			#{
				resp_status := 204,
				resp_headers := ExpectedRespHeaders,
				resp_body_length := 0
			} = Metrics,
			ExpectedRespHeaders = maps:from_list(RespHeaders),
			%% The request process executed normally.
			#{procs := Procs} = Metrics,
			[{_, #{
				spawn := ProcSpawn,
				exit := ProcExit,
				reason := normal
			}}] = maps:to_list(Procs),
			true = ProcSpawn =< ProcExit,
			%% Confirm other metadata are as expected.
			#{
				ref := _,
				pid := From,
				streamid := 1,
				reason := normal,
				req := #{},
				informational := []
			} = Metrics,
			%% All good!
			ok
	after 1000 ->
		error(timeout)
	end.

early_error(Config) ->
	case config(protocol, Config) of
		http -> do_early_error(Config);
		http2 -> doc("The callback early_error/5 is not currently used for HTTP/2.")
	end.

do_early_error(Config) ->
	doc("Confirm metrics are correct for an early_error response."),
	%% Perform a malformed GET request.
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"host">>, <<"host:port">>},
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	{response, fin, 400, RespHeaders} = gun:await(ConnPid, Ref),
	gun:close(ConnPid),
	%% Receive the metrics and validate them.
	receive
		{metrics, From, Metrics} ->
			%% Confirm the metadata is there as expected.
			#{
				ref := _,
				pid := From,
				streamid := 1,
				reason := {stream_error, 1, protocol_error, _},
				partial_req := #{},
				resp_status := 400,
				resp_headers := ExpectedRespHeaders,
				early_error_time := _,
				resp_body_length := 0
			} = Metrics,
			ExpectedRespHeaders = maps:from_list(RespHeaders),
			%% All good!
			ok
	after 1000 ->
		error(timeout)
	end.

early_error_request_line(Config) ->
	case config(protocol, Config) of
		http -> do_early_error_request_line(Config);
		http2 -> doc("The callback early_error/5 is not currently used for HTTP/2.")
	end.

do_early_error_request_line(Config) ->
	doc("Confirm metrics are correct for an early_error response "
		"that occurred on the request-line."),
	%% Register the process in order to receive the metrics event.
	register(early_error_metrics, self()),
	%% Send a malformed request-line.
	Client = raw_open(Config),
	ok = raw_send(Client, <<"FOO bar\r\n">>),
	{'HTTP/1.1', 400, _, Rest} = cow_http:parse_status_line(raw_recv_head(Client)),
	{RespHeaders, _} = cow_http:parse_headers(Rest),
	%% Receive the metrics and validate them.
	receive
		{metrics, From, Metrics} ->
			%% Confirm the metadata is there as expected.
			#{
				ref := _,
				pid := From,
				streamid := 1,
				reason := {connection_error, protocol_error, _},
				partial_req := #{},
				resp_status := 400,
				resp_headers := ExpectedRespHeaders,
				early_error_time := _,
				resp_body_length := 0
			} = Metrics,
			ExpectedRespHeaders = maps:from_list(RespHeaders),
			%% All good!
			ok
	after 1000 ->
		error(timeout)
	end.

%% This test is identical to normal GET except for the handler.
stream_reply(Config) ->
	doc("Confirm metrics are correct for long polling."),
	do_get("/resp/stream_reply2/200", Config).

ws(Config) ->
	case config(protocol, Config) of
		http -> do_ws(Config);
		http2 -> doc("It is not currently possible to switch to Websocket over HTTP/2.")
	end.

do_ws(Config) ->
	doc("Confirm metrics are correct when switching to Websocket."),
	ConnPid = gun_open(Config),
	{ok, http} = gun:await_up(ConnPid),
	StreamRef = gun:ws_upgrade(ConnPid, "/ws_echo", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	receive
		{metrics, From, Metrics} ->
			%% Ensure the timestamps are in the expected order.
			#{
				req_start := ReqStart,
				req_end := ReqEnd
			} = Metrics,
			true = ReqStart =< ReqEnd,
			%% We didn't send a body.
			#{
				req_body_start := undefined,
				req_body_end := undefined,
				req_body_length := 0
			} = Metrics,
			%% We didn't send a response.
			#{
				resp_start := undefined,
				resp_end := undefined,
				resp_status := undefined,
				resp_headers := undefined,
				resp_body_length := 0
			} = Metrics,
			%% The request process may not have terminated before terminate
			%% is called. We therefore only check when it spawned.
			#{procs := Procs} = Metrics,
			[{_, #{
				spawn := _
			}}] = maps:to_list(Procs),
			%% Confirm other metadata are as expected.
			#{
				ref := _,
				pid := From,
				streamid := 1,
				reason := switch_protocol,
				req := #{},
				%% A 101 upgrade response was sent.
				informational := [#{
					status := 101,
					headers := #{
						<<"connection">> := <<"Upgrade">>,
						<<"upgrade">> := <<"websocket">>,
						<<"sec-websocket-accept">> := _
					},
					time := _
				}]
			} = Metrics,
			%% All good!
			ok
	after 1000 ->
		error(timeout)
	end,
	%% And of course the upgrade completed successfully after that.
	receive
		{gun_upgrade, ConnPid, StreamRef, _, _} ->
			ok
	after 1000 ->
		error(timeout)
	end,
	gun:close(ConnPid).

error_response(Config) ->
	doc("Confirm metrics are correct when an error_response command is returned."),
	%% Perform a GET request.
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/crash/no_reply", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	{response, fin, 500, RespHeaders} = gun:await(ConnPid, Ref),
	timer:sleep(100),
	gun:close(ConnPid),
	%% Receive the metrics and validate them.
	receive
		{metrics, From, Metrics} ->
			%% Ensure the timestamps are in the expected order.
			#{
				req_start := ReqStart, req_end := ReqEnd,
				resp_start := RespStart, resp_end := RespEnd
			} = Metrics,
			true = (ReqStart =< RespStart)
				and (RespStart =< RespEnd)
				and (RespEnd =< ReqEnd),
			%% We didn't send a body.
			#{
				req_body_start := undefined,
				req_body_end := undefined,
				req_body_length := 0
			} = Metrics,
			%% We got a 500 response without a body.
			#{
				resp_status := 500,
				resp_headers := ExpectedRespHeaders,
				resp_body_length := 0
			} = Metrics,
			ExpectedRespHeaders = maps:from_list(RespHeaders),
			%% The request process executed normally.
			#{procs := Procs} = Metrics,
			[{_, #{
				spawn := ProcSpawn,
				exit := ProcExit,
				reason := {crash, _StackTrace}
			}}] = maps:to_list(Procs),
			true = ProcSpawn =< ProcExit,
			%% Confirm other metadata are as expected.
			#{
				ref := _,
				pid := From,
				streamid := 1,
				reason := {internal_error, {'EXIT', _Pid, {crash, _StackTrace}}, 'Stream process crashed.'},
				req := #{},
				informational := []
			} = Metrics,
			%% All good!
			ok
	after 1000 ->
		error(timeout)
	end.

error_response_after_reply(Config) ->
	doc("Confirm metrics are correct when an error_response command is returned "
		"after a response was sent."),
	%% Perform a GET request.
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/crash/reply", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	{response, fin, 200, RespHeaders} = gun:await(ConnPid, Ref),
	timer:sleep(100),
	gun:close(ConnPid),
	%% Receive the metrics and validate them.
	receive
		{metrics, From, Metrics} ->
			%% Ensure the timestamps are in the expected order.
			#{
				req_start := ReqStart, req_end := ReqEnd,
				resp_start := RespStart, resp_end := RespEnd
			} = Metrics,
			true = (ReqStart =< RespStart)
				and (RespStart =< RespEnd)
				and (RespEnd =< ReqEnd),
			%% We didn't send a body.
			#{
				req_body_start := undefined,
				req_body_end := undefined,
				req_body_length := 0
			} = Metrics,
			%% We got a 200 response without a body.
			#{
				resp_status := 200,
				resp_headers := ExpectedRespHeaders,
				resp_body_length := 0
			} = Metrics,
			ExpectedRespHeaders = maps:from_list(RespHeaders),
			%% The request process executed normally.
			#{procs := Procs} = Metrics,
			[{_, #{
				spawn := ProcSpawn,
				exit := ProcExit,
				reason := {crash, _StackTrace}
			}}] = maps:to_list(Procs),
			true = ProcSpawn =< ProcExit,
			%% Confirm other metadata are as expected.
			#{
				ref := _,
				pid := From,
				streamid := 1,
				reason := {internal_error, {'EXIT', _Pid, {crash, _StackTrace}}, 'Stream process crashed.'},
				req := #{},
				informational := []
			} = Metrics,
			%% All good!
			ok
	after 1000 ->
		error(timeout)
	end.
