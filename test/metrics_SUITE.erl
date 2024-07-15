%% Copyright (c) 2017-2024, Loïc Hoguin <essen@ninenines.eu>
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

suite() ->
	[{timetrap, 30000}].

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
init_per_group(Name = h3, Config) ->
	cowboy_test:init_http3(Name, init_plain_opts(Config), Config);
init_per_group(Name = http_compress, Config) ->
	cowboy_test:init_http(Name, init_compress_opts(Config), Config);
init_per_group(Name = https_compress, Config) ->
	cowboy_test:init_http(Name, init_compress_opts(Config), Config);
init_per_group(Name = h2_compress, Config) ->
	cowboy_test:init_http2(Name, init_compress_opts(Config), Config);
init_per_group(Name = h2c_compress, Config) ->
	Config1 = cowboy_test:init_http(Name, init_compress_opts(Config), Config),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2});
init_per_group(Name = h3_compress, Config) ->
	cowboy_test:init_http3(Name, init_compress_opts(Config), Config).

end_per_group(Name, _) ->
	cowboy_test:stop_group(Name).

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
		{"/set_options/:key", set_options_h, []},
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
	do_get("/", #{}, Config).

user_data(Config) ->
	doc("Confirm user data can be attached to metrics."),
	do_get("/set_options/metrics_user_data", #{handler => set_options_h}, Config).

do_get(Path, UserData, Config) ->
	%% Perform a GET request.
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	{response, nofin, 200, RespHeaders} = gun:await(ConnPid, Ref, infinity),
	{ok, RespBody} = gun:await_body(ConnPid, Ref, infinity),
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
				streamid := StreamID,
				reason := normal, %% @todo Getting h3_no_error here.
				req := #{},
				informational := [],
				user_data := UserData
			} = Metrics,
			do_check_streamid(StreamID, Config),
			%% All good!
			gun:close(ConnPid)
	end.

do_check_streamid(StreamID, Config) ->
	case config(protocol, Config) of
		http -> 1 = StreamID;
		http2 -> 1 = StreamID;
		http3 -> 0 = StreamID
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
	{response, nofin, 200, RespHeaders} = gun:await(ConnPid, Ref, infinity),
	{ok, RespBody} = gun:await_body(ConnPid, Ref, infinity),
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
				streamid := StreamID,
				reason := normal,
				req := #{},
				informational := [],
				user_data := #{}
			} = Metrics,
			do_check_streamid(StreamID, Config),
			%% All good!
			gun:close(ConnPid)
	end.

no_resp_body(Config) ->
	doc("Confirm metrics are correct for a default 204 response to a GET request."),
	%% Perform a GET request.
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/default", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	{response, fin, 204, RespHeaders} = gun:await(ConnPid, Ref, infinity),
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
				streamid := StreamID,
				reason := normal,
				req := #{},
				informational := [],
				user_data := #{}
			} = Metrics,
			do_check_streamid(StreamID, Config),
			%% All good!
			gun:close(ConnPid)
	end.

early_error(Config) ->
	doc("Confirm metrics are correct for an early_error response."),
	%% Perform a malformed GET request.
	ConnPid = gun_open(Config),
	%% We must use different solutions to hit early_error with a stream_error
	%% reason in both protocols.
	{Method, Headers, Status, Error} = case config(protocol, Config) of
		http -> {<<"GET">>, [{<<"host">>, <<"host:port">>}], 400, protocol_error};
		http2 -> {<<"TRACE">>, [], 501, no_error};
		http3 -> {<<"TRACE">>, [], 501, h3_no_error}
	end,
	Ref = gun:request(ConnPid, Method, "/", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-pid">>, pid_to_list(self())}
	|Headers], <<>>),
	{response, fin, Status, RespHeaders} = gun:await(ConnPid, Ref, infinity),
	%% Receive the metrics and validate them.
	receive
		{metrics, From, Metrics} ->
			%% Confirm the metadata is there as expected.
			#{
				ref := _,
				pid := From,
				streamid := StreamID,
				reason := {stream_error, Error, _},
				partial_req := #{},
				resp_status := Status,
				resp_headers := ExpectedRespHeaders,
				early_error_time := _,
				resp_body_length := 0
			} = Metrics,
			do_check_streamid(StreamID, Config),
			ExpectedRespHeaders = maps:from_list(RespHeaders),
			%% All good!
			gun:close(ConnPid)
	end.

early_error_request_line(Config) ->
	case config(protocol, Config) of
		http -> do_early_error_request_line(Config);
		http2 -> doc("There are no request lines in HTTP/2.");
		http3 -> doc("There are no request lines in HTTP/3.")
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
				streamid := StreamID,
				reason := {connection_error, protocol_error, _},
				partial_req := #{},
				resp_status := 400,
				resp_headers := ExpectedRespHeaders,
				early_error_time := _,
				resp_body_length := 0
			} = Metrics,
			do_check_streamid(StreamID, Config),
			ExpectedRespHeaders = maps:from_list(RespHeaders),
			%% All good!
			ok
	end.

%% This test is identical to normal GET except for the handler.
stream_reply(Config) ->
	doc("Confirm metrics are correct for long polling."),
	do_get("/resp/stream_reply2/200", #{}, Config).

ws(Config) ->
	case config(protocol, Config) of
		http -> do_ws(Config);
		%% @todo The test can be implemented for HTTP/2.
		http2 -> doc("It is not currently possible to switch to Websocket over HTTP/2.");
		http3 -> {skip, "Gun does not currently support Websocket over HTTP/3."}
	end.

do_ws(Config) ->
	doc("Confirm metrics are correct when switching to Websocket."),
	ConnPid = gun_open(Config),
	{ok, http} = gun:await_up(ConnPid, infinity),
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
				streamid := StreamID,
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
				}],
				user_data := #{}
			} = Metrics,
			do_check_streamid(StreamID, Config),
			%% All good!
			ok
	end,
	%% And of course the upgrade completed successfully after that.
	receive
		{gun_upgrade, ConnPid, StreamRef, _, _} ->
			ok
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
	Protocol = config(protocol, Config),
	RespHeaders = case gun:await(ConnPid, Ref, infinity) of
		{response, fin, 500, RespHeaders0} ->
			RespHeaders0;
		%% The RST_STREAM arrived before the start of the response.
		%% See maybe_h3_error comment for details.
		{error, {stream_error, {stream_error, h3_internal_error, _}}} when Protocol =:= http3 ->
			unknown
	end,
	timer:sleep(100),
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
			case RespHeaders of
				%% The HTTP/3 stream has reset too early so we can't
				%% verify the response headers.
				unknown ->
					ok;
				_ ->
					ExpectedRespHeaders = maps:from_list(RespHeaders)
			end,
			%% The request process executed normally.
			#{procs := Procs} = Metrics,
			[{_, #{
				spawn := ProcSpawn,
				exit := ProcExit,
				reason := {crash, StackTrace}
			}}] = maps:to_list(Procs),
			true = ProcSpawn =< ProcExit,
			%% Confirm other metadata are as expected.
			#{
				ref := _,
				pid := From,
				streamid := StreamID,
				reason := {internal_error, {'EXIT', _Pid, {crash, StackTrace}}, 'Stream process crashed.'},
				req := #{},
				informational := [],
				user_data := #{}
			} = Metrics,
			do_check_streamid(StreamID, Config),
			%% All good!
			gun:close(ConnPid)
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
	Protocol = config(protocol, Config),
	RespHeaders = case gun:await(ConnPid, Ref, infinity) of
		{response, fin, 200, RespHeaders0} ->
			RespHeaders0;
		%% The RST_STREAM arrived before the start of the response.
		%% See maybe_h3_error comment for details.
		{error, {stream_error, {stream_error, h3_internal_error, _}}} when Protocol =:= http3 ->
			unknown
	end,
	timer:sleep(100),
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
			case RespHeaders of
				%% The HTTP/3 stream has reset too early so we can't
				%% verify the response headers.
				unknown ->
					ok;
				_ ->
					ExpectedRespHeaders = maps:from_list(RespHeaders)
			end,
			%% The request process executed normally.
			#{procs := Procs} = Metrics,
			[{_, #{
				spawn := ProcSpawn,
				exit := ProcExit,
				reason := {crash, StackTrace}
			}}] = maps:to_list(Procs),
			true = ProcSpawn =< ProcExit,
			%% Confirm other metadata are as expected.
			#{
				ref := _,
				pid := From,
				streamid := StreamID,
				reason := {internal_error, {'EXIT', _Pid, {crash, StackTrace}}, 'Stream process crashed.'},
				req := #{},
				informational := [],
				user_data := #{}
			} = Metrics,
			do_check_streamid(StreamID, Config),
			%% All good!
			gun:close(ConnPid)
	end.
