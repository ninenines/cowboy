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

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [gun_down/1]).

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
	cowboy_test:init_http(Name, init_plain_opts(Config), Config);
init_per_group(Name = h2c, Config) ->
	Config1 = cowboy_test:init_http(Name, init_plain_opts(Config), Config),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2});
init_per_group(Name = http_compress, Config) ->
	cowboy_test:init_http(Name, init_compress_opts(Config), Config);
init_per_group(Name = https_compress, Config) ->
	cowboy_test:init_http(Name, init_compress_opts(Config), Config);
init_per_group(Name = h2_compress, Config) ->
	cowboy_test:init_http(Name, init_compress_opts(Config), Config);
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
		{"/", hello_h, []}
	]}
].

do_metrics_callback() ->
	fun(Metrics=#{req := #{headers := #{<<"x-test-pid">> := PidBin}}}) ->
		Pid = list_to_pid(binary_to_list(PidBin)),
		Pid ! {metrics, self(), Metrics},
		ok
	end.

%% Tests.

hello_world(Config) ->
	%% Perform a request.
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/", [{<<"x-test-pid">>, pid_to_list(self())}]),
	{response, nofin, 200, RespHeaders} = gun:await(ConnPid, Ref),
	{ok, RespBody} = gun:await_body(ConnPid, Ref),
	gun:close(ConnPid),
	%% Receive the metrics and print them.
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
			ExpectedRespHeaders = maps:from_list(RespHeaders),
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
				req := #{}
			} = Metrics,
			%% All good!
			ok
	after 1000 ->
		error(timeout)
	end.
