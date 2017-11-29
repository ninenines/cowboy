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

-module(tracer_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [gun_down/1]).

%% ct.

%% We initialize trace patterns here. Appropriate would be in
%% init_per_suite/1, but this works just as well.
all() ->
	case code:is_module_native(?MODULE) of
		true ->
			{skip, "The Cowboy tracer is not compatible with native code."};
		false ->
			cowboy_tracer_h:set_trace_patterns(),
			cowboy_test:common_all()
	end.

%% We want tests for each group to execute sequentially
%% because we need to modify the protocol options. Groups
%% can run in parallel however.
groups() ->
	Tests = ct_helper:all(?MODULE),
	[
		{http, [], Tests},
		{https, [], Tests},
		{h2, [], Tests},
		{h2c, [], Tests},
		{http_compress, [], Tests},
		{https_compress, [], Tests},
		{h2_compress, [], Tests},
		{h2c_compress, [], Tests}
	].

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
		stream_handlers => [cowboy_tracer_h, cowboy_stream_h]
	}.

init_compress_opts(Config) ->
	#{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))},
		stream_handlers => [cowboy_tracer_h, cowboy_compress_h, cowboy_stream_h]
	}.

init_routes(_) -> [
	{"localhost", [
		{"/", hello_h, []},
		{"/longer/hello/path", hello_h, []}
	]}
].

do_get(Path, Config) ->
	%% Perform a GET request.
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	{response, nofin, 200, _Headers} = gun:await(ConnPid, Ref),
	{ok, _Body} = gun:await_body(ConnPid, Ref),
	gun:close(ConnPid).

%% We only care about cowboy_req:reply/4 calls and init/terminate events.
do_tracer_callback(Pid) ->
	fun
		(Event, _) when Event =:= init; Event =:= terminate ->
			Pid ! Event,
			0;
		(Event={trace_ts, _, call, {cowboy_req, reply, _}, _}, State) ->
			Pid ! Event,
			Pid ! {state, State},
			State + 1;
		(_, State) ->
			State + 1
	end.

%% Tests.

init(Config) ->
	doc("Ensure the init event is triggered."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [fun(_,_,_) -> true end]
	}),
	do_get("/", Config),
	receive
		init ->
			ok
	after 100 ->
		error(timeout)
	end.

terminate(Config) ->
	doc("Ensure the terminate event is triggered."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [fun(_,_,_) -> true end]
	}),
	do_get("/", Config),
	receive
		terminate ->
			ok
	after 100 ->
		error(timeout)
	end.

state(Config) ->
	doc("Ensure the returned state is used."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [fun(_,_,_) -> true end]
	}),
	do_get("/", Config),
	receive
		{state, St} ->
			true = St > 0,
			ok
	after 100 ->
		error(timeout)
	end.

empty(Config) ->
	doc("Empty match specs unconditionally enable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => []
	}),
	do_get("/", Config),
	receive
		{trace_ts, _, call, {cowboy_req, reply, [200, _, _, _]}, _} ->
			ok
	after 100 ->
		error(timeout)
	end.

predicate_true(Config) ->
	doc("Predicate function returns true, unconditionally enable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [fun(_,_,_) -> true end]
	}),
	do_get("/", Config),
	receive
		{trace_ts, _, call, {cowboy_req, reply, [200, _, _, _]}, _} ->
			ok
	after 100 ->
		error(timeout)
	end.

predicate_false(Config) ->
	doc("Predicate function returns false, unconditionally disable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [fun(_,_,_) -> false end]
	}),
	do_get("/", Config),
	receive
		Msg when element(1, Msg) =:= trace_ts ->
			error(Msg)
	after 100 ->
		ok
	end.

method(Config) ->
	doc("Method is the same as the request's, enable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [{method, <<"GET">>}]
	}),
	do_get("/", Config),
	receive
		{trace_ts, _, call, {cowboy_req, reply, [200, _, _, _]}, _} ->
			ok
	after 100 ->
		error(timeout)
	end.

method_no_match(Config) ->
	doc("Method is different from the request's, disable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [{method, <<"POST">>}]
	}),
	do_get("/", Config),
	receive
		Msg when element(1, Msg) =:= trace_ts ->
			error(Msg)
	after 100 ->
		ok
	end.

host(Config) ->
	doc("Host is the same as the request's, enable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [{host, <<"localhost">>}]
	}),
	do_get("/", Config),
	receive
		{trace_ts, _, call, {cowboy_req, reply, [200, _, _, _]}, _} ->
			ok
	after 100 ->
		error(timeout)
	end.

host_no_match(Config) ->
	doc("Host is different from the request's, disable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [{host, <<"ninenines.eu">>}]
	}),
	do_get("/", Config),
	receive
		Msg when element(1, Msg) =:= trace_ts ->
			error(Msg)
	after 100 ->
		ok
	end.

path(Config) ->
	doc("Path is the same as the request's, enable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [{path, <<"/longer/hello/path">>}]
	}),
	do_get("/longer/hello/path", Config),
	receive
		{trace_ts, _, call, {cowboy_req, reply, [200, _, _, _]}, _} ->
			ok
	after 100 ->
		error(timeout)
	end.

path_no_match(Config) ->
	doc("Path is different from the request's, disable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [{path, <<"/some/other/path">>}]
	}),
	do_get("/longer/hello/path", Config),
	receive
		Msg when element(1, Msg) =:= trace_ts ->
			error(Msg)
	after 100 ->
		ok
	end.

path_start(Config) ->
	doc("Start of path is the same as request's, enable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [{path_start, <<"/longer/hello">>}]
	}),
	do_get("/longer/hello/path", Config),
	receive
		{trace_ts, _, call, {cowboy_req, reply, [200, _, _, _]}, _} ->
			ok
	after 100 ->
		error(timeout)
	end.

path_start_no_match(Config) ->
	doc("Start of path is different from the request's, disable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [{path_start, <<"/shorter/hello">>}]
	}),
	do_get("/longer/hello/path", Config),
	receive
		Msg when element(1, Msg) =:= trace_ts ->
			error(Msg)
	after 100 ->
		ok
	end.

header_defined(Config) ->
	doc("Header is defined in the request, enable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [{header, <<"accept-encoding">>}]
	}),
	do_get("/", Config),
	receive
		{trace_ts, _, call, {cowboy_req, reply, [200, _, _, _]}, _} ->
			ok
	after 100 ->
		error(timeout)
	end.

header_defined_no_match(Config) ->
	doc("Header is not defined in the request, disable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [{header, <<"accept-language">>}]
	}),
	do_get("/", Config),
	receive
		Msg when element(1, Msg) =:= trace_ts ->
			error(Msg)
	after 100 ->
		ok
	end.

header_value(Config) ->
	doc("Header value is the same as the request's, enable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [{header, <<"accept-encoding">>, <<"gzip">>}]
	}),
	do_get("/", Config),
	receive
		{trace_ts, _, call, {cowboy_req, reply, [200, _, _, _]}, _} ->
			ok
	after 100 ->
		error(timeout)
	end.

header_value_no_match(Config) ->
	doc("Header value is different from the request's, disable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [{header, <<"accept-encoding">>, <<"nope">>}]
	}),
	do_get("/", Config),
	receive
		Msg when element(1, Msg) =:= trace_ts ->
			error(Msg)
	after 100 ->
		ok
	end.

peer_ip(Config) ->
	doc("Peer IP is the same as the request's, enable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [{peer_ip, {127, 0, 0, 1}}]
	}),
	do_get("/", Config),
	receive
		{trace_ts, _, call, {cowboy_req, reply, [200, _, _, _]}, _} ->
			ok
	after 100 ->
		error(timeout)
	end.

peer_ip_no_match(Config) ->
	doc("Peer IP is different from the request's, disable tracing."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [{peer_ip, {8, 8, 8, 8}}]
	}),
	do_get("/", Config),
	receive
		Msg when element(1, Msg) =:= trace_ts ->
			error(Msg)
	after 100 ->
		ok
	end.

missing_callback(Config) ->
	doc("Ensure the request is still processed if the callback is not provided."),
	Ref = config(ref, Config),
	Opts0 = ranch:get_protocol_options(Ref),
	Opts = maps:remove(tracer_callback, Opts0),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_match_specs => [{method, <<"GET">>}]
	}),
	do_get("/", Config),
	receive
		Msg when element(1, Msg) =:= trace_ts ->
			error(Msg)
	after 100 ->
		ok
	end.

missing_match_specs(Config) ->
	doc("Ensure the request is still processed if match specs are not provided."),
	Ref = config(ref, Config),
	Opts0 = ranch:get_protocol_options(Ref),
	Opts = maps:remove(tracer_match_specs, Opts0),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self())
	}),
	do_get("/", Config),
	receive
		Msg when element(1, Msg) =:= trace_ts ->
			error(Msg)
	after 100 ->
		ok
	end.

two_matching_requests(Config) ->
	doc("Perform two requests that enable tracing on the same connection."),
	Ref = config(ref, Config),
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		tracer_callback => do_tracer_callback(self()),
		tracer_match_specs => [fun(_,_,_) -> true end]
	}),
	%% Perform a GET request.
	ConnPid = gun_open(Config),
	Ref1 = gun:get(ConnPid, "/", []),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref1),
	{ok, _} = gun:await_body(ConnPid, Ref1),
	receive
		{trace_ts, _, call, {cowboy_req, reply, [200, _, _, _]}, _} ->
			ok
	after 100 ->
		error(timeout)
	end,
	%% Perform a second GET request on the same connection.
	Ref2 = gun:get(ConnPid, "/", []),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref2),
	{ok, _} = gun:await_body(ConnPid, Ref2),
	receive
		{trace_ts, _, call, {cowboy_req, reply, [200, _, _, _]}, _} ->
			ok
	after 100 ->
		error(timeout)
	end,
	gun:close(ConnPid).
