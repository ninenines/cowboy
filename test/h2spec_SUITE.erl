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

-module(h2spec_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

%% ct.

all() ->
	[h2spec].

init_per_suite(Config) ->
	case os:getenv("H2SPEC") of
		false ->
			skip;
		_ ->
			cowboy_test:init_http(h2spec, #{
				env => #{dispatch => init_dispatch()},
				max_concurrent_streams => 100
			}, Config)
	end.

end_per_suite(_Config) ->
	cowboy:stop_listener(h2spec).

%% Dispatch configuration.

init_dispatch() ->
	cowboy_router:compile([
		{'_', [
			{"/", delay_hello_h, 50}
		]}
	]).

%% Tests.

h2spec(Config) ->
	doc("h2spec test suite for the HTTP/2 protocol."),
	Self = self(),
	spawn_link(fun() -> start_port(Config, Self) end),
	receive
		{h2spec_exit, 0, Log} ->
			ct:log("~ts", [Log]),
			ok;
		{h2spec_exit, Status, Log} ->
			ct:log("~ts", [Log]),
			error({exit_status, Status})
	end.

start_port(Config, Pid) ->
	H2spec = os:getenv("H2SPEC"),
	ListenPort = config(port, Config),
	Port = open_port(
		{spawn, H2spec ++ " -S -p "
			++ integer_to_list(ListenPort)},
		[{line, 10000}, {cd, config(priv_dir, Config)}, binary, exit_status]),
	receive_infinity(Port, Pid, []).

receive_infinity(Port, Pid, Acc) ->
	receive
		{Port, {data, {eol, Line}}} ->
			io:format(user, "~s~n", [Line]),
			receive_infinity(Port, Pid, [Line|Acc]);
		{Port, {exit_status, Status}} ->
			Pid ! {h2spec_exit, Status, [[L, $\n] || L <- lists:reverse(Acc)]}
	end.
