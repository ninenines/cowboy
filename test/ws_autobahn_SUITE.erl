%% Copyright (c) 2011-2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(ws_autobahn_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

%% ct.

all() ->
	[{group, autobahn}].

groups() ->
	[{autobahn, [], [autobahn_fuzzingclient]}].

init_per_group(Name, Config) ->
	%% Some systems have it named pip2.
	Out = os:cmd("pip show autobahntestsuite ; pip2 show autobahntestsuite"),
	case string:str(Out, "autobahntestsuite") of
		0 ->
			ct:print("Skipping the autobahn group because the "
				"Autobahn Test Suite is not installed.~nTo install it, "
				"please follow the instructions on this page:~n~n    "
				"http://autobahn.ws/testsuite/installation.html"),
			{skip, "Autobahn Test Suite not installed."};
		_ ->
			{ok, _} = cowboy:start_clear(Name, [{port, 33080}], #{
				env => #{dispatch => init_dispatch()}
			}),
			Config
	end.

end_per_group(Listener, _Config) ->
	cowboy:stop_listener(Listener).

%% Dispatch configuration.

init_dispatch() ->
	cowboy_router:compile([
		{"localhost", [
			{"/ws_echo", ws_echo, []}
		]}
	]).

%% Tests.

autobahn_fuzzingclient(Config) ->
	doc("Autobahn test suite for the Websocket protocol."),
	Self = self(),
	spawn_link(fun() -> do_start_port(Config, Self) end),
	receive autobahn_exit -> ok end,
	ct:log("<h2><a href=\"log_private/reports/servers/index.html\">Full report</a></h2>~n"),
	Report = config(priv_dir, Config) ++ "reports/servers/index.html",
	ct:print("Autobahn Test Suite report: file://~s~n", [Report]),
	{ok, HTML} = file:read_file(Report),
	case length(binary:matches(HTML, <<"case_failed">>)) > 2 of
		true -> error(failed);
		false -> ok
	end.

do_start_port(Config, Pid) ->
	Port = open_port({spawn, "wstest -m fuzzingclient -s " ++ config(data_dir, Config) ++ "client.json"},
		[{line, 10000}, {cd, config(priv_dir, Config)}, binary, eof]),
	do_receive_infinity(Port, Pid).

do_receive_infinity(Port, Pid) ->
	receive
		{Port, {data, {eol, Line}}} ->
			io:format(user, "~s~n", [Line]),
			do_receive_infinity(Port, Pid);
		{Port, eof} ->
			Pid ! autobahn_exit
	end.
