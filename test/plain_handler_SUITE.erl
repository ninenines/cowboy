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

-module(plain_handler_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

%% ct.

all() ->
	cowboy_test:common_all().

groups() ->
	cowboy_test:common_groups(ct_helper:all(?MODULE)).

init_per_suite(Config) ->
	ct_helper:create_static_dir(config(priv_dir, Config) ++ "/static"),
	Config.

end_per_suite(Config) ->
	ct_helper:delete_static_dir(config(priv_dir, Config) ++ "/static").

init_per_group(Name, Config) ->
	cowboy_test:init_common_groups(Name, Config, ?MODULE).

end_per_group(Name, _) ->
	cowboy_test:stop_group(Name).

%% Routes.

init_dispatch(_) ->
	cowboy_router:compile([{"localhost", [
		{"/crash/external_exit", crash_h, external_exit},
		{"/crash/no_reply", crash_h, no_reply},
		{"/crash/reply", crash_h, reply}
	]}]).

%% Tests.

crash_after_reply(Config) ->
	doc("A plain handler crash after a response was sent "
		"results in no 500 response."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/crash/reply", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	Protocol = config(protocol, Config),
	_ = case gun:await(ConnPid, Ref) of
		{response, fin, 200, _} ->
			{error, timeout} = gun:await(ConnPid, Ref, 1000);
		%% See maybe_h3_error comment for details.
		{error, {stream_error, {stream_error, h3_internal_error, _}}}
				when Protocol =:= http3 ->
			ok
	end,
	gun:close(ConnPid).

crash_before_reply(Config) ->
	doc("A plain handler crash before a response was sent "
		"results in a 500 response."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/crash/no_reply", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, fin, 500, _} = gun:await(ConnPid, Ref),
	gun:close(ConnPid).

external_exit_before_reply(Config) ->
	doc("A plain handler exits externally before a response was sent "
		"results in a 500 response."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/crash/external_exit", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, fin, 500, _} = gun:await(ConnPid, Ref),
	gun:close(ConnPid).
