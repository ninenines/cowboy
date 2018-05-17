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

-module(loop_handler_SUITE).
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

init_per_group(Name, Config) ->
	cowboy_test:init_common_groups(Name, Config, ?MODULE).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

%% Dispatch configuration.

init_dispatch(_) ->
	cowboy_router:compile([{'_', [
		{"/long_polling", long_polling_h, []},
		{"/loop_body", loop_handler_body_h, []},
		{"/loop_timeout", loop_handler_timeout_h, []}
	]}]).

%% Tests.

info_read_body(Config) ->
	doc("Check that a loop handler can read the request body in info/3."),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/loop_body", [{<<"accept-encoding">>, <<"gzip">>}],
		<< 0:100000/unit:8 >>),
	{response, fin, 200, _} = gun:await(ConnPid, Ref),
	ok.

long_polling(Config) ->
	doc("Simple long-polling."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/long_polling", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, fin, 299, _} = gun:await(ConnPid, Ref),
	ok.

long_polling_unread_body(Config) ->
	doc("Long-polling with a body that is not read by the handler."),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/long_polling", [{<<"accept-encoding">>, <<"gzip">>}],
		<< 0:100000/unit:8 >>),
	{response, fin, 299, _} = gun:await(ConnPid, Ref),
	ok.

long_polling_pipeline(Config) ->
	doc("Pipeline of long-polling calls."),
	ConnPid = gun_open(Config),
	Refs = [gun:get(ConnPid, "/long_polling", [{<<"accept-encoding">>, <<"gzip">>}])
		|| _ <- lists:seq(1, 2)],
	_ = [{response, fin, 299, _} = gun:await(ConnPid, Ref) || Ref <- Refs],
	ok.

request_timeout(Config) ->
	doc("Ensure that the request_timeout isn't applied when a request is ongoing."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/loop_timeout", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref, 10000),
	ok.
