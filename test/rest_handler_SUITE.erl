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

-module(rest_handler_SUITE).
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
		{"/switch_handler", switch_handler_h, run},
		{"/switch_handler_opts", switch_handler_h, hibernate}
	]}]).

%% Internal.

do_decode(Headers, Body) ->
	case lists:keyfind(<<"content-encoding">>, 1, Headers) of
		{_, <<"gzip">>} -> zlib:gunzip(Body);
		_ -> Body
	end.

%% Tests.

switch_handler(Config) ->
	doc("Switch REST to loop handler for streaming the response body."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/switch_handler", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	<<"Hello\nstreamed\nworld!\n">> = do_decode(Headers, Body),
	ok.

switch_handler_opts(Config) ->
	doc("Switch REST to loop handler for streaming the response body; with options."),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/switch_handler_opts", [{<<"accept-encoding">>, <<"gzip">>}]),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	<<"Hello\nstreamed\nworld!\n">> = do_decode(Headers, Body),
	ok.
