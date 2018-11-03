%% Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(rfc6585_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

all() ->
	cowboy_test:common_all().

groups() ->
	cowboy_test:common_groups(ct_helper:all(?MODULE)).

init_per_group(Name, Config) ->
	cowboy_test:init_common_groups(Name, Config, ?MODULE).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

init_dispatch(_) ->
	cowboy_router:compile([{"[...]", [
		{"/resp/:key[/:arg]", resp_h, []}
	]}]).

status_code_428(Config) ->
	doc("The 428 Precondition Required status code can be sent. (RFC6585 3)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/428", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 428, _} = gun:await(ConnPid, Ref),
	ok.

status_code_429(Config) ->
	doc("The 429 Too Many Requests status code can be sent. (RFC6585 4)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/429", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 429, _} = gun:await(ConnPid, Ref),
	ok.

%% @todo
%   The (429) response MAY include a Retry-After header indicating how long
%   to wait before making a new request. (RFC6585 4)

status_code_431(Config) ->
	doc("The 431 Request Header Fields Too Large status code can be sent. (RFC6585 5)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/431", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 431, _} = gun:await(ConnPid, Ref),
	ok.

status_code_511(Config) ->
	doc("The 511 Network Authentication Required status code can be sent. (RFC6585 6)"),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resp/reply2/511", [
		{<<"accept-encoding">>, <<"gzip">>}
	]),
	{response, _, 511, _} = gun:await(ConnPid, Ref),
	ok.
