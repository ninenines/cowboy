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

-module(security_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [raw_open/1]).
-import(cowboy_test, [raw_send/2]).
-import(cowboy_test, [raw_recv_head/1]).
-import(cowboy_test, [raw_recv/3]).

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
	cowboy:stop_listener(Name).

%% Routes.

init_dispatch(_) ->
	cowboy_router:compile([{"localhost", [
		{"/", hello_h, []}
	]}]).

%% Tests.

nc_rand(Config) ->
	doc("Throw random garbage at the server, then check if it's still up."),
	do_nc(Config, "/dev/urandom").

nc_zero(Config) ->
	doc("Throw zeroes at the server, then check if it's still up."),
	do_nc(Config, "/dev/zero").

do_nc(Config, Input) ->
	Cat = os:find_executable("cat"),
	Nc = os:find_executable("nc"),
	case {Cat, Nc} of
		{false, _} ->
			{skip, {not_found, cat}};
		{_, false} ->
			{skip, {not_found, nc}};
		_ ->
			StrPort = integer_to_list(config(port, Config)),
			_ = [
				os:cmd("cat " ++ Input ++ " | nc localhost " ++ StrPort)
			|| _ <- lists:seq(1, 100)],
			ConnPid = gun_open(Config),
			Ref = gun:get(ConnPid, "/"),
			{response, _, 200, _} = gun:await(ConnPid, Ref),
			ok
	end.

slowloris(Config) ->
	doc("Send request headers one byte at a time. "
		"Confirm that the connection gets closed."),
	_ = case config(protocol, Config) of
		http ->
			do_http_slowloris(Config);
		http2 ->
			%% @todo Write an equivalent test for HTTP2.
			ok
	end.

do_http_slowloris(Config) ->
	Client = raw_open(Config),
	try
		[begin
			ok = raw_send(Client, [C]),
			timer:sleep(250)
		end || C <- "GET / HTTP/1.1\r\nHost: localhost\r\n"
			"User-Agent: Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US)\r\n"
			"Cookie: name=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n\r\n"],
		error(failure)
	catch error:{badmatch, _} ->
		ok
	end.

slowloris_chunks(Config) ->
	_ = case config(protocol, Config) of
		http ->
			do_http_slowloris_chunks(Config);
		http2 ->
			%% @todo Write an equivalent test for HTTP2.
			ok
	end.

do_http_slowloris_chunks(Config) ->
	doc("Send request headers one line at a time. "
		"Confirm that the connection gets closed."),
	Client = raw_open(Config),
	ok = raw_send(Client, "GET / HTTP/1.1\r\n"),
	timer:sleep(300),
	ok = raw_send(Client, "Host: localhost\r\n"),
	timer:sleep(300),
	Data = raw_recv_head(Client),
	{'HTTP/1.1', 408, _, Rest} = cow_http:parse_status_line(Data),
	{Headers, _} = cow_http:parse_headers(Rest),
	{_, <<"close">>} = lists:keyfind(<<"connection">>, 1, Headers),
	{error, closed} = raw_recv(Client, 0, 1000).
