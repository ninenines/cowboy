%% Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
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

-module(spdy_SUITE).
-compile(export_all).

-import(ct_helper, [config/2]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [raw_open/1]).
-import(cowboy_test, [raw_send/2]).

%% ct.

all() ->
	[{group, spdy}].

groups() ->
	[{spdy, [], ct_helper:all(?MODULE)}].

init_per_suite(Config) ->
	case proplists:get_value(ssl_app, ssl:versions()) of
		Version when Version < "5.2.1" ->
			{skip, "No NPN support in SSL application."};
		_ ->
			Dir = config(priv_dir, Config) ++ "/static",
			ct_helper:create_static_dir(Dir),
			[{static_dir, Dir}|Config]
	end.

end_per_suite(Config) ->
	ct_helper:delete_static_dir(config(static_dir, Config)).

init_per_group(Name, Config) ->
	cowboy_test:init_spdy(Name, [
		{env, [{dispatch, init_dispatch(Config)}]},
		{max_concurrent_streams, 2}
	], Config).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

%% Dispatch configuration.

init_dispatch(Config) ->
	cowboy_router:compile([
		{"localhost", [
			{"/static/[...]", cowboy_static,
				{dir, config(static_dir, Config)}},
			{"/echo/body", http_echo_body, []},
			{"/concurrent_streams", http_concurrent_streams, []},
			{"/chunked", http_chunked, []},
			{"/", http_handler, []}
		]}
	]).

%% Convenience functions.

do_get(ConnPid, Host, Path) ->
	StreamRef = gun:get(ConnPid, Path, [{<<"host">>, Host}]),
	{response, IsFin, Status, _} = gun:await(ConnPid, StreamRef),
	{IsFin, Status}.

%% Tests.

check_status(Config) ->
	Tests = [
		{200, nofin, "localhost", "/"},
		{200, nofin, "localhost", "/chunked"},
		{200, nofin, "localhost", "/static/style.css"},
		{400, fin, "bad-host", "/"},
		{400, fin, "localhost", "bad-path"},
		{404, fin, "localhost", "/this/path/does/not/exist"}
	],
	ConnPid = gun_open(Config),
	_ = [{Status, Fin, Host, Path} = begin
		{IsFin, Ret} = do_get(ConnPid, Host, Path),
		{Ret, IsFin, Host, Path}
	end || {Status, Fin, Host, Path} <- Tests],
	gun:close(ConnPid).

echo_body(Config) ->
	ConnPid = gun_open(Config),
	Body = << 0:800000 >>,
	StreamRef = gun:post(ConnPid, "/echo/body", [
		{<<"content-type">>, "application/octet-stream"}
	], Body),
	{response, nofin, 200, _} = gun:await(ConnPid, StreamRef),
	{ok, Body} = gun:await_body(ConnPid, StreamRef),
	gun:close(ConnPid).

echo_body_multi(Config) ->
	ConnPid = gun_open(Config),
	BodyChunk = << 0:80000 >>,
	StreamRef = gun:post(ConnPid, "/echo/body", [
		%% @todo I'm still unhappy with this. It shouldn't be required...
		{<<"content-length">>, integer_to_list(byte_size(BodyChunk) * 10)},
		{<<"content-type">>, "application/octet-stream"}
	]),
	_ = [gun:data(ConnPid, StreamRef, nofin, BodyChunk) || _ <- lists:seq(1, 9)],
	gun:data(ConnPid, StreamRef, fin, BodyChunk),
	{response, nofin, 200, _} = gun:await(ConnPid, StreamRef),
	{ok, << 0:800000 >>} = gun:await_body(ConnPid, StreamRef),
	gun:close(ConnPid).

two_frames_one_packet(Config) ->
	{raw_client, Socket, Transport} = Client = raw_open([
		{opts, [{client_preferred_next_protocols,
			{client, [<<"spdy/3">>], <<"spdy/3">>}}]}
		|Config]),
	Zdef = cow_spdy:deflate_init(),
	Zinf = cow_spdy:inflate_init(),
	ok = raw_send(Client, iolist_to_binary([
		cow_spdy:syn_stream(Zdef, 1, 0, true, false,
			0, <<"GET">>, <<"https">>, <<"localhost">>,
			<<"/">>, <<"HTTP/1.1">>, []),
		cow_spdy:syn_stream(Zdef, 3, 0, true, false,
			0, <<"GET">>, <<"https">>, <<"localhost">>,
			<<"/">>, <<"HTTP/1.1">>, [])
	])),
	{Frame1, Rest1} = spdy_recv(Socket, Transport, <<>>),
	{settings, _, _Settings} = cow_spdy:parse(Frame1, Zinf),
	{Frame2, Rest2} = spdy_recv(Socket, Transport, Rest1),
	{syn_reply, _, false, <<"200 OK">>, _, _} = cow_spdy:parse(Frame2, Zinf),
	{Frame3, Rest3} = spdy_recv(Socket, Transport, Rest2),
	{data, 1, true, _} = cow_spdy:parse(Frame3, Zinf),
	{Frame4, Rest4} = spdy_recv(Socket, Transport, Rest3),
	{syn_reply, _, false, <<"200 OK">>, _, _} = cow_spdy:parse(Frame4, Zinf),
	{Frame5, <<>>} = spdy_recv(Socket, Transport, Rest4),
	{data, 3, true, _} = cow_spdy:parse(Frame5, Zinf),
	ok.

spdy_recv(Socket, Transport, Acc) ->
	{ok, Data} = Transport:recv(Socket, 0, 5000),
	Data2 = << Acc/binary, Data/bits >>,
	case cow_spdy:split(Data2) of
		false ->
			spdy_recv(Socket, Transport, Data2);
		{true, Frame, Rest} ->
			{Frame, Rest}
	end.

concurrent_streams(Config) ->
	{raw_client, Socket, Transport} = Client = raw_open([
		{opts, [{client_preferred_next_protocols,
			{client, [<<"spdy/3">>], <<"spdy/3">>}}]}
		|Config]),
	Zdef = cow_spdy:deflate_init(),
	Zinf = cow_spdy:inflate_init(),
	ok = raw_send(Client, iolist_to_binary([
		cow_spdy:syn_stream(Zdef, 1, 0, true, false,
			0, <<"GET">>, <<"https">>, <<"localhost">>,
			<<"/">>, <<"HTTP/1.1">>, []),
		cow_spdy:syn_stream(Zdef, 3, 0, true, false,
			0, <<"GET">>, <<"https">>, <<"localhost">>,
			<<"/">>, <<"HTTP/1.1">>, []),
		cow_spdy:syn_stream(Zdef, 5, 0, true, false,
			0, <<"GET">>, <<"https">>, <<"localhost">>,
			<<"/">>, <<"HTTP/1.1">>, [])
	])),
	{Frame1, Rest1} = spdy_recv(Socket, Transport, <<>>),
	{settings, _, [{max_concurrent_streams, _, _, _},
			{initial_window_size, _, _, _}]} = cow_spdy:parse(Frame1, Zinf),
	{Frame2, _Rest2} = spdy_recv(Socket, Transport, Rest1),
	{rst_stream,5,refused_stream} = cow_spdy:parse(Frame2, Zinf),
	ok.
