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

-module(proxy_header_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [raw_send/2]).
-import(cowboy_test, [raw_recv_head/1]).
-import(cowboy_test, [raw_recv/3]).

%% ct.

all() ->
	[
		{group, http},
		{group, https},
		{group, h2},
		{group, h2c},
		{group, h2c_upgrade}
	].

groups() ->
	Tests = ct_helper:all(?MODULE),
	[{h2c_upgrade, [parallel], Tests}|cowboy_test:common_groups(Tests)].

init_per_group(Name=http, Config) ->
	cowboy_test:init_http(Name, #{
		env => #{dispatch => init_dispatch()},
		proxy_header => true
	}, Config);
init_per_group(Name=https, Config) ->
	cowboy_test:init_https(Name, #{
		env => #{dispatch => init_dispatch()},
		proxy_header => true
	}, Config);
init_per_group(Name=h2, Config) ->
	cowboy_test:init_http2(Name, #{
		env => #{dispatch => init_dispatch()},
		proxy_header => true
	}, Config);
init_per_group(Name, Config) ->
	Config1 = cowboy_test:init_http(Name, #{
		env => #{dispatch => init_dispatch()},
		proxy_header => true
	}, Config),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2}).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

%% Routes.

init_dispatch() ->
	cowboy_router:compile([{"[...]", [
		{"/direct/:key/[...]", echo_h, []}
	]}]).

%% Tests.

v1_proxy_header(Config) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	ProxyInfo = #{
		version => 1,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header(Config, ProxyInfo).

v2_proxy_header(Config) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	ProxyInfo = #{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header(Config, ProxyInfo).

v2_local_header(Config) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	ProxyInfo = #{
		version => 2,
		command => local
	},
	do_proxy_header(Config, ProxyInfo).

do_proxy_header(Config, ProxyInfo) ->
	case config(ref, Config) of
		http -> do_proxy_header_http(Config, ProxyInfo);
		https -> do_proxy_header_https(Config, ProxyInfo);
		h2 -> do_proxy_header_h2(Config, ProxyInfo);
		h2c -> do_proxy_header_h2c(Config, ProxyInfo);
		h2c_upgrade -> do_proxy_header_h2c_upgrade(Config, ProxyInfo)
	end.

do_proxy_header_http(Config, ProxyInfo) ->
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config),
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, ranch_proxy_header:header(ProxyInfo)),
	do_proxy_header_http_common({raw_client, Socket, gen_tcp}, ProxyInfo).

do_proxy_header_https(Config, ProxyInfo) ->
	{ok, Socket0} = gen_tcp:connect("localhost", config(port, Config),
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket0, ranch_proxy_header:header(ProxyInfo)),
	{ok, Socket} = ssl:connect(Socket0, [], 1000),
	do_proxy_header_http_common({raw_client, Socket, ssl}, ProxyInfo).

do_proxy_header_http_common(Client, ProxyInfo) ->
	ok = raw_send(Client,
		"GET /direct/proxy_header HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"\r\n"),
	{_, 200, _, Rest0} = cow_http:parse_status_line(raw_recv_head(Client)),
	{Headers, Body0} = cow_http:parse_headers(Rest0),
	{_, LenBin} = lists:keyfind(<<"content-length">>, 1, Headers),
	Len = binary_to_integer(LenBin),
	Body = if
		byte_size(Body0) =:= Len -> Body0;
		true ->
			{ok, Body1} = raw_recv(Client, Len - byte_size(Body0), 5000),
			<<Body0/bits, Body1/bits>>
	end,
	ProxyInfo = do_parse_term(Body),
	ok.

do_proxy_header_h2(Config, ProxyInfo) ->
	{ok, Socket0} = gen_tcp:connect("localhost", config(port, Config),
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket0, ranch_proxy_header:header(ProxyInfo)),
	{ok, Socket} = ssl:connect(Socket0, [{alpn_advertised_protocols, [<<"h2">>]}], 1000),
	do_proxy_header_h2_common({raw_client, Socket, ssl}, ProxyInfo).

do_proxy_header_h2c(Config, ProxyInfo) ->
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config),
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, ranch_proxy_header:header(ProxyInfo)),
	do_proxy_header_h2_common({raw_client, Socket, gen_tcp}, ProxyInfo).

do_proxy_header_h2c_upgrade(Config, ProxyInfo) ->
	{ok, Socket} = gen_tcp:connect("localhost", config(port, Config),
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, ranch_proxy_header:header(ProxyInfo)),
	Client = {raw_client, Socket, gen_tcp},
	ok = raw_send(Client, [
		"GET /direct/proxy_header HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade, HTTP2-Settings\r\n"
		"Upgrade: h2c\r\n"
		"HTTP2-Settings: ", base64:encode(iolist_to_binary(cow_http2:settings_payload(#{}))), "\r\n"
		"\r\n"]),
	ok = do_recv_101(Client),
	%% Receive the server preface.
	{ok, <<PrefaceLen:24>>} = raw_recv(Client, 3, 1000),
	{ok, <<4:8, 0:40, _:PrefaceLen/binary>>} = raw_recv(Client, 6 + PrefaceLen, 1000),
	do_proxy_header_h2_response_common(Client, ProxyInfo),
	ok.

do_proxy_header_h2_common(Client, ProxyInfo) ->
	%% Send a valid preface.
	ok = raw_send(Client, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", cow_http2:settings(#{})]),
	%% Receive the server preface.
	{ok, <<PrefaceLen:24>>} = raw_recv(Client, 3, 1000),
	{ok, <<4:8, 0:40, _:PrefaceLen/binary>>} = raw_recv(Client, 6 + PrefaceLen, 1000),
	%% Send the SETTINGS ack.
	ok = raw_send(Client, cow_http2:settings_ack()),
	%% Receive the SETTINGS ack.
	{ok, <<0:24, 4:8, 1:8, 0:32>>} = raw_recv(Client, 9, 1000),
	%% Send a GET request.
	{HeadersBlock, _} = cow_hpack:encode([
		{<<":method">>, <<"GET">>},
		{<<":scheme">>, <<"http">>},
		{<<":authority">>, <<"localhost">>}, %% @todo Correct port number.
		{<<":path">>, <<"/direct/proxy_header">>}
	]),
	Len = iolist_size(HeadersBlock),
	ok = raw_send(Client, [
		<<Len:24, 1:8,
			0:2, %% Undefined.
			0:1, %% PRIORITY.
			0:1, %% Undefined.
			0:1, %% PADDED.
			1:1, %% END_HEADERS.
			0:1, %% Undefined.
			1:1, %% END_STREAM.
			0:1, 1:31>>,
		HeadersBlock
	]),
	do_proxy_header_h2_response_common(Client, ProxyInfo).

do_proxy_header_h2_response_common(Client, ProxyInfo) ->
	%% Receive a response with the proxy header data.
	{ok, <<SkipLen:24, 1:8, _:8, 1:32>>} = raw_recv(Client, 9, 1000),
	{ok, _} = raw_recv(Client, SkipLen, 1000),
	{ok, <<BodyLen:24, 0:8, 1:8, 1:32>>} = raw_recv(Client, 9, 1000),
	{ok, Body} = raw_recv(Client, BodyLen, 1000),
	ProxyInfo = do_parse_term(Body),
	ok.

do_parse_term(Body) ->
	{ok, Tokens, _} = erl_scan:string(binary_to_list(Body) ++ "."),
	{ok, Exprs} = erl_parse:parse_exprs(Tokens),
	{value, Term, _} = erl_eval:exprs(Exprs, erl_eval:new_bindings()),
	Term.

%% Match directly for now.
do_recv_101(Client) ->
	{ok, <<
		"HTTP/1.1 101 Switching Protocols\r\n"
		"connection: Upgrade\r\n"
		"upgrade: h2c\r\n"
		"\r\n"
	>>} = raw_recv(Client, 71, 1000),
	ok.
