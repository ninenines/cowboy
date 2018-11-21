%% Copyright (c) 2014-2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_test).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).

%% Listeners initialization.

init_http(Ref, ProtoOpts, Config) ->
	{ok, _} = cowboy:start_clear(Ref, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(Ref),
	[{ref, Ref}, {type, tcp}, {protocol, http}, {port, Port}, {opts, []}|Config].

init_https(Ref, ProtoOpts, Config) ->
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = cowboy:start_tls(Ref, Opts ++ [{port, 0}], ProtoOpts),
	Port = ranch:get_port(Ref),
	[{ref, Ref}, {type, ssl}, {protocol, http}, {port, Port}, {opts, Opts}|Config].

init_http2(Ref, ProtoOpts, Config) ->
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = cowboy:start_tls(Ref, Opts ++ [{port, 0}], ProtoOpts),
	Port = ranch:get_port(Ref),
	[{ref, Ref}, {type, ssl}, {protocol, http2}, {port, Port}, {opts, Opts}|Config].

%% Common group of listeners used by most suites.

common_all() ->
	[
		{group, http},
		{group, https},
		{group, h2},
		{group, h2c},
		{group, http_compress},
		{group, https_compress},
		{group, h2_compress},
		{group, h2c_compress}
	].

common_groups(Tests) ->
	[
		{http, [parallel], Tests},
		{https, [parallel], Tests},
		{h2, [parallel], Tests},
		{h2c, [parallel], Tests},
		{http_compress, [parallel], Tests},
		{https_compress, [parallel], Tests},
		{h2_compress, [parallel], Tests},
		{h2c_compress, [parallel], Tests}
	].

init_common_groups(Name = http, Config, Mod) ->
	init_http(Name, #{
		env => #{dispatch => Mod:init_dispatch(Config)}
	}, [{flavor, vanilla}|Config]);
init_common_groups(Name = https, Config, Mod) ->
	init_https(Name, #{
		env => #{dispatch => Mod:init_dispatch(Config)}
	}, [{flavor, vanilla}|Config]);
init_common_groups(Name = h2, Config, Mod) ->
	init_http2(Name, #{
		env => #{dispatch => Mod:init_dispatch(Config)}
	}, [{flavor, vanilla}|Config]);
init_common_groups(Name = h2c, Config, Mod) ->
	Config1 = init_http(Name, #{
		env => #{dispatch => Mod:init_dispatch(Config)}
	}, [{flavor, vanilla}|Config]),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2});
init_common_groups(Name = http_compress, Config, Mod) ->
	init_http(Name, #{
		env => #{dispatch => Mod:init_dispatch(Config)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h]
	}, [{flavor, compress}|Config]);
init_common_groups(Name = https_compress, Config, Mod) ->
	init_https(Name, #{
		env => #{dispatch => Mod:init_dispatch(Config)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h]
	}, [{flavor, compress}|Config]);
init_common_groups(Name = h2_compress, Config, Mod) ->
	init_http2(Name, #{
		env => #{dispatch => Mod:init_dispatch(Config)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h]
	}, [{flavor, compress}|Config]);
init_common_groups(Name = h2c_compress, Config, Mod) ->
	Config1 = init_http(Name, #{
		env => #{dispatch => Mod:init_dispatch(Config)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h]
	}, [{flavor, compress}|Config]),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2}).

%% Support functions for testing using Gun.

gun_open(Config) ->
	gun_open(Config, #{}).

gun_open(Config, Opts) ->
	{ok, ConnPid} = gun:open("localhost", config(port, Config), Opts#{
		retry => 0,
		transport => config(type, Config),
		transport_opts => proplists:get_value(transport_opts, Config, []),
		protocols => [config(protocol, Config)]
	}),
	ConnPid.

gun_down(ConnPid) ->
	receive {gun_down, ConnPid, _, _, _, _} -> ok
	after 500 -> error(timeout) end.

%% Support functions for testing using a raw socket.

raw_open(Config) ->
	Transport = case config(type, Config) of
		tcp -> gen_tcp;
		ssl -> ssl
	end,
	{_, Opts} = lists:keyfind(opts, 1, Config),
	{ok, Socket} = Transport:connect("localhost", config(port, Config),
		[binary, {active, false}, {packet, raw},
			{reuseaddr, true}, {nodelay, true}|Opts]),
	{raw_client, Socket, Transport}.

raw_send({raw_client, Socket, Transport}, Data) ->
	Transport:send(Socket, Data).

raw_recv_head({raw_client, Socket, Transport}) ->
	{ok, Data} = Transport:recv(Socket, 0, 10000),
	raw_recv_head(Socket, Transport, Data).

raw_recv_head(Socket, Transport, Buffer) ->
	case binary:match(Buffer, <<"\r\n\r\n">>) of
		nomatch ->
			{ok, Data} = Transport:recv(Socket, 0, 10000),
			raw_recv_head(Socket, Transport, << Buffer/binary, Data/binary >>);
		{_, _} ->
			Buffer
	end.

raw_recv({raw_client, Socket, Transport}, Length, Timeout) ->
	Transport:recv(Socket, Length, Timeout).

raw_expect_recv({raw_client, _, _}, <<>>) ->
	ok;
raw_expect_recv({raw_client, Socket, Transport}, Expect) ->
	{ok, Expect} = Transport:recv(Socket, iolist_size(Expect), 10000),
	ok.
