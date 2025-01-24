%% Copyright (c) 2014-2024, Loïc Hoguin <essen@ninenines.eu>
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
	{ok, _} = cowboy:start_tls(Ref, Opts ++ [{port, 0}, {verify, verify_none}], ProtoOpts),
	Port = ranch:get_port(Ref),
	[{ref, Ref}, {type, ssl}, {protocol, http}, {port, Port}, {opts, Opts}|Config].

init_http2(Ref, ProtoOpts, Config) ->
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = cowboy:start_tls(Ref, Opts ++ [{port, 0}, {verify, verify_none}], ProtoOpts),
	Port = ranch:get_port(Ref),
	[{ref, Ref}, {type, ssl}, {protocol, http2}, {port, Port}, {opts, Opts}|Config].

%% @todo This will probably require TransOpts as argument.
init_http3(Ref, ProtoOpts, Config) ->
	%% @todo Quicer does not currently support non-file cert/key,
	%%       so we use quicer test certificates for now.
	%% @todo Quicer also does not support cacerts which means
	%%       we currently have no authentication based security.
	DataDir = filename:dirname(filename:dirname(config(data_dir, Config)))
		++ "/rfc9114_SUITE_data",
	TransOpts = #{
		socket_opts => [
			{certfile, DataDir ++ "/server.pem"},
			{keyfile, DataDir ++ "/server.key"}
		]
	},
	{ok, Listener} = cowboy:start_quic(Ref, TransOpts, ProtoOpts),
	{ok, {_, Port}} = quicer:sockname(Listener),
	%% @todo Keep listener information around in a better place.
	persistent_term:put({cowboy_test_quic, Ref}, Listener),
	[{ref, Ref}, {type, quic}, {protocol, http3}, {port, Port}, {opts, TransOpts}|Config].

stop_group(Ref) ->
	case persistent_term:get({cowboy_test_quic, Ref}, undefined) of
		undefined ->
			cowboy:stop_listener(Ref);
		Listener ->
			quicer:close_listener(Listener)
	end.

%% Common group of listeners used by most suites.

common_all() ->
	All = [
		{group, http},
		{group, https},
		{group, h2},
		{group, h2c},
		{group, h3},
		{group, http_compress},
		{group, https_compress},
		{group, h2_compress},
		{group, h2c_compress},
		{group, h3_compress}
	],
	%% Don't run HTTP/3 tests on Windows for now.
	case os:type() of
		{win32, _} ->
			All -- [{group, h3}, {group, h3_compress}];
		_ ->
			All
	end.

common_groups(Tests) ->
	Parallel = case os:getenv("NO_PARALLEL") of
		false -> parallel;
		_ -> no_parallel
	end,
	common_groups(Tests, Parallel).

common_groups(Tests, Parallel) ->
	Opts = case Parallel of
		parallel -> [parallel];
		no_parallel -> []
	end,
	Groups = [
		{http, Opts, Tests},
		{https, Opts, Tests},
		{h2, Opts, Tests},
		{h2c, Opts, Tests},
		{h3, Opts, Tests},
		{http_compress, Opts, Tests},
		{https_compress, Opts, Tests},
		{h2_compress, Opts, Tests},
		{h2c_compress, Opts, Tests},
		{h3_compress, Opts, Tests}
	],
	%% Don't run HTTP/3 tests on Windows for now.
	case os:type() of
		{win32, _} ->
			Groups -- [{h3, Opts, Tests}, {h3_compress, Opts, Tests}];
		_ ->
			Groups
	end.

init_common_groups(Name, Config, Mod) ->
	init_common_groups(Name, Config, Mod, #{}).

init_common_groups(Name = http, Config, Mod, ProtoOpts) ->
	init_http(Name, ProtoOpts#{
		env => #{dispatch => Mod:init_dispatch(Config)}
	}, [{flavor, vanilla}|Config]);
init_common_groups(Name = https, Config, Mod, ProtoOpts) ->
	init_https(Name, ProtoOpts#{
		env => #{dispatch => Mod:init_dispatch(Config)}
	}, [{flavor, vanilla}|Config]);
init_common_groups(Name = h2, Config, Mod, ProtoOpts) ->
	init_http2(Name, ProtoOpts#{
		env => #{dispatch => Mod:init_dispatch(Config)}
	}, [{flavor, vanilla}|Config]);
init_common_groups(Name = h2c, Config, Mod, ProtoOpts) ->
	Config1 = init_http(Name, ProtoOpts#{
		env => #{dispatch => Mod:init_dispatch(Config)}
	}, [{flavor, vanilla}|Config]),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2});
init_common_groups(Name = h3, Config, Mod, ProtoOpts) ->
	init_http3(Name, ProtoOpts#{
		env => #{dispatch => Mod:init_dispatch(Config)}
	}, [{flavor, vanilla}|Config]);
init_common_groups(Name = http_compress, Config, Mod, ProtoOpts) ->
	init_http(Name, ProtoOpts#{
		env => #{dispatch => Mod:init_dispatch(Config)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h]
	}, [{flavor, compress}|Config]);
init_common_groups(Name = https_compress, Config, Mod, ProtoOpts) ->
	init_https(Name, ProtoOpts#{
		env => #{dispatch => Mod:init_dispatch(Config)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h]
	}, [{flavor, compress}|Config]);
init_common_groups(Name = h2_compress, Config, Mod, ProtoOpts) ->
	init_http2(Name, ProtoOpts#{
		env => #{dispatch => Mod:init_dispatch(Config)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h]
	}, [{flavor, compress}|Config]);
init_common_groups(Name = h2c_compress, Config, Mod, ProtoOpts) ->
	Config1 = init_http(Name, ProtoOpts#{
		env => #{dispatch => Mod:init_dispatch(Config)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h]
	}, [{flavor, compress}|Config]),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2});
init_common_groups(Name = h3_compress, Config, Mod, ProtoOpts) ->
	init_http3(Name, ProtoOpts#{
		env => #{dispatch => Mod:init_dispatch(Config)},
		stream_handlers => [cowboy_compress_h, cowboy_stream_h]
	}, [{flavor, compress}|Config]).

%% Support functions for testing using Gun.

gun_open(Config) ->
	gun_open(Config, #{}).

gun_open(Config, Opts) ->
	TlsOpts = case proplists:get_value(no_cert, Config, false) of
		true -> [{verify, verify_none}];
		false -> ct_helper:get_certs_from_ets() %% @todo Wrong in current quicer.
	end,
	{ok, ConnPid} = gun:open("localhost", config(port, Config), Opts#{
		retry => 0,
		transport => config(type, Config),
		tls_opts => TlsOpts,
		protocols => [config(protocol, Config)]
	}),
	ConnPid.

gun_down(ConnPid) ->
	receive {gun_down, ConnPid, _, _, _} -> ok
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

raw_recv_rest({raw_client, _, _}, Length, Buffer) when Length =:= byte_size(Buffer) ->
	Buffer;
raw_recv_rest({raw_client, Socket, Transport}, Length, Buffer) when Length > byte_size(Buffer) ->
	{ok, Data} = Transport:recv(Socket, Length - byte_size(Buffer), 10000),
	<< Buffer/binary, Data/binary >>.

raw_recv({raw_client, Socket, Transport}, Length, Timeout) ->
	Transport:recv(Socket, Length, Timeout).

raw_expect_recv({raw_client, _, _}, <<>>) ->
	ok;
raw_expect_recv({raw_client, Socket, Transport}, Expect) ->
	{ok, Expect} = Transport:recv(Socket, iolist_size(Expect), 10000),
	ok.
