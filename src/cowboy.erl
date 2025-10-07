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

-module(cowboy).

-export([start_clear/3]).
-export([start_tls/3]).
-export([stop_listener/1]).
-export([get_env/2]).
-export([get_env/3]).
-export([set_env/3]).

-ifdef(COWBOY_QUICER).
-export([start_quic/3]).

%% Don't warn about the bad quicer specs.
-dialyzer([{nowarn_function, start_quic/3}]).
-endif.

%% Internal.
-export([log/2]).
-export([log/4]).

-type opts() :: cowboy_http:opts() | cowboy_http2:opts().
-export_type([opts/0]).

-type fields() :: [atom()
	| {atom(), cowboy_constraints:constraint() | [cowboy_constraints:constraint()]}
	| {atom(), cowboy_constraints:constraint() | [cowboy_constraints:constraint()], any()}].
-export_type([fields/0]).

-type http_headers() :: #{binary() => iodata()}.
-export_type([http_headers/0]).

-type http_status() :: non_neg_integer() | binary().
-export_type([http_status/0]).

-type http_version() :: 'HTTP/2' | 'HTTP/1.1' | 'HTTP/1.0'.
-export_type([http_version/0]).

-spec start_clear(ranch:ref(), ranch:opts(), opts())
	-> {ok, pid()} | {error, any()}.

start_clear(Ref, TransOpts0, ProtoOpts0) ->
	TransOpts1 = ranch:normalize_opts(TransOpts0),
	{TransOpts2, DynamicBuffer} = ensure_dynamic_buffer(TransOpts1, ProtoOpts0),
	{TransOpts, ConnectionType} = ensure_connection_type(TransOpts2),
	ProtoOpts = ProtoOpts0#{
		connection_type => ConnectionType,
		dynamic_buffer => DynamicBuffer
	},
	ranch:start_listener(Ref, ranch_tcp, TransOpts, cowboy_clear, ProtoOpts).

-spec start_tls(ranch:ref(), ranch:opts(), opts())
	-> {ok, pid()} | {error, any()}.

start_tls(Ref, TransOpts0, ProtoOpts0) ->
	TransOpts1 = ranch:normalize_opts(TransOpts0),
	{TransOpts2, DynamicBuffer} = ensure_dynamic_buffer(TransOpts1, ProtoOpts0),
	TransOpts3 = ensure_alpn(TransOpts2),
	{TransOpts, ConnectionType} = ensure_connection_type(TransOpts3),
	ProtoOpts = ProtoOpts0#{
		connection_type => ConnectionType,
		dynamic_buffer => DynamicBuffer
	},
	ranch:start_listener(Ref, ranch_ssl, TransOpts, cowboy_tls, ProtoOpts).

-ifdef(COWBOY_QUICER).

%% @todo Experimental function to start a barebone QUIC listener.
%%       This will need to be reworked to be closer to Ranch
%%       listeners and provide equivalent features.
%%
%% @todo Better type for transport options. Might require fixing quicer types.

-spec start_quic(ranch:ref(), #{socket_opts => [{atom(), _}]}, cowboy_http3:opts())
	-> {ok, pid()}.

%% @todo Implement dynamic_buffer for HTTP/3 if/when it applies.
start_quic(Ref, TransOpts, ProtoOpts) ->
	{ok, _} = application:ensure_all_started(quicer),
	Parent = self(),
	SocketOpts0 = maps:get(socket_opts, TransOpts, []),
	{Port, SocketOpts2} = case lists:keytake(port, 1, SocketOpts0) of
		{value, {port, Port0}, SocketOpts1} ->
			{Port0, SocketOpts1};
		false ->
			{port_0(), SocketOpts0}
	end,
	SocketOpts = [
		{alpn, ["h3"]}, %% @todo Why not binary?
		%% We only need 3 for control and QPACK enc/dec,
		%% but we need more for WebTransport. %% @todo Use 3 if WT is disabled.
		{peer_unidi_stream_count, 100},
		{peer_bidi_stream_count, 100},
		%% For WebTransport.
		%% @todo We probably don't want it enabled if WT isn't used.
		{datagram_send_enabled, 1},
		{datagram_receive_enabled, 1}
	|SocketOpts2],
	_ListenerPid = spawn(fun() ->
		{ok, Listener} = quicer:listen(Port, SocketOpts),
		Parent ! {ok, Listener},
		_AcceptorPid = [spawn(fun AcceptLoop() ->
			{ok, Conn} = quicer:accept(Listener, []),
			Pid = spawn(fun() ->
				receive go -> ok end,
				%% We have to do the handshake after handing control of
				%% the connection otherwise streams may come in before
				%% the controlling process is changed and messages will
				%% not be sent to the correct process.
				{ok, Conn} = quicer:handshake(Conn),
				process_flag(trap_exit, true), %% @todo Only if supervisor though.
				try cowboy_http3:init(Parent, Ref, Conn, ProtoOpts)
				catch
					exit:{shutdown,_} -> ok;
					C:E:S -> log(error, "CRASH ~p:~p:~p", [C,E,S], ProtoOpts)
				end
			end),
			ok = quicer:controlling_process(Conn, Pid),
			Pid ! go,
			AcceptLoop()
		end) || _ <- lists:seq(1, 20)],
		%% Listener process must not terminate.
		receive after infinity -> ok end
	end),
	receive
		{ok, Listener} ->
			{ok, Listener}
	end.

%% Select a random UDP port using gen_udp because quicer
%% does not provide equivalent functionality. Taken from
%% quicer test suites.
port_0() ->
	{ok, Socket} = gen_udp:open(0, [{reuseaddr, true}]),
	{ok, {_, Port}} = inet:sockname(Socket),
	gen_udp:close(Socket),
	case os:type() of
		{unix, darwin} ->
			%% Apparently macOS doesn't free the port immediately.
			timer:sleep(500);
		_ ->
			ok
	end,
	Port.

-endif.

ensure_alpn(TransOpts) ->
	SocketOpts = maps:get(socket_opts, TransOpts, []),
	TransOpts#{socket_opts => [
		{alpn_preferred_protocols, [<<"h2">>, <<"http/1.1">>]}
	|SocketOpts]}.

ensure_connection_type(TransOpts=#{connection_type := ConnectionType}) ->
	{TransOpts, ConnectionType};
ensure_connection_type(TransOpts) ->
	{TransOpts#{connection_type => supervisor}, supervisor}.

%% Dynamic buffer was set; accept transport options as-is.
%% Note that initial 'buffer' size may be lower than dynamic buffer allows.
ensure_dynamic_buffer(TransOpts, #{dynamic_buffer := DynamicBuffer}) ->
	{TransOpts, DynamicBuffer};
%% Dynamic buffer was not set; define default dynamic buffer
%% only if 'buffer' size was not configured. In that case we
%% set the 'buffer' size to the lowest value.
ensure_dynamic_buffer(TransOpts=#{socket_opts := SocketOpts}, _) ->
	case proplists:get_value(buffer, SocketOpts, undefined) of
		undefined ->
			{TransOpts#{socket_opts => [{buffer, 512}|SocketOpts]}, {512, 131072}};
		_ ->
			{TransOpts, false}
	end.

-spec stop_listener(ranch:ref()) -> ok | {error, not_found}.

stop_listener(Ref) ->
	ranch:stop_listener(Ref).

-spec get_env(ranch:ref(), atom()) -> ok.

get_env(Ref, Name) ->
	Opts = ranch:get_protocol_options(Ref),
	Env = maps:get(env, Opts, #{}),
	maps:get(Name, Env).

-spec get_env(ranch:ref(), atom(), any()) -> ok.

get_env(Ref, Name, Default) ->
	Opts = ranch:get_protocol_options(Ref),
	Env = maps:get(env, Opts, #{}),
	maps:get(Name, Env, Default).

-spec set_env(ranch:ref(), atom(), any()) -> ok.

set_env(Ref, Name, Value) ->
	Opts = ranch:get_protocol_options(Ref),
	Env = maps:get(env, Opts, #{}),
	Opts2 = maps:put(env, maps:put(Name, Value, Env), Opts),
	ok = ranch:set_protocol_options(Ref, Opts2).

%% Internal.

-spec log({log, logger:level(), io:format(), list()}, opts()) -> ok.

log({log, Level, Format, Args}, Opts) ->
	log(Level, Format, Args, Opts).

-spec log(logger:level(), io:format(), list(), opts()) -> ok.

log(Level, Format, Args, #{logger := Logger})
		when Logger =/= error_logger ->
	_ = Logger:Level(Format, Args),
	ok;
%% We use error_logger by default. Because error_logger does
%% not have all the levels we accept we have to do some
%% mapping to error_logger functions.
log(Level, Format, Args, _) ->
	Function = case Level of
		emergency -> error_msg;
		alert -> error_msg;
		critical -> error_msg;
		error -> error_msg;
		warning -> warning_msg;
		notice -> warning_msg;
		info -> info_msg;
		debug -> info_msg
	end,
	error_logger:Function(Format, Args).
