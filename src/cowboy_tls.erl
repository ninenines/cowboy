%% Copyright (c) 2015-2024, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_tls).
-behavior(ranch_protocol).

-export([start_link/3]).
-export([start_link/4]).
-export([connection_process/4]).

%% Ranch 1.
-spec start_link(ranch:ref(), ssl:sslsocket(), module(), cowboy:opts()) -> {ok, pid()}.
start_link(Ref, _Socket, Transport, Opts) ->
	start_link(Ref, Transport, Opts).

%% Ranch 2.
-spec start_link(ranch:ref(), module(), cowboy:opts()) -> {ok, pid()}.
start_link(Ref, Transport, Opts) ->
	Pid = proc_lib:spawn_link(?MODULE, connection_process,
		[self(), Ref, Transport, Opts]),
	{ok, Pid}.

-spec connection_process(pid(), ranch:ref(), module(), cowboy:opts()) -> ok.
connection_process(Parent, Ref, Transport, Opts) ->
	ProxyInfo = get_proxy_info(Ref, Opts),
	{ok, Socket} = ranch:handshake(Ref),
	case ssl:negotiated_protocol(Socket) of
		{ok, <<"h2">>} ->
			init(Parent, Ref, Socket, Transport, ProxyInfo, Opts, cowboy_http2);
		_ -> %% http/1.1 or no protocol negotiated.
			init(Parent, Ref, Socket, Transport, ProxyInfo, Opts, cowboy_http)
	end.

init(Parent, Ref, Socket, Transport, ProxyInfo, Opts, Protocol) ->
	_ = case maps:get(connection_type, Opts, supervisor) of
		worker -> ok;
		supervisor -> process_flag(trap_exit, true)
	end,
	Protocol:init(Parent, Ref, Socket, Transport, ProxyInfo, Opts).

get_proxy_info(Ref, #{proxy_header := true}) ->
	case ranch:recv_proxy_header(Ref, 1000) of
		{ok, ProxyInfo} -> ProxyInfo;
		{error, closed} -> exit({shutdown, closed})
	end;
get_proxy_info(_, _) ->
	undefined.
