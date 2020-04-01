%% Copyright (c) 2016-2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_clear).
-behavior(ranch_protocol).

-export([start_link/3]).
-export([start_link/4]).
-export([connection_process/4]).

%% Ranch 1.
-spec start_link(ranch:ref(), inet:socket(), module(), cowboy:opts()) -> {ok, pid()}.
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
	ProxyInfo = case maps:get(proxy_header, Opts, false) of
		true ->
			{ok, ProxyInfo0} = ranch:recv_proxy_header(Ref, 1000),
			ProxyInfo0;
		false ->
			undefined
	end,
	{ok, Socket} = ranch:handshake(Ref),
	%% Use cowboy_http2 directly only when 'http' is missing.
	%% Otherwise switch to cowboy_http2 from cowboy_http.
	%%
	%% @todo Extend this option to cowboy_tls and allow disabling
	%% the switch to cowboy_http2 in cowboy_http. Also document it.
	Protocol = case maps:get(protocols, Opts, [http2, http]) of
		[http2] -> cowboy_http2;
		[_|_] -> cowboy_http
	end,
	init(Parent, Ref, Socket, Transport, ProxyInfo, Opts, Protocol).

init(Parent, Ref, Socket, Transport, ProxyInfo, Opts, Protocol) ->
	_ = case maps:get(connection_type, Opts, supervisor) of
		worker -> ok;
		supervisor -> process_flag(trap_exit, true)
	end,
	Protocol:init(Parent, Ref, Socket, Transport, ProxyInfo, Opts).
