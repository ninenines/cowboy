%% Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>
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

%% @private
-module(cowboy_listener_sup).
-behaviour(supervisor).

-export([start_link/5]). %% API.
-export([init/1]). %% supervisor.

%% API.

-spec start_link(non_neg_integer(), module(), any(), module(), any())
	-> {ok, pid()}.
start_link(NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts) ->
	MaxConns = proplists:get_value(max_connections, TransOpts, 1024),
	{ok, SupPid} = supervisor:start_link(?MODULE, []),
	{ok, ListenerPid} = supervisor:start_child(SupPid,
		{cowboy_listener, {cowboy_listener, start_link, [MaxConns, ProtoOpts]},
		 permanent, 5000, worker, [cowboy_listener]}),
	{ok, ReqsPid} = supervisor:start_child(SupPid,
		{cowboy_requests_sup, {cowboy_requests_sup, start_link, []},
		 permanent, 5000, supervisor, [cowboy_requests_sup]}),
	{ok, _PoolPid} = supervisor:start_child(SupPid,
		{cowboy_acceptors_sup, {cowboy_acceptors_sup, start_link, [
			NbAcceptors, Transport, TransOpts,
			Protocol, ProtoOpts, ListenerPid, ReqsPid
		]}, permanent, 5000, supervisor, [cowboy_acceptors_sup]}),
	{ok, SupPid}.

%% supervisor.

-spec init([]) -> {ok, {{one_for_all, 10, 10}, []}}.
init([]) ->
	{ok, {{one_for_all, 10, 10}, []}}.
