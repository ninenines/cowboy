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
-module(cowboy_acceptors_sup).
-behaviour(supervisor).

-export([start_link/7]). %% API.
-export([init/1]). %% supervisor.

%% API.

-spec start_link(non_neg_integer(), module(), any(),
	module(), any(), pid(), pid()) -> {ok, pid()}.
start_link(NbAcceptors, Transport, TransOpts,
		Protocol, ProtoOpts, ListenerPid, ReqsPid) ->
	supervisor:start_link(?MODULE, [NbAcceptors, Transport, TransOpts,
		Protocol, ProtoOpts, ListenerPid, ReqsPid]).

%% supervisor.

-spec init([any()]) -> {'ok', {{'one_for_one', 10, 10}, [{
	any(), {atom() | tuple(), atom(), 'undefined' | [any()]},
	'permanent' | 'temporary' | 'transient',
	'brutal_kill' | 'infinity' | non_neg_integer(),
	'supervisor' | 'worker',
	'dynamic' | [atom() | tuple()]}]
}}.
init([NbAcceptors, Transport, TransOpts,
		Protocol, ProtoOpts, ListenerPid, ReqsPid]) ->
	{ok, LSocket} = Transport:listen(TransOpts),
	Procs = [{{acceptor, self(), N}, {cowboy_acceptor, start_link, [
				LSocket, Transport, Protocol, ProtoOpts,
				ListenerPid, ReqsPid
      ]}, permanent, brutal_kill, worker, []}
		|| N <- lists:seq(1, NbAcceptors)],
	{ok, {{one_for_one, 10, 10}, Procs}}.
