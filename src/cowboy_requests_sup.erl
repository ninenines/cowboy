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
-module(cowboy_requests_sup).
-behaviour(supervisor).

-export([start_link/0, start_request/5]). %% API.
-export([init/1]). %% supervisor.

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link(?MODULE, []).

-spec start_request(pid(), inet:socket(), module(), module(), any())
	-> {ok, pid()}.
start_request(ListenerPid, Socket, Transport, Protocol, Opts) ->
	Protocol:start_link(ListenerPid, Socket, Transport, Opts).

%% supervisor.

-spec init([]) -> {'ok', {{'simple_one_for_one', 0, 1}, [{
	any(), {atom() | tuple(), atom(), 'undefined' | [any()]},
	'permanent' | 'temporary' | 'transient',
	'brutal_kill' | 'infinity' | non_neg_integer(),
	'supervisor' | 'worker',
	'dynamic' | [atom() | tuple()]}]
}}.
init([]) ->
	{ok, {{simple_one_for_one, 0, 1}, [{?MODULE, {?MODULE, start_request, []},
		temporary, brutal_kill, worker, [?MODULE]}]}}.
