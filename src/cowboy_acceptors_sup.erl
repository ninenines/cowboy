%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
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

-module(cowboy_acceptors_sup).
-behaviour(supervisor).

-export([start_link/6]). %% API.
-export([init/1]). %% supervisor.

-include("include/types.hrl").

%% API.

-spec start_link(NbAcceptors::non_neg_integer(), Transport::module(),
	TransOpts::term(), Protocol::module(), ProtoOpts::term(), ReqsPid::pid())
	-> {ok, Pid::pid()}.
start_link(LSocket, NbAcceptors, Transport, Protocol, ProtoOpts, ReqsPid) ->
	supervisor:start_link(?MODULE, [LSocket, NbAcceptors,
		Transport, Protocol, ProtoOpts, ReqsPid]).

%% supervisor.

-spec init(list(term())) -> term(). %% @todo These specs should be improved.
init([LSocket, NbAcceptors, Transport, Protocol, ProtoOpts, ReqsPid]) ->
	Procs = [{{acceptor, self(), N}, {cowboy_acceptor, start_link, [
				LSocket, Transport, Protocol, ProtoOpts, ReqsPid
			]}, permanent, brutal_kill, worker, dynamic}
		|| N <- lists:seq(1, NbAcceptors)],
	{ok, {{one_for_one, 10, 10}, Procs}}.
