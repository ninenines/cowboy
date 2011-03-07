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

-module(cowboy_listener_sup).
-behaviour(supervisor).

-export([start_link/5]). %% API.
-export([init/1]). %% supervisor.

%% API.

-spec start_link(NbAcceptors::non_neg_integer(), Transport::module(),
	TransOpts::term(), Protocol::module(), ProtoOpts::term())
	-> {ok, Pid::pid()}.
start_link(NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts) ->
	case Transport:listen(TransOpts) of
		{ok, LSocket} ->
			supervisor:start_link(?MODULE, [LSocket,
				NbAcceptors, Transport, Protocol, ProtoOpts]);
		{error, Reason} ->
			{error, Reason}
	end.

%% supervisor.

%% @todo These specs should be improved.
-spec init(list(term())) -> term().
init([LSocket, NbAcceptors, Transport, Protocol, ProtoOpts]) ->
	Procs = [{{acceptor, self(), N}, {cowboy_acceptor, start_link,
		[LSocket, Transport, Protocol, ProtoOpts]}, permanent,
		brutal_kill, worker, dynamic} || N <- lists:seq(1, NbAcceptors)],
	{ok, {{one_for_one, 10, 10}, Procs}}.
