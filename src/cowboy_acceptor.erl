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

-module(cowboy_acceptor).
-export([start_link/5]). %% API.
-export([acceptor/5]). %% Internal.

-include("include/types.hrl").

%% API.

-spec start_link(LSocket::inet:socket(), Transport::module(),
	Protocol::module(), Opts::term(), ReqsSup::pid()) -> {ok, Pid::pid()}.
start_link(LSocket, Transport, Protocol, Opts, ReqsSup) ->
	Pid = spawn_link(?MODULE, acceptor,
		[LSocket, Transport, Protocol, Opts, ReqsSup]),
	{ok, Pid}.

%% Internal.

-spec acceptor(LSocket::inet:socket(), Transport::module(),
	Protocol::module(), Opts::term(), ReqsSup::pid()) -> no_return().
acceptor(LSocket, Transport, Protocol, Opts, ReqsSup) ->
	case Transport:accept(LSocket, 2000) of
		{ok, CSocket} ->
			{ok, Pid} = supervisor:start_child(ReqsSup,
				[CSocket, Transport, Protocol, Opts]),
			Transport:controlling_process(CSocket, Pid);
		{error, timeout} ->
			ignore;
		{error, _Reason} ->
			%% @todo Probably do something here. If the socket was closed,
			%%       we may want to try and listen again on the port?
			ignore
	end,
	?MODULE:acceptor(LSocket, Transport, Protocol, Opts, ReqsSup).
