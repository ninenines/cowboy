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

-module(cowboy_quic).
%% @todo -behavior(corral_protocol).

-export([start_link/4]).
-export([connection_process/5]).

-opaque conn() :: any().
-export_type([conn/0]).

-spec start_link(corral:ref(), module(), cowboy_quic:conn(), cowboy:opts())
	-> {ok, pid()}.

start_link(Ref, QuicBackend, Conn, Opts) ->
	Pid = proc_lib:spawn_link(?MODULE, connection_process,
		[self(), Ref, QuicBackend, Conn, Opts]),
	{ok, Pid}.

-spec connection_process(pid(), corral:ref(), module(), cowboy_quic:conn(), cowboy:opts())
	-> no_return().

connection_process(Parent, Ref, QuicBackend, Conn, Opts) ->
	{ok, #{alpn := <<"h3">>}} = QuicBackend:handshake(Conn),
	_ = case maps:get(connection_type, Opts, supervisor) of
		worker -> ok;
		supervisor -> process_flag(trap_exit, true)
	end,
	cowboy_http3:init(Parent, Ref, QuicBackend, Conn, Opts).
