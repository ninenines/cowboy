%% Copyright (c) 2012, Hunter Morris <huntermorris@gmail.com>
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

%% @doc TCP transport API when behind an SSL proxy which supports the
%% haproxy PROXY protocol.
%%
%% Wrapper around <em>cowboy_haproxy_transport</em> implementing the
%% Cowboy transport API when behind an SSL proxy.
%%
%% @see cowboy_haproxy_transport
-module(cowboy_haproxy_ssl_transport).

-export([name/0, messages/0, listen/1, accept/2, recv/3, send/2, setopts/2,
	controlling_process/2, peername/1, close/1, sockname/1]).

%% @doc Name of this transport API, <em>ssl</em>.
-spec name() -> ssl.
name() -> ssl.

%% @see cowboy_haproxy_transport:messages/0
-spec messages() -> {tcp, tcp_closed, tcp_error}.
messages() ->
	cowboy_haproxy_transport:messages().

%% @see cowboy_haproxy_transport:listen/1
-spec listen([{port, inet:port_number()} | {ip, inet:ip_address()}])
	-> {ok, inet:socket()} | {error, atom()}.
listen(Opts) ->
	cowboy_haproxy_transport:listen(Opts).

%% @see cowboy_haproxy_transport:accept/2
-spec accept(inet:socket(), timeout())
	-> {ok, cowboy_haproxy_transport:haproxy_socket()}
		| {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	cowboy_haproxy_transport:accept(LSocket, Timeout).

%% @see cowboy_haproxy_transport:recv/3
-spec recv(cowboy_haproxy_transport:haproxy_socket(),
		non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	cowboy_haproxy_transport:recv(Socket, Length, Timeout).

%% @see cowboy_haproxy_transport:send/2
-spec send(cowboy_haproxy_transport:haproxy_socket(), iolist())
	-> ok | {error, atom()}.
send(Socket, Packet) ->
	cowboy_haproxy_transport:send(Socket, Packet).

%% @see cowboy_haproxy_transport:setopts/2
-spec setopts(cowboy_haproxy_transport:haproxy_socket(), list())
	-> ok | {error, atom()}.
setopts(Socket, Opts) ->
	cowboy_haproxy_transport:setopts(Socket, Opts).

%% @see cowboy_haproxy_transport:controlling_process/2
-spec controlling_process(cowboy_haproxy_transport:haproxy_socket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	cowboy_haproxy_transport:controlling_process(Socket, Pid).

%% @see cowboy_haproxy_transport:peername/1
-spec peername(cowboy_haproxy_transport:haproxy_socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
	cowboy_haproxy_transport:peername(Socket).

%% @see cowboy_haproxy_transport:close/1
-spec close(cowboy_haproxy_transport:haproxy_socket()) -> ok.
close(Socket) ->
	cowboy_haproxy_transport:close(Socket).

%% @see cowboy_haproxy_transport:sockname/1
-spec sockname(cowboy_haproxy_transport:haproxy_socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
	cowboy_haproxy_transport:sockname(Socket).
