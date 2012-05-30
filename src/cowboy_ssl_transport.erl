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

%% @doc SSL transport API.
%%
%% Wrapper around <em>ssl</em> implementing the Cowboy transport API.
%%
%% This transport requires the <em>crypto</em>, <em>public_key</em>
%% and <em>ssl</em> applications to be started. If they aren't started,
%% it will try to start them itself before opening a port to listen.
%% Applications aren't stopped when the listening socket is closed, though.
%%
%% @see ssl
-module(cowboy_ssl_transport).
-export([name/0, messages/0, listen/1, accept/2, recv/3, send/2, setopts/2,
	controlling_process/2, peername/1, close/1, sockname/1]).
-export([connect/3]).

%% @doc Name of this transport API, <em>ssl</em>.
-spec name() -> ssl.
name() -> ssl.

%% @doc Atoms used in the process messages sent by this API.
%%
%% They identify incoming data, closed connection and errors when receiving
%% data in active mode.
-spec messages() -> {ssl, ssl_closed, ssl_error}.
messages() -> {ssl, ssl_closed, ssl_error}.

%% @private
%% @todo Probably filter Opts?
connect(Host, Port, Opts) when is_list(Host), is_integer(Port) ->
	ssl:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}]).

%% @doc Setup a socket to listen on the given port on the local host.
%%
%% The available options are:
%% <dl>
%%  <dt>port</dt><dd>Mandatory. TCP port number to open.</dd>
%%  <dt>backlog</dt><dd>Maximum length of the pending connections queue.
%%   Defaults to 1024.</dd>
%%  <dt>ip</dt><dd>Interface to listen on. Listen on all interfaces
%%   by default.</dd>
%%  <dt>certfile</dt><dd>Mandatory. Path to a file containing the user's
%%   certificate.</dd>
%%  <dt>keyfile</dt><dd>Optional. Path to the file containing the user's
%%   private PEM encoded key.</dd>
%%  <dt>cacertfile</dt><dd>Optional. Path to file containing PEM encoded
%%   CA certificates (trusted certificates used for verifying a peer
%%   certificate).</dd>
%%  <dt>password</dt><dd>Optional. String containing the user's password.
%%   All private keyfiles must be password protected currently.</dd>
%%  <dt>ciphers</dt><dd>Optional. The cipher suites that should be supported.
%%  The function ssl:cipher_suites/0 can be used to find all available
%%  ciphers.</dd>
%% </dl>
%%
%% @see ssl:listen/2
-spec listen([{port, inet:port_number()} | {certfile, string()}
	| {keyfile, string()} | {password, string()}
	| {cacertfile, string()} | {ip, inet:ip_address()}])
	-> {ok, ssl:sslsocket()} | {error, atom()}.
listen(Opts) ->
	require([crypto, public_key, ssl]),
	{port, Port} = lists:keyfind(port, 1, Opts),
	Backlog = proplists:get_value(backlog, Opts, 1024),
	{certfile, CertFile} = lists:keyfind(certfile, 1, Opts),

	ListenOpts0 = [binary, {active, false},
		{backlog, Backlog}, {packet, raw}, {reuseaddr, true},
		{certfile, CertFile}],
	ListenOpts = lists:foldl(fun
		({ip, _} = Ip, Acc) -> [Ip | Acc];
		({keyfile, _} = KeyFile, Acc) -> [KeyFile | Acc];
		({cacertfile, _} = CACertFile, Acc) -> [CACertFile | Acc];
		({password, _} = Password, Acc) -> [Password | Acc];
		({ciphers, _} = Ciphers, Acc) -> [Ciphers | Acc];
		(_, Acc) -> Acc
	end, ListenOpts0, Opts),
	ssl:listen(Port, ListenOpts).

%% @doc Accept an incoming connection on a listen socket.
%%
%% Note that this function does both the transport accept and
%% the SSL handshake.
%%
%% @see ssl:transport_accept/2
%% @see ssl:ssl_accept/2
-spec accept(ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	case ssl:transport_accept(LSocket, Timeout) of
		{ok, CSocket} ->
			ssl_accept(CSocket, Timeout);
		{error, Reason} ->
			{error, Reason}
	end.

%% @doc Receive a packet from a socket in passive mode.
%% @see ssl:recv/3
-spec recv(ssl:sslsocket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	ssl:recv(Socket, Length, Timeout).

%% @doc Send a packet on a socket.
%% @see ssl:send/2
-spec send(ssl:sslsocket(), iolist()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	ssl:send(Socket, Packet).

%% @doc Set one or more options for a socket.
%% @see ssl:setopts/2
-spec setopts(ssl:sslsocket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	ssl:setopts(Socket, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
%% @see ssl:controlling_process/2
-spec controlling_process(ssl:sslsocket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	ssl:controlling_process(Socket, Pid).

%% @doc Return the address and port for the other end of a connection.
%% @see ssl:peername/1
-spec peername(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
	ssl:peername(Socket).

%% @doc Close a TCP socket.
%% @see ssl:close/1
-spec close(ssl:sslsocket()) -> ok.
close(Socket) ->
	ssl:close(Socket).

%% @doc Get the local address and port of a socket
%% @see ssl:sockname/1
-spec sockname(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
	ssl:sockname(Socket).

%% Internal.

-spec require(list(module())) -> ok.
require([]) ->
	ok;
require([App|Tail]) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end,
	require(Tail).

-spec ssl_accept(ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {error, closed | timeout | atom()}.
ssl_accept(Socket, Timeout) ->
	case ssl:ssl_accept(Socket, Timeout) of
		ok ->
			{ok, Socket};
		{error, Reason} ->
			{error, Reason}
	end.
