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

-module(cowboy_ssl_transport).
-export([name/0, messages/0, listen/1, accept/2, recv/3, send/2, setopts/2,
	controlling_process/2, peername/1, close/1]). %% API.

-include("include/types.hrl").

%% API.

-spec name() -> ssl.
name() -> ssl.

-spec messages() -> {ssl, ssl_closed, ssl_error}.
messages() -> {ssl, ssl_closed, ssl_error}.

-spec listen([{port, Port::port_number()} | {certfile, CertPath::string()}
	| {keyfile, KeyPath::string()} | {password, Password::string()}])
	-> {ok, LSocket::sslsocket()} | {error, Reason::posix()}.
listen(Opts) ->
	{port, Port} = lists:keyfind(port, 1, Opts),
	{certfile, CertFile} = lists:keyfind(certfile, 1, Opts),
	{keyfile, KeyFile} = lists:keyfind(keyfile, 1, Opts),
	{password, Password} = lists:keyfind(password, 1, Opts),
	ssl:listen(Port, [binary, {active, false},
		{packet, raw}, {reuseaddr, true},
		{certfile, CertFile}, {keyfile, KeyFile}, {password, Password}]).

-spec accept(LSocket::sslsocket(), Timeout::timeout())
	-> {ok, Socket::sslsocket()} | {error, Reason::closed | timeout | posix()}.
accept(LSocket, Timeout) ->
	case ssl:transport_accept(LSocket, Timeout) of
		{ok, CSocket} ->
			ssl_accept(CSocket, Timeout);
		{error, Reason} ->
			{error, Reason}
	end.

-spec recv(Socket::sslsocket(), Length::integer(), Timeout::timeout())
	-> {ok, Packet::term()} | {error, Reason::closed | posix()}.
recv(Socket, Length, Timeout) ->
	ssl:recv(Socket, Length, Timeout).

-spec send(Socket::sslsocket(), Packet::iolist())
	-> ok | {error, Reason::posix()}.
send(Socket, Packet) ->
	ssl:send(Socket, Packet).

-spec setopts(Socket::sslsocket(), Opts::list(term()))
	-> ok | {error, Reason::posix()}.
setopts(Socket, Opts) ->
	ssl:setopts(Socket, Opts).

-spec controlling_process(Socket::sslsocket(), Pid::pid())
	-> ok | {error, Reason::closed | not_owner | posix()}.
controlling_process(Socket, Pid) ->
	ssl:controlling_process(Socket, Pid).

-spec peername(Socket::sslsocket())
	-> {ok, {Address::ip_address(), Port::port_number()}} | {error, posix()}.
peername(Socket) ->
	ssl:peername(Socket).

-spec close(Socket::sslsocket()) -> ok.
close(Socket) ->
	ssl:close(Socket).

%% Internal.

-spec ssl_accept(CSocket::sslsocket(), Timeout::timeout())
	-> {ok, Socket::sslsocket()} | {error, Reason::closed | timeout | posix()}.
ssl_accept(CSocket, Timeout) ->
	case ssl:ssl_accept(CSocket, Timeout) of
		ok ->
			{ok, CSocket};
		{error, Reason} ->
			{error, Reason}
	end.
