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

-include_lib("kernel/include/inet.hrl").

%% API.

-spec name() -> ssl.
name() -> ssl.

-spec messages() -> {ssl, ssl_closed, ssl_error}.
messages() -> {ssl, ssl_closed, ssl_error}.

-spec listen([{port, Port::ip_port()} | {certfile, CertPath::string()}
	| {keyfile, KeyPath::string()} | {password, Password::string()}])
	-> {ok, LSocket::ssl:sslsocket()} | {error, Reason::atom()}.
listen(Opts) ->
	{port, Port} = lists:keyfind(port, 1, Opts),
	Backlog = proplists:get_value(backlog, Opts, 128),
	{certfile, CertFile} = lists:keyfind(certfile, 1, Opts),
	{keyfile, KeyFile} = lists:keyfind(keyfile, 1, Opts),
	{password, Password} = lists:keyfind(password, 1, Opts),
	ssl:listen(Port, [binary, {active, false},
		{backlog, Backlog}, {packet, raw}, {reuseaddr, true},
		{certfile, CertFile}, {keyfile, KeyFile}, {password, Password}]).

-spec accept(LSocket::ssl:sslsocket(), Timeout::timeout())
	-> {ok, Socket::ssl:sslsocket()} | {error, Reason::closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	case ssl:transport_accept(LSocket, Timeout) of
		{ok, CSocket} ->
			ssl_accept(CSocket, Timeout);
		{error, Reason} ->
			{error, Reason}
	end.

-spec recv(Socket::ssl:sslsocket(), Length::integer(), Timeout::timeout())
	-> {ok, Packet::term()} | {error, Reason::closed | atom()}.
recv(Socket, Length, Timeout) ->
	ssl:recv(Socket, Length, Timeout).

-spec send(Socket::ssl:sslsocket(), Packet::iolist())
	-> ok | {error, Reason::atom()}.
send(Socket, Packet) ->
	ssl:send(Socket, Packet).

-spec setopts(Socket::ssl:sslsocket(), Opts::list(term()))
	-> ok | {error, Reason::atom()}.
setopts(Socket, Opts) ->
	ssl:setopts(Socket, Opts).

-spec controlling_process(Socket::ssl:sslsocket(), Pid::pid())
	-> ok | {error, Reason::closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	ssl:controlling_process(Socket, Pid).

-spec peername(Socket::ssl:sslsocket())
	-> {ok, {Address::ip_address(), Port::ip_port()}} | {error, atom()}.
peername(Socket) ->
	ssl:peername(Socket).

-spec close(Socket::ssl:sslsocket()) -> ok.
close(Socket) ->
	ssl:close(Socket).

%% Internal.

-spec ssl_accept(CSocket::ssl:sslsocket(), Timeout::timeout())
	-> {ok, Socket::ssl:sslsocket()} | {error, Reason::closed | timeout | atom()}.
ssl_accept(CSocket, Timeout) ->
	case ssl:ssl_accept(CSocket, Timeout) of
		ok ->
			{ok, CSocket};
		{error, Reason} ->
			{error, Reason}
	end.
