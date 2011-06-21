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

%% API.

-spec name() -> ssl.
name() -> ssl.

-spec messages() -> {ssl, ssl_closed, ssl_error}.
messages() -> {ssl, ssl_closed, ssl_error}.

-spec listen([{port, inet:ip_port()} | {certfile, string()}
	| {keyfile, string()} | {password, string()}
	| {ip, inet:ip_address()}])
	-> {ok, ssl:sslsocket()} | {error, atom()}.
listen(Opts) ->
	require([crypto, public_key, ssl]),
	{port, Port} = lists:keyfind(port, 1, Opts),
	Backlog = proplists:get_value(backlog, Opts, 1024),
	{certfile, CertFile} = lists:keyfind(certfile, 1, Opts),
	{keyfile, KeyFile} = lists:keyfind(keyfile, 1, Opts),
	{password, Password} = lists:keyfind(password, 1, Opts),
	ListenOpts0 = [binary, {active, false},
		{backlog, Backlog}, {packet, raw}, {reuseaddr, true},
		{certfile, CertFile}, {keyfile, KeyFile}, {password, Password}],
	ListenOpts =
		case lists:keyfind(ip, 1, Opts) of
			false -> ListenOpts0;
			Ip -> [Ip|ListenOpts0]
		end,
	ssl:listen(Port, ListenOpts).

-spec accept(ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	case ssl:transport_accept(LSocket, Timeout) of
		{ok, CSocket} ->
			ssl_accept(CSocket, Timeout);
		{error, Reason} ->
			{error, Reason}
	end.

-spec recv(ssl:sslsocket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	ssl:recv(Socket, Length, Timeout).

-spec send(ssl:sslsocket(), iolist()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	ssl:send(Socket, Packet).

-spec setopts(ssl:sslsocket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	ssl:setopts(Socket, Opts).

-spec controlling_process(ssl:sslsocket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	ssl:controlling_process(Socket, Pid).

-spec peername(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:ip_port()}} | {error, atom()}.
peername(Socket) ->
	ssl:peername(Socket).

-spec close(ssl:sslsocket()) -> ok.
close(Socket) ->
	ssl:close(Socket).

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
