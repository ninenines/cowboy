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

%% @doc TCP transport API when behind an proxy which supports the
%% haproxy PROXY protocol.
%%
%% Wrapper around <em>gen_tcp</em> implementing the Cowboy transport
%% API when behind a proxy such as haproxy.
%%
%% @see gen_tcp
-module(cowboy_haproxy_transport).

-export([name/0, messages/0, listen/1, accept/2, recv/3, send/2, setopts/2,
	controlling_process/2, peername/1, close/1, sockname/1]).

-include_lib("eunit/include/eunit.hrl").

-record(haproxy_socket, {
	socket = undefined :: undefined | inet:socket(),
	protocol = undefined :: undefined | tcp4 | tcp6 | unknown,
	source_addr = undefined :: undefined | inet:ip_address(),
	dest_addr = undefined :: undefined | inet:ip_address(),
	source_port = undefined :: undefined | inet:port_number(),
	dest_port = undefined :: undefined | inet:port_number()
}).

-type haproxy_socket() :: #haproxy_socket{}.
-export_type([haproxy_socket/0]).

-define(PROXY_LINE_TIMEOUT, 5000).

%% @doc Name of this transport API, <em>tcp</em>.
-spec name() -> tcp.
name() -> tcp.

%% @doc Atoms used in the process messages sent by this API.
%%
%% They identify incoming data, closed connection and errors when receiving
%% data in active mode.
-spec messages() -> {tcp, tcp_closed, tcp_error}.
messages() -> {tcp, tcp_closed, tcp_error}.

%% @doc Setup a socket to listen on the given port on the local host.
%%
%% The available options are:
%% <dl>
%%  <dt>port</dt><dd>Mandatory. TCP port number to open.</dd>
%%  <dt>backlog</dt><dd>Maximum length of the pending connections queue.
%%   Defaults to 1024.</dd>
%%  <dt>ip</dt><dd>Interface to listen on. Listen on all interfaces
%%   by default.</dd>
%% </dl>
%%
%% @see gen_tcp:listen/2
-spec listen([{port, inet:port_number()} | {ip, inet:ip_address()}])
	-> {ok, inet:socket()} | {error, atom()}.
listen(Opts) ->
	{port, Port} = lists:keyfind(port, 1, Opts),
	Backlog = proplists:get_value(backlog, Opts, 1024),
	ListenOpts0 = [binary, {active, false},
		{backlog, Backlog}, {packet, raw}, {reuseaddr, true}],
	ListenOpts =
		case lists:keyfind(ip, 1, Opts) of
			false -> ListenOpts0;
			Ip -> [Ip|ListenOpts0]
		end,
	gen_tcp:listen(Port, ListenOpts).

%% @doc Accept an incoming connection on a listen socket.
%% @see gen_tcp:accept/2
-spec accept(inet:socket(), timeout())
	-> {ok, #haproxy_socket{}} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	case gen_tcp:accept(LSocket, Timeout) of
		{ok, Socket} ->
			case receive_proxy_line(#haproxy_socket{socket=Socket}) of
				{ok, _PSocket} = Success -> Success;
				{error, _PReason} = Error ->
					gen_tcp:close(Socket),
					Error
			end;
		{error, _Reason} = Error ->
			Error
	end.

%% @doc Receive a packet from a socket in passive mode.
%% @see gen_tcp:recv/3
-spec recv(#haproxy_socket{}, non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(#haproxy_socket{socket=Socket}, Length, Timeout) ->
	gen_tcp:recv(Socket, Length, Timeout).

%% @doc Send a packet on a socket.
%% @see gen_tcp:send/2
-spec send(#haproxy_socket{}, iolist()) -> ok | {error, atom()}.
send(#haproxy_socket{socket=Socket}, Packet) ->
	gen_tcp:send(Socket, Packet).

%% @doc Set one or more options for a socket.
%% @see inet:setopts/2
-spec setopts(#haproxy_socket{}, list()) -> ok | {error, atom()}.
setopts(#haproxy_socket{socket=Socket}, Opts) ->
	inet:setopts(Socket, Opts).

%% @doc Assign a new controlling process <em>Pid</em> to <em>Socket</em>.
%% @see gen_tcp:controlling_process/2
-spec controlling_process(#haproxy_socket{}, pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(#haproxy_socket{socket=Socket}, Pid) ->
	gen_tcp:controlling_process(Socket, Pid).

%% @doc Return the address and port for the other end of a connection.
%% @see inet:peername/1
-spec peername(#haproxy_socket{})
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(#haproxy_socket{source_addr=Addr, source_port=Port}) ->
	{ok, {Addr, Port}}.

%% @doc Close a TCP socket.
%% @see gen_tcp:close/1
-spec close(#haproxy_socket{}) -> ok.
close(#haproxy_socket{socket=Socket}) ->
	gen_tcp:close(Socket).

%% @doc Get the local address and port of a socket
%% @see inet:sockname/1
-spec sockname(#haproxy_socket{})
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(#haproxy_socket{dest_addr=Addr, dest_port=Port}) ->
	{ok, {Addr, Port}}.

%% @doc Parse the haproxy PROXY protocol line
-spec receive_proxy_line(#haproxy_socket{})
	-> {ok, #haproxy_socket{}} | {error, atom()}.
receive_proxy_line(PSocket=#haproxy_socket{socket=Socket}) ->
	{ok, OriginalOpts} = inet:getopts(Socket, [active, packet]),
	ok = inet:setopts(Socket, [{active, once}, {packet, line}]),
	Result = receive_parse_proxy_line(PSocket),
	ok = inet:setopts(Socket, OriginalOpts),
	Result.

%% @doc Receive the PROXY line and parse it
-spec receive_parse_proxy_line(#haproxy_socket{})
	-> {ok, #haproxy_socket{}} | {error, atom()}.
receive_parse_proxy_line(PSocket=#haproxy_socket{socket=Socket}) ->
	receive
		{tcp, Socket, ProxyLine} ->
			parse_proxy_line(PSocket, ProxyLine);
		{tcp, Socket, _InvalidData} ->
			{error, proxy_line_not_received};
		_Other ->
			{error, unexpected_socket_message}
	after ?PROXY_LINE_TIMEOUT ->
		{error, proxy_line_timeout}
	end.

%% @doc Parse the PROXY line received by the socket
-spec parse_proxy_line(#haproxy_socket{}, binary())
	-> {ok, #haproxy_socket{}} | {error, atom()}.
parse_proxy_line(PSocket, <<"PROXY ", ProxyLine/binary>>) ->
	case binary:split(ProxyLine, <<" ">>, [trim, global]) of
		[ProtoBin, SrcAddrBin, DestAddrBin, SrcPortBin, DestPortBin] ->
			Proto = parse_proxy_protocol(ProtoBin),
			SrcAddrResult = parse_proxy_address(SrcAddrBin),
			DestAddrResult = parse_proxy_address(DestAddrBin),
			SrcPortResult = parse_proxy_port(SrcPortBin),
			DestPortResult = parse_proxy_port(DestPortBin),
			case SrcAddrResult of
				{error, _Reason} -> {error, invalid_proxy_line};
				{ok, SrcAddr} ->
					case DestAddrResult of
						{error, _Reason} -> {error, invalid_proxy_line};
						{ok, DestAddr} ->
							case SrcPortResult of
								{error, _Reason} -> {error, invalid_proxy_line};
								{ok, SrcPort} ->
									case DestPortResult of
										{error, _Reason} -> {error, invalid_proxy_line};
										{ok, DestPort} ->
											{ok, PSocket#haproxy_socket{
												protocol = Proto,
												source_addr = SrcAddr,
												dest_addr = DestAddr,
												source_port = SrcPort,
												dest_port = DestPort
											}}
									end
							end
					end
			end;
		_InvalidLine -> {error, invalid_proxy_line}
	end;
parse_proxy_line(_PSocket, _Invalid) ->
	{error, proxy_line_not_received}.

%% @doc Parse the protocol fragment of a PROXY line
-spec parse_proxy_protocol(binary()) -> tcp4 | tcp6 | unknown.
parse_proxy_protocol(<<"TCP4">>) -> tcp4;
parse_proxy_protocol(<<"TCP6">>) -> tcp6;
parse_proxy_protocol(_) -> unknown.

%% @doc Parse an address from a PROXY line
-spec parse_proxy_address(binary())
	-> {ok, inet:ip_address()} | {error, atom()}.
parse_proxy_address(AddressBin) ->
	AddressList = binary_to_list(AddressBin),
	inet_parse:address(AddressList).

%% @doc Parse a port from a PROXY line, ignoring trailing \r\n
-spec parse_proxy_port(binary())
	-> {ok, inet:port_number()} | {error, atom()}.
parse_proxy_port(PortBin) ->
	PortList = binary_to_list(PortBin),
	case string:to_integer(PortList) of
		{error, _Reason} = Error ->
			Error;
		{Port, _Trail} ->
			{ok, Port}
	end.

%% Tests.

-ifdef(TEST).

proxy_line_test_() ->
	Tests = [
		{<<"PROXY TCP4 192.168.0.1 192.168.0.11 56324 443\r\n">>,
			{ok, #haproxy_socket{
				protocol = tcp4,
				source_addr = {192,168,0,1},
				dest_addr = {192,168,0,11},
				source_port = 56324,
				dest_port = 443
			}}},
		{<<"PROXY TCP6 fe80:100:7f:ffee::128 ::ffff:192.168.0.11 1009 443\r\n">>,
			{ok, #haproxy_socket{
				protocol = tcp6,
				source_addr = {65152,256,127,65518,0,0,0,296},
				dest_addr = {0,0,0,0,0,65535,49320,11},
				source_port = 1009,
				dest_port = 443
			}}},
		{<<"PROXY TCP4 192.168.0.1 192.168.0.11 56324\r\n">>,
			{error, invalid_proxy_line}},
		{<<"PROXY TCP4 192.168.0.1 garbage 56324 443\r\n">>,
			{error, invalid_proxy_line}},
		{<<"TCP4 192.168.0.1 192.168.0.11 56324\r\n">>,
			{error, proxy_line_not_received}}
	],
	Parse = fun(H) -> parse_proxy_line(#haproxy_socket{}, H) end,
	[{H, fun() -> R = Parse(H) end} || {H, R} <- Tests].

-endif.
