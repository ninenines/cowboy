%% Copyright (c) 2012, Loïc Hoguin <essen@ninenines.eu>
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

%% @private
-module(cowboy_client).

-export([init/1]).
-export([state/1]).
-export([transport/1]).

-export([connect/4]).
-export([raw_request/2]).
-export([request/3]).
-export([request/4]).
-export([request/5]).
-export([response/1]).
-export([response_body/1]).
-export([skip_body/1]).
-export([stream_status/1]).
-export([stream_headers/1]).
-export([stream_header/1]).
-export([stream_body/1]).

-record(client, {
	state = wait :: wait | request | response | response_body,
	opts = [] :: [any()],
	socket = undefined :: undefined | inet:socket(),
	transport = undefined :: module(),
	timeout = 5000 :: timeout(), %% @todo Configurable.
	buffer = <<>> :: binary(),
	connection = keepalive :: keepalive | close,
	version = {1, 1} :: cowboy_http:version(),
	response_body = undefined :: undefined | non_neg_integer()
}).

init(Opts) ->
	{ok, #client{opts=Opts}}.

state(#client{state=State}) ->
	State.

transport(#client{socket=undefined}) ->
	{error, notconnected};
transport(#client{transport=Transport, socket=Socket}) ->
	{ok, Transport, Socket}.

connect(Transport, Host, Port, Client)
		when is_binary(Host) ->
	connect(Transport, binary_to_list(Host), Port, Client);
connect(Transport, Host, Port, Client=#client{state=State, opts=Opts})
		when is_atom(Transport), is_list(Host),
			is_integer(Port), is_record(Client, client),
			State =:= wait ->
	{ok, Socket} = Transport:connect(Host, Port, Opts),
	{ok, Client#client{state=request, socket=Socket, transport=Transport}}.

raw_request(Data, Client=#client{state=response_body}) ->
	{done, Client2} = skip_body(Client),
	raw_request(Data, Client2);
raw_request(Data, Client=#client{
		state=State, socket=Socket, transport=Transport})
		when State =:= request ->
	ok = Transport:send(Socket, Data),
	{ok, Client}.

request(Method, URL, Client) ->
	request(Method, URL, [], <<>>, Client).

request(Method, URL, Headers, Client) ->
	request(Method, URL, Headers, <<>>, Client).

request(Method, URL, Headers, Body, Client=#client{state=response_body}) ->
	{done, Client2} = skip_body(Client),
	request(Method, URL, Headers, Body, Client2);
request(Method, URL, Headers, Body, Client=#client{
		state=State, version=Version})
		when State =:= wait; State =:= request ->
	{Transport, FullHost, Host, Port, Path} = parse_url(URL),
	{ok, Client2} = case State of
		wait -> connect(Transport, Host, Port, Client);
		request -> {ok, Client}
	end,
	VersionBin = cowboy_http:version_to_binary(Version),
	%% @todo do keepalive too, allow override...
	Headers2 = [
		{<<"host">>, FullHost},
		{<<"user-agent">>, <<"Cow">>}
	|Headers],
	Headers3 = case iolist_size(Body) of
		0 -> Headers2;
		Length -> [{<<"content-length">>, integer_to_list(Length)}|Headers2]
	end,
	HeadersData = [[Name, <<": ">>, Value, <<"\r\n">>]
		|| {Name, Value} <- Headers3],
	Data = [Method, <<" ">>, Path, <<" ">>, VersionBin, <<"\r\n">>,
		HeadersData, <<"\r\n">>, Body],
	raw_request(Data, Client2).

parse_url(<< "https://", Rest/binary >>) ->
	parse_url(Rest, cowboy_ssl_transport);
parse_url(<< "http://", Rest/binary >>) ->
	parse_url(Rest, cowboy_tcp_transport);
parse_url(URL) ->
	parse_url(URL, cowboy_tcp_transport).

parse_url(URL, Transport) ->
	case binary:split(URL, <<"/">>) of
		[Peer] ->
			{Host, Port} = parse_peer(Peer, Transport),
			{Transport, Peer, Host, Port, <<"/">>};
		[Peer, Path] ->
			{Host, Port} = parse_peer(Peer, Transport),
			{Transport, Peer, Host, Port, [<<"/">>, Path]}
	end.

parse_peer(Peer, Transport) ->
	case binary:split(Peer, <<":">>) of
		[Host] when Transport =:= cowboy_tcp_transport ->
			{binary_to_list(Host), 80};
		[Host] when Transport =:= cowboy_ssl_transport ->
			{binary_to_list(Host), 443};
		[Host, Port] ->
			{binary_to_list(Host), list_to_integer(binary_to_list(Port))}
	end.

response(Client=#client{state=response_body}) ->
	{done, Client2} = skip_body(Client),
	response(Client2);
response(Client=#client{state=request}) ->
	case stream_status(Client) of
		{ok, Status, _, Client2} ->
			case stream_headers(Client2) of
				{ok, Headers, Client3} ->
					{ok, Status, Headers, Client3};
				{error, Reason} ->
					{error, Reason}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

response_body(Client=#client{state=response_body}) ->
	response_body_loop(Client, <<>>).

response_body_loop(Client, Acc) ->
	case stream_body(Client) of
		{ok, Data, Client2} ->
			response_body_loop(Client2, << Acc/binary, Data/binary >>);
		{done, Client2} ->
			{ok, Acc, Client2};
		{error, Reason} ->
			{error, Reason}
	end.

skip_body(Client=#client{state=response_body}) ->
	case stream_body(Client) of
		{ok, _, Client2} -> skip_body(Client2);
		Done -> Done
	end.

stream_status(Client=#client{state=State, buffer=Buffer})
		when State =:= request ->
	case binary:split(Buffer, <<"\r\n">>) of
		[Line, Rest] ->
			parse_status(Client#client{state=response, buffer=Rest}, Line);
		_ ->
			case recv(Client) of
				{ok, Data} ->
					Buffer2 = << Buffer/binary, Data/binary >>,
					stream_status(Client#client{buffer=Buffer2});
				{error, Reason} ->
					{error, Reason}
			end
	end.

parse_status(Client, << "HTTP/", High, ".", Low, " ",
		S3, S2, S1, " ", StatusStr/binary >>)
		when High >= $0, High =< $9, Low >= $0, Low =< $9,
			S3 >= $0, S3 =< $9, S2 >= $0, S2 =< $9, S1 >= $0, S1 =< $9 ->
	Version = {High - $0, Low - $0},
	Status = (S3 - $0) * 100 + (S2 - $0) * 10 + S1 - $0,
	{ok, Status, StatusStr, Client#client{version=Version}}.

stream_headers(Client=#client{state=State})
		when State =:= response ->
	stream_headers(Client, []).

stream_headers(Client, Acc) ->
	case stream_header(Client) of
		{ok, Name, Value, Client2} ->
			stream_headers(Client2, [{Name, Value}|Acc]);
		{done, Client2} ->
			{ok, Acc, Client2};
		{error, Reason} ->
			{error, Reason}
	end.

stream_header(Client=#client{state=State, buffer=Buffer,
		response_body=RespBody}) when State =:= response ->
	case binary:split(Buffer, <<"\r\n">>) of
		[<<>>, Rest] ->
			%% If we have a body, set response_body.
			Client2 = case RespBody of
				undefined -> Client#client{state=request};
				0 -> Client#client{state=request};
				_ -> Client#client{state=response_body}
			end,
			{done, Client2#client{buffer=Rest}};
		[Line, Rest] ->
			%% @todo Do a better parsing later on.
			[Name, Value] = binary:split(Line, <<": ">>),
			Name2 = cowboy_bstr:to_lower(Name),
			Client2 = case Name2 of
				<<"content-length">> ->
					Length = list_to_integer(binary_to_list(Value)),
					if Length >= 0 -> ok end,
					Client#client{response_body=Length};
				_ ->
					Client
			end,
			{ok, Name2, Value, Client2#client{buffer=Rest}};
		_ ->
			case recv(Client) of
				{ok, Data} ->
					Buffer2 = << Buffer/binary, Data/binary >>,
					stream_header(Client#client{buffer=Buffer2});
				{error, Reason} ->
					{error, Reason}
			end
	end.

stream_body(Client=#client{state=response_body, response_body=RespBody})
		when RespBody =:= undefined; RespBody =:= 0 ->
	{done, Client#client{state=request, response_body=undefined}};
stream_body(Client=#client{state=response_body, buffer=Buffer,
		response_body=Length}) when is_integer(Length) ->
	case byte_size(Buffer) of
		0 ->
			case recv(Client) of
				{ok, Body} when byte_size(Body) =< Length ->
					Length2 = Length - byte_size(Body),
					{ok, Body, Client#client{response_body=Length2}};
				{ok, Data} ->
					<< Body:Length/binary, Rest/binary >> = Data,
					{ok, Body, Client#client{buffer=Rest,
						response_body=undefined}};
				{error, Reason} ->
					{error, Reason}
			end;
		N when N =< Length ->
			Length2 = Length - N,
			{ok, Buffer, Client#client{buffer= <<>>, response_body=Length2}};
		_ ->
			<< Body:Length/binary, Rest/binary >> = Buffer,
			{ok, Body, Client#client{buffer=Rest, response_body=undefined}}
	end.

recv(#client{socket=Socket, transport=Transport, timeout=Timeout}) ->
	Transport:recv(Socket, 0, Timeout).
