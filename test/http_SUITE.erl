%% Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(http_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(ct_helper, [get_remote_pid_tcp/1]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [raw_open/1]).
-import(cowboy_test, [raw_send/2]).
-import(cowboy_test, [raw_recv_head/1]).
-import(cowboy_test, [raw_recv/3]).
-import(cowboy_test, [raw_expect_recv/2]).

all() -> [{group, clear}].

groups() -> [{clear, [parallel], ct_helper:all(?MODULE)}].

init_per_group(Name, Config) ->
	cowboy_test:init_http(Name, #{
		env => #{dispatch => init_dispatch(Config)}
	}, Config).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

init_dispatch(_) ->
	cowboy_router:compile([{"localhost", [
		{"/", hello_h, []},
		{"/echo/:key", echo_h, []},
		{"/resp/:key[/:arg]", resp_h, []},
		{"/set_options/:key", set_options_h, []}
	]}]).

chunked_false(Config) ->
	doc("Confirm the option chunked => false disables chunked "
		"transfer-encoding for HTTP/1.1 connections."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		chunked => false
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "GET /resp/stream_reply2/200 HTTP/1.1\r\nhost: localhost\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		Rest = case catch raw_recv_head(Client) of
			{'EXIT', _} -> error(closed);
			Data ->
				%% Cowboy always advertises itself as HTTP/1.1.
				{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
				{Headers, Rest1} = cow_http:parse_headers(Rest0),
				false = lists:keyfind(<<"content-length">>, 1, Headers),
				false = lists:keyfind(<<"transfer-encoding">>, 1, Headers),
				Rest1
		end,
		Bits = 8000000 - bit_size(Rest),
		raw_expect_recv(Client, <<0:Bits>>),
		{error, closed} = raw_recv(Client, 1, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

chunked_one_byte_at_a_time(Config) ->
	doc("Confirm that chunked transfer-encoding works when "
		"the body is received one byte at a time."),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	ChunkedBody = iolist_to_binary(do_chunked_body(50, Body, [])),
	Client = raw_open(Config),
	ok = raw_send(Client,
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n\r\n"),
	_ = [begin
		raw_send(Client, <<C>>),
		timer:sleep(10)
	end || <<C>> <= ChunkedBody],
	Rest = case catch raw_recv_head(Client) of
		{'EXIT', _} -> error(closed);
		Data ->
			{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
			{_, Rest1} = cow_http:parse_headers(Rest0),
			Rest1
	end,
	RestSize = byte_size(Rest),
	<<Rest:RestSize/binary, Expect/bits>> = Body,
	raw_expect_recv(Client, Expect).

chunked_one_chunk_at_a_time(Config) ->
	doc("Confirm that chunked transfer-encoding works when "
		"the body is received one chunk at a time."),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = do_chunked_body(50, Body, []),
	Client = raw_open(Config),
	ok = raw_send(Client,
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n\r\n"),
	_ = [begin
		raw_send(Client, Chunk),
		timer:sleep(10)
	end || Chunk <- Chunks],
	Rest = case catch raw_recv_head(Client) of
		{'EXIT', _} -> error(closed);
		Data ->
			{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
			{_, Rest1} = cow_http:parse_headers(Rest0),
			Rest1
	end,
	RestSize = byte_size(Rest),
	<<Rest:RestSize/binary, Expect/bits>> = Body,
	raw_expect_recv(Client, Expect).

chunked_split_delay_in_chunk_body(Config) ->
	doc("Confirm that chunked transfer-encoding works when "
		"the body is received with a delay inside the chunks."),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = do_chunked_body(50, Body, []),
	Client = raw_open(Config),
	ok = raw_send(Client,
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n\r\n"),
	_ = [begin
		case Chunk of
			<<"0\r\n\r\n">> ->
				raw_send(Client, Chunk);
			_ ->
				[Size, ChunkBody, <<>>] = binary:split(Chunk, <<"\r\n">>, [global]),
				PartASize = rand:uniform(byte_size(ChunkBody)),
				<<PartA:PartASize/binary, PartB/binary>> = ChunkBody,
				raw_send(Client, [Size, <<"\r\n">>, PartA]),
				timer:sleep(10),
				raw_send(Client, [PartB, <<"\r\n">>])
		end
	end || Chunk <- Chunks],
	Rest = case catch raw_recv_head(Client) of
		{'EXIT', _} -> error(closed);
		Data ->
			{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
			{_, Rest1} = cow_http:parse_headers(Rest0),
			Rest1
	end,
	RestSize = byte_size(Rest),
	<<Rest:RestSize/binary, Expect/bits>> = Body,
	raw_expect_recv(Client, Expect).

chunked_split_delay_in_chunk_crlf(Config) ->
	doc("Confirm that chunked transfer-encoding works when "
		"the body is received with a delay inside the chunks end CRLF."),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = do_chunked_body(50, Body, []),
	Client = raw_open(Config),
	ok = raw_send(Client,
		"POST /echo/read_body HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Transfer-encoding: chunked\r\n\r\n"),
	_ = [begin
		Len = byte_size(Chunk) - (rand:uniform(2) - 1),
		<<Begin:Len/binary, End/binary>> = Chunk,
		raw_send(Client, Begin),
		timer:sleep(10),
		raw_send(Client, End)
	end || Chunk <- Chunks],
	Rest = case catch raw_recv_head(Client) of
		{'EXIT', _} -> error(closed);
		Data ->
			{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
			{_, Rest1} = cow_http:parse_headers(Rest0),
			Rest1
	end,
	RestSize = byte_size(Rest),
	<<Rest:RestSize/binary, Expect/bits>> = Body,
	raw_expect_recv(Client, Expect).

do_chunked_body(_, <<>>, Acc) ->
	lists:reverse([cow_http_te:last_chunk()|Acc]);
do_chunked_body(ChunkSize0, Data, Acc) ->
	ChunkSize = min(byte_size(Data), ChunkSize0),
	<<Chunk:ChunkSize/binary, Rest/binary>> = Data,
	do_chunked_body(ChunkSize, Rest,
		[iolist_to_binary(cow_http_te:chunk(Chunk))|Acc]).

http10_keepalive_false(Config) ->
	doc("Confirm the option http10_keepalive => false disables keep-alive "
		"completely for HTTP/1.0 connections."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		http10_keepalive => false
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Keepalive = "GET / HTTP/1.0\r\nhost: localhost\r\nConnection: keep-alive\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Keepalive),
		_ = case catch raw_recv_head(Client) of
			{'EXIT', _} -> error(closed);
			Data ->
				%% Cowboy always advertises itself as HTTP/1.1.
				{'HTTP/1.1', 200, _, Rest} = cow_http:parse_status_line(Data),
				{Headers, _} = cow_http:parse_headers(Rest),
				{_, <<"close">>} = lists:keyfind(<<"connection">>, 1, Headers)
		end,
		ok = raw_send(Client, Keepalive),
		case catch raw_recv_head(Client) of
			{'EXIT', _} -> closed;
			_ -> error(not_closed)
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

idle_timeout_infinity(Config) ->
	doc("Ensure the idle_timeout option accepts the infinity value."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		idle_timeout => infinity
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		timer:sleep(500),
		#{socket := Socket} = gun:info(ConnPid),
		Pid = get_remote_pid_tcp(Socket),
		_ = gun:post(ConnPid, "/echo/read_body",
			[{<<"content-type">>, <<"text/plain">>}]),
		Ref = erlang:monitor(process, Pid),
		receive
			{'DOWN', Ref, process, Pid, Reason} ->
				error(Reason)
		after 1000 ->
			ok
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

request_timeout_infinity(Config) ->
	doc("Ensure the request_timeout option accepts the infinity value."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		request_timeout => infinity
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		timer:sleep(500),
		#{socket := Socket} = gun:info(ConnPid),
		Pid = get_remote_pid_tcp(Socket),
		Ref = erlang:monitor(process, Pid),
		receive
			{'DOWN', Ref, process, Pid, Reason} ->
				error(Reason)
		after 1000 ->
			ok
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

set_options_chunked_false(Config) ->
	doc("Confirm the option chunked can be dynamically set to disable "
		"chunked transfer-encoding. This results in the closing of the "
		"connection after the current request."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		chunked => true
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Request = "GET /set_options/chunked_false HTTP/1.1\r\nhost: localhost\r\n\r\n",
		Client = raw_open([{type, tcp}, {port, Port}, {opts, []}|Config]),
		ok = raw_send(Client, Request),
		Rest = case catch raw_recv_head(Client) of
			{'EXIT', _} -> error(closed);
			Data ->
				%% Cowboy always advertises itself as HTTP/1.1.
				{'HTTP/1.1', 200, _, Rest0} = cow_http:parse_status_line(Data),
				{Headers, Rest1} = cow_http:parse_headers(Rest0),
				false = lists:keyfind(<<"content-length">>, 1, Headers),
				false = lists:keyfind(<<"transfer-encoding">>, 1, Headers),
				Rest1
		end,
		Bits = 8000000 - bit_size(Rest),
		raw_expect_recv(Client, <<0:Bits>>),
		{error, closed} = raw_recv(Client, 1, 1000)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

set_options_chunked_false_ignored(Config) ->
	doc("Confirm the option chunked can be dynamically set to disable "
		"chunked transfer-encoding, and that it is ignored if the "
		"response is not streamed."),
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		chunked => true
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		%% We do a first request setting the option but not
		%% using chunked transfer-encoding in the response.
		StreamRef1 = gun:get(ConnPid, "/set_options/chunked_false_ignored"),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef1),
		{ok, <<"Hello world!">>} = gun:await_body(ConnPid, StreamRef1),
		%% We then do a second request to confirm that chunked
		%% is not disabled for that second request.
		StreamRef2 = gun:get(ConnPid, "/resp/stream_reply2/200"),
		{response, nofin, 200, Headers} = gun:await(ConnPid, StreamRef2),
		{_, <<"chunked">>} = lists:keyfind(<<"transfer-encoding">>, 1, Headers)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

set_options_idle_timeout(Config) ->
	doc("Confirm that the idle_timeout option can be dynamically "
		"set to change how long Cowboy will wait before it closes the connection."),
	%% We start with a long timeout and then cut it short.
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		idle_timeout => 60000
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		timer:sleep(500),
		#{socket := Socket} = gun:info(ConnPid),
		Pid = get_remote_pid_tcp(Socket),
		_ = gun:post(ConnPid, "/set_options/idle_timeout_short",
			[{<<"content-type">>, <<"text/plain">>}]),
		Ref = erlang:monitor(process, Pid),
		receive
			{'DOWN', Ref, process, Pid, _} ->
				ok
		after 2000 ->
			error(timeout)
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

set_options_idle_timeout_only_applies_to_current_request(Config) ->
	doc("Confirm that changes to the idle_timeout option only apply to the current stream."),
	%% We start with a long timeout and then cut it short.
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], #{
		env => #{dispatch => init_dispatch(Config)},
		idle_timeout => 500
	}),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		ConnPid = gun_open([{type, tcp}, {protocol, http}, {port, Port}|Config]),
		{ok, http} = gun:await_up(ConnPid),
		timer:sleep(500),
		#{socket := Socket} = gun:info(ConnPid),
		Pid = get_remote_pid_tcp(Socket),
		StreamRef = gun:post(ConnPid, "/set_options/idle_timeout_long",
			[{<<"content-type">>, <<"text/plain">>}]),
		Ref = erlang:monitor(process, Pid),
		receive
			{'DOWN', Ref, process, Pid, Reason} ->
				error(Reason)
		after 2000 ->
			ok
		end,
		%% Finish the first request and start a second one to confirm
		%% the idle_timeout option is back to normal.
		gun:data(ConnPid, StreamRef, fin, <<"Hello!">>),
		{response, nofin, 200, _} = gun:await(ConnPid, StreamRef),
		{ok, <<"Hello!">>} = gun:await_body(ConnPid, StreamRef),
		_ = gun:post(ConnPid, "/echo/read_body",
			[{<<"content-type">>, <<"text/plain">>}]),
		receive
			{'DOWN', Ref, process, Pid, _} ->
				ok
		after 2000 ->
			error(timeout)
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

switch_protocol_flush(Config) ->
	doc("Confirm that switch_protocol does not flush unrelated messages."),
	ProtoOpts = #{
		env => #{dispatch => init_dispatch(Config)},
		stream_handlers => [switch_protocol_flush_h]
	},
	{ok, _} = cowboy:start_clear(?FUNCTION_NAME, [{port, 0}], ProtoOpts),
	Port = ranch:get_port(?FUNCTION_NAME),
	try
		Self = self(),
		ConnPid = gun_open([{port, Port}, {type, tcp}, {protocol, http}|Config]),
		_ = gun:get(ConnPid, "/", [
			{<<"x-test-pid">>, pid_to_list(Self)}
		]),
		receive
			{Self, Events} ->
				switch_protocol_flush_h:validate(Events)
		after 5000 ->
			error(timeout)
		end
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.
