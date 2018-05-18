%% Copyright (c) 2011-2017, Loïc Hoguin <essen@ninenines.eu>
%% Copyright (c) 2011, Anthony Ramine <nox@dev-extend.eu>
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

-module(old_http_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [gun_open/2]).
-import(cowboy_test, [gun_down/1]).
-import(cowboy_test, [raw_open/1]).
-import(cowboy_test, [raw_send/2]).
-import(cowboy_test, [raw_recv_head/1]).
-import(cowboy_test, [raw_expect_recv/2]).

%% ct.

all() ->
	[
		{group, http},
		{group, https},
		{group, http_compress},
		{group, https_compress}
	].

groups() ->
	Tests = ct_helper:all(?MODULE),
	[
		{http, [], Tests}, %% @todo parallel
		{https, [parallel], Tests},
		{http_compress, [parallel], Tests},
		{https_compress, [parallel], Tests}
	].

init_per_group(Name = http, Config) ->
	cowboy_test:init_http(Name, #{env => #{dispatch => init_dispatch(Config)}}, Config);
init_per_group(Name = https, Config) ->
	cowboy_test:init_https(Name, #{env => #{dispatch => init_dispatch(Config)}}, Config);
init_per_group(Name = http_compress, Config) ->
	cowboy_test:init_http(Name, #{
		env => #{dispatch => init_dispatch(Config)},
		compress => true
	}, Config);
init_per_group(Name = https_compress, Config) ->
	cowboy_test:init_https(Name, #{
		env => #{dispatch => init_dispatch(Config)},
		compress => true
	}, Config).

end_per_group(Name, _) ->
	ok = cowboy:stop_listener(Name).

%% Dispatch configuration.

init_dispatch(_) ->
	cowboy_router:compile([
		{"localhost", [
			{"/chunked_response", http_chunked, []},
			{"/headers/dupe", http_handler,
				[{headers, #{<<"connection">> => <<"close">>}}]},
			{"/set_resp/header", http_set_resp,
				[{headers, #{<<"vary">> => <<"Accept">>}}]},
			{"/set_resp/overwrite", http_set_resp,
				[{headers, #{<<"server">> => <<"DesireDrive/1.0">>}}]},
			{"/set_resp/body", http_set_resp,
				[{body, <<"A flameless dance does not equal a cycle">>}]},
			{"/handler_errors", http_errors, []},
			{"/echo/body", http_echo_body, []},
			{"/param_all", rest_param_all, []},
			{"/bad_accept", rest_simple_resource, []},
			{"/bad_content_type", rest_patch_resource, []},
			{"/simple", rest_simple_resource, []},
			{"/forbidden_post", rest_forbidden_resource, [true]},
			{"/simple_post", rest_forbidden_resource, [false]},
			{"/missing_get_callbacks", rest_missing_callbacks, []},
			{"/missing_put_callbacks", rest_missing_callbacks, []},
			{"/nodelete", rest_nodelete_resource, []},
			{"/post_charset", rest_post_charset_resource, []},
			{"/postonly", rest_postonly_resource, []},
			{"/patch", rest_patch_resource, []},
			{"/resetags", rest_resource_etags, []},
			{"/rest_expires", rest_expires, []},
			{"/rest_expires_binary", rest_expires_binary, []},
			{"/rest_empty_resource", rest_empty_resource, []},
			{"/loop_stream_recv", http_loop_stream_recv, []},
			{"/", http_handler, []}
		]}
	]).

%% Convenience functions.

do_raw(Data, Config) ->
	Client = raw_open(Config),
	ok = raw_send(Client, Data),
	case catch raw_recv_head(Client) of
		{'EXIT', _} -> closed;
		Resp -> element(2, cow_http:parse_status_line(Resp))
	end.

do_get(Path, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path),
	{response, _, Status, _} = gun:await(ConnPid, Ref),
	gun:close(ConnPid),
	Status.

%% Tests.

check_raw_status(Config) ->
	Huge = [$0 || _ <- lists:seq(1, 5000)],
	HugeCookie = lists:flatten(["whatever_man_biiiiiiiiiiiig_cookie_me_want_77="
		"Wed Apr 06 2011 10:38:52 GMT-0500 (CDT)" || _ <- lists:seq(1, 40)]),
	Tests = [
		{200, ["GET / HTTP/1.0\r\nHost: localhost\r\n"
			"Set-Cookie: ", HugeCookie, "\r\n\r\n"]},
		{200, "\r\n\r\n\r\n\r\n\r\nGET / HTTP/1.1\r\nHost: localhost\r\n\r\n"},
		{400, "\n"},
		{400, "Garbage\r\n\r\n"},
		{400, "\r\n\r\n\r\n\r\n\r\n\r\n"},
		{400, "GET  HTTP/1.1\r\nHost: localhost\r\n\r\n"},
		{400, "GET / HTTP/1.1\r\nHost: ninenines.eu\r\n\r\n"},
		{400, "GET / HTTP/1.1\r\nHost: localhost:bad_port\r\n\r\n"},
		{closed, Huge}
	],
	_ = [{Status, Packet} = begin
		Ret = do_raw(Packet, Config),
		{Ret, Packet}
	end || {Status, Packet} <- Tests],
	ok.

check_status(Config) ->
	Tests = [
		{200, "/simple"},
		{404, "/not/found"},
		{500, "/handler_errors?case=init_before_reply"}
	],
	_ = [{Status, URL} = begin
		Ret = do_get(URL, Config),
		{Ret, URL}
	end || {Status, URL} <- Tests].

error_init_after_reply(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/handler_errors?case=init_after_reply"),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	ok.

headers_dupe(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/headers/dupe"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	%% Ensure that only one connection header was received.
	[<<"close">>] = [V || {Name, V} <- Headers, Name =:= <<"connection">>],
	gun_down(ConnPid).

http10_chunkless(Config) ->
	ConnPid = gun_open(Config, #{http_opts => #{version => 'HTTP/1.0'}}),
	Ref = gun:get(ConnPid, "/chunked_response"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	false = lists:keyfind(<<"transfer-encoding">>, 1, Headers),
	{ok, <<"chunked_handler\r\nworks fine!">>} = gun:await_body(ConnPid, Ref),
	gun_down(ConnPid).

http10_hostless(Config) ->
	Name = http10_hostless,
	Port10 = config(port, Config) + 10,
	{Transport, Protocol} = case config(type, Config) of
		tcp -> {ranch_tcp, cowboy_clear};
		ssl -> {ranch_ssl, cowboy_tls}
	end,
	ranch:start_listener(Name, 5, Transport,
		config(opts, Config) ++ [{port, Port10}],
		Protocol, #{
			env =>#{dispatch => cowboy_router:compile([
				{'_', [{"/http1.0/hostless", http_handler, []}]}])},
			max_keepalive => 50,
			timeout => 500
	}),
	200 = do_raw("GET /http1.0/hostless HTTP/1.0\r\n\r\n",
		[{port, Port10}|Config]),
	cowboy:stop_listener(http10_hostless).

http10_keepalive_default(Config) ->
	Normal = "GET / HTTP/1.0\r\nhost: localhost\r\n\r\n",
	Client = raw_open(Config),
	ok = raw_send(Client, Normal),
	case catch raw_recv_head(Client) of
		{'EXIT', _} -> error(closed);
		Data ->
			%% Cowboy always advertises itself as HTTP/1.1.
			{'HTTP/1.1', 200, _, Rest} = cow_http:parse_status_line(Data),
			{Headers, _} = cow_http:parse_headers(Rest),
			{_, <<"close">>} = lists:keyfind(<<"connection">>, 1, Headers)
	end,
	ok = raw_send(Client, Normal),
	case catch raw_recv_head(Client) of
		{'EXIT', _} -> closed;
		_ -> error(not_closed)
	end.

http10_keepalive_forced(Config) ->
	Keepalive = "GET / HTTP/1.0\r\nhost: localhost\r\nConnection: keep-alive\r\n\r\n",
	Client = raw_open(Config),
	ok = raw_send(Client, Keepalive),
	case catch raw_recv_head(Client) of
		{'EXIT', _} -> error(closed);
		Data ->
			%% Cowboy always advertises itself as HTTP/1.1.
			{'HTTP/1.1', 200, _, Rest} = cow_http:parse_status_line(Data),
			{Headers, _} = cow_http:parse_headers(Rest),
			{_, <<"keep-alive">>} = lists:keyfind(<<"connection">>, 1, Headers)
	end,
	ok = raw_send(Client, Keepalive),
	case catch raw_recv_head(Client) of
		{'EXIT', Err} -> error({closed, Err});
		_ -> ok
	end.

keepalive_nl(Config) ->
	ConnPid = gun_open(Config),
	Refs = [begin
		Ref = gun:get(ConnPid, "/", [{<<"connection">>, <<"keep-alive">>}]),
		gun:dbg_send_raw(ConnPid, <<"\r\n">>),
		Ref
	end || _ <- lists:seq(1, 10)],
	_ = [begin
		{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
		false = lists:keymember(<<"connection">>, 1, Headers)
	end || Ref <- Refs],
	ok.

keepalive_stream_loop(Config) ->
	ConnPid = gun_open(Config),
	Refs = [begin
		Ref = gun:post(ConnPid, "/loop_stream_recv",
			[{<<"content-type">>, <<"application/octet-stream">>}]),
		_ = [gun:data(ConnPid, Ref, nofin, << ID:32 >>)
			|| ID <- lists:seq(1, 250)],
		gun:data(ConnPid, Ref, fin, <<>>),
		Ref
	end || _ <- lists:seq(1, 10)],
	_ = [begin
		{response, fin, 200, _} = gun:await(ConnPid, Ref)
	end || Ref <- Refs],
	ok.

do_nc(Config, Input) ->
	Cat = os:find_executable("cat"),
	Nc = os:find_executable("nc"),
	case {Cat, Nc} of
		{false, _} ->
			{skip, {notfound, cat}};
		{_, false} ->
			{skip, {notfound, nc}};
		_Good ->
			%% Throw garbage at the server then check if it's still up.
			StrPort = integer_to_list(config(port, Config)),
			[os:cmd("cat " ++ Input ++ " | nc localhost " ++ StrPort)
				|| _ <- lists:seq(1, 100)],
			200 = do_get("/", Config)
	end.

nc_rand(Config) ->
	do_nc(Config, "/dev/urandom").

nc_zero(Config) ->
	do_nc(Config, "/dev/zero").

rest_param_all(Config) ->
	ConnPid = gun_open(Config),
	%% Accept without param.
	Ref1 = gun:get(ConnPid, "/param_all",
		[{<<"accept">>, <<"text/plain">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref1),
	{ok, <<"[]">>} = gun:await_body(ConnPid, Ref1),
	%% Accept with param.
	Ref2 = gun:get(ConnPid, "/param_all",
		[{<<"accept">>, <<"text/plain;level=1">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref2),
	{ok, <<"level=1">>} = gun:await_body(ConnPid, Ref2),
	%% Accept with param and quality.
	Ref3 = gun:get(ConnPid, "/param_all",
		[{<<"accept">>, <<"text/plain;level=1;q=0.8, text/plain;level=2;q=0.5">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref3),
	{ok, <<"level=1">>} = gun:await_body(ConnPid, Ref3),
	Ref4 = gun:get(ConnPid, "/param_all",
		[{<<"accept">>, <<"text/plain;level=1;q=0.5, text/plain;level=2;q=0.8">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref4),
	{ok, <<"level=2">>} = gun:await_body(ConnPid, Ref4),
	%% Without Accept.
	Ref5 = gun:get(ConnPid, "/param_all"),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref5),
	{ok, <<"'*'">>} = gun:await_body(ConnPid, Ref5),
	%% Content-Type without param.
	Ref6 = gun:put(ConnPid, "/param_all",
		[{<<"content-type">>, <<"text/plain">>}]),
	gun:data(ConnPid, Ref6, fin, "Hello world!"),
	{response, fin, 204, _} = gun:await(ConnPid, Ref6),
	%% Content-Type with param.
	Ref7 = gun:put(ConnPid, "/param_all",
		[{<<"content-type">>, <<"text/plain; charset=utf-8">>}]),
	gun:data(ConnPid, Ref7, fin, "Hello world!"),
	{response, fin, 204, _} = gun:await(ConnPid, Ref7),
	ok.

rest_bad_accept(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/bad_accept",
		[{<<"accept">>, <<"1">>}]),
	{response, fin, 400, _} = gun:await(ConnPid, Ref),
	ok.

rest_bad_content_type(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:patch(ConnPid, "/bad_content_type",
		[{<<"content-type">>, <<"text/plain, text/html">>}], <<"Whatever">>),
	{response, fin, 415, _} = gun:await(ConnPid, Ref),
	ok.

rest_expires(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/rest_expires"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, Expires} = lists:keyfind(<<"expires">>, 1, Headers),
	{_, LastModified} = lists:keyfind(<<"last-modified">>, 1, Headers),
	Expires = LastModified = <<"Fri, 21 Sep 2012 22:36:14 GMT">>,
	ok.

rest_expires_binary(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/rest_expires_binary"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"0">>} = lists:keyfind(<<"expires">>, 1, Headers),
	ok.

rest_last_modified_undefined(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/simple",
		[{<<"if-modified-since">>, <<"Fri, 21 Sep 2012 22:36:14 GMT">>}]),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	ok.

rest_keepalive(Config) ->
	ConnPid = gun_open(Config),
	Refs = [gun:get(ConnPid, "/simple") || _ <- lists:seq(1, 10)],
	_ = [begin
		{response, nofin, 200, Headers} =  gun:await(ConnPid, Ref),
		false = lists:keymember(<<"connection">>, 1, Headers)
	end || Ref <- Refs],
	ok.

rest_keepalive_post(Config) ->
	ConnPid = gun_open(Config),
	Refs = [begin
		Ref1 = gun:post(ConnPid, "/forbidden_post", [
			{<<"content-type">>, <<"text/plain">>},
			{<<"content-length">>, <<"12">>}
		]),
		gun:data(ConnPid, Ref1, fin, "Hello world!"),
		Ref2 = gun:post(ConnPid, "/simple_post", [
			{<<"content-type">>, <<"text/plain">>},
			{<<"content-length">>, <<"12">>}
		]),
		gun:data(ConnPid, Ref2, fin, "Hello world!"),
		{Ref1, Ref2}
	end || _ <- lists:seq(1, 5)],
	_ = [begin
		{response, fin, 403, Headers1} = gun:await(ConnPid, Ref1),
		false = lists:keymember(<<"connection">>, 1, Headers1),
		{response, fin, 303, Headers2} = gun:await(ConnPid, Ref2),
		false = lists:keymember(<<"connection">>, 1, Headers2)
	end || {Ref1, Ref2} <- Refs],
	ok.

rest_missing_get_callbacks(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/missing_get_callbacks"),
	{response, fin, 500, _} = gun:await(ConnPid, Ref),
	ok.

rest_missing_put_callbacks(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:put(ConnPid, "/missing_put_callbacks",
		[{<<"content-type">>, <<"application/json">>}], <<"{}">>),
	{response, fin, 500, _} = gun:await(ConnPid, Ref),
	ok.

rest_nodelete(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:delete(ConnPid, "/nodelete"),
	{response, fin, 500, _} = gun:await(ConnPid, Ref),
	ok.

rest_options_default(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:options(ConnPid, "/rest_empty_resource"),
	{response, fin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"HEAD, GET, OPTIONS">>} = lists:keyfind(<<"allow">>, 1, Headers),
	ok.

rest_patch(Config) ->
	Tests = [
		{204, [{<<"content-type">>, <<"text/plain">>}], <<"whatever">>},
		{400, [{<<"content-type">>, <<"text/plain">>}], <<"false">>},
		{400, [{<<"content-type">>, <<"text/plain">>}], <<"stop">>},
		{415, [{<<"content-type">>, <<"application/json">>}], <<"bad_content_type">>}
	],
	ConnPid = gun_open(Config),
	_ = [begin
		Ref = gun:patch(ConnPid, "/patch", Headers, Body),
		{response, fin, Status, _} = gun:await(ConnPid, Ref)
	end || {Status, Headers, Body} <- Tests],
	ok.

rest_post_charset(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/post_charset",
		[{<<"content-type">>, <<"text/plain;charset=UTF-8">>}], "12345"),
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	ok.

rest_postonly(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/postonly",
		[{<<"content-type">>, <<"text/plain">>}], "12345"),
	{response, fin, 204, _} = gun:await(ConnPid, Ref),
	ok.

rest_resource_get_etag(Config, Type) ->
	rest_resource_get_etag(Config, Type, []).

rest_resource_get_etag(Config, Type, Headers) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/resetags?type=" ++ Type, Headers),
	{response, _, Status, RespHeaders} = gun:await(ConnPid, Ref),
	case lists:keyfind(<<"etag">>, 1, RespHeaders) of
		false -> {Status, false};
		{<<"etag">>, ETag} -> {Status, ETag}
	end.

rest_resource_etags(Config) ->
	Tests = [
		{200, <<"W/\"etag-header-value\"">>, "tuple-weak"},
		{200, <<"\"etag-header-value\"">>, "tuple-strong"},
		{200, <<"W/\"etag-header-value\"">>, "binary-weak-quoted"},
		{200, <<"\"etag-header-value\"">>, "binary-strong-quoted"},
		{500, false, "binary-strong-unquoted"},
		{500, false, "binary-weak-unquoted"}
	],
	_ = [{Status, ETag, Type} = begin
		{Ret, RespETag} = rest_resource_get_etag(Config, Type),
		{Ret, RespETag, Type}
	end || {Status, ETag, Type} <- Tests].

rest_resource_etags_if_none_match(Config) ->
	Tests = [
		{304, <<"W/\"etag-header-value\"">>, "tuple-weak"},
		{304, <<"\"etag-header-value\"">>, "tuple-strong"},
		{304, <<"W/\"etag-header-value\"">>, "binary-weak-quoted"},
		{304, <<"\"etag-header-value\"">>, "binary-strong-quoted"}
	],
	_ = [{Status, Type} = begin
		{Ret, _} = rest_resource_get_etag(Config, Type,
			[{<<"if-none-match">>, ETag}]),
		{Ret, Type}
	end || {Status, ETag, Type} <- Tests].

set_resp_overwrite(Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/set_resp/overwrite"),
	{response, nofin, 200, Headers} = gun:await(ConnPid, Ref),
	{_, <<"DesireDrive/1.0">>} = lists:keyfind(<<"server">>, 1, Headers),
	ok.

slowloris(Config) ->
	Client = raw_open(Config),
	try
		[begin
			ok = raw_send(Client, [C]),
			receive after 250 -> ok end
		end || C <- "GET / HTTP/1.1\r\nHost: localhost\r\n"
			"User-Agent: Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US)\r\n"
			"Cookie: name=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n\r\n"],
		error(failure)
	catch error:{badmatch, _} ->
		ok
	end.

slowloris2(Config) ->
	Client = raw_open(Config),
	ok = raw_send(Client, "GET / HTTP/1.1\r\n"),
	receive after 300 -> ok end,
	ok = raw_send(Client, "Host: localhost\r\n"),
	receive after 300 -> ok end,
	Data = raw_recv_head(Client),
	{_, 408, _, _} = cow_http:parse_status_line(Data),
	ok.

te_chunked(Config) ->
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body", [{<<"content-type">>, <<"text/plain">>}]),
	gun:data(ConnPid, Ref, fin, Body),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.

do_body_to_chunks(_, <<>>, Acc) ->
	lists:reverse([<<"0\r\n\r\n">>|Acc]);
do_body_to_chunks(ChunkSize, Body, Acc) ->
	BodySize = byte_size(Body),
	ChunkSize2 = case BodySize < ChunkSize of
		true -> BodySize;
		false -> ChunkSize
	end,
	<< Chunk:ChunkSize2/binary, Rest/binary >> = Body,
	ChunkSizeBin = integer_to_binary(ChunkSize2, 16),
	do_body_to_chunks(ChunkSize, Rest,
		[<< ChunkSizeBin/binary, "\r\n", Chunk/binary, "\r\n" >>|Acc]).

te_chunked_chopped(Config) ->
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Body2 = iolist_to_binary(do_body_to_chunks(50, Body, [])),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body",
		[{<<"content-type">>, <<"text/plain">>}]),
	_ = [begin
		ok = gun:dbg_send_raw(ConnPid, << C >>),
		receive after 10 -> ok end
	end || << C >> <= Body2],
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.

te_chunked_delayed(Config) ->
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = do_body_to_chunks(50, Body, []),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body",
		[{<<"content-type">>, <<"text/plain">>}]),
	_ = [begin
		ok = gun:dbg_send_raw(ConnPid, Chunk),
		receive after 10 -> ok end
	end || Chunk <- Chunks],
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.

te_chunked_split_body(Config) ->
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = do_body_to_chunks(50, Body, []),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body",
		[{<<"content-type">>, <<"text/plain">>}]),
	_ = [begin
		case Chunk of
			<<"0\r\n\r\n">> ->
				ok = gun:dbg_send_raw(ConnPid, Chunk);
			_ ->
				[Size, ChunkBody, <<>>] =
					binary:split(Chunk, [<<"\r\n">>], [global]),
				PartASize = random:uniform(byte_size(ChunkBody)),
				<<PartA:PartASize/binary, PartB/binary>> = ChunkBody,
				ok = gun:dbg_send_raw(ConnPid, [Size, <<"\r\n">>, PartA]),
				receive after 10 -> ok end,
				ok = gun:dbg_send_raw(ConnPid, [PartB, <<"\r\n">>])
		end
	end || Chunk <- Chunks],
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.

te_chunked_split_crlf(Config) ->
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = do_body_to_chunks(50, Body, []),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body",
		[{<<"content-type">>, <<"text/plain">>}]),
	_ = [begin
		%% Split in the newline just before the end of the chunk.
		Len = byte_size(Chunk) - (random:uniform(2) - 1),
		<< Chunk2:Len/binary, End/binary >> = Chunk,
		ok = gun:dbg_send_raw(ConnPid, Chunk2),
		receive after 10 -> ok end,
		ok = gun:dbg_send_raw(ConnPid, End)
	end || Chunk <- Chunks],
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.

te_identity(Config) ->
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	ConnPid = gun_open(Config),
	Ref = gun:post(ConnPid, "/echo/body", [], Body),
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	{ok, Body} = gun:await_body(ConnPid, Ref),
	ok.
