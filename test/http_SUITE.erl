%% Copyright (c) 2011-2013, Loïc Hoguin <essen@ninenines.eu>
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

-module(http_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([check_raw_status/1]).
-export([check_status/1]).
-export([chunked_response/1]).
-export([echo_body/1]).
-export([echo_body_max_length/1]).
-export([echo_body_qs/1]).
-export([echo_body_qs_max_length/1]).
-export([error_chain_handle_after_reply/1]).
-export([error_chain_handle_before_reply/1]).
-export([error_handle_after_reply/1]).
-export([error_init_after_reply/1]).
-export([error_init_reply_handle_error/1]).
-export([headers_dupe/1]).
-export([http10_chunkless/1]).
-export([http10_hostless/1]).
-export([keepalive_max/1]).
-export([keepalive_nl/1]).
-export([multipart/1]).
-export([nc_rand/1]).
-export([nc_zero/1]).
-export([onrequest/1]).
-export([onrequest_reply/1]).
-export([onresponse_capitalize/1]).
-export([onresponse_crash/1]).
-export([onresponse_reply/1]).
-export([pipeline/1]).
-export([pipeline_long_polling/1]).
-export([rest_bad_accept/1]).
-export([rest_bad_content_type/1]).
-export([rest_expires/1]).
-export([rest_keepalive/1]).
-export([rest_keepalive_post/1]).
-export([rest_missing_get_callbacks/1]).
-export([rest_missing_put_callbacks/1]).
-export([rest_nodelete/1]).
-export([rest_options_default/1]).
-export([rest_param_all/1]).
-export([rest_patch/1]).
-export([rest_resource_etags/1]).
-export([rest_resource_etags_if_none_match/1]).
-export([set_env_dispatch/1]).
-export([set_resp_body/1]).
-export([set_resp_header/1]).
-export([set_resp_overwrite/1]).
-export([slowloris/1]).
-export([slowloris2/1]).
-export([static_attribute_etag/1]).
-export([static_function_etag/1]).
-export([static_mimetypes_function/1]).
-export([static_specify_file/1]).
-export([static_specify_file_catchall/1]).
-export([static_test_file/1]).
-export([static_test_file_css/1]).
-export([stream_body_set_resp/1]).
-export([stream_body_set_resp_close/1]).
-export([te_chunked/1]).
-export([te_chunked_chopped/1]).
-export([te_chunked_delayed/1]).
-export([te_identity/1]).

%% ct.

all() ->
	[
		{group, http},
		{group, https},
		{group, http_compress},
		{group, https_compress},
		{group, onrequest},
		{group, onresponse},
		{group, onresponse_capitalize},
		{group, set_env}
	].

groups() ->
	Tests = [
		check_raw_status,
		check_status,
		chunked_response,
		echo_body,
		echo_body_max_length,
		echo_body_qs,
		echo_body_qs_max_length,
		error_chain_handle_after_reply,
		error_chain_handle_before_reply,
		error_handle_after_reply,
		error_init_after_reply,
		error_init_reply_handle_error,
		headers_dupe,
		http10_chunkless,
		http10_hostless,
		keepalive_max,
		keepalive_nl,
		multipart,
		nc_rand,
		nc_zero,
		pipeline,
		pipeline_long_polling,
		rest_bad_accept,
		rest_bad_content_type,
		rest_expires,
		rest_keepalive,
		rest_keepalive_post,
		rest_missing_get_callbacks,
		rest_missing_put_callbacks,
		rest_nodelete,
		rest_options_default,
		rest_param_all,
		rest_patch,
		rest_resource_etags,
		rest_resource_etags_if_none_match,
		set_resp_body,
		set_resp_header,
		set_resp_overwrite,
		slowloris,
		slowloris2,
		static_attribute_etag,
		static_function_etag,
		static_mimetypes_function,
		static_specify_file,
		static_specify_file_catchall,
		static_test_file,
		static_test_file_css,
		stream_body_set_resp,
		stream_body_set_resp_close,
		te_chunked,
		te_chunked_chopped,
		te_chunked_delayed,
		te_identity
	],
	[
		{http, [parallel], Tests},
		{https, [parallel], Tests},
		{http_compress, [parallel], Tests},
		{https_compress, [parallel], Tests},
		{onrequest, [parallel], [
			onrequest,
			onrequest_reply
		]},
		{onresponse, [parallel], [
			onresponse_crash,
			onresponse_reply
		]},
		{onresponse_capitalize, [parallel], [
			onresponse_capitalize
		]},
		{set_env, [], [
			set_env_dispatch
		]}
	].

init_per_suite(Config) ->
	application:start(crypto),
	application:start(ranch),
	application:start(cowboy),
	Config.

end_per_suite(_Config) ->
	application:stop(cowboy),
	application:stop(ranch),
	application:stop(crypto),
	ok.

init_per_group(http, Config) ->
	Transport = ranch_tcp,
	Config1 = init_static_dir(Config),
	{ok, _} = cowboy:start_http(http, 100, [{port, 0}], [
		{env, [{dispatch, init_dispatch(Config1)}]},
		{max_keepalive, 50},
		{timeout, 500}
	]),
	Port = ranch:get_port(http),
	{ok, Client} = cowboy_client:init([]),
	[{scheme, <<"http">>}, {port, Port}, {opts, []},
		{transport, Transport}, {client, Client}|Config1];
init_per_group(https, Config) ->
	Transport = ranch_ssl,
	Opts = [
		{certfile, ?config(data_dir, Config) ++ "cert.pem"},
		{keyfile, ?config(data_dir, Config) ++ "key.pem"},
		{password, "cowboy"}
	],
	Config1 = init_static_dir(Config),
	application:start(public_key),
	application:start(ssl),
	{ok, _} = cowboy:start_https(https, 100, Opts ++ [{port, 0}], [
		{env, [{dispatch, init_dispatch(Config1)}]},
		{max_keepalive, 50},
		{timeout, 500}
	]),
	Port = ranch:get_port(https),
	{ok, Client} = cowboy_client:init(Opts),
	[{scheme, <<"https">>}, {port, Port}, {opts, Opts},
		{transport, Transport}, {client, Client}|Config1];
init_per_group(http_compress, Config) ->
	Transport = ranch_tcp,
	Config1 = init_static_dir(Config),
	{ok, _} = cowboy:start_http(http_compress, 100, [{port, 0}], [
		{compress, true},
		{env, [{dispatch, init_dispatch(Config1)}]},
		{max_keepalive, 50},
		{timeout, 500}
	]),
	Port = ranch:get_port(http_compress),
	{ok, Client} = cowboy_client:init([]),
	[{scheme, <<"http">>}, {port, Port}, {opts, []},
		{transport, Transport}, {client, Client}|Config1];
init_per_group(https_compress, Config) ->
	Transport = ranch_ssl,
	Opts = [
		{certfile, ?config(data_dir, Config) ++ "cert.pem"},
		{keyfile, ?config(data_dir, Config) ++ "key.pem"},
		{password, "cowboy"}
	],
	Config1 = init_static_dir(Config),
	application:start(public_key),
	application:start(ssl),
	{ok, _} = cowboy:start_https(https_compress, 100, Opts ++ [{port, 0}], [
		{compress, true},
		{env, [{dispatch, init_dispatch(Config1)}]},
		{max_keepalive, 50},
		{timeout, 500}
	]),
	Port = ranch:get_port(https_compress),
	{ok, Client} = cowboy_client:init(Opts),
	[{scheme, <<"https">>}, {port, Port}, {opts, Opts},
		{transport, Transport}, {client, Client}|Config1];
init_per_group(onrequest, Config) ->
	Transport = ranch_tcp,
	{ok, _} = cowboy:start_http(onrequest, 100, [{port, 0}], [
		{env, [{dispatch, init_dispatch(Config)}]},
		{max_keepalive, 50},
		{onrequest, fun onrequest_hook/1},
		{timeout, 500}
	]),
	Port = ranch:get_port(onrequest),
	{ok, Client} = cowboy_client:init([]),
	[{scheme, <<"http">>}, {port, Port}, {opts, []},
		{transport, Transport}, {client, Client}|Config];
init_per_group(onresponse, Config) ->
	Transport = ranch_tcp,
	{ok, _} = cowboy:start_http(onresponse, 100, [{port, 0}], [
		{env, [{dispatch, init_dispatch(Config)}]},
		{max_keepalive, 50},
		{onresponse, fun onresponse_hook/4},
		{timeout, 500}
	]),
	Port = ranch:get_port(onresponse),
	{ok, Client} = cowboy_client:init([]),
	[{scheme, <<"http">>}, {port, Port}, {opts, []},
		{transport, Transport}, {client, Client}|Config];
init_per_group(onresponse_capitalize, Config) ->
	Transport = ranch_tcp,
	{ok, _} = cowboy:start_http(onresponse_capitalize, 100, [{port, 0}], [
		{env, [{dispatch, init_dispatch(Config)}]},
		{max_keepalive, 50},
		{onresponse, fun onresponse_capitalize_hook/4},
		{timeout, 500}
	]),
	Port = ranch:get_port(onresponse_capitalize),
	{ok, Client} = cowboy_client:init([]),
	[{scheme, <<"http">>}, {port, Port}, {opts, []},
		{transport, Transport}, {client, Client}|Config];
init_per_group(set_env, Config) ->
	Transport = ranch_tcp,
	{ok, _} = cowboy:start_http(set_env, 100, [{port, 0}], [
		{env, [{dispatch, []}]},
		{max_keepalive, 50},
		{timeout, 500}
	]),
	Port = ranch:get_port(set_env),
	{ok, Client} = cowboy_client:init([]),
	[{scheme, <<"http">>}, {port, Port}, {opts, []},
		{transport, Transport}, {client, Client}|Config].

end_per_group(Group, Config) when Group =:= https; Group =:= https_compress ->
	cowboy:stop_listener(https),
	application:stop(ssl),
	application:stop(public_key),
	end_static_dir(Config),
	ok;
end_per_group(Group, Config) when Group =:= http; Group =:= http_compress ->
	cowboy:stop_listener(http),
	end_static_dir(Config);
end_per_group(Name, _) ->
	cowboy:stop_listener(Name),
	ok.

%% Dispatch configuration.

init_dispatch(Config) ->
	cowboy_router:compile([
		{"localhost", [
			{"/chunked_response", chunked_handler, []},
			{"/init_shutdown", http_handler_init_shutdown, []},
			{"/long_polling", http_handler_long_polling, []},
			{"/headers/dupe", http_handler,
				[{headers, [{<<"connection">>, <<"close">>}]}]},
			{"/set_resp/header", http_handler_set_resp,
				[{headers, [{<<"vary">>, <<"Accept">>}]}]},
			{"/set_resp/overwrite", http_handler_set_resp,
				[{headers, [{<<"server">>, <<"DesireDrive/1.0">>}]}]},
			{"/set_resp/body", http_handler_set_resp,
				[{body, <<"A flameless dance does not equal a cycle">>}]},
			{"/stream_body/set_resp", http_handler_stream_body,
				[{reply, set_resp}, {body, <<"stream_body_set_resp">>}]},
			{"/stream_body/set_resp_close",
				http_handler_stream_body, [
					{reply, set_resp_close},
					{body, <<"stream_body_set_resp_close">>}]},
			{"/static/[...]", cowboy_static,
				[{directory, ?config(static_dir, Config)},
				 {mimetypes, [{<<".css">>, [<<"text/css">>]}]}]},
			{"/static_mimetypes_function/[...]", cowboy_static,
				[{directory, ?config(static_dir, Config)},
				 {mimetypes, {fun(Path, data) when is_binary(Path) ->
					[<<"text/html">>] end, data}}]},
			{"/handler_errors", http_handler_errors, []},
			{"/static_attribute_etag/[...]", cowboy_static,
				[{directory, ?config(static_dir, Config)},
				 {etag, {attributes, [filepath, filesize, inode, mtime]}}]},
			{"/static_function_etag/[...]", cowboy_static,
				[{directory, ?config(static_dir, Config)},
				 {etag, {fun static_function_etag/2, etag_data}}]},
			{"/static_specify_file/[...]",  cowboy_static,
				[{directory, ?config(static_dir, Config)},
				 {mimetypes, [{<<".css">>, [<<"text/css">>]}]},
				 {file, <<"test_file.css">>}]},
			{"/multipart", http_handler_multipart, []},
			{"/echo/body", http_handler_echo_body, []},
			{"/echo/body_qs", http_handler_body_qs, []},
			{"/param_all", rest_param_all, []},
			{"/bad_accept", rest_simple_resource, []},
			{"/bad_content_type", rest_patch_resource, []},
			{"/simple", rest_simple_resource, []},
			{"/forbidden_post", rest_forbidden_resource, [true]},
			{"/simple_post", rest_forbidden_resource, [false]},
			{"/missing_get_callbacks", rest_missing_callbacks, []},
			{"/missing_put_callbacks", rest_missing_callbacks, []},
			{"/nodelete", rest_nodelete_resource, []},
			{"/patch", rest_patch_resource, []},
			{"/resetags", rest_resource_etags, []},
			{"/rest_expires", rest_expires, []},
			{"/rest_empty_resource", rest_empty_resource, []},
			{"/loop_recv", http_handler_loop_recv, []},
			{"/loop_timeout", http_handler_loop_timeout, []},
			{"/", http_handler, []}
		]}
	]).

init_static_dir(Config) ->
	Dir = filename:join(?config(priv_dir, Config), "static"),
	Level1 = fun(Name) -> filename:join(Dir, Name) end,
	ok = file:make_dir(Dir),
	ok = file:write_file(Level1("test_file"), "test_file\n"),
	ok = file:write_file(Level1("test_file.css"), "test_file.css\n"),
	ok = file:write_file(Level1("test_noread"), "test_noread\n"),
	ok = file:change_mode(Level1("test_noread"), 8#0333),
	ok = file:write_file(Level1("test.html"), "test.html\n"),
	ok = file:make_dir(Level1("test_dir")),
	[{static_dir, Dir}|Config].

end_static_dir(Config) ->
	Dir = ?config(static_dir, Config),
	Level1 = fun(Name) -> filename:join(Dir, Name) end,
	ok = file:delete(Level1("test_file")),
	ok = file:delete(Level1("test_file.css")),
	ok = file:delete(Level1("test_noread")),
	ok = file:delete(Level1("test.html")),
	ok = file:del_dir(Level1("test_dir")),
	ok = file:del_dir(Dir),
	Config.

%% Convenience functions.

quick_raw(Data, Config) ->
	Client = ?config(client, Config),
	Transport = ?config(transport, Config),
	{ok, Client2} = cowboy_client:connect(
		Transport, "localhost", ?config(port, Config), Client),
	{ok, Client3} = cowboy_client:raw_request(Data, Client2),
	case cowboy_client:response(Client3) of
		{ok, Status, _, _} -> Status;
		{error, _} -> closed
	end.

build_url(Path, Config) ->
	{scheme, Scheme} = lists:keyfind(scheme, 1, Config),
	{port, Port} = lists:keyfind(port, 1, Config),
	PortBin = list_to_binary(integer_to_list(Port)),
	PathBin = list_to_binary(Path),
	<< Scheme/binary, "://localhost:", PortBin/binary, PathBin/binary >>.

quick_get(URL, Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url(URL, Config), Client),
	{ok, Status, _, _} = cowboy_client:response(Client2),
	Status.

body_to_chunks(_, <<>>, Acc) ->
	lists:reverse([<<"0\r\n\r\n">>|Acc]);
body_to_chunks(ChunkSize, Body, Acc) ->
	BodySize = byte_size(Body),
	ChunkSize2 = case BodySize < ChunkSize of
		true -> BodySize;
		false -> ChunkSize
	end,
	<< Chunk:ChunkSize2/binary, Rest/binary >> = Body,
	ChunkSizeBin = list_to_binary(integer_to_list(ChunkSize2, 16)),
	body_to_chunks(ChunkSize, Rest,
		[<< ChunkSizeBin/binary, "\r\n", Chunk/binary, "\r\n" >>|Acc]).

%% Tests.

check_raw_status(Config) ->
	Huge = [$0 || _ <- lists:seq(1, 5000)],
	HugeCookie = lists:flatten(["whatever_man_biiiiiiiiiiiig_cookie_me_want_77="
		"Wed Apr 06 2011 10:38:52 GMT-0500 (CDT)" || _ <- lists:seq(1, 40)]),
	ResponsePacket =
"HTTP/1.0 302 Found\r
Location: http://www.google.co.il/\r
Cache-Control: private\r
Content-Type: text/html; charset=UTF-8\r
Set-Cookie: PREF=ID=568f67013d4a7afa:FF=0:TM=1323014101:LM=1323014101:S=XqctDWC65MzKT0zC; expires=Tue, 03-Dec-2013 15:55:01 GMT; path=/; domain=.google.com\r
Date: Sun, 04 Dec 2011 15:55:01 GMT\r
Server: gws\r
Content-Length: 221\r
X-XSS-Protection: 1; mode=block\r
X-Frame-Options: SAMEORIGIN\r
\r
<HTML><HEAD><meta http-equiv=\"content-type\" content=\"text/html;charset=utf-8\">
<TITLE>302 Moved</TITLE></HEAD><BODY>
<H1>302 Moved</H1>
The document has moved
<A HREF=\"http://www.google.co.il/\">here</A>.
</BODY></HTML>",
	Tests = [
		{102, <<"GET /long_polling HTTP/1.1\r\nHost: localhost\r\n"
			"Content-Length: 5000\r\n\r\n", 0:5000/unit:8 >>},
		{200, ["GET / HTTP/1.0\r\nHost: localhost\r\n"
			"Set-Cookie: ", HugeCookie, "\r\n\r\n"]},
		{200, "\r\n\r\n\r\n\r\n\r\nGET / HTTP/1.1\r\nHost: localhost\r\n\r\n"},
		{200, "GET http://proxy/ HTTP/1.1\r\nHost: localhost\r\n\r\n"},
		{200, <<"POST /loop_recv HTTP/1.1\r\nHost: localhost\r\n"
			"Content-Length: 100000\r\n\r\n", 0:100000/unit:8 >>},
		{400, "\n"},
		{400, "Garbage\r\n\r\n"},
		{400, "\r\n\r\n\r\n\r\n\r\n\r\n"},
		{400, "GET / HTTP/1.1\r\nHost: ninenines.eu\r\n\r\n"},
		{400, "GET http://proxy/ HTTP/1.1\r\n\r\n"},
		{400, "GET / HTTP/1.1\r\nHost: localhost:bad_port\r\n\r\n"},
		{505, ResponsePacket},
		{408, "GET / HTTP/1.1\r\n"},
		{408, "GET / HTTP/1.1\r\nHost: localhost"},
		{408, "GET / HTTP/1.1\r\nHost: localhost\r\n"},
		{408, "GET / HTTP/1.1\r\nHost: localhost\r\n\r"},
		{414, Huge},
		{400, "GET / HTTP/1.1\r\n" ++ Huge},
		{500, <<"GET /long_polling HTTP/1.1\r\nHost: localhost\r\n"
			"Content-Length: 100000\r\n\r\n", 0:100000/unit:8 >>},
		{505, "GET / HTTP/1.2\r\nHost: localhost\r\n\r\n"},
		{closed, ""},
		{closed, "\r\n"},
		{closed, "\r\n\r\n"},
		{closed, "GET / HTTP/1.1"}
	],
	_ = [{Status, Packet} = begin
		Ret = quick_raw(Packet, Config),
		{Ret, Packet}
	end || {Status, Packet} <- Tests].

check_status(Config) ->
	Tests = [
		{102, "/long_polling"},
		{200, "/"},
		{200, "/simple"},
		{204, "/loop_timeout"},
		{400, "/static/%2f"},
		{400, "/static/%2e"},
		{400, "/static/%2e%2e"},
		{403, "/static/test_dir"},
		{403, "/static/test_dir/"},
		{403, "/static/test_noread"},
		{404, "/not/found"},
		{404, "/static/not_found"},
		{500, "/handler_errors?case=handler_before_reply"},
		{500, "/handler_errors?case=init_before_reply"},
		{666, "/init_shutdown"}
	],
	_ = [{Status, URL} = begin
		Ret = quick_get(URL, Config),
		{Ret, URL}
	end || {Status, URL} <- Tests].

chunked_response(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/chunked_response", Config), Client),
	{ok, 200, Headers, Client3} = cowboy_client:response(Client2),
	true = lists:keymember(<<"transfer-encoding">>, 1, Headers),
	{ok, Transport, Socket} = cowboy_client:transport(Client3),
	{ok, <<"11\r\nchunked_handler\r\n\r\nB\r\nworks fine!\r\n0\r\n\r\n">>}
		= Transport:recv(Socket, 44, 1000),
	{error, closed} = cowboy_client:response(Client3).

%% Check if sending requests whose size is around the MTU breaks something.
echo_body(Config) ->
	Client = ?config(client, Config),
	{ok, [{mtu, MTU}]} = inet:ifget("lo", [mtu]),
	[begin
		Body = list_to_binary(lists:duplicate(Size, $a)),
		{ok, Client2} = cowboy_client:request(<<"POST">>,
			build_url("/echo/body", Config),
			[{<<"connection">>, <<"close">>}],
			Body, Client),
		{ok, 200, _, Client3} = cowboy_client:response(Client2),
		{ok, Body, _} = cowboy_client:response_body(Client3)
	end || Size <- lists:seq(MTU - 500, MTU)].

%% Check if sending request whose size is bigger than 1000000 bytes causes 413
echo_body_max_length(Config) ->
	Client = ?config(client, Config),
	Body = <<$a:8000008>>,
	{ok, Client2} = cowboy_client:request(<<"POST">>,
		build_url("/echo/body", Config),
		[{<<"connection">>, <<"close">>}],
		Body, Client),
	{ok, 413, _, _} = cowboy_client:response(Client2).

% check if body_qs echo's back results
echo_body_qs(Config) ->
	Client = ?config(client, Config),
	Body = <<"echo=67890">>,
	{ok, Client2} = cowboy_client:request(<<"POST">>,
		build_url("/echo/body_qs", Config),
		[{<<"connection">>, <<"close">>}],
		Body, Client),
	{ok, 200, _, Client3} = cowboy_client:response(Client2),
	{ok, <<"67890">>, _} = cowboy_client:response_body(Client3).

%% Check if sending request whose size is bigger 16000 bytes causes 413
echo_body_qs_max_length(Config) ->
	Client = ?config(client, Config),
	DefaultMaxBodyQsLength = 16000,
	% subtract "echo=" minus 1 byte from max to hit the limit
	Bits = (DefaultMaxBodyQsLength - 4) * 8,
	AppendedBody = <<$a:Bits>>,
	Body = <<"echo=", AppendedBody/binary>>,
	{ok, Client2} = cowboy_client:request(<<"POST">>,
		build_url("/echo/body_qs", Config),
		[{<<"connection">>, <<"close">>}],
		Body, Client),
	{ok, 413, _, _} = cowboy_client:response(Client2).

error_chain_handle_after_reply(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/", Config), Client),
	{ok, Client3} = cowboy_client:request(<<"GET">>,
		build_url("/handler_errors?case=handle_after_reply", Config), Client2),
	{ok, 200, _, Client4} = cowboy_client:response(Client3),
	{ok, 200, _, Client5} = cowboy_client:response(Client4),
	{error, closed} = cowboy_client:response(Client5).

error_chain_handle_before_reply(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/", Config), Client),
	{ok, Client3} = cowboy_client:request(<<"GET">>,
		build_url("/handler_errors?case=handle_before_reply", Config), Client2),
	{ok, 200, _, Client4} = cowboy_client:response(Client3),
	{ok, 500, _, Client5} = cowboy_client:response(Client4),
	{error, closed} = cowboy_client:response(Client5).

error_handle_after_reply(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/handler_errors?case=handle_after_reply", Config), Client),
	{ok, 200, _, Client3} = cowboy_client:response(Client2),
	{error, closed} = cowboy_client:response(Client3).

error_init_after_reply(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/handler_errors?case=init_after_reply", Config), Client),
	{ok, 200, _, Client3} = cowboy_client:response(Client2),
	{error, closed} = cowboy_client:response(Client3).

error_init_reply_handle_error(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/handler_errors?case=init_reply_handle_error", Config), Client),
	{ok, 200, _, Client3} = cowboy_client:response(Client2),
	{error, closed} = cowboy_client:response(Client3).

headers_dupe(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/headers/dupe", Config), Client),
	{ok, 200, Headers, Client3} = cowboy_client:response(Client2),
	{<<"connection">>, <<"close">>}
		= lists:keyfind(<<"connection">>, 1, Headers),
	Connections = [H || H = {Name, _} <- Headers, Name =:= <<"connection">>],
	1 = length(Connections),
	{error, closed} = cowboy_client:response(Client3).

http10_chunkless(Config) ->
	Client = ?config(client, Config),
	Transport = ?config(transport, Config),
	{ok, Client2} = cowboy_client:connect(
		Transport, "localhost", ?config(port, Config), Client),
	Data = "GET /chunked_response HTTP/1.0\r\nHost: localhost\r\n\r\n",
	{ok, Client3} = cowboy_client:raw_request(Data, Client2),
	{ok, 200, Headers, Client4} = cowboy_client:response(Client3),
	false = lists:keyfind(<<"transfer-encoding">>, 1, Headers),
	%% Hack: we just try to get 28 bytes and compare.
	{ok, Transport, Socket} = cowboy_client:transport(Client4),
	Buffer = element(7, Client4),
	Buffer2 = case Transport:recv(Socket, 28 - byte_size(Buffer), 1000) of
		{ok, Recv} -> << Buffer/binary, Recv/binary >>;
		_ -> Buffer
	end,
	<<"chunked_handler\r\nworks fine!">> = Buffer2.

http10_hostless(Config) ->
	Port10 = ?config(port, Config) + 10,
	Name = list_to_atom("http10_hostless_" ++ integer_to_list(Port10)),
	ranch:start_listener(Name, 5,
		?config(transport, Config), ?config(opts, Config) ++ [{port, Port10}],
		cowboy_protocol, [
			{env, [{dispatch, cowboy_router:compile([
				{'_', [{"/http1.0/hostless", http_handler, []}]}])}]},
			{max_keepalive, 50},
			{timeout, 500}]
	),
	200 = quick_raw("GET /http1.0/hostless HTTP/1.0\r\n\r\n",
		[{port, Port10}|Config]),
	cowboy:stop_listener(http10).

keepalive_max(Config) ->
	Client = ?config(client, Config),
	URL = build_url("/", Config),
	ok = keepalive_max_loop(Client, URL, 50).

keepalive_max_loop(Client, _, 0) ->
	{error, closed} = cowboy_client:response(Client),
	ok;
keepalive_max_loop(Client, URL, N) ->
	Headers = [{<<"connection">>, <<"keep-alive">>}],
	{ok, Client2} = cowboy_client:request(<<"GET">>, URL, Headers, Client),
	{ok, 200, RespHeaders, Client3} = cowboy_client:response(Client2),
	Expected = case N of
		1 -> <<"close">>;
		N -> <<"keep-alive">>
	end,
	{<<"connection">>, Expected}
		= lists:keyfind(<<"connection">>, 1, RespHeaders),
	keepalive_max_loop(Client3, URL, N - 1).

keepalive_nl(Config) ->
	Client = ?config(client, Config),
	URL = build_url("/", Config),
	ok = keepalive_nl_loop(Client, URL, 10).

keepalive_nl_loop(Client, _, 0) ->
	{error, closed} = cowboy_client:response(Client),
	ok;
keepalive_nl_loop(Client, URL, N) ->
	Headers = [{<<"connection">>, <<"keep-alive">>}],
	{ok, Client2} = cowboy_client:request(<<"GET">>, URL, Headers, Client),
	{ok, 200, RespHeaders, Client3} = cowboy_client:response(Client2),
	{<<"connection">>, <<"keep-alive">>}
		= lists:keyfind(<<"connection">>, 1, RespHeaders),
	{ok, Transport, Socket} = cowboy_client:transport(Client2),
	ok = Transport:send(Socket, <<"\r\n">>), %% empty line
	keepalive_nl_loop(Client3, URL, N - 1).

multipart(Config) ->
	Client = ?config(client, Config),
	Body = <<
		"This is a preamble."
		"\r\n--OHai\r\nX-Name:answer\r\n\r\n42"
		"\r\n--OHai\r\nServer:Cowboy\r\n\r\nIt rocks!\r\n"
		"\r\n--OHai--"
		"This is an epiloque."
	>>,
	{ok, Client2} = cowboy_client:request(<<"POST">>,
		build_url("/multipart", Config),
		[{<<"content-type">>, <<"multipart/x-makes-no-sense; boundary=OHai">>}],
		Body, Client),
	{ok, 200, _, Client3} = cowboy_client:response(Client2),
	{ok, RespBody, _} = cowboy_client:response_body(Client3),
	Parts = binary_to_term(RespBody),
	Parts = [
		{[{<<"x-name">>, <<"answer">>}], <<"42">>},
		{[{<<"server">>, <<"Cowboy">>}], <<"It rocks!\r\n">>}
	].

nc_reqs(Config, Input) ->
	Cat = os:find_executable("cat"),
	Nc = os:find_executable("nc"),
	case {Cat, Nc} of
		{false, _} ->
			{skip, {notfound, cat}};
		{_, false} ->
			{skip, {notfound, nc}};
		_Good ->
			%% Throw garbage at the server then check if it's still up.
			{port, Port} = lists:keyfind(port, 1, Config),
			StrPort = integer_to_list(Port),
			[os:cmd("cat " ++ Input ++ " | nc localhost " ++ StrPort)
				|| _ <- lists:seq(1, 100)],
			200 = quick_get("/", Config)
	end.

nc_rand(Config) ->
	nc_reqs(Config, "/dev/urandom").

nc_zero(Config) ->
	nc_reqs(Config, "/dev/zero").

onrequest(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/", Config), Client),
	{ok, 200, Headers, Client3} = cowboy_client:response(Client2),
	{<<"server">>, <<"Serenity">>} = lists:keyfind(<<"server">>, 1, Headers),
	{ok, <<"http_handler">>, _} = cowboy_client:response_body(Client3).

onrequest_reply(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/?reply=1", Config), Client),
	{ok, 200, Headers, Client3} = cowboy_client:response(Client2),
	{<<"server">>, <<"Cowboy">>} = lists:keyfind(<<"server">>, 1, Headers),
	{ok, <<"replied!">>, _} = cowboy_client:response_body(Client3).

%% Hook for the above onrequest tests.
onrequest_hook(Req) ->
	case cowboy_req:qs_val(<<"reply">>, Req) of
		{undefined, Req2} ->
			cowboy_req:set_resp_header(<<"server">>, <<"Serenity">>, Req2);
		{_, Req2} ->
			{ok, Req3} = cowboy_req:reply(
				200, [], <<"replied!">>, Req2),
			Req3
	end.

onresponse_capitalize(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/", Config), Client),
	{ok, Transport, Socket} = cowboy_client:transport(Client2),
	{ok, Data} = Transport:recv(Socket, 0, 1000),
	false = nomatch =:= binary:match(Data, <<"Content-Length">>).

%% Hook for the above onresponse_capitalize test.
onresponse_capitalize_hook(Status, Headers, Body, Req) ->
	Headers2 = [{cowboy_bstr:capitalize_token(N), V}
		|| {N, V} <- Headers],
	{ok, Req2} = cowboy_req:reply(Status, Headers2, Body, Req),
	Req2.

onresponse_crash(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/handler_errors?case=init_before_reply", Config), Client),
	{ok, 777, Headers, _} = cowboy_client:response(Client2),
	{<<"x-hook">>, <<"onresponse">>} = lists:keyfind(<<"x-hook">>, 1, Headers).

onresponse_reply(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/", Config), Client),
	{ok, 777, Headers, Client3} = cowboy_client:response(Client2),
	{<<"x-hook">>, <<"onresponse">>} = lists:keyfind(<<"x-hook">>, 1, Headers),
	%% Make sure we don't get the body initially sent.
	{error, closed} = cowboy_client:response_body(Client3).

%% Hook for the above onresponse tests.
onresponse_hook(_, Headers, _, Req) ->
	{ok, Req2} = cowboy_req:reply(
		<<"777 Lucky">>, [{<<"x-hook">>, <<"onresponse">>}|Headers], Req),
	Req2.

pipeline(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/", Config), Client),
	{ok, Client3} = cowboy_client:request(<<"GET">>,
		build_url("/", Config), Client2),
	{ok, Client4} = cowboy_client:request(<<"GET">>,
		build_url("/", Config), Client3),
	{ok, Client5} = cowboy_client:request(<<"GET">>,
		build_url("/", Config), Client4),
	{ok, Client6} = cowboy_client:request(<<"GET">>,
		build_url("/", Config), [{<<"connection">>, <<"close">>}], Client5),
	{ok, 200, _, Client7} = cowboy_client:response(Client6),
	{ok, 200, _, Client8} = cowboy_client:response(Client7),
	{ok, 200, _, Client9} = cowboy_client:response(Client8),
	{ok, 200, _, Client10} = cowboy_client:response(Client9),
	{ok, 200, _, Client11} = cowboy_client:response(Client10),
	{error, closed} = cowboy_client:response(Client11).

pipeline_long_polling(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/long_polling", Config), Client),
	{ok, Client3} = cowboy_client:request(<<"GET">>,
		build_url("/long_polling", Config), Client2),
	{ok, 102, _, Client4} = cowboy_client:response(Client3),
	{ok, 102, _, Client5} = cowboy_client:response(Client4),
	{error, closed} = cowboy_client:response(Client5).

rest_param_all(Config) ->
	Client = ?config(client, Config),
	URL = build_url("/param_all", Config),
	% Accept without param
	{ok, Client2} = cowboy_client:request(<<"GET">>, URL,
		[{<<"accept">>, <<"text/plain">>}], Client),
	Client3 = check_response(Client2, <<"[]">>),
	% Accept with param
	{ok, Client4} = cowboy_client:request(<<"GET">>, URL,
		[{<<"accept">>, <<"text/plain;level=1">>}], Client3),
	Client5 = check_response(Client4, <<"level=1">>),
	% Accept with param and quality
	{ok, Client6} = cowboy_client:request(<<"GET">>, URL,
		[{<<"accept">>,
			<<"text/plain;level=1;q=0.8, text/plain;level=2;q=0.5">>}],
		Client5),
	Client7 = check_response(Client6, <<"level=1">>),
	{ok, Client8} = cowboy_client:request(<<"GET">>, URL,
		[{<<"accept">>,
			<<"text/plain;level=1;q=0.5, text/plain;level=2;q=0.8">>}],
		Client7),
	Client9 = check_response(Client8, <<"level=2">>),
	% Without Accept
	{ok, Client10} = cowboy_client:request(<<"GET">>, URL, [], Client9),
	Client11 = check_response(Client10, <<"'*'">>),
	% Content-Type without param
	{ok, Client12} = cowboy_client:request(<<"PUT">>, URL,
		[{<<"content-type">>, <<"text/plain">>}], Client11),
	{ok, 204, _, Client13} = cowboy_client:response(Client12),
	% Content-Type with param
	{ok, Client14} = cowboy_client:request(<<"PUT">>, URL,
		[{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Client13),
	{ok, 204, _, _} = cowboy_client:response(Client14).

check_response(Client, Body) ->
	{ok, 200, _, Client2} = cowboy_client:response(Client),
	{ok, Body, Client3} = cowboy_client:response_body(Client2),
	Client3.

rest_bad_accept(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/bad_accept", Config),
		[{<<"accept">>, <<"1">>}],
		Client),
	{ok, 400, _, _} = cowboy_client:response(Client2).

rest_bad_content_type(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"PATCH">>,
		build_url("/bad_content_type", Config),
		[{<<"content-type">>, <<"text/plain, text/html">>}],
		<<"Whatever">>, Client),
	{ok, 415, _, _} = cowboy_client:response(Client2).

rest_expires(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/rest_expires", Config), Client),
	{ok, 200, RespHeaders, _} = cowboy_client:response(Client2),
	{_, Expires} = lists:keyfind(<<"expires">>, 1, RespHeaders),
	{_, LastModified} = lists:keyfind(<<"last-modified">>, 1, RespHeaders),
	Expires = LastModified = <<"Fri, 21 Sep 2012 22:36:14 GMT">>,
	ok.

rest_keepalive(Config) ->
	Client = ?config(client, Config),
	URL = build_url("/simple", Config),
	ok = rest_keepalive_loop(Client, URL, 10).

rest_keepalive_loop(_, _, 0) ->
	ok;
rest_keepalive_loop(Client, URL, N) ->
	Headers = [{<<"connection">>, <<"keep-alive">>}],
	{ok, Client2} = cowboy_client:request(<<"GET">>, URL, Headers, Client),
	{ok, 200, RespHeaders, Client3} = cowboy_client:response(Client2),
	{<<"connection">>, <<"keep-alive">>}
		= lists:keyfind(<<"connection">>, 1, RespHeaders),
	rest_keepalive_loop(Client3, URL, N - 1).

rest_keepalive_post(Config) ->
	Client = ?config(client, Config),
	ok = rest_keepalive_post_loop(Config, Client, forbidden_post, 10).

rest_keepalive_post_loop(_, _, _, 0) ->
	ok;
rest_keepalive_post_loop(Config, Client, simple_post, N) ->
	Headers = [
		{<<"connection">>, <<"keep-alive">>},
		{<<"content-type">>, <<"text/plain">>}
	],
	{ok, Client2} = cowboy_client:request(<<"POST">>,
		build_url("/simple_post", Config), Headers, "12345", Client),
	{ok, 303, RespHeaders, Client3} = cowboy_client:response(Client2),
	{<<"connection">>, <<"keep-alive">>}
		= lists:keyfind(<<"connection">>, 1, RespHeaders),
	rest_keepalive_post_loop(Config, Client3, forbidden_post, N - 1);
rest_keepalive_post_loop(Config, Client, forbidden_post, N) ->
	Headers = [
		{<<"connection">>, <<"keep-alive">>},
		{<<"content-type">>, <<"text/plain">>}
	],
	{ok, Client2} = cowboy_client:request(<<"POST">>,
		build_url("/forbidden_post", Config), Headers, "12345", Client),
	{ok, 403, RespHeaders, Client3} = cowboy_client:response(Client2),
	{<<"connection">>, <<"keep-alive">>}
		= lists:keyfind(<<"connection">>, 1, RespHeaders),
	rest_keepalive_post_loop(Config, Client3, simple_post, N - 1).

rest_missing_get_callbacks(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/missing_get_callbacks", Config), Client),
	{ok, 500, _, _} = cowboy_client:response(Client2).

rest_missing_put_callbacks(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"PUT">>,
		build_url("/missing_put_callbacks", Config),
		[{<<"content-type">>, <<"application/json">>}],
		<<"{}">>, Client),
	{ok, 500, _, _} = cowboy_client:response(Client2).

rest_nodelete(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"DELETE">>,
		build_url("/nodelete", Config), Client),
	{ok, 500, _, _} = cowboy_client:response(Client2).

rest_options_default(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"OPTIONS">>,
		build_url("/rest_empty_resource", Config), Client),
	{ok, 200, Headers, _} = cowboy_client:response(Client2),
	{_, <<"HEAD, GET, OPTIONS">>} = lists:keyfind(<<"allow">>, 1, Headers).

rest_patch(Config) ->
	Tests = [
		{204, [{<<"content-type">>, <<"text/plain">>}], <<"whatever">>},
		{422, [{<<"content-type">>, <<"text/plain">>}], <<"false">>},
		{400, [{<<"content-type">>, <<"text/plain">>}], <<"halt">>},
		{415, [{<<"content-type">>, <<"application/json">>}], <<"bad_content_type">>}
	],
	Client = ?config(client, Config),
	_ = [begin
		{ok, Client2} = cowboy_client:request(<<"PATCH">>,
			build_url("/patch", Config), Headers, Body, Client),
		{ok, Status, _, _} = cowboy_client:response(Client2),
		ok
	end || {Status, Headers, Body} <- Tests].

rest_resource_get_etag(Config, Type) ->
	rest_resource_get_etag(Config, Type, []).

rest_resource_get_etag(Config, Type, Headers) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/resetags?type=" ++ Type, Config), Headers, Client),
	{ok, Status, RespHeaders, _} = cowboy_client:response(Client2),
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

set_env_dispatch(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/", Config), Client),
	{ok, 400, _, _} = cowboy_client:response(Client2),
	ok = cowboy:set_env(set_env, dispatch,
		cowboy_router:compile([{'_', [{"/", http_handler, []}]}])),
	{ok, Client3} = cowboy_client:request(<<"GET">>,
		build_url("/", Config), Client),
	{ok, 200, _, _} = cowboy_client:response(Client3).

set_resp_body(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/set_resp/body", Config), Client),
	{ok, 200, _, Client3} = cowboy_client:response(Client2),
	{ok, <<"A flameless dance does not equal a cycle">>, _}
		= cowboy_client:response_body(Client3).

set_resp_header(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/set_resp/header", Config), Client),
	{ok, 200, Headers, _} = cowboy_client:response(Client2),
	{<<"vary">>, <<"Accept">>} = lists:keyfind(<<"vary">>, 1, Headers),
	{<<"set-cookie">>, _} = lists:keyfind(<<"set-cookie">>, 1, Headers).

set_resp_overwrite(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/set_resp/overwrite", Config), Client),
	{ok, 200, Headers, _} = cowboy_client:response(Client2),
	{<<"server">>, <<"DesireDrive/1.0">>}
		= lists:keyfind(<<"server">>, 1, Headers).

slowloris(Config) ->
	Client = ?config(client, Config),
	Transport = ?config(transport, Config),
	{ok, Client2} = cowboy_client:connect(
		Transport, "localhost", ?config(port, Config), Client),
	try
		[begin
			{ok, _} = cowboy_client:raw_request([C], Client2),
			receive after 25 -> ok end
		end || C <- "GET / HTTP/1.1\r\nHost: localhost\r\n"
			"User-Agent: Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US)\r\n"
			"Cookie: name=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n\r\n"],
		error(failure)
	catch error:{badmatch, _} ->
		ok
	end.

slowloris2(Config) ->
	Client = ?config(client, Config),
	Transport = ?config(transport, Config),
	{ok, Client2} = cowboy_client:connect(
		Transport, "localhost", ?config(port, Config), Client),
	{ok, _} = cowboy_client:raw_request("GET / HTTP/1.1\r\n", Client2),
	receive after 300 -> ok end,
	{ok, _} = cowboy_client:raw_request("Host: localhost\r\n", Client2),
	receive after 300 -> ok end,
	{ok, 408, _, _} = cowboy_client:response(Client2).

static_attribute_etag(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/static_attribute_etag/test.html", Config), Client),
	{ok, Client3} = cowboy_client:request(<<"GET">>,
		build_url("/static_attribute_etag/test.html", Config), Client2),
	{ok, 200, Headers1, Client4} = cowboy_client:response(Client3),
	{ok, 200, Headers2, _} = cowboy_client:response(Client4),
	{<<"etag">>, ETag1} = lists:keyfind(<<"etag">>, 1, Headers1),
	{<<"etag">>, ETag2} = lists:keyfind(<<"etag">>, 1, Headers2),
	false = ETag1 =:= undefined,
	ETag1 = ETag2.

static_function_etag(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/static_function_etag/test.html", Config), Client),
	{ok, Client3} = cowboy_client:request(<<"GET">>,
		build_url("/static_function_etag/test.html", Config), Client2),
	{ok, 200, Headers1, Client4} = cowboy_client:response(Client3),
	{ok, 200, Headers2, _} = cowboy_client:response(Client4),
	{<<"etag">>, ETag1} = lists:keyfind(<<"etag">>, 1, Headers1),
	{<<"etag">>, ETag2} = lists:keyfind(<<"etag">>, 1, Headers2),
	false = ETag1 =:= undefined,
	ETag1 = ETag2.

%% Callback function for generating the ETag for the above test.
static_function_etag(Arguments, etag_data) ->
	{_, Filepath} = lists:keyfind(filepath, 1, Arguments),
	{_, _Filesize} = lists:keyfind(filesize, 1, Arguments),
	{_, _INode} = lists:keyfind(inode, 1, Arguments),
	{_, _Modified} = lists:keyfind(mtime, 1, Arguments),
	ChecksumCommand = lists:flatten(io_lib:format("sha1sum ~s", [Filepath])),
	[Checksum|_] = string:tokens(os:cmd(ChecksumCommand), " "),
	{strong, iolist_to_binary(Checksum)}.

static_mimetypes_function(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/static_mimetypes_function/test.html", Config), Client),
	{ok, 200, Headers, _} = cowboy_client:response(Client2),
	{<<"content-type">>, <<"text/html">>}
		= lists:keyfind(<<"content-type">>, 1, Headers).

static_specify_file(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/static_specify_file", Config), Client),
	{ok, 200, Headers, Client3} = cowboy_client:response(Client2),
	{<<"content-type">>, <<"text/css">>}
		= lists:keyfind(<<"content-type">>, 1, Headers),
	{ok, <<"test_file.css\n">>, _} = cowboy_client:response_body(Client3).

static_specify_file_catchall(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/static_specify_file/none", Config), Client),
	{ok, 200, Headers, Client3} = cowboy_client:response(Client2),
	{<<"content-type">>, <<"text/css">>}
		= lists:keyfind(<<"content-type">>, 1, Headers),
	{ok, <<"test_file.css\n">>, _} = cowboy_client:response_body(Client3).

static_test_file(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/static/test_file", Config), Client),
	{ok, 200, Headers, _} = cowboy_client:response(Client2),
	{<<"content-type">>, <<"application/octet-stream">>}
		= lists:keyfind(<<"content-type">>, 1, Headers).

static_test_file_css(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/static/test_file.css", Config), Client),
	{ok, 200, Headers, _} = cowboy_client:response(Client2),
	{<<"content-type">>, <<"text/css">>}
		= lists:keyfind(<<"content-type">>, 1, Headers).

stream_body_set_resp(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/stream_body/set_resp", Config), Client),
	{ok, 200, _, Client3} = cowboy_client:response(Client2),
	{ok, <<"stream_body_set_resp">>, _}
		= cowboy_client:response_body(Client3).

stream_body_set_resp_close(Config) ->
	Client = ?config(client, Config),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/stream_body/set_resp_close", Config), Client),
	{ok, 200, _, Client3} = cowboy_client:response(Client2),
	{ok, Transport, Socket} = cowboy_client:transport(Client3),
	case element(7, Client3) of
		<<"stream_body_set_resp_close">> ->
			ok;
		Buffer ->
			{ok, Rest} = Transport:recv(Socket, 26 - byte_size(Buffer), 1000),
			<<"stream_body_set_resp_close">> = << Buffer/binary, Rest/binary >>,
			ok
	end,
	{error, closed} = Transport:recv(Socket, 0, 1000).

te_chunked(Config) ->
	Client = ?config(client, Config),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = body_to_chunks(50, Body, []),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/echo/body", Config),
		[{<<"transfer-encoding">>, <<"chunked">>}],
		Chunks, Client),
	{ok, 200, _, Client3} = cowboy_client:response(Client2),
	{ok, Body, _} = cowboy_client:response_body(Client3).

te_chunked_chopped(Config) ->
	Client = ?config(client, Config),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Body2 = iolist_to_binary(body_to_chunks(50, Body, [])),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/echo/body", Config),
		[{<<"transfer-encoding">>, <<"chunked">>}], Client),
	{ok, Transport, Socket} = cowboy_client:transport(Client2),
	_ = [begin
		ok = Transport:send(Socket, << C >>),
		ok = timer:sleep(10)
	end || << C >> <= Body2],
	{ok, 200, _, Client3} = cowboy_client:response(Client2),
	{ok, Body, _} = cowboy_client:response_body(Client3).

te_chunked_delayed(Config) ->
	Client = ?config(client, Config),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	Chunks = body_to_chunks(50, Body, []),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/echo/body", Config),
		[{<<"transfer-encoding">>, <<"chunked">>}], Client),
	{ok, Transport, Socket} = cowboy_client:transport(Client2),
	_ = [begin
		ok = Transport:send(Socket, Chunk),
		ok = timer:sleep(10)
	end || Chunk <- Chunks],
	{ok, 200, _, Client3} = cowboy_client:response(Client2),
	{ok, Body, _} = cowboy_client:response_body(Client3).

te_identity(Config) ->
	Client = ?config(client, Config),
	Body = list_to_binary(io_lib:format("~p", [lists:seq(1, 100)])),
	{ok, Client2} = cowboy_client:request(<<"GET">>,
		build_url("/echo/body", Config), [], Body, Client),
	{ok, 200, _, Client3} = cowboy_client:response(Client2),
	{ok, Body, _} = cowboy_client:response_body(Client3).
