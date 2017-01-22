%% Copyright (c) 2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(compress_SUITE).
-compile(export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

%% ct.

all() ->
	[
		{group, http_compress},
		{group, https_compress},
		{group, h2_compress},
		{group, h2c_compress}
	].

groups() ->
	cowboy_test:common_groups(ct_helper:all(?MODULE)).

init_per_group(Name, Config) ->
	cowboy_test:init_common_groups(Name, Config, ?MODULE).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

%% Routes.

init_dispatch(Config) ->
	cowboy_router:compile([{"[...]", [
		{"/reply/:what", compress_h, reply},
		{"/stream_reply/:what", compress_h, stream_reply}
	]}]).

%% Internal.

do_get(Path, ReqHeaders, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, ReqHeaders),
	{response, IsFin, Status, RespHeaders} = gun:await(ConnPid, Ref),
	{ok, Body} = case IsFin of
		nofin -> gun:await_body(ConnPid, Ref);
		fin -> {ok, <<>>}
	end,
	gun:close(ConnPid),
	{Status, RespHeaders, Body}.

%% Tests.

gzip_accept_encoding_missing(Config) ->
	doc("Don't send accept-encoding; get an uncompressed response."),
	{200, Headers, _} = do_get("/reply/large",
		[], Config),
	false = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"100000">>} = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

gzip_accept_encoding_no_gzip(Config) ->
	doc("Send accept-encoding: compress (unsupported by Cowboy); get an uncompressed response."),
	{200, Headers, _} = do_get("/reply/large",
		[{<<"accept-encoding">>, <<"compress">>}], Config),
	false = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"100000">>} = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

gzip_reply_content_encoding(Config) ->
	doc("Reply with content-encoding header; get an uncompressed response."),
	{200, Headers, _} = do_get("/reply/content-encoding",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	%% We set the content-encoding to compress; without actually compressing.
	{_, <<"compress">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"100000">>} = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

gzip_reply_large_body(Config) ->
	doc("Reply a large body; get a gzipped response."),
	{200, Headers, GzBody} = do_get("/reply/large",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, Length} = lists:keyfind(<<"content-length">>, 1, Headers),
	ct:log("Original length: 100000; compressed: ~s.", [Length]),
	_ = zlib:gunzip(GzBody),
	ok.

gzip_reply_sendfile(Config) ->
	doc("Reply using sendfile; get an uncompressed response."),
	{200, Headers, Body} = do_get("/reply/sendfile",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	false = lists:keyfind(<<"content-encoding">>, 1, Headers),
	ct:log("Body received:~n~p~n", [Body]),
	ok.

gzip_reply_small_body(Config) ->
	doc("Reply a small body; get an uncompressed response."),
	{200, Headers, _} = do_get("/reply/small",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	false = lists:keyfind(<<"content-encoding">>, 1, Headers),
	{_, <<"100">>} = lists:keyfind(<<"content-length">>, 1, Headers),
	ok.

gzip_stream_reply(Config) ->
	doc("Stream reply; get a gzipped response."),
	{200, Headers, GzBody} = do_get("/stream_reply/large",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	{_, <<"gzip">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	_ = zlib:gunzip(GzBody),
	ok.

gzip_stream_reply_content_encoding(Config) ->
	doc("Stream reply with content-encoding header; get an uncompressed response."),
	{200, Headers, Body} = do_get("/stream_reply/content-encoding",
		[{<<"accept-encoding">>, <<"gzip">>}], Config),
	{_, <<"compress">>} = lists:keyfind(<<"content-encoding">>, 1, Headers),
	100000 = iolist_size(Body),
	ok.
