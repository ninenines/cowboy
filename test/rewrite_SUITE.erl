%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
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

-module(rewrite_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [init_http/3]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [gun_down/1]).

%% ct.

suite() ->
	[{timetrap, 30000}].

all() ->
	[{group, rewrite}].

groups() ->
	[{rewrite, [parallel], ct_helper:all(?MODULE)}].

init_opts() ->
	Dispatch = cowboy_router:compile([{"localhost", [
		{"/[...]", send_message_h, #{}}
	]}]),
	#{
		env => #{dispatch => Dispatch},
		stream_handlers => [cowboy_rewrite_h, cowboy_compress_h, cowboy_stream_h]
	}.

set_rewrite_opt(Ref, Rewrite) ->
	Opts = ranch:get_protocol_options(Ref),
	ranch:set_protocol_options(Ref, Opts#{
		rewrite => Rewrite
	}).

-define(REWRITE(R), set_rewrite_opt(?FUNCTION_NAME, R)).

do_get(Path, Config) ->
	%% Perform a GET request.
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, Path, [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-pid">>, pid_to_list(self())}
	]),
	try gun:await(ConnPid, Ref) of
		{response, _, 200, _} ->
			%% Receive the message from send_message_h.
			Self = self(),
			receive {Self, _FromPid, init, Req, _State} ->
				{ok, Req}
			after 1000 ->
				error(timeout)
			end;
		{response, _, Status, Headers} when Status >= 300, Status =< 399 ->
			{redirect, Status, Headers}
	after
		gun:close(ConnPid)
	end.

%% Tests.

%% @todo custom_fun(Config) ->

%% @todo custom_mf(Config) ->

do_check_location(Path, Headers) ->
	{_, Location} = lists:keyfind(<<"location">>, 1, Headers),
	#{path := Path} = uri_string:parse(Location),
	ok.

index(Config0) ->
	Config = init_http(?FUNCTION_NAME, init_opts(), Config0),
	try
		%% Default options.
		?REWRITE([{index, #{}}]),
		{ok, #{path := <<"/index.html">>}} = do_get("/", Config),
		{ok, #{path := <<"/path/to">>}} = do_get("/path/to", Config),
		{ok, #{path := <<"/path/to/index.html">>}} = do_get("/path/to/", Config),

		%% Custom filename.
		?REWRITE([{index, #{filename => <<"oops.html">>}}]),
		{ok, #{path := <<"/oops.html">>}} = do_get("/", Config),
		{ok, #{path := <<"/path/to">>}} = do_get("/path/to", Config),
		{ok, #{path := <<"/path/to/oops.html">>}} = do_get("/path/to/", Config),

		%% Custom outcome: external redirect.
		?REWRITE([{index, #{outcome => {redirect, 302}}}]),
		{redirect, 302, Headers1} = do_get("/", Config),
		do_check_location(<<"/index.html">>, Headers1),
		{ok, #{path := <<"/path/to">>}} = do_get("/path/to", Config),
		{redirect, 302, Headers2} = do_get("/path/to/", Config),
		do_check_location(<<"/path/to/index.html">>, Headers2),

		%% Custom outcome: stop processing.
		?REWRITE([
			{index, #{outcome => stop}},
			{fun(Req,_) -> {continue, Req#{path => <<"/the_end">>}} end, #{}}
		]),
		{ok, #{path := <<"/index.html">>}} = do_get("/", Config),
		{ok, #{path := <<"/the_end">>}} = do_get("/path/to", Config),
		{ok, #{path := <<"/path/to/index.html">>}} = do_get("/path/to/", Config)
	after
		cowboy:stop_listener(?FUNCTION_NAME)
	end.

%% @todo slash(Config) ->

