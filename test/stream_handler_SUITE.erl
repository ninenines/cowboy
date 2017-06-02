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

-module(stream_handler_SUITE).
-compile(export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(cowboy_test, [gun_open/1]).

%% ct.

all() ->
	cowboy_test:common_all().

groups() ->
	cowboy_test:common_groups(ct_helper:all(?MODULE)).

init_per_group(Name = http, Config) ->
	cowboy_test:init_http(Name, #{stream_handlers => [stream_handler_h]}, Config);
init_per_group(Name = https, Config) ->
	cowboy_test:init_https(Name, #{stream_handlers => [stream_handler_h]}, Config);
init_per_group(Name = h2, Config) ->
	cowboy_test:init_http2(Name, #{stream_handlers => [stream_handler_h]}, Config);
init_per_group(Name = h2c, Config) ->
	Config1 = cowboy_test:init_http(Name, #{stream_handlers => [stream_handler_h]}, Config),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2});
init_per_group(Name = http_compress, Config) ->
	cowboy_test:init_http(Name, #{
		stream_handlers => [cowboy_compress_h, stream_handler_h]
	}, Config);
init_per_group(Name = https_compress, Config) ->
	cowboy_test:init_https(Name, #{
		stream_handlers => [cowboy_compress_h, stream_handler_h]
	}, Config);
init_per_group(Name = h2_compress, Config) ->
	cowboy_test:init_http2(Name, #{
		stream_handlers => [cowboy_compress_h, stream_handler_h]
	}, Config);
init_per_group(Name = h2c_compress, Config) ->
	Config1 = cowboy_test:init_http(Name, #{
		stream_handlers => [cowboy_compress_h, stream_handler_h]
	}, Config),
	lists:keyreplace(protocol, 1, Config1, {protocol, http2}).

end_per_group(Name, _) ->
	cowboy:stop_listener(Name).

%% Tests.

terminate_on_socket_close(Config) ->
	doc("Confirm terminate/3 is called when the socket gets closed brutally."),
	Self = self(),
	ConnPid = gun_open(Config),
	Ref = gun:get(ConnPid, "/long_polling", [
		{<<"accept-encoding">>, <<"gzip">>},
		{<<"x-test-case">>, <<"stream">>},
		{<<"x-test-pid">>, pid_to_list(Self)}
	]),
	%% Confirm init/3 is called and receive the beginning of the response.
	Pid = receive {Self, P, init, _, _, _} -> P after 1000 -> error(timeout) end,
	{response, nofin, 200, _} = gun:await(ConnPid, Ref),
	%% Close the socket.
	ok = gun:close(ConnPid),
	%% Confirm terminate/3 is called.
	receive {Self, Pid, terminate, _, _, _} -> ok after 1000 -> error(timeout) end,
	ok.
