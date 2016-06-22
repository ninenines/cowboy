%% Copyright (c) 2016, Andrei Nesterov <ae.nesterov@gmail.com>
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

-module(cors_SUITE).
-compile(export_all).

-import(ct_helper, [config/2]).
-import(cowboy_test, [gun_open/1]).
-import(cowboy_test, [gun_open/2]).
-import(cowboy_test, [gun_down/1]).

%% Definitions.
-define(ORIGIN_URI, <<"http://example.org">>).
-define(REQUEST_METHOD, <<"PUT">>).

%% ct.

all() ->
	[
		{group, http},
		{group, https}
	].

groups() ->
	Tests = ct_helper:all(?MODULE),
	[
		{http, [parallel], Tests},
		{https, [parallel], Tests}
	].

init_per_group(Name = http, Config) ->
	cowboy_test:init_http(Name, [
		{env, [{dispatch, init_dispatch(Config)}]}
	], Config);
init_per_group(Name = https, Config) ->
	cowboy_test:init_https(Name, [
		{env, [{dispatch, init_dispatch(Config)}]}
	], Config).

end_per_group(Name, _) ->
	ok = cowboy:stop_listener(Name).

%% Dispatch configuration.

init_dispatch(_Config) ->
	OriginsVal = {<<"http">>, <<"example.org">>, 80},
	OriginsAny = '*',
	OriginsList =
		[{<<"http">>, <<"example.com">>, 80},
		 {<<"http">>, <<"example.org">>, 80},
		 {<<"http">>, <<"example.org">>, 8080}],
	Methods = [<<"GET">>, <<"PUT">>],
	ExposedHeaders = Headers = [<<"h1">>, <<"h2">>, <<"h3">>],
	MaxAge = 0,

	cowboy_router:compile([
		{"localhost", [
			{"/origins/val", cors_echo,
				[{hs, #{origins => OriginsVal}},
				 {phs, #{origins => OriginsVal, methods => Methods}}]},
			{"/origins/any", cors_echo,
				[{hs, #{origins => OriginsAny}},
				 {phs, #{origins => OriginsAny, methods => Methods}}]},
			{"/origins/list", cors_echo,
				[{hs, #{origins => OriginsList}},
				 {phs, #{origins => OriginsList, methods => Methods}}]},
			{"/credentials/false", cors_echo,
				[{hs, #{origins => OriginsVal, credentials => false}},
				 {phs, #{origins => OriginsVal, credentials => false, methods => Methods}}]},
			{"/credentials/true", cors_echo,
				[{hs, #{origins => OriginsVal, credentials => true}},
				 {phs, #{origins => OriginsVal, credentials => true, methods => Methods}}]},
			{"/credentials/true/origins/any", cors_echo,
				[{hs, #{origins => OriginsAny, credentials => true}},
				 {phs, #{origins => OriginsAny, credentials => true, methods => Methods}}]},
			{"/exposed_headers/undef", cors_echo,
				[{hs, #{origins => OriginsVal}},
				 {phs, #{origins => OriginsVal, methods => Methods}}]},
			{"/exposed_headers/list", cors_echo,
				[{hs, #{origins => OriginsVal, exposed_headers => ExposedHeaders}},
				 {phs, #{origins => OriginsVal, methods => Methods}}]},
			{"/max_age/undef", cors_echo,
				[{hs, #{origins => OriginsVal}},
				 {phs, #{origins => OriginsVal, methods => Methods}}]},
			{"/max_age/val", cors_echo,
				[{hs, #{origins => OriginsVal}},
				 {phs, #{origins => OriginsVal, max_age => MaxAge, methods => Methods}}]},
			{"/methods/list", cors_echo,
				[{hs, #{origins => OriginsVal}},
				 {phs, #{origins => OriginsVal, methods => Methods}}]},
			{"/headers/list", cors_echo,
				[{hs, #{origins => OriginsVal}},
				 {phs, #{origins => OriginsVal, headers => Headers, methods => Methods}}]}
		]}
	]).

%% Convenience functions.

do_request(Path, Headers, Config) ->
	do_request(?REQUEST_METHOD, Path, Headers, Config).

do_request(Method, Path, Headers, Config) ->
	ConnPid = gun_open(Config),
	Ref = gun:request(ConnPid, Method, Path, Headers),
	{response, fin, 200, RespHeaders} = gun:await(ConnPid, Ref),
	RespHeaders.

do_preflight_request(Path, Headers, Config) ->
	do_preflight_request(?REQUEST_METHOD, Path, Headers, Config).

do_preflight_request(Method, Path, Headers, Config) ->
	Headers2 = [{<<"access-control-request-method">>, Method}|Headers],
	do_request(<<"OPTIONS">>, Path, Headers2, Config).

do_find_header(Key, Headers) ->
	case lists:keyfind(Key, 1, Headers) of
		false -> error;
		{_, Val} -> {ok, Val}
	end.

%% Tests.

origins(Config) ->
	OriginH = {<<"origin">>, ?ORIGIN_URI},
	OriginNoMatchH = {<<"origin">>, <<"null">>},
	Tests =
		[%% Origin isn't presented
		 {"/origins/val", [], error},
		 % Origin isn't allowed
		 {"/origins/val", [OriginNoMatchH], error},
		 %% Single origin value is allowed
		 {"/origins/val", [OriginH], {ok, ?ORIGIN_URI}},
		 %% Any origin is allowed
		 {"/origins/any", [OriginH], {ok, ?ORIGIN_URI}},
		 %% Origin is presented in the allowed origins list
		 {"/origins/list", [OriginH], {ok, ?ORIGIN_URI}},
		 %% Origin isn't presented in the allowed origins list
		 {"/origins/list", [OriginNoMatchH], error}],

	%% cors requests
	[begin
		RespHeaders = do_request(Path, Headers, Config),
		MaybeOrigin = do_find_header(<<"access-control-allow-origin">>, RespHeaders)
	end || {Path, Headers, MaybeOrigin} <- Tests],

	%% cors preflight requests
	[begin
		RespHeaders = do_preflight_request(Path, Headers, Config),
		MaybeOrigin = do_find_header(<<"access-control-allow-origin">>, RespHeaders)
	end || {Path, Headers, MaybeOrigin} <- Tests].

credentials(Config) ->
	OriginH = {<<"origin">>, ?ORIGIN_URI},
	Tests =
		[%% Credentials aren't supported
		 {"/credentials/false", [OriginH], error},
		 %% Credentials are supported for this particular origin
		 {"/credentials/true", [OriginH], {ok, <<"true">>}},
		 %% Credentials are supported for any origin
		 {"/credentials/true/origins/any", [OriginH], {ok, <<"true">>}}],

	%% cors requests
	[begin
		RespHeaders = do_request(Path, Headers, Config),
		MaybeCredentials = do_find_header(<<"access-control-allow-credentials">>, RespHeaders),
		{ok, ?ORIGIN_URI} = do_find_header(<<"access-control-allow-origin">>, RespHeaders)
	end || {Path, Headers, MaybeCredentials} <- Tests],

	%% cors preflight requests
	[begin
		RespHeaders = do_preflight_request(Path, Headers, Config),
		MaybeCredentials = do_find_header(<<"access-control-allow-credentials">>, RespHeaders),
		{ok, ?ORIGIN_URI} = do_find_header(<<"access-control-allow-origin">>, RespHeaders)
	end || {Path, Headers, MaybeCredentials} <- Tests].

exposed_headers(Config) ->
	OriginH = {<<"origin">>, ?ORIGIN_URI},
	Tests =
		[%% Exposed headers isn't set
		 {"/exposed_headers/undef", [OriginH], error},
		 %% Exposed headers is set
		 {"/exposed_headers/list", [OriginH], {ok, <<"h1,h2,h3">>}}],

	%% cors requests
	[begin
		RespHeaders = do_request(Path, Headers, Config),
		MaybeExposedHeaders = do_find_header(<<"access-control-expose-headers">>, RespHeaders),
		{ok, ?ORIGIN_URI} = do_find_header(<<"access-control-allow-origin">>, RespHeaders)
	end || {Path, Headers, MaybeExposedHeaders} <- Tests].

max_age(Config) ->
	OriginH = {<<"origin">>, ?ORIGIN_URI},
	Tests =
		[%% Max age isn't set
		 {"/max_age/undef", [OriginH], error},
		 %% Max age is set
		 {"/max_age/val", [OriginH], {ok, <<"0">>}}],

	%% cors preflight requests
	[begin
		RespHeaders = do_preflight_request(Path, Headers, Config),
		MaybeMaxAge = do_find_header(<<"access-control-max-age">>, RespHeaders),
		{ok, ?ORIGIN_URI} = do_find_header(<<"access-control-allow-origin">>, RespHeaders)
	end || {Path, Headers, MaybeMaxAge} <- Tests].

methods(Config) ->
	OriginH = {<<"origin">>, ?ORIGIN_URI},
	MethodH = fun(Val) -> {<<"access-control-request-method">>, Val} end,
	Tests =
		[%% Method isn't presented
		 {"/methods/list", [OriginH], error, error},
		 %% Method isn't allowed
		 {"/methods/list", [OriginH, MethodH(<<"PATCH">>)], error, error},
		 %% Method is allowed
		 {"/methods/list", [OriginH, MethodH(?REQUEST_METHOD)], {ok, ?REQUEST_METHOD}, {ok, ?ORIGIN_URI}}],

	%% cors preflight requests
	[begin
		RespHeaders = do_request(<<"OPTIONS">>, Path, Headers, Config),
		MaybeMethods = do_find_header(<<"access-control-allow-methods">>, RespHeaders),
		MaybeOrigin = do_find_header(<<"access-control-allow-origin">>, RespHeaders)
	end || {Path, Headers, MaybeMethods, MaybeOrigin} <- Tests].

headers(Config) ->
	OriginH = {<<"origin">>, ?ORIGIN_URI},
	HeadersH = fun(Val) -> {<<"access-control-request-headers">>, Val} end,
	Tests =
		[%% Headers aren't presented
		 {"/headers/list", [OriginH], error, {ok, ?ORIGIN_URI}},
		 %% Headers arent't allowed
		 {"/headers/list", [OriginH, HeadersH(<<"h8">>)], error, error},
		 {"/headers/list", [OriginH, HeadersH(<<"h8,h9">>)], error, error},
		 {"/headers/list", [OriginH, HeadersH(<<"h1,h9">>)], error, error},
		 %% Headers are allowed
		 {"/headers/list", [OriginH, HeadersH(<<>>)], error, {ok, ?ORIGIN_URI}},
		 {"/headers/list", [OriginH, HeadersH(<<"h1">>)], {ok, <<"h1">>}, {ok, ?ORIGIN_URI}},
		 {"/headers/list", [OriginH, HeadersH(<<"h1,h2">>)], {ok, <<"h1,h2">>}, {ok, ?ORIGIN_URI}}],

	%% cors preflight requests
	[begin
		RespHeaders = do_preflight_request(Path, Headers, Config),
		MaybeHeaders = do_find_header(<<"access-control-allow-headers">>, RespHeaders),
		MaybeOrigin = do_find_header(<<"access-control-allow-origin">>, RespHeaders)
	end || {Path, Headers, MaybeHeaders, MaybeOrigin} <- Tests].

