%% Copyright (c) 2011-2014, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy).

-export([start_http/4]).
-export([start_https/4]).
-export([start_spdy/4]).
-export([stop_listener/1]).
-export([set_env/3]).

-type http_headers() :: [{binary(), iodata()}].
-export_type([http_headers/0]).

-type http_status() :: non_neg_integer() | binary().
-export_type([http_status/0]).

-type http_version() :: 'HTTP/1.1' | 'HTTP/1.0'.
-export_type([http_version/0]).

-type onrequest_fun() :: fun((Req) -> Req).
-export_type([onrequest_fun/0]).

-type onresponse_fun() ::
	fun((http_status(), http_headers(), iodata(), Req) -> Req).
-export_type([onresponse_fun/0]).

-spec start_http(ranch:ref(), non_neg_integer(), ranch_tcp:opts(),
	cowboy_protocol:opts()) -> {ok, pid()} | {error, any()}.
start_http(Ref, NbAcceptors, TransOpts, ProtoOpts)
		when is_integer(NbAcceptors), NbAcceptors > 0 ->
	ranch:start_listener(Ref, NbAcceptors,
		ranch_tcp, TransOpts, cowboy_protocol, ProtoOpts).

-spec start_https(ranch:ref(), non_neg_integer(), ranch_ssl:opts(),
	cowboy_protocol:opts()) -> {ok, pid()} | {error, any()}.
start_https(Ref, NbAcceptors, TransOpts, ProtoOpts)
		when is_integer(NbAcceptors), NbAcceptors > 0 ->
	ranch:start_listener(Ref, NbAcceptors,
		ranch_ssl, TransOpts, cowboy_protocol, ProtoOpts).

-spec start_spdy(ranch:ref(), non_neg_integer(), ranch_ssl:opts(),
	cowboy_spdy:opts()) -> {ok, pid()} | {error, any()}.
start_spdy(Ref, NbAcceptors, TransOpts, ProtoOpts)
		when is_integer(NbAcceptors), NbAcceptors > 0 ->
	TransOpts2 = [
		{connection_type, supervisor},
		{next_protocols_advertised,
			[<<"spdy/3">>, <<"http/1.1">>, <<"http/1.0">>]}
	|TransOpts],
	ranch:start_listener(Ref, NbAcceptors,
		ranch_ssl, TransOpts2, cowboy_spdy, ProtoOpts).

-spec stop_listener(ranch:ref()) -> ok | {error, not_found}.
stop_listener(Ref) ->
	ranch:stop_listener(Ref).

-spec set_env(ranch:ref(), atom(), any()) -> ok.
set_env(Ref, Name, Value) ->
	Opts = ranch:get_protocol_options(Ref),
	{_, Env} = lists:keyfind(env, 1, Opts),
	Opts2 = lists:keyreplace(env, 1, Opts,
		{env, lists:keystore(Name, 1, Env, {Name, Value})}),
	ok = ranch:set_protocol_options(Ref, Opts2).
