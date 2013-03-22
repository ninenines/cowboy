%% Copyright (c) 2011-2013, Loïc Hoguin <essen@ninenines.eu>
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

%% @doc Convenience API to start and stop HTTP/HTTPS listeners.
-module(cowboy).

-export([start_http/4]).
-export([start_https/4]).
-export([stop_listener/1]).
-export([set_env/3]).

%% @doc Start an HTTP listener.
-spec start_http(any(), non_neg_integer(), any(), any()) -> {ok, pid()}.
start_http(Ref, NbAcceptors, TransOpts, ProtoOpts)
		when is_integer(NbAcceptors), NbAcceptors > 0 ->
	ranch:start_listener(Ref, NbAcceptors,
		ranch_tcp, TransOpts, cowboy_protocol, ProtoOpts).

%% @doc Start an HTTPS listener.
-spec start_https(any(), non_neg_integer(), any(), any()) -> {ok, pid()}.
start_https(Ref, NbAcceptors, TransOpts, ProtoOpts)
		when is_integer(NbAcceptors), NbAcceptors > 0 ->
	ranch:start_listener(Ref, NbAcceptors,
		ranch_ssl, TransOpts, cowboy_protocol, ProtoOpts).

%% @doc Stop a listener.
-spec stop_listener(any()) -> ok.
stop_listener(Ref) ->
	ranch:stop_listener(Ref).

%% @doc Convenience function for setting an environment value.
%%
%% Allows you to update live an environment value used by middlewares.
%% This function is primarily intended to simplify updating the dispatch
%% list used for routing.
-spec set_env(any(), atom(), any()) -> ok.
set_env(Ref, Name, Value) ->
	Opts = ranch:get_protocol_options(Ref),
	{_, Env} = lists:keyfind(env, 1, Opts),
	Env2 = [{Name, Value}|lists:keydelete(Name, 1, Env)],
	Opts2 = lists:keyreplace(env, 1, Opts, {env, Env2}),
	ok = ranch:set_protocol_options(Ref, Opts2).
