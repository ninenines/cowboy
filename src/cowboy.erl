%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
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
-export([start_listener/6, stop_listener/1]). %% API.

%% API.

-spec start_listener(Ref::term(), NbAcceptors::non_neg_integer(),
	Transport::module(), TransOpts::term(), Protocol::module(),
	ProtoOpts::term()) -> {ok, Pid::pid()}.
start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts) ->
	supervisor:start_child(cowboy_sup,
		{{cowboy_listener_sup, Ref}, {cowboy_listener_sup, start_link, [
			NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
		]},
		permanent, 5000, supervisor, [cowboy_listener_sup]}).

-spec stop_listener(Ref::term()) -> ok.
stop_listener(Ref) ->
	supervisor:terminate_child(cowboy_sup, {cowboy_listener_sup, Ref}),
	supervisor:delete_child(cowboy_sup, {cowboy_listener_sup, Ref}).
