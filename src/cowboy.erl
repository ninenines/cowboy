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

-spec start_listener(any(), non_neg_integer(), module(), any(), module(), any())
	-> {ok, pid()}.
start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts) ->
	supervisor:start_child(cowboy_sup,
		{{cowboy_listener_sup, Ref}, {cowboy_listener_sup, start_link, [
			NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
		]},
		permanent, 5000, supervisor, [cowboy_listener_sup]}).

-spec stop_listener(any()) -> ok | {error, not_found}.
stop_listener(Ref) ->
	case supervisor:terminate_child(cowboy_sup, {cowboy_listener_sup, Ref}) of
		ok ->
			supervisor:delete_child(cowboy_sup, {cowboy_listener_sup, Ref});
		{error, Reason} ->
			{error, Reason}
	end.
