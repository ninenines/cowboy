%% Copyright (c) 2011, Magnus Klaar <magnus.klaar@gmail.com>
%% Copyright (c) 2011, Loïc Hoguin <essen@ninenines.eu>
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

-module(dispatcher_prop).
-include_lib("proper/include/proper.hrl").

%% Generators.

hostname_head_char() ->
	oneof([choose($a, $z), choose($A, $Z), choose($0, $9)]).

hostname_char() ->
	oneof([choose($a, $z), choose($A, $Z), choose($0, $9), $-]).

hostname_label() ->
	?SUCHTHAT(Label, [hostname_head_char()|list(hostname_char())],
		length(Label) < 64).

hostname() ->
	?SUCHTHAT(Hostname,
		?LET(Labels, list(hostname_label()), string:join(Labels, ".")),
		length(Hostname) > 0 andalso length(Hostname) =< 255).

port_number() ->
	choose(1, 16#ffff).

port_str() ->
	oneof(["", ?LET(Port, port_number(), ":" ++ integer_to_list(Port))]).

server() ->
	?LET({Hostname, PortStr}, {hostname(), port_str()},
		list_to_binary(Hostname ++ PortStr)).

%% Properties.

prop_split_host_symmetric() ->
	?FORALL(Server, server(),
	begin case cowboy_dispatcher:split_host(Server) of
			{Tokens, RawHost, undefined} ->
				(Server == RawHost) and (Server == binary_join(Tokens, "."));
			{Tokens, RawHost, Port} ->
				PortBin = (list_to_binary(":" ++ integer_to_list(Port))),
				(Server == << RawHost/binary, PortBin/binary >>)
				and (Server == << (binary_join(Tokens, "."))/binary,
					PortBin/binary >>)
	end end).

%% Internal.

%% Contributed by MononcQc on #erlounge.
binary_join(Flowers, Leaf) ->
	case Flowers of
		[] -> <<>>;
		[Petal|Pot] -> iolist_to_binary(
			[Petal | [[Leaf | Pollen] || Pollen <- Pot]])
	end.
