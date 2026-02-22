%% Copyright (c) Loic Hoguin <essen@ninenines.eu>
%% Copyright (c) Benoit Chesneau <bchesneau@gmail.com>
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

%% QUIC adapter selection header.
%%
%% This header selects the appropriate QUIC backend module based on
%% compile-time flags:
%% - COWBOY_QUICER=1: Use emqx/quicer NIF (cowboy_quicer)
%% - Default: Use pure Erlang erlang_quic (cowboy_quic)

-ifdef(COWBOY_QUICER).
-define(QUIC_ADAPTER, cowboy_quicer).
-else.
-define(QUIC_ADAPTER, cowboy_quic).
-endif.
