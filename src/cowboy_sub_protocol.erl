%% Copyright (c) 2013-2024, Loïc Hoguin <essen@ninenines.eu>
%% Copyright (c) 2013, James Fish <james@fishcakez.com>
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

-module(cowboy_sub_protocol).

-callback upgrade(Req, Env, module(), any())
	-> {ok, Req, Env} | {suspend, module(), atom(), [any()]} | {stop, Req}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().

-callback upgrade(Req, Env, module(), any(), any())
	-> {ok, Req, Env} | {suspend, module(), atom(), [any()]} | {stop, Req}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
