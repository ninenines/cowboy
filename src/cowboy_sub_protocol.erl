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

%% @doc Behaviour for sub protocols.
%%
%% Only one function needs to be implemented, <em>upgrade/4</em>.
%% It receives the Req, the environment, the handler that the request has been
%% routed to and the handler's options. It acts exactly the same as a
%% middleware, so returns the same values a middleware's execute/2.
%%
%% Once the sub protocol has processed the request it should add the result
%% to the environment. This is done by adding the tuple {result, Value} to the
%% environment list. To continue handling requests on the current connection the
%% Value should be the atom ok. Any other value will prevent the processing of
%% subsequent requests.
%%
%% <em>upgrade/4</em> will be called when a handler's init/3 returns
%% {upgrade, protocol, Module}, where Module is the module of the sub protocol.
-module(cowboy_sub_protocol).

-callback upgrade(Req, Env, module(), any())
	-> {ok, Req, Env}
	| {suspend, module(), atom(), any()}
	| {halt, Req}
	| {error, cowboy_http:status(), Req}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
