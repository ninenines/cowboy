%% Copyright (c) 2013, Loïc Hoguin <essen@ninenines.eu>
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

%% @doc Behaviour for middlewares.
%%
%% Only one function needs to be implemented, <em>execute/2</em>.
%% It receives the Req and the environment and returns them
%% optionally modified. It can decide to stop the processing with
%% or without an error. It is also possible to hibernate the process
%% if needed.
%%
%% A middleware can perform any operation. Make sure you always return
%% the last modified Req so that Cowboy has the up to date information
%% about the request.
-module(cowboy_middleware).

-type env() :: [{atom(), any()}].
-export_type([env/0]).

-callback execute(Req, Env)
	-> {ok, Req, Env}
	| {suspend, module(), atom(), any()}
	| {halt, Req}
	| {error, cowboy_http:status(), Req}
	when Req::cowboy_req:req(), Env::env().
