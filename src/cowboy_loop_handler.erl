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

-module(cowboy_loop_handler).

-type opts() :: any().
-type state() :: any().
%% @todo see terminate
%-type terminate_reason() :: {normal, shutdown}
%	| {normal, timeout}
%	| {error, closed}
%	| {error, overflow}
%	| {error, atom()}.

-callback init(Req, opts())
	-> {http, Req, state()}
	| {long_polling | rest | ws | module(), Req, state()}
	| {long_polling | rest | ws | module(), Req, state(), hibernate}
	| {long_polling | rest | ws | module(), Req, state(), timeout()}
	| {long_polling | rest | ws | module(), Req, state(), timeout(), hibernate}
	| {shutdown, Req, state()}
	when Req::cowboy_req:req().
-callback info(any(), Req, State)
	-> {ok, Req, State}
	| {loop, Req, State}
	| {loop, Req, State, hibernate}
	when Req::cowboy_req:req(), State::state().
%% @todo optional -callback terminate(terminate_reason(), cowboy_req:req(), state()) -> ok.
