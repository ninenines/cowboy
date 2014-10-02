%% Copyright (c) 2014, James Fish <james@fishcakez.com>
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
-module(cowboy_system).

-type replace_state() ::
	fun(({module(), cowboy_req:req() | undefined, any()})
		-> {module(), cowboy_req:req() | undefined, any()}).
-export_type([replace_state/0]).

-callback continue(cowboy_req:req(), cowboy_middleware:env(), any())
	-> {ok, cowboy_req:req(), cowboy_middleware:env()}
	| {suspend, module(), atom(), [any()]}
	| {system, {pid(), term()}, any(), module(), cowboy_req:req(),
		cowboy_middleware:env(), any()}
	| {halt, cowboy_req:req(), cowboy_middleware:env()}.
-callback terminate(any(), cowboy_req:req(), cowboy_middleware:env(), any())
	-> no_return().
-callback code_change(cowboy_req:req(), any(), module(), any(), any())
	-> {ok, cowboy_req:req(), any()}.
%% @todo optional -callback get_state(cowboy_req:req(), any())
%%	-> {ok, {module(), cowboy_req:req(), any()}}.
%% @todo optional -callback replace_state(replace_state(), cowboy_req:req(), any())
%%	-> {ok, {module(), cowboy_req:req(), any()}, cowboy_req:req(), any()}.
%% @todo optional -callback format_status(normal, [[{any(), any()}] |
%%		running | suspended | cowboy_req:req() | cowboy_middleware:env() | any()])
%%	-> any().
