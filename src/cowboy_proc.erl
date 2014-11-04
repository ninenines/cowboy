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
-module(cowboy_proc).

%% API.
-export([spawn_link/3]).
-export([hibernate/3]).
-export([continue/3]).

%% Internal.
-export([init/3]).

%% API.

-spec spawn_link(module(), atom(), [any()]) -> pid().
spawn_link(Module, Fun, Args) ->
	proc_lib:spawn_link(?MODULE, init, [Module, Fun, Args]).

-spec hibernate(module(), atom(), [any()]) -> no_return().
hibernate(Module, Fun, Args) ->
	proc_lib:hibernate(?MODULE, continue, [Module, Fun, Args]).

-spec continue(module(), atom(), [any()]) -> any().
continue(Module, Fun, Args) ->
	try
		apply(Module, Fun, Args)
	catch
		error:Reason ->
			exit({Reason, erlang:get_stacktrace()});
		throw:Value ->
			exit({{nocatch, Value}, erlang:get_stacktrace()})
	end.

%% Internal.

-spec init(module(), atom(), [any()]) -> any().
init(Module, Fun, Args) ->
	_ = put('$initial_call', {Module, Fun, length(Args)}),
	try
		apply(Module, Fun, Args)
	catch
		error:Reason ->
			exit({Reason, erlang:get_stacktrace()});
		throw:Value ->
			exit({{nocatch, Value}, erlang:get_stacktrace()})
	end.
