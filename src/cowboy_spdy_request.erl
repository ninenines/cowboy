%% Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
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
-module(cowboy_spdy_request).
-behaviour(cowboy_sys).

%% API.
-export([spawn_link/10]).

%% Internal.
-export([init/11]).
-export([resume/5]).

%% System.
-export([sys_continue/2]).
-export([sys_terminate/3]).

-spec spawn_link(cowboy_spdy:socket(), {inet:ip_address(), inet:port_number()},
		cowboy:onresponse_fun(), cowboy_middleware:env(), [module()],
		binary(), binary(), binary(), binary(), [{binary(), binary()}])
	-> pid().
spawn_link(FakeSocket, Peer, OnResponse,
		Env, Middlewares, Method, Host, Path, Version, Headers) ->
	cowboy_proc:spawn_link(?MODULE, init,
		[FakeSocket, self(), Peer, OnResponse, Env, Middlewares, Method, Host,
			Path, Version, Headers]).

%% Internal.

-spec init(cowboy_spdy:socket(), pid(), {inet:ip_address(), inet:port_number()},
		cowboy:onresponse_fun(), cowboy_middleware:env(), [module()],
		binary(), binary(), binary(), binary(), [{binary(), binary()}])
	-> no_return().
init(FakeSocket, Parent, Peer, OnResponse,
		Env, Middlewares, Method, Host, Path, Version, Headers) ->
	{Host2, Port} = cow_http:parse_fullhost(Host),
	{Path2, Qs} = cow_http:parse_fullpath(Path),
	Version2 = cow_http:parse_version(Version),
	Req = cowboy_req:new(FakeSocket, cowboy_spdy, Peer,
		Method, Path2, Qs, Version2, Headers,
		Host2, Port, <<>>, true, false, OnResponse),
	execute(Req, Parent, [{parent, Parent}|Env], Middlewares).


execute(Req, _Parent, _Env, []) ->
	cowboy_req:ensure_response(Req, 204);
execute(Req, Parent, Env, [Middleware|Tail]) ->
	case Middleware:execute(Req, Env) of
		{ok, Req2, Env2} ->
			execute(Req2, Parent, Env2, Tail);
		{suspend, Module, Fun, Args} ->
			cowboy_proc:hibernate(?MODULE, resume,
							 [Parent, Tail, Module, Fun, Args]);
		{system, From, Msg, Module, Req2, ModState} ->
			cowboy_sys:handle_msg(Msg, From, Parent, ?MODULE, Req2,
				{Parent, Tail, Module, ModState});
		{halt, Req2} ->
			cowboy_req:ensure_response(Req2, 204)
	end.

-spec resume(pid(), [module()], module(), module(), [any()]) -> no_return().
resume(Parent, Tail, Module, Fun, Args) ->
	case apply(Module, Fun, Args) of
		{ok, Req, Env} ->
			execute(Req, Parent, Env, Tail);
		{suspend, Module2, Fun2, Args2} ->
			cowboy_proc:hibernate(?MODULE, resume,
							 [Parent, Tail, Module2, Fun2, Args2]);
		{system, From, Msg, Module2, Req2, ModState2} ->
			cowboy_sys:handle_msg(Msg, From, Parent, ?MODULE, Req2,
				{Parent, Tail, Module2, ModState2});
		{halt, Req2} ->
			cowboy_req:ensure_response(Req2, 204)
	end.

%% System.

-spec sys_continue(cowboy_req:req(), {pid(), [module()], module(), any()})
	-> no_return().
sys_continue(Req, {Parent, Tail, Module, ModState}) ->
	resume(Parent, Tail, Module, sys_continue, [Req, ModState]).

-spec sys_terminate(any(), cowboy_req:req(),
		{pid(), [module()], module(), any()})
	-> no_return().
sys_terminate(Reason, Req, {_Parent, _Tail, Module, ModState}) ->
	Module:sys_terminate(Reason, Req, ModState).
