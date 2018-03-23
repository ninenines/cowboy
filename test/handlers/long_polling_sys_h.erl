%% This module implements a loop handler that does nothing
%% and expects a crash to happen.

-module(long_polling_sys_h).

-export([init/2]).
-export([info/3]).
-export([terminate/3]).

init(Req, _) ->
	process_flag(trap_exit, true),
	erlang:send_after(500, self(), timeout),
	{cowboy_loop, Req, undefined}.

info(timeout, Req, State) ->
	%% Send an unused status code to make sure there's no
	%% conflict with whatever Cowboy may send itself.
	{ok, cowboy_req:reply(<<"299 OK!">>, Req), State};
info(_, Req, State) ->
	{ok, Req, State}.

terminate(_, _, _) ->
	ok.
