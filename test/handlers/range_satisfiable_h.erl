%% This module defines the range_satisfiable callback
%% and return something different depending on query string.

-module(range_satisfiable_h).

-export([init/2]).
-export([content_types_provided/2]).
-export([ranges_provided/2]).
-export([range_satisfiable/2]).
-export([get_text_plain/2]).
-export([get_text_plain_bytes/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

ranges_provided(Req, State) ->
	{[{<<"bytes">>, get_text_plain_bytes}], Req, State}.

%% Simulate the callback being missing, otherwise expect true/false.
range_satisfiable(#{qs := <<"missing">>}, _) ->
	no_call;
range_satisfiable(Req=#{qs := <<"false-int">>}, State) ->
	{{false, 123}, Req, State};
range_satisfiable(Req=#{qs := <<"false-bin">>}, State) ->
	{{false, <<"*/456">>}, Req, State};
range_satisfiable(Req=#{qs := Qs}, State) ->
	{Qs =:= <<"true">>, Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.

get_text_plain_bytes(Req, State) ->
	%% We send everything in one part, since we are not testing
	%% this callback specifically.
	Body = <<"This is ranged REST!">>,
	{[{{0, byte_size(Body) - 1, byte_size(Body)}, Body}], Req, State}.
