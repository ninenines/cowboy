%% This module defines the ranges_provided callback
%% and a generate_etag callback that returns something
%% different depending on query string. It also defines
%% a last_modified callback that must be ignored when a
%% date is provided in if_range.

-module(if_range_h).

-export([init/2]).
-export([content_types_provided/2]).
-export([ranges_provided/2]).
-export([generate_etag/2]).
-export([last_modified/2]).
-export([get_text_plain/2]).
-export([get_text_plain_bytes/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

%% Simulate the callback being missing.
ranges_provided(#{qs := <<"missing-ranges_provided">>}, _) ->
	no_call;
ranges_provided(Req=#{qs := <<"empty-ranges_provided">>}, State) ->
	{[], Req, State};
ranges_provided(Req, State) ->
	{[{<<"bytes">>, get_text_plain_bytes}], Req, State}.

generate_etag(Req=#{qs := <<"weak-etag">>}, State) ->
	{{weak, <<"weak-no-match">>}, Req, State};
generate_etag(Req, State) ->
	{{strong, <<"strong-and-match">>}, Req, State}.

last_modified(Req, State) ->
	{{{2222, 2, 22}, {11, 11, 11}}, Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.

get_text_plain_bytes(Req, State) ->
	%% We send everything in one part, since we are not testing
	%% this callback specifically.
	Body = <<"This is ranged REST!">>,
	{[{{0, byte_size(Body) - 1, byte_size(Body)}, Body}], Req, State}.
