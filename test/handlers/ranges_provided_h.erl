%% This module defines the ranges_provided callback
%% and return something different depending on query string.

-module(ranges_provided_h).

-export([init/2]).
-export([content_types_provided/2]).
-export([ranges_provided/2]).
-export([get_text_plain/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

ranges_provided(Req=#{qs := <<"list">>}, State) ->
	{[
		{<<"bytes">>, get_text_plain_bytes},
		{<<"pages">>, get_text_plain_pages},
		{<<"chapters">>, get_text_plain_chapters}
	], Req, State};
ranges_provided(Req=#{qs := <<"none">>}, State) ->
	{[], Req, State};
%% Simulate the callback being missing in other cases.
ranges_provided(_, _) ->
	no_call.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.
