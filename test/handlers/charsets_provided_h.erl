%% This module has a text and non-text media type,
%% and provides two charsets.

-module(charsets_provided_h).

-export([init/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([get_text_plain/2]).
-export([get_application_json/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"plain">>, []}, get_text_plain},
		{{<<"application">>, <<"json">>, []}, get_application_json}
	], Req, State}.

charsets_provided(Req, State) ->
	{[<<"utf-8">>, <<"utf-16">>], Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.

get_application_json(Req, State) ->
	{<<"{\"hello\": \"rest\"}">>, Req, State}.
