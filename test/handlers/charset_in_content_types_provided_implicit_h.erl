%% This module has a media type provided with a wildcard
%% and a list of charsets that is limited.

-module(charset_in_content_types_provided_implicit_h).

-export([init/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([get_text_plain/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"plain">>, '*'}, get_text_plain}
	], Req, State}.

charsets_provided(Req, State) ->
	{[<<"utf-8">>, <<"utf-16">>], Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.

