%% This module has a media type provided with an explicit charset.

-module(charset_in_content_types_provided_h).

-export([init/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([get_text_plain/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"plain">>, [{<<"charset">>, <<"utf-8">>}]}, get_text_plain}
	], Req, State}.

charsets_provided(Req, State) ->
	{[<<"utf-16">>, <<"iso-8861-1">>], Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.
