%% This module does rate limiting based on the query string value.

-module(rate_limited_h).

-export([init/2]).
-export([rate_limited/2]).
-export([content_types_provided/2]).
-export([get_text_plain/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

rate_limited(Req=#{qs := <<"false">>}, State) ->
	{false, Req, State};
rate_limited(Req=#{qs := <<"true-date">>}, State) ->
	{{true, {{2222, 2, 22}, {11, 11, 11}}}, Req, State};
rate_limited(Req=#{qs := <<"true">>}, State) ->
	{{true, 3600}, Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.
