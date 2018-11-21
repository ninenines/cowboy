%% This module sends a different last-modified value
%% depending on the query string.

-module(last_modified_h).

-export([init/2]).
-export([content_types_provided/2]).
-export([get_text_plain/2]).
-export([last_modified/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.

last_modified(Req=#{qs := <<"tuple">>}, State) ->
	{{{2012, 9, 21}, {22, 36, 14}}, Req, State};
%% Simulate the callback being missing in other cases.
last_modified(#{qs := <<"missing">>}, _) ->
	no_call.
