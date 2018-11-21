%% This module sends a different expires value
%% depending on the query string.

-module(expires_h).

-export([init/2]).
-export([content_types_provided/2]).
-export([get_text_plain/2]).
-export([expires/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.

expires(Req=#{qs := <<"tuple">>}, State) ->
	{{{2012, 9, 21}, {22, 36, 14}}, Req, State};
expires(Req=#{qs := <<"binary">>}, State) ->
	{<<"0">>, Req, State};
expires(Req=#{qs := <<"undefined">>}, State) ->
	{undefined, Req, State};
%% Simulate the callback being missing in other cases.
expires(#{qs := <<"missing">>}, _) ->
	no_call.
