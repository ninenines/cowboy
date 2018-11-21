%% This module returns something different in
%% AcceptCallback depending on the query string.

-module(accept_callback_h).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([put_text_plain/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"PUT">>, <<"POST">>, <<"PATCH">>], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, put_text_plain}], Req, State}.

put_text_plain(Req=#{qs := <<"false">>}, State) ->
	{false, Req, State};
put_text_plain(Req=#{qs := <<"true">>}, State) ->
	{true, Req, State}.
