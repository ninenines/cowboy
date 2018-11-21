%% This module has different content_types_provided values
%% and/or sends a different response body depending on the
%% query string.

-module(content_types_provided_h).

-export([init/2]).
-export([content_types_provided/2]).
-export([get_text_plain/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req=#{qs := <<"wildcard-param">>}, State) ->
	{[{{<<"text">>, <<"plain">>, '*'}, get_text_plain}], Req, State}.

get_text_plain(Req=#{qs := <<"wildcard-param">>}, State) ->
	{_, _, Param} = maps:get(media_type, Req),
	Body = if
		Param =:= [] -> <<"[]">>;
		Param =/= [] ->
			iolist_to_binary([[Key, $=, Value] || {Key, Value} <- Param])
	end,
	{Body, Req, State}.
