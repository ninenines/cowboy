-module(rest_expires_binary).

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

expires(Req, State) ->
	{<<"0">>, Req, State}.
