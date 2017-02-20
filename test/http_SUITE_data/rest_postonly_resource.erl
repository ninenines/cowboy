-module(rest_postonly_resource).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([from_text/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"text">>, <<"plain">>, '*'}, from_text}], Req, State}.

from_text(Req, State) ->
	{true, Req, State}.
