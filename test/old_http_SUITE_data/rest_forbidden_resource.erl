-module(rest_forbidden_resource).

-export([init/2]).
-export([allowed_methods/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([to_text/2]).
-export([from_text/2]).

init(Req, [Forbidden]) ->
	{cowboy_rest, Req, Forbidden}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"HEAD">>, <<"POST">>], Req, State}.

forbidden(Req, State=true) ->
	{true, Req, State};
forbidden(Req, State=false) ->
	{false, Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, to_text}], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, from_text}], Req, State}.

to_text(Req, State) ->
	{<<"This is REST!">>, Req, State}.

from_text(Req, State) ->
	{{true, cowboy_req:path(Req)}, Req, State}.
