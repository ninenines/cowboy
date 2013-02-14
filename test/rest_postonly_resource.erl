-module(rest_postonly_resource).
-export([init/3, allowed_methods/2, content_types_accepted/2,
	    post_is_create/2, create_path/2, from_text/2]).

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, from_text}], Req, State}.

post_is_create(Req, State) ->
	{true, Req, State}.

create_path(Req, State) ->
	{Path, Req2} = cowboy_req:path(Req),
	{Path, Req2, State}.

from_text(Req, State) ->
	{true, Req, State}.
