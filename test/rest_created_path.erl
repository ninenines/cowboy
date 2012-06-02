-module(rest_created_path).
-export([init/3, allowed_methods/2, content_types_provided/2, to_text/2,
	post_is_create/2, content_types_accepted/2, from_text/2,
	created_path/2
	]).

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_http_rest}.

allowed_methods(Req, State) ->
	{['HEAD', 'GET', 'POST'], Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, to_text}], Req, State}.

to_text(Req, State) ->
	{<<"This is REST!">>, Req, State}.

post_is_create(Req, State) ->
	{true, Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, from_text}], Req, State}.

from_text(Req, State) ->
	{true, Req, State}.

created_path(Req, State) ->
	{<<"/newpath">>, Req, State}.
