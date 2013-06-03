-module(rest_post_charset_resource).
-export([init/3, allowed_methods/2, content_types_accepted/2, from_text/2]).

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"text">>, <<"plain">>, [{<<"charset">>, <<"utf-8">>}]},
		from_text}], Req, State}.

from_text(Req, State) ->
	{true, Req, State}.
