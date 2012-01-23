-module(rest_nodelete_resource).
-export([init/3, allowed_methods/2, content_types_provided/2,
		get_text_plain/2]).

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_http_rest}.

allowed_methods(Req, State) ->
	{['GET', 'HEAD', 'DELETE'], Req, State}.


content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.

