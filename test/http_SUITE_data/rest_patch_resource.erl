-module(rest_patch_resource).
-export([init/3, allowed_methods/2, content_types_provided/2, get_text_plain/2,
	content_types_accepted/2, patch_text_plain/2]).

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"HEAD">>, <<"GET">>, <<"PATCH">>], Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.

content_types_accepted(Req, State) ->
	case cowboy_req:method(Req) of
		{<<"PATCH">>, Req0} ->
			{[{{<<"text">>, <<"plain">>, []}, patch_text_plain}], Req0, State};
		{_, Req0} ->
			{[], Req0, State}
	end.

patch_text_plain(Req, State) ->
	case cowboy_req:body(Req) of
		{ok, <<"halt">>, Req0} ->
			{ok, Req1} = cowboy_req:reply(400, Req0),
			{halt, Req1, State};
		{ok, <<"false">>, Req0} ->
			{false, Req0, State};
		{ok, _Body, Req0} ->
			{true, Req0, State}
	end.
