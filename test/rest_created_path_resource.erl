-module(rest_created_path_resource).
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([get_text_plain/2]).
-export([post_is_create/2]).
-export([content_types_accepted/2]).
-export([post_text_plain/2]).
-export([created_path/2]).

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
{[<<"HEAD">>, <<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.

post_is_create(Req, State) ->
	{true, Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, post_text_plain}], Req, State}.

post_text_plain(Req, State) ->
	{true, Req, State}.

created_path(Req, State) ->
	{<<"/created">>, Req, State}.


