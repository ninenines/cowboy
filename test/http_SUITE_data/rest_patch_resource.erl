-module(rest_patch_resource).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([get_text_plain/2]).
-export([content_types_accepted/2]).
-export([patch_text_plain/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"HEAD">>, <<"GET">>, <<"PATCH">>], Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.

content_types_accepted(Req, State) ->
	case cowboy_req:method(Req) of
		<<"PATCH">> ->
			{[{{<<"text">>, <<"plain">>, []}, patch_text_plain}], Req, State};
		_ ->
			{[], Req, State}
	end.

patch_text_plain(Req, State) ->
	case cowboy_req:read_body(Req) of
		{ok, <<"stop">>, Req0} ->
			{stop, cowboy_req:reply(400, Req0), State};
		{ok, <<"false">>, Req0} ->
			{false, Req0, State};
		{ok, _Body, Req0} ->
			{true, Req0, State}
	end.
