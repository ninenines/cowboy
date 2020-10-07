-module(create_resource_h).

-export([init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([content_types_accepted/2]).
-export([from_text/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"POST">>], Req, State}.

resource_exists(Req, State) ->
	{true, Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"text">>, []}, from_text}], Req, State}.

from_text(Req=#{qs := Qs}, State) ->
	NewURI = [cowboy_req:uri(Req), "/foo"],
	case Qs of
		<<"created">> ->
			{{created, NewURI}, Req, State};
		<<"see_other">> ->
			{{see_other, NewURI}, Req, State}
	end.
