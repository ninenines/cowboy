%% This module accepts a multipart media type with parameters
%% that do not include boundary.

-module(delete_resource_h).

-export([init/2]).
-export([allowed_methods/2]).
-export([delete_resource/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"DELETE">>], Req, State}.

delete_resource(#{qs := <<"missing">>}, _) ->
	no_call.
