%% This module accepts a multipart media type with parameters
%% that do not include boundary.

-module(content_types_accepted_h).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([put_multipart_mixed/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"PUT">>], Req, State}.

content_types_accepted(Req=#{qs := <<"multipart">>}, State) ->
	{[
		{{<<"multipart">>, <<"mixed">>, [{<<"v">>, <<"1">>}]}, put_multipart_mixed}
	], Req, State}.

put_multipart_mixed(Req, State) ->
	{true, Req, State}.
