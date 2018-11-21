%% This module accepts a multipart media type with parameters
%% that do not include boundary.

-module(content_types_accepted_h).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([put_multipart_mixed/2]).
-export([put_text_plain/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"PUT">>], Req, State}.

content_types_accepted(Req=#{qs := <<"multipart">>}, State) ->
	{[
		{{<<"multipart">>, <<"mixed">>, [{<<"v">>, <<"1">>}]}, put_multipart_mixed}
	], Req, State};
content_types_accepted(Req=#{qs := <<"wildcard-param">>}, State) ->
	{[{{<<"text">>, <<"plain">>, '*'}, put_text_plain}], Req, State}.

put_multipart_mixed(Req, State) ->
	{true, Req, State}.

put_text_plain(Req0, State) ->
	{ok, _, Req} = cowboy_req:read_body(Req0),
	{true, Req, State}.
