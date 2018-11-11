%% This module defines the ranges_provided callback
%% which returns the auto option for bytes ranges
%% and the normal ProvideCallback that returns
%% something different depending on query string.

-module(ranges_provided_auto_h).

-export([init/2]).
-export([content_types_provided/2]).
-export([ranges_provided/2]).
-export([get_text_plain/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

ranges_provided(Req, State) ->
	{[{<<"bytes">>, auto}], Req, State}.

get_text_plain(Req=#{qs := <<"data">>}, State) ->
	{<<"This is ranged REST!">>, Req, State};
get_text_plain(Req=#{qs := <<"sendfile">>}, State) ->
	Path = code:lib_dir(cowboy) ++ "/ebin/cowboy.app",
	Size = filelib:file_size(Path),
	{{sendfile, 0, Size, Path}, Req, State}.
