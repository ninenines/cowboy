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
get_text_plain(Req=#{qs := <<"01234">>}, State) ->
	{<<"01234">>, Req, State};
get_text_plain(Req=#{qs := <<"size-", SizeBin/bits>>}, State) ->
	{binary:copy(<<"a">>, binary_to_integer(SizeBin)), Req, State};
%% The text is 20 bytes. The four ranges in the multipart
%% test cover 17 of them. 17 + 4 * 256 still fits.
get_text_plain(Req=#{qs := <<"multipart">>}, State) ->
	{<< <<"This is ranged REST!">>/binary, 0:1021/unit:8 >>, Req, State};
get_text_plain(Req=#{qs := <<"sendfile">>}, State) ->
	Path = code:lib_dir(cowboy) ++ "/ebin/cowboy.app",
	Size = filelib:file_size(Path),
	{{sendfile, 0, Size, Path}, Req, State};
%% cowboy.app is too small for the four multipart chunks.
%% 249 + 4 * 256 still fits in this file.
get_text_plain(Req=#{qs := <<"sendfile-large">>}, State) ->
	Path = code:lib_dir(cowboy) ++ "/src/cowboy_rest.erl",
	Size = filelib:file_size(Path),
	{{sendfile, 0, Size, Path}, Req, State};
%% Declared length is shorter than the file so a range that ends
%% on that length can include the following byte.
get_text_plain(Req=#{qs := <<"sendfile5">>}, State) ->
	Path = code:lib_dir(cowboy) ++ "/ebin/cowboy.app",
	{{sendfile, 0, 5, Path}, Req, State}.
