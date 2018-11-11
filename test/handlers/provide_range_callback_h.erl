%% This module defines many callbacks relevant to range requests
%% and return something different depending on query string.

-module(provide_range_callback_h).

-export([init/2]).
-export([content_types_provided/2]).
-export([ranges_provided/2]).
-export([expires/2]).
-export([generate_etag/2]).
-export([last_modified/2]).
-export([get_text_plain/2]).
-export([get_text_plain_bytes/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"plain">>, []}, get_text_plain},
		%% This one only exists so we generate a vary header.
		{{<<"text">>, <<"html">>, []}, get_text_html}
	], Req, State}.

ranges_provided(Req, State) ->
	{[{<<"bytes">>, get_text_plain_bytes}], Req, State}.

generate_etag(Req=#{qs := <<"weak-etag">>}, State) ->
	{{weak, <<"weak-no-match">>}, Req, State};
generate_etag(Req, State) ->
	{{strong, <<"strong-and-match">>}, Req, State}.

last_modified(Req, State) ->
	{{{2222, 2, 22}, {11, 11, 11}}, Req, State}.

expires(Req, State) ->
	{{{3333, 3, 3}, {11, 11, 11}}, Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.

%% Simulate the callback being missing, otherwise expect true/false.
get_text_plain_bytes(#{qs := <<"missing">>}, _) ->
	ct_helper_error_h:ignore(cowboy_rest, set_ranged_body_callback, 3),
	no_call;
get_text_plain_bytes(Req=#{qs := <<"sendfile">>, range := {_, [{From=0, infinity}]}}, State) ->
	Path = code:lib_dir(cowboy) ++ "/ebin/cowboy.app",
	Size = filelib:file_size(Path),
	{[{{From, Size - 1, Size}, {sendfile, From, Size, Path}}], Req, State};
get_text_plain_bytes(Req=#{range := {_, [{From=0, infinity}]}}, State) ->
	%% We send everything in one part.
	Body = <<"This is ranged REST!">>,
	Total = byte_size(Body),
	{[{{From, Total - 1, Total}, Body}], Req, State};
get_text_plain_bytes(Req=#{qs := <<"sendfile">>, range := {_, Range}}, State) ->
	%% We check the range header we get and send everything hardcoded.
	[
		{50, 99},
		{150, 199},
		{250, 299},
		-99
	] = Range,
	Path = code:lib_dir(cowboy) ++ "/ebin/cowboy.app",
	Size = filelib:file_size(Path),
	{[
		{{50, 99, Size}, {sendfile, 50, 50, Path}},
		{{150, 199, Size}, {sendfile, 150, 50, Path}},
		{{250, 299, Size}, {sendfile, 250, 50, Path}},
		{{Size - 99, Size - 1, Size}, {sendfile, Size - 99, 99, Path}}
	], Req, State};
get_text_plain_bytes(Req=#{range := {_, Range}}, State) ->
	%% We check the range header we get and send everything hardcoded.
	[
		{0, 3},
		{5, 6},
		{8, 13},
		{15, infinity}
	] = Range,
	Body = <<"This is ranged REST!">>,
	Total = byte_size(Body),
	{[
		{{0, 3, Total}, <<"This">>},
		{{5, 6, Total}, <<"is">>},
		{{8, 13, Total}, <<"ranged">>},
		{{15, 19, Total}, <<"REST!">>}
	], Req, State}.
