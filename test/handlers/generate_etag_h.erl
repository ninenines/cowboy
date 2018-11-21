%% This module sends a different etag value
%% depending on the query string.

-module(generate_etag_h).

-export([init/2]).
-export([content_types_provided/2]).
-export([get_text_plain/2]).
-export([generate_etag/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.

%% Correct return values from generate_etag/2.
generate_etag(Req=#{qs := <<"tuple-weak">>}, State) ->
	{{weak, <<"etag-header-value">>}, Req, State};
generate_etag(Req=#{qs := <<"tuple-strong">>}, State) ->
	{{strong, <<"etag-header-value">>}, Req, State};
%% Backwards compatible return values from generate_etag/2.
generate_etag(Req=#{qs := <<"binary-weak-quoted">>}, State) ->
	{<<"W/\"etag-header-value\"">>, Req, State};
generate_etag(Req=#{qs := <<"binary-strong-quoted">>}, State) ->
	{<<"\"etag-header-value\"">>, Req, State};
%% Invalid return values from generate_etag/2.
generate_etag(Req=#{qs := <<"binary-weak-unquoted">>}, State) ->
	ct_helper_error_h:ignore(cow_http_hd, parse_etag, 1),
	{<<"W/etag-header-value">>, Req, State};
generate_etag(Req=#{qs := <<"binary-strong-unquoted">>}, State) ->
	ct_helper_error_h:ignore(cow_http_hd, parse_etag, 1),
	{<<"etag-header-value">>, Req, State};
%% Simulate the callback being missing in other cases.
generate_etag(#{qs := <<"missing">>}, _) ->
	no_call.
