-module(rest_resource_etags).

-export([init/2]).
-export([generate_etag/2]).
-export([content_types_provided/2]).
-export([get_text_plain/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

generate_etag(Req, State) ->
	#{type := Type} = cowboy_req:match_qs([type], Req),
	case Type of
		%% Correct return values from generate_etag/2.
		<<"tuple-weak">> ->
			{{weak, <<"etag-header-value">>}, Req, State};
		<<"tuple-strong">> ->
			{{strong, <<"etag-header-value">>}, Req, State};
		%% Backwards compatible return values from generate_etag/2.
		<<"binary-weak-quoted">> ->
			{<<"W/\"etag-header-value\"">>, Req, State};
		<<"binary-strong-quoted">> ->
			{<<"\"etag-header-value\"">>, Req, State};
		%% Invalid return values from generate_etag/2.
		<<"binary-strong-unquoted">> ->
			ct_helper_error_h:ignore(cow_http_hd, parse_etag, 1),
			{<<"etag-header-value">>, Req, State};
		<<"binary-weak-unquoted">> ->
			ct_helper_error_h:ignore(cow_http_hd, parse_etag, 1),
			{<<"W/etag-header-value">>, Req, State}
	end.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.
