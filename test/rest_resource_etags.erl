-module(rest_resource_etags).
-export([init/3, generate_etag/2, content_types_provided/2, get_text_plain/2]).

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_http_rest}.

generate_etag(Req, State) ->
	case cowboy_http_req:qs_val(<<"type">>, Req) of
		%% Correct return values from generate_etag/2.
		{<<"tuple-weak">>, Req2} ->
			{{weak, <<"etag-header-value">>}, Req2, State};
		{<<"tuple-strong">>, Req2} ->
			{{strong, <<"etag-header-value">>}, Req2, State};
		%% Backwards compatible return values from generate_etag/2.
		{<<"binary-weak-quoted">>, Req2} ->
			{<<"W/\"etag-header-value\"">>, Req2, State};
		{<<"binary-strong-quoted">>, Req2} ->
			{<<"\"etag-header-value\"">>, Req2, State};
		%% Invalid return values from generate_etag/2.
		{<<"binary-strong-unquoted">>, Req2} ->
			{<<"etag-header-value">>, Req2, State};
		{<<"binary-weak-unquoted">>, Req2} ->
			{<<"W/etag-header-value">>, Req2, State}
	end.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"plain">>, []}, get_text_plain}], Req, State}.

get_text_plain(Req, State) ->
	{<<"This is REST!">>, Req, State}.
