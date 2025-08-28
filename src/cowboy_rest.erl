%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% Originally based on the Webmachine Diagram from Alan Dean and
%% Justin Sheehy.
-module(cowboy_rest).
-behaviour(cowboy_sub_protocol).

-export([upgrade/4]).
-export([upgrade/5]).

-type switch_handler() :: {switch_handler, module()}
	| {switch_handler, module(), any()}.

%% Common handler callbacks.

-callback init(Req, any())
	-> {ok | module(), Req, any()}
	| {module(), Req, any(), any()}
	when Req::cowboy_req:req().

-callback terminate(any(), cowboy_req:req(), any()) -> ok.
-optional_callbacks([terminate/3]).

%% REST handler callbacks.

-callback allowed_methods(Req, State)
	-> {[binary()], Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([allowed_methods/2]).

-callback allow_missing_post(Req, State)
	-> {boolean(), Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([allow_missing_post/2]).

-callback charsets_provided(Req, State)
	-> {[binary()], Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([charsets_provided/2]).

-callback content_types_accepted(Req, State)
	-> {[{'*' | binary() | {binary(), binary(), '*' | [{binary(), binary()}]}, atom()}], Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([content_types_accepted/2]).

-callback content_types_provided(Req, State)
	-> {[{binary() | {binary(), binary(), '*' | [{binary(), binary()}]}, atom()}], Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([content_types_provided/2]).

-callback delete_completed(Req, State)
	-> {boolean(), Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([delete_completed/2]).

-callback delete_resource(Req, State)
	-> {boolean(), Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([delete_resource/2]).

-callback expires(Req, State)
	-> {calendar:datetime() | binary() | undefined, Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([expires/2]).

-callback forbidden(Req, State)
	-> {boolean(), Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([forbidden/2]).

-callback generate_etag(Req, State)
	-> {binary() | {weak | strong, binary()} | undefined, Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([generate_etag/2]).

-callback is_authorized(Req, State)
	-> {true | {false, iodata()}, Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([is_authorized/2]).

-callback is_conflict(Req, State)
	-> {boolean(), Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([is_conflict/2]).

-callback known_methods(Req, State)
	-> {[binary()], Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([known_methods/2]).

-callback languages_provided(Req, State)
	-> {[binary()], Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([languages_provided/2]).

-callback last_modified(Req, State)
	-> {calendar:datetime() | undefined, Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([last_modified/2]).

-callback malformed_request(Req, State)
	-> {boolean(), Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([malformed_request/2]).

-callback moved_permanently(Req, State)
	-> {{true, iodata()} | false, Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([moved_permanently/2]).

-callback moved_temporarily(Req, State)
	-> {{true, iodata()} | false, Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([moved_temporarily/2]).

-callback multiple_choices(Req, State)
	-> {boolean(), Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([multiple_choices/2]).

-callback options(Req, State)
	-> {ok, Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([options/2]).

-callback previously_existed(Req, State)
	-> {boolean(), Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([previously_existed/2]).

-callback range_satisfiable(Req, State)
	-> {boolean() | {false, non_neg_integer() | iodata()}, Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([range_satisfiable/2]).

-callback ranges_provided(Req, State)
	-> {[{binary(), atom()}], Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([ranges_provided/2]).

-callback rate_limited(Req, State)
	-> {{true, non_neg_integer() | calendar:datetime()} | false, Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([rate_limited/2]).

-callback resource_exists(Req, State)
	-> {boolean(), Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([resource_exists/2]).

-callback service_available(Req, State)
	-> {boolean(), Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([service_available/2]).

-callback uri_too_long(Req, State)
	-> {boolean(), Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([uri_too_long/2]).

-callback valid_content_headers(Req, State)
	-> {boolean(), Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([valid_content_headers/2]).

-callback valid_entity_length(Req, State)
	-> {boolean(), Req, State}
	| {stop, Req, State}
	| {switch_handler(), Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([valid_entity_length/2]).

-callback variances(Req, State)
	-> {[binary()], Req, State}
	when Req::cowboy_req:req(), State::any().
-optional_callbacks([variances/2]).

%% End of REST callbacks. Whew!

-record(state, {
	method = undefined :: binary(),

	%% Handler.
	handler :: atom(),
	handler_state :: any(),

	%% Media type.
	content_types_p = [] ::
		[{binary() | {binary(), binary(), [{binary(), binary()}] | '*'},
			atom()}],
	content_type_a :: undefined
		| {binary() | {binary(), binary(), [{binary(), binary()}] | '*'},
			atom()},

	%% Language.
	languages_p = [] :: [binary()],
	language_a :: undefined | binary(),

	%% Charset.
	charsets_p = undefined :: undefined | [binary()],
	charset_a :: undefined | binary(),

	%% Range units.
	ranges_a = [] :: [{binary(), atom()}],

	%% Whether the resource exists.
	exists = false :: boolean(),

	%% Cached resource calls.
	etag :: undefined | no_call | {strong | weak, binary()},
	last_modified :: undefined | no_call | calendar:datetime(),
	expires :: undefined | no_call | calendar:datetime() | binary()
}).

-spec upgrade(Req, Env, module(), any())
	-> {ok, Req, Env} when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade(Req0, Env, Handler, HandlerState0) ->
	Method = cowboy_req:method(Req0),
	case service_available(Req0, #state{method=Method,
			handler=Handler, handler_state=HandlerState0}) of
		{ok, Req, Result} ->
			{ok, Req, Env#{result => Result}};
		{Mod, Req, HandlerState} ->
			Mod:upgrade(Req, Env, Handler, HandlerState);
		{Mod, Req, HandlerState, Opts} ->
			Mod:upgrade(Req, Env, Handler, HandlerState, Opts)
	end.

-spec upgrade(Req, Env, module(), any(), any())
	-> {ok, Req, Env} when Req::cowboy_req:req(), Env::cowboy_middleware:env().
%% cowboy_rest takes no options.
upgrade(Req, Env, Handler, HandlerState, _Opts) ->
	upgrade(Req, Env, Handler, HandlerState).

service_available(Req, State) ->
	expect(Req, State, service_available, true, fun known_methods/2, 503).

%% known_methods/2 should return a list of binary methods.
known_methods(Req, State=#state{method=Method}) ->
	case call(Req, State, known_methods) of
		no_call when Method =:= <<"HEAD">>; Method =:= <<"GET">>;
				Method =:= <<"POST">>; Method =:= <<"PUT">>;
				Method =:= <<"PATCH">>; Method =:= <<"DELETE">>;
				Method =:= <<"OPTIONS">> ->
			uri_too_long(Req, State);
		no_call ->
			respond(Req, State, 501);
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{List, Req2, State2} ->
			case lists:member(Method, List) of
				true -> uri_too_long(Req2, State2);
				false -> respond(Req2, State2, 501)
			end
	end.

uri_too_long(Req, State) ->
	expect(Req, State, uri_too_long, false, fun allowed_methods/2, 414).

%% allowed_methods/2 should return a list of binary methods.
allowed_methods(Req, State=#state{method=Method}) ->
	case call(Req, State, allowed_methods) of
		no_call when Method =:= <<"HEAD">>; Method =:= <<"GET">>; Method =:= <<"OPTIONS">> ->
			Req2 = cowboy_req:set_resp_header(<<"allow">>, <<"HEAD, GET, OPTIONS">>, Req),
			malformed_request(Req2, State);
		no_call ->
			Req2 = cowboy_req:set_resp_header(<<"allow">>, <<"HEAD, GET, OPTIONS">>, Req),
			respond(Req2, State, 405);
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{List, Req2, State2} ->
			Req3 = cowboy_req:set_resp_header(<<"allow">>, cow_http_hd:allow(List), Req2),
			case lists:member(Method, List) of
				true ->
					malformed_request(Req3, State2);
				false ->
					respond(Req3, State2, 405)
			end
	end.

malformed_request(Req, State) ->
	expect(Req, State, malformed_request, false, fun is_authorized/2, 400).

%% is_authorized/2 should return true or {false, WwwAuthenticateHeader}.
is_authorized(Req, State) ->
	case call(Req, State, is_authorized) of
		no_call ->
			forbidden(Req, State);
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{true, Req2, State2} ->
			forbidden(Req2, State2);
		{{false, AuthHead}, Req2, State2} ->
			Req3 = cowboy_req:set_resp_header(
				<<"www-authenticate">>, AuthHead, Req2),
			respond(Req3, State2, 401)
	end.

forbidden(Req, State) ->
	expect(Req, State, forbidden, false, fun rate_limited/2, 403).

rate_limited(Req, State) ->
	case call(Req, State, rate_limited) of
		no_call ->
			valid_content_headers(Req, State);
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{false, Req2, State2} ->
			valid_content_headers(Req2, State2);
		{{true, RetryAfter0}, Req2, State2} ->
			RetryAfter = if
				is_integer(RetryAfter0), RetryAfter0 >= 0 ->
					integer_to_binary(RetryAfter0);
				is_tuple(RetryAfter0) ->
					cowboy_clock:rfc1123(RetryAfter0)
			end,
			Req3 = cowboy_req:set_resp_header(<<"retry-after">>, RetryAfter, Req2),
			respond(Req3, State2, 429)
	end.

valid_content_headers(Req, State) ->
	expect(Req, State, valid_content_headers, true,
		fun valid_entity_length/2, 501).

valid_entity_length(Req, State) ->
	expect(Req, State, valid_entity_length, true, fun options/2, 413).

%% If you need to add additional headers to the response at this point,
%% you should do it directly in the options/2 call using set_resp_headers.
options(Req, State=#state{method= <<"OPTIONS">>}) ->
	case call(Req, State, options) of
		no_call ->
			respond(Req, State, 200);
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{ok, Req2, State2} ->
			respond(Req2, State2, 200)
	end;
options(Req, State) ->
	content_types_provided(Req, State).

%% content_types_provided/2 should return a list of content types and their
%% associated callback function as a tuple: {{Type, SubType, Params}, Fun}.
%% Type and SubType are the media type as binary. Params is a list of
%% Key/Value tuple, with Key and Value a binary. Fun is the name of the
%% callback that will be used to return the content of the response. It is
%% given as an atom.
%%
%% An example of such return value would be:
%%    {{<<"text">>, <<"html">>, []}, to_html}
%%
%% Note that it is also possible to return a binary content type that will
%% then be parsed by Cowboy. However note that while this may make your
%% resources a little more readable, this is a lot less efficient.
%%
%% An example of such return value would be:
%%    {<<"text/html">>, to_html}
content_types_provided(Req, State) ->
	case call(Req, State, content_types_provided) of
		no_call ->
			State2 = State#state{
				content_types_p=[{{<<"text">>, <<"html">>, '*'}, to_html}]},
			try cowboy_req:parse_header(<<"accept">>, Req) of
				undefined ->
					languages_provided(
						Req#{media_type => {<<"text">>, <<"html">>, []}},
						State2#state{content_type_a={{<<"text">>, <<"html">>, []}, to_html}});
				Accept ->
					choose_media_type(Req, State2, prioritize_accept(Accept))
			catch _:_ ->
				respond(Req, State2, 400)
			end;
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{[], Req2, State2} ->
			not_acceptable(Req2, State2);
		{CTP, Req2, State2} ->
			CTP2 = [normalize_content_types(P, provide) || P <- CTP],
			State3 = State2#state{content_types_p=CTP2},
			try cowboy_req:parse_header(<<"accept">>, Req2) of
				undefined ->
					{PMT0, _Fun} = HeadCTP = hd(CTP2),
					%% We replace the wildcard by an empty list of parameters.
					PMT = case PMT0 of
						{Type, SubType, '*'} -> {Type, SubType, []};
						_ -> PMT0
					end,
					languages_provided(
						Req2#{media_type => PMT},
						State3#state{content_type_a=HeadCTP});
				Accept ->
					choose_media_type(Req2, State3, prioritize_accept(Accept))
			catch _:_ ->
				respond(Req2, State3, 400)
			end
	end.

normalize_content_types({ContentType, Callback}, _)
		when is_binary(ContentType) ->
	{cow_http_hd:parse_content_type(ContentType), Callback};
normalize_content_types(Normalized = {{Type, SubType, _}, _}, _)
		when is_binary(Type), is_binary(SubType) ->
	Normalized;
%% Wildcard for content_types_accepted.
normalize_content_types(Normalized = {'*', _}, accept) ->
	Normalized.

prioritize_accept(Accept) ->
	lists:sort(
		fun ({MediaTypeA, Quality, _AcceptParamsA},
			 {MediaTypeB, Quality, _AcceptParamsB}) ->
				%% Same quality, check precedence in more details.
				prioritize_mediatype(MediaTypeA, MediaTypeB);
			({_MediaTypeA, QualityA, _AcceptParamsA},
			 {_MediaTypeB, QualityB, _AcceptParamsB}) ->
				%% Just compare the quality.
				QualityA > QualityB
		end, Accept).

%% Media ranges can be overridden by more specific media ranges or
%% specific media types. If more than one media range applies to a given
%% type, the most specific reference has precedence.
%%
%% We always choose B over A when we can't decide between the two.
prioritize_mediatype({TypeA, SubTypeA, ParamsA}, {TypeB, SubTypeB, ParamsB}) ->
	case TypeB of
		TypeA ->
			case SubTypeB of
				SubTypeA -> length(ParamsA) > length(ParamsB);
				<<"*">> -> true;
				_Any -> false
			end;
		<<"*">> -> true;
		_Any -> false
	end.

%% Ignoring the rare AcceptParams. Not sure what should be done about them.
choose_media_type(Req, State, []) ->
	not_acceptable(Req, State);
choose_media_type(Req, State=#state{content_types_p=CTP},
		[MediaType|Tail]) ->
	match_media_type(Req, State, Tail, CTP, MediaType).

match_media_type(Req, State, Accept, [], _MediaType) ->
	choose_media_type(Req, State, Accept);
match_media_type(Req, State, Accept, CTP,
		MediaType = {{<<"*">>, <<"*">>, _Params_A}, _QA, _APA}) ->
	match_media_type_params(Req, State, Accept, CTP, MediaType);
match_media_type(Req, State, Accept,
			CTP = [{{Type, SubType_P, _PP}, _Fun}|_Tail],
			MediaType = {{Type, SubType_A, _PA}, _QA, _APA})
		when SubType_P =:= SubType_A; SubType_A =:= <<"*">> ->
	match_media_type_params(Req, State, Accept, CTP, MediaType);
match_media_type(Req, State, Accept, [_Any|Tail], MediaType) ->
	match_media_type(Req, State, Accept, Tail, MediaType).

match_media_type_params(Req, State, Accept,
		[Provided = {{TP, STP, '*'}, _Fun}|Tail],
		MediaType = {{TA, _STA, Params_A0}, _QA, _APA}) ->
	case lists:keytake(<<"charset">>, 1, Params_A0) of
		{value, {_, Charset}, Params_A} when TA =:= <<"text">> ->
			%% When we match against a wildcard, the media type is text
			%% and has a charset parameter, we call charsets_provided
			%% and check that the charset is provided. If the callback
			%% is not exported, we accept inconditionally but ignore
			%% the given charset so as to not send a wrong value back.
			case call(Req, State, charsets_provided) of
				no_call ->
					languages_provided(Req#{media_type => {TP, STP, Params_A0}},
						State#state{content_type_a=Provided});
				{stop, Req2, State2} ->
					terminate(Req2, State2);
				{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
					switch_handler(Switch, Req2, State2);
				{CP, Req2, State2} ->
					State3 = State2#state{charsets_p=CP},
					case lists:member(Charset, CP) of
						false ->
							match_media_type(Req2, State3, Accept, Tail, MediaType);
						true ->
							languages_provided(Req2#{media_type => {TP, STP, Params_A}},
								State3#state{content_type_a=Provided,
									charset_a=Charset})
					end
			end;
		_ ->
			languages_provided(Req#{media_type => {TP, STP, Params_A0}},
				State#state{content_type_a=Provided})
	end;
match_media_type_params(Req, State, Accept,
		[Provided = {PMT = {TP, STP, Params_P0}, Fun}|Tail],
		MediaType = {{_TA, _STA, Params_A}, _QA, _APA}) ->
	case lists:sort(Params_P0) =:= lists:sort(Params_A) of
		true when TP =:= <<"text">> ->
			%% When a charset was provided explicitly in both the charset header
			%% and the media types provided and the negotiation is successful,
			%% we keep the charset and don't call charsets_provided. This only
			%% applies to text media types, however.
			{Charset, Params_P} = case lists:keytake(<<"charset">>, 1, Params_P0) of
				false -> {undefined, Params_P0};
				{value, {_, Charset0}, Params_P1} -> {Charset0, Params_P1}
			end,
			languages_provided(Req#{media_type => {TP, STP, Params_P}},
				State#state{content_type_a={{TP, STP, Params_P}, Fun},
					charset_a=Charset});
		true ->
			languages_provided(Req#{media_type => PMT},
				State#state{content_type_a=Provided});
		false ->
			match_media_type(Req, State, Accept, Tail, MediaType)
	end.

%% languages_provided should return a list of binary values indicating
%% which languages are accepted by the resource.
%%
%% @todo I suppose we should also ask the resource if it wants to
%% set a language itself or if it wants it to be automatically chosen.
languages_provided(Req, State) ->
	case call(Req, State, languages_provided) of
		no_call ->
			charsets_provided(Req, State);
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{[], Req2, State2} ->
			not_acceptable(Req2, State2);
		{LP, Req2, State2} ->
			State3 = State2#state{languages_p=LP},
			case cowboy_req:parse_header(<<"accept-language">>, Req2) of
				undefined ->
					set_language(Req2, State3#state{language_a=hd(LP)});
				AcceptLanguage ->
					AcceptLanguage2 = prioritize_languages(AcceptLanguage),
					choose_language(Req2, State3, AcceptLanguage2)
			end
	end.

%% A language-range matches a language-tag if it exactly equals the tag,
%% or if it exactly equals a prefix of the tag such that the first tag
%% character following the prefix is "-". The special range "*", if
%% present in the Accept-Language field, matches every tag not matched
%% by any other range present in the Accept-Language field.
%%
%% @todo The last sentence probably means we should always put '*'
%% at the end of the list.
prioritize_languages(AcceptLanguages) ->
	lists:sort(
		fun ({_TagA, QualityA}, {_TagB, QualityB}) ->
			QualityA > QualityB
		end, AcceptLanguages).

choose_language(Req, State, []) ->
	not_acceptable(Req, State);
choose_language(Req, State=#state{languages_p=LP}, [Language|Tail]) ->
	match_language(Req, State, Tail, LP, Language).

match_language(Req, State, Accept, [], _Language) ->
	choose_language(Req, State, Accept);
match_language(Req, State, _Accept, [Provided|_Tail], {'*', _Quality}) ->
	set_language(Req, State#state{language_a=Provided});
match_language(Req, State, _Accept, [Provided|_Tail], {Provided, _Quality}) ->
	set_language(Req, State#state{language_a=Provided});
match_language(Req, State, Accept, [Provided|Tail],
		Language = {Tag, _Quality}) ->
	Length = byte_size(Tag),
	case Provided of
		<< Tag:Length/binary, $-, _Any/bits >> ->
			set_language(Req, State#state{language_a=Provided});
		_Any ->
			match_language(Req, State, Accept, Tail, Language)
	end.

set_language(Req, State=#state{language_a=Language}) ->
	Req2 = cowboy_req:set_resp_header(<<"content-language">>, Language, Req),
	charsets_provided(Req2#{language => Language}, State).

%% charsets_provided should return a list of binary values indicating
%% which charsets are accepted by the resource.
%%
%% A charset may have been selected while negotiating the accept header.
%% There's no need to select one again.
charsets_provided(Req, State=#state{charset_a=Charset})
		when Charset =/= undefined ->
	set_content_type(Req, State);
%% If charsets_p is defined, use it instead of calling charsets_provided
%% again. We also call this clause during normal execution to avoid
%% duplicating code.
charsets_provided(Req, State=#state{charsets_p=[]}) ->
	not_acceptable(Req, State);
charsets_provided(Req, State=#state{charsets_p=CP})
		when CP =/= undefined ->
	case cowboy_req:parse_header(<<"accept-charset">>, Req) of
		undefined ->
			set_content_type(Req, State#state{charset_a=hd(CP)});
		AcceptCharset0 ->
			AcceptCharset = prioritize_charsets(AcceptCharset0),
			choose_charset(Req, State, AcceptCharset)
	end;
charsets_provided(Req, State) ->
	case call(Req, State, charsets_provided) of
		no_call ->
			set_content_type(Req, State);
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{CP, Req2, State2} ->
			charsets_provided(Req2, State2#state{charsets_p=CP})
	end.

prioritize_charsets(AcceptCharsets) ->
	lists:sort(
		fun ({_CharsetA, QualityA}, {_CharsetB, QualityB}) ->
			QualityA > QualityB
		end, AcceptCharsets).

choose_charset(Req, State, []) ->
	not_acceptable(Req, State);
%% A q-value of 0 means not acceptable.
choose_charset(Req, State, [{_, 0}|Tail]) ->
	choose_charset(Req, State, Tail);
choose_charset(Req, State=#state{charsets_p=CP}, [Charset|Tail]) ->
	match_charset(Req, State, Tail, CP, Charset).

match_charset(Req, State, Accept, [], _Charset) ->
	choose_charset(Req, State, Accept);
match_charset(Req, State, _Accept, [Provided|_], {<<"*">>, _}) ->
	set_content_type(Req, State#state{charset_a=Provided});
match_charset(Req, State, _Accept, [Provided|_], {Provided, _}) ->
	set_content_type(Req, State#state{charset_a=Provided});
match_charset(Req, State, Accept, [_|Tail], Charset) ->
	match_charset(Req, State, Accept, Tail, Charset).

set_content_type(Req, State=#state{
		content_type_a={{Type, SubType, Params}, _Fun},
		charset_a=Charset}) ->
	ParamsBin = set_content_type_build_params(Params, []),
	ContentType = [Type, <<"/">>, SubType, ParamsBin],
	ContentType2 = case {Type, Charset} of
		{<<"text">>, Charset} when Charset =/= undefined ->
			[ContentType, <<"; charset=">>, Charset];
		_ ->
			ContentType
	end,
	Req2 = cowboy_req:set_resp_header(<<"content-type">>, ContentType2, Req),
	encodings_provided(Req2#{charset => Charset}, State).

set_content_type_build_params('*', []) ->
	<<>>;
set_content_type_build_params([], []) ->
	<<>>;
set_content_type_build_params([], Acc) ->
	lists:reverse(Acc);
set_content_type_build_params([{Attr, Value}|Tail], Acc) ->
	set_content_type_build_params(Tail, [[Attr, <<"=">>, Value], <<";">>|Acc]).

%% @todo Match for identity as we provide nothing else for now.
%% @todo Don't forget to set the Content-Encoding header when we reply a body
%% and the found encoding is something other than identity.
encodings_provided(Req, State) ->
	ranges_provided(Req, State).

not_acceptable(Req, State) ->
	respond(Req, State, 406).

ranges_provided(Req, State) ->
	case call(Req, State, ranges_provided) of
		no_call ->
			variances(Req, State);
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{[], Req2, State2} ->
			Req3 = cowboy_req:set_resp_header(<<"accept-ranges">>, <<"none">>, Req2),
			variances(Req3, State2#state{ranges_a=[]});
		{RP, Req2, State2} ->
			<<", ", AcceptRanges/binary>> = <<<<", ", R/binary>> || {R, _} <- RP>>,
			Req3 = cowboy_req:set_resp_header(<<"accept-ranges">>, AcceptRanges, Req2),
			variances(Req3, State2#state{ranges_a=RP})
	end.

%% variances/2 should return a list of headers that will be added
%% to the Vary response header. The Accept, Accept-Language,
%% Accept-Charset and Accept-Encoding headers do not need to be
%% specified.
%%
%% @todo Do Accept-Encoding too when we handle it.
%% @todo Does the order matter?
variances(Req, State=#state{content_types_p=CTP,
		languages_p=LP, charsets_p=CP}) ->
	Variances = case CTP of
		[] -> [];
		[_] -> [];
		[_|_] -> [<<"accept">>]
	end,
	Variances2 = case LP of
		[] -> Variances;
		[_] -> Variances;
		[_|_] -> [<<"accept-language">>|Variances]
	end,
	Variances3 = case CP of
		undefined -> Variances2;
		[] -> Variances2;
		[_] -> Variances2;
		[_|_] -> [<<"accept-charset">>|Variances2]
	end,
	try variances(Req, State, Variances3) of
		{Variances4, Req2, State2} ->
			case [[<<", ">>, V] || V <- Variances4] of
				[] ->
					resource_exists(Req2, State2);
				[[<<", ">>, H]|Variances5] ->
					Req3 = cowboy_req:set_resp_header(
						<<"vary">>, [H|Variances5], Req2),
					resource_exists(Req3, State2)
			end
	catch Class:Reason:Stacktrace ->
		error_terminate(Req, State, Class, Reason, Stacktrace)
	end.

variances(Req, State, Variances) ->
	case unsafe_call(Req, State, variances) of
		no_call ->
			{Variances, Req, State};
		{HandlerVariances, Req2, State2} ->
			{Variances ++ HandlerVariances, Req2, State2}
	end.

resource_exists(Req, State) ->
	expect(Req, State, resource_exists, true,
		fun if_match_exists/2, fun if_match_must_not_exist/2).

if_match_exists(Req, State) ->
	State2 = State#state{exists=true},
	case cowboy_req:parse_header(<<"if-match">>, Req) of
		undefined ->
			if_unmodified_since_exists(Req, State2);
		'*' ->
			if_unmodified_since_exists(Req, State2);
		ETagsList ->
			if_match(Req, State2, ETagsList)
	end.

if_match(Req, State, EtagsList) ->
	try generate_etag(Req, State) of
		%% Strong Etag comparison: weak Etag never matches.
		{{weak, _}, Req2, State2} ->
			precondition_failed(Req2, State2);
		{Etag, Req2, State2} ->
			case lists:member(Etag, EtagsList) of
				true -> if_none_match_exists(Req2, State2);
				%% Etag may be `undefined' which cannot be a member.
				false -> precondition_failed(Req2, State2)
			end
	catch Class:Reason:Stacktrace ->
		error_terminate(Req, State, Class, Reason, Stacktrace)
	end.

if_match_must_not_exist(Req, State) ->
	case cowboy_req:header(<<"if-match">>, Req) of
		undefined -> is_put_to_missing_resource(Req, State);
		_ -> precondition_failed(Req, State)
	end.

if_unmodified_since_exists(Req, State) ->
	try cowboy_req:parse_header(<<"if-unmodified-since">>, Req) of
		undefined ->
			if_none_match_exists(Req, State);
		IfUnmodifiedSince ->
			if_unmodified_since(Req, State, IfUnmodifiedSince)
	catch _:_ ->
		if_none_match_exists(Req, State)
	end.

%% If LastModified is the atom 'no_call', we continue.
if_unmodified_since(Req, State, IfUnmodifiedSince) ->
	try last_modified(Req, State) of
		{LastModified, Req2, State2} ->
			case LastModified > IfUnmodifiedSince of
				true -> precondition_failed(Req2, State2);
				false -> if_none_match_exists(Req2, State2)
			end
	catch Class:Reason:Stacktrace ->
		error_terminate(Req, State, Class, Reason, Stacktrace)
	end.

if_none_match_exists(Req, State) ->
	case cowboy_req:parse_header(<<"if-none-match">>, Req) of
		undefined ->
			if_modified_since_exists(Req, State);
		'*' ->
			precondition_is_head_get(Req, State);
		EtagsList ->
			if_none_match(Req, State, EtagsList)
	end.

if_none_match(Req, State, EtagsList) ->
	try generate_etag(Req, State) of
		{Etag, Req2, State2} ->
			case Etag of
				undefined ->
					precondition_failed(Req2, State2);
				Etag ->
					case is_weak_match(Etag, EtagsList) of
						true -> precondition_is_head_get(Req2, State2);
						false -> method(Req2, State2)
					end
			end
	catch Class:Reason:Stacktrace ->
		error_terminate(Req, State, Class, Reason, Stacktrace)
	end.

%% Weak Etag comparison: only check the opaque tag.
is_weak_match(_, []) ->
	false;
is_weak_match({_, Tag}, [{_, Tag}|_]) ->
	true;
is_weak_match(Etag, [_|Tail]) ->
	is_weak_match(Etag, Tail).

precondition_is_head_get(Req, State=#state{method=Method})
		when Method =:= <<"HEAD">>; Method =:= <<"GET">> ->
	not_modified(Req, State);
precondition_is_head_get(Req, State) ->
	precondition_failed(Req, State).

if_modified_since_exists(Req, State) ->
	try cowboy_req:parse_header(<<"if-modified-since">>, Req) of
		undefined ->
			method(Req, State);
		IfModifiedSince ->
			if_modified_since_now(Req, State, IfModifiedSince)
	catch _:_ ->
		method(Req, State)
	end.

if_modified_since_now(Req, State, IfModifiedSince) ->
	case IfModifiedSince > erlang:universaltime() of
		true -> method(Req, State);
		false -> if_modified_since(Req, State, IfModifiedSince)
	end.

if_modified_since(Req, State, IfModifiedSince) ->
	try last_modified(Req, State) of
		{undefined, Req2, State2} ->
			method(Req2, State2);
		{LastModified, Req2, State2} ->
			case LastModified > IfModifiedSince of
				true -> method(Req2, State2);
				false -> not_modified(Req2, State2)
			end
	catch Class:Reason:Stacktrace ->
		error_terminate(Req, State, Class, Reason, Stacktrace)
	end.

not_modified(Req, State) ->
	Req2 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
	try set_resp_etag(Req2, State) of
		{Req3, State2} ->
			try set_resp_expires(Req3, State2) of
				{Req4, State3} ->
					respond(Req4, State3, 304)
			catch Class:Reason:Stacktrace ->
				error_terminate(Req, State2, Class, Reason, Stacktrace)
			end
	catch Class:Reason:Stacktrace ->
		error_terminate(Req, State, Class, Reason, Stacktrace)
	end.

precondition_failed(Req, State) ->
	respond(Req, State, 412).

is_put_to_missing_resource(Req, State=#state{method= <<"PUT">>}) ->
	moved_permanently(Req, State, fun is_conflict/2);
is_put_to_missing_resource(Req, State) ->
	previously_existed(Req, State).

%% moved_permanently/2 should return either false or {true, Location}
%% with Location the full new URI of the resource.
moved_permanently(Req, State, OnFalse) ->
	case call(Req, State, moved_permanently) of
		{{true, Location}, Req2, State2} ->
			Req3 = cowboy_req:set_resp_header(
				<<"location">>, Location, Req2),
			respond(Req3, State2, 301);
		{false, Req2, State2} ->
			OnFalse(Req2, State2);
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		no_call ->
			OnFalse(Req, State)
	end.

previously_existed(Req, State) ->
	expect(Req, State, previously_existed, false,
		fun (R, S) -> is_post_to_missing_resource(R, S, 404) end,
		fun (R, S) -> moved_permanently(R, S, fun moved_temporarily/2) end).

%% moved_temporarily/2 should return either false or {true, Location}
%% with Location the full new URI of the resource.
moved_temporarily(Req, State) ->
	case call(Req, State, moved_temporarily) of
		{{true, Location}, Req2, State2} ->
			Req3 = cowboy_req:set_resp_header(
				<<"location">>, Location, Req2),
			respond(Req3, State2, 307);
		{false, Req2, State2} ->
			is_post_to_missing_resource(Req2, State2, 410);
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		no_call ->
			is_post_to_missing_resource(Req, State, 410)
	end.

is_post_to_missing_resource(Req, State=#state{method= <<"POST">>}, OnFalse) ->
	allow_missing_post(Req, State, OnFalse);
is_post_to_missing_resource(Req, State, OnFalse) ->
	respond(Req, State, OnFalse).

allow_missing_post(Req, State, OnFalse) ->
	expect(Req, State, allow_missing_post, true, fun accept_resource/2, OnFalse).

method(Req, State=#state{method= <<"DELETE">>}) ->
	delete_resource(Req, State);
method(Req, State=#state{method= <<"PUT">>}) ->
	is_conflict(Req, State);
method(Req, State=#state{method=Method})
		when Method =:= <<"POST">>; Method =:= <<"PATCH">> ->
	accept_resource(Req, State);
method(Req, State=#state{method=Method})
		when Method =:= <<"GET">>; Method =:= <<"HEAD">> ->
	set_resp_body_etag(Req, State);
method(Req, State) ->
	multiple_choices(Req, State).

%% delete_resource/2 should start deleting the resource and return.
delete_resource(Req, State) ->
	expect(Req, State, delete_resource, false, 500, fun delete_completed/2).

%% delete_completed/2 indicates whether the resource has been deleted yet.
delete_completed(Req, State) ->
	expect(Req, State, delete_completed, true, fun has_resp_body/2, 202).

is_conflict(Req, State) ->
	expect(Req, State, is_conflict, false, fun accept_resource/2, 409).

%% content_types_accepted should return a list of media types and their
%% associated callback functions in the same format as content_types_provided.
%%
%% The callback will then be called and is expected to process the content
%% pushed to the resource in the request body.
%%
%% content_types_accepted SHOULD return a different list
%% for each HTTP method.
accept_resource(Req, State) ->
	case call(Req, State, content_types_accepted) of
		no_call ->
			respond(Req, State, 415);
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{CTA, Req2, State2} ->
			CTA2 = [normalize_content_types(P, accept) || P <- CTA],
			try cowboy_req:parse_header(<<"content-type">>, Req2) of
				%% We do not match against the boundary parameter for multipart.
				{Type = <<"multipart">>, SubType, Params} ->
					ContentType = {Type, SubType, lists:keydelete(<<"boundary">>, 1, Params)},
					choose_content_type(Req2, State2, ContentType, CTA2);
				ContentType ->
					choose_content_type(Req2, State2, ContentType, CTA2)
			catch _:_ ->
				respond(Req2, State2, 415)
			end
	end.

%% The special content type '*' will always match. It can be used as a
%% catch-all content type for accepting any kind of request content.
%% Note that because it will always match, it should be the last of the
%% list of content types, otherwise it'll shadow the ones following.
choose_content_type(Req, State, _ContentType, []) ->
	respond(Req, State, 415);
choose_content_type(Req, State, ContentType, [{Accepted, Fun}|_Tail])
		when Accepted =:= '*'; Accepted =:= ContentType ->
	process_content_type(Req, State, Fun);
%% The special parameter '*' will always match any kind of content type
%% parameters.
%% Note that because it will always match, it should be the last of the
%% list for specific content type, otherwise it'll shadow the ones following.
choose_content_type(Req, State, {Type, SubType, Param},
		[{{Type, SubType, AcceptedParam}, Fun}|_Tail])
		when AcceptedParam =:= '*'; AcceptedParam =:= Param ->
	process_content_type(Req, State, Fun);
choose_content_type(Req, State, ContentType, [_Any|Tail]) ->
	choose_content_type(Req, State, ContentType, Tail).

process_content_type(Req, State=#state{method=Method, exists=Exists}, Fun) ->
	try case call(Req, State, Fun) of
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{true, Req2, State2} when Exists ->
			has_resp_body(Req2, State2);
		{true, Req2, State2} ->
			maybe_created(Req2, State2);
		{false, Req2, State2} ->
			respond(Req2, State2, 400);
		{{created, ResURL}, Req2, State2} when Method =:= <<"POST">> ->
			Req3 = cowboy_req:set_resp_header(
				<<"location">>, ResURL, Req2),
			respond(Req3, State2, 201);
		{{see_other, ResURL}, Req2, State2} when Method =:= <<"POST">> ->
			Req3 = cowboy_req:set_resp_header(
				<<"location">>, ResURL, Req2),
			respond(Req3, State2, 303);
		{{true, ResURL}, Req2, State2} when Method =:= <<"POST">> ->
			Req3 = cowboy_req:set_resp_header(
				<<"location">>, ResURL, Req2),
			if
				Exists -> respond(Req3, State2, 303);
				true -> respond(Req3, State2, 201)
			end
	end catch Class:Reason = {case_clause, no_call}:Stacktrace ->
		error_terminate(Req, State, Class, Reason, Stacktrace)
	end.

%% If PUT was used then the resource has been created at the current URL.
%% Otherwise, if a location header has been set then the resource has been
%% created at a new URL. If not, send a 200 or 204 as expected from a
%% POST or PATCH request.
maybe_created(Req, State=#state{method= <<"PUT">>}) ->
	respond(Req, State, 201);
maybe_created(Req, State) ->
	case cowboy_req:has_resp_header(<<"location">>, Req) of
		true -> respond(Req, State, 201);
		false -> has_resp_body(Req, State)
	end.

has_resp_body(Req, State) ->
	case cowboy_req:has_resp_body(Req) of
		true -> multiple_choices(Req, State);
		false -> respond(Req, State, 204)
	end.

%% Set the Etag header if any for the response provided.
set_resp_body_etag(Req, State) ->
	try set_resp_etag(Req, State) of
		{Req2, State2} ->
			set_resp_body_last_modified(Req2, State2)
	catch Class:Reason:Stacktrace ->
		error_terminate(Req, State, Class, Reason, Stacktrace)
	end.

%% Set the Last-Modified header if any for the response provided.
set_resp_body_last_modified(Req, State) ->
	try last_modified(Req, State) of
		{LastModified, Req2, State2} ->
			case LastModified of
				LastModified when is_atom(LastModified) ->
					set_resp_body_expires(Req2, State2);
				LastModified ->
					LastModifiedBin = cowboy_clock:rfc1123(LastModified),
					Req3 = cowboy_req:set_resp_header(
						<<"last-modified">>, LastModifiedBin, Req2),
					set_resp_body_expires(Req3, State2)
			end
	catch Class:Reason:Stacktrace ->
		error_terminate(Req, State, Class, Reason, Stacktrace)
	end.

%% Set the Expires header if any for the response provided.
set_resp_body_expires(Req, State) ->
	try set_resp_expires(Req, State) of
		{Req2, State2} ->
			if_range(Req2, State2)
	catch Class:Reason:Stacktrace ->
		error_terminate(Req, State, Class, Reason, Stacktrace)
	end.

%% When both the if-range and range headers are set, we perform
%% a strong comparison. If it fails, we send a full response.
if_range(Req=#{headers := #{<<"if-range">> := _, <<"range">> := _}},
		State=#state{etag=Etag}) ->
	try cowboy_req:parse_header(<<"if-range">>, Req) of
		%% Strong etag comparison is an exact match with the generate_etag result.
		Etag={strong, _} ->
			range(Req, State);
		%% We cannot do a strong date comparison because we have
		%% no way of knowing whether the representation changed
		%% twice during the second covered by the presented
		%% validator. (RFC7232 2.2.2)
		_ ->
			set_resp_body(Req, State)
	catch _:_ ->
		set_resp_body(Req, State)
	end;
if_range(Req, State) ->
	range(Req, State).

%% @todo This can probably be moved to if_range directly.
range(Req, State=#state{ranges_a=[]}) ->
	set_resp_body(Req, State);
range(Req, State) ->
	try cowboy_req:parse_header(<<"range">>, Req) of
		undefined ->
			set_resp_body(Req, State);
		%% @todo Maybe change parse_header to return <<"bytes">> in 3.0.
		{bytes, BytesRange} ->
			choose_range(Req, State, {<<"bytes">>, BytesRange});
		Range ->
			choose_range(Req, State, Range)
	catch _:_ ->
		%% We send a 416 response back when we can't parse the
		%% range header at all. I'm not sure this is the right
		%% way to go but at least this can help clients identify
		%% what went wrong when their range requests never work.
		range_not_satisfiable(Req, State, undefined)
	end.

choose_range(Req, State=#state{ranges_a=RangesAccepted}, Range={RangeUnit, _}) ->
	case lists:keyfind(RangeUnit, 1, RangesAccepted) of
		{_, Callback} ->
			%% We pass the selected range onward in the Req.
			range_satisfiable(Req#{range => Range}, State, Callback);
		false ->
			set_resp_body(Req, State)
	end.

range_satisfiable(Req, State, Callback) ->
	case call(Req, State, range_satisfiable) of
		no_call ->
			set_ranged_body(Req, State, Callback);
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{true, Req2, State2} ->
			set_ranged_body(Req2, State2, Callback);
		{false, Req2, State2} ->
			range_not_satisfiable(Req2, State2, undefined);
		{{false, Int}, Req2, State2} when is_integer(Int) ->
			range_not_satisfiable(Req2, State2, [<<"*/">>, integer_to_binary(Int)]);
		{{false, Iodata}, Req2, State2} when is_binary(Iodata); is_list(Iodata) ->
			range_not_satisfiable(Req2, State2, Iodata)
	end.

%% When the callback selected is 'auto' and the range unit
%% is bytes, we call the normal provide callback and split
%% the content automatically.
set_ranged_body(Req=#{range := {<<"bytes">>, _}}, State, auto) ->
	set_ranged_body_auto(Req, State);
set_ranged_body(Req, State, Callback) ->
	set_ranged_body_callback(Req, State, Callback).

set_ranged_body_auto(Req, State=#state{handler=Handler, content_type_a={_, Callback}}) ->
	try case call(Req, State, Callback) of
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{Body, Req2, State2} ->
			maybe_set_ranged_body_auto(Req2, State2, Body)
	end catch Class:{case_clause, no_call}:Stacktrace ->
		error_terminate(Req, State, Class, {error, {missing_callback, {Handler, Callback, 2}},
			'A callback specified in content_types_provided/2 is not exported.'},
			Stacktrace)
	end.

maybe_set_ranged_body_auto(Req=#{range := {_, Ranges}}, State, Body) ->
	Size = case Body of
		{sendfile, _, Bytes, _} -> Bytes;
		_ -> iolist_size(Body)
	end,
	Checks = [case Range of
		{From, infinity} -> From < Size;
		{From, To} -> (From < Size) andalso (From =< To) andalso (To =< Size);
		Neg -> (Neg =/= 0) andalso (-Neg < Size)
	end || Range <- Ranges],
	case lists:usort(Checks) of
		[true] -> set_ranged_body_auto(Req, State, Body);
		_ -> range_not_satisfiable(Req, State, [<<"*/">>, integer_to_binary(Size)])
	end.

%% We might also want to have some checks about range order,
%% number of ranges, and perhaps also join ranges that are
%% too close into one contiguous range. Some of these can
%% be done before calling the ProvideCallback.

set_ranged_body_auto(Req=#{range := {_, Ranges}}, State, Body) ->
	Parts = [ranged_partition(Range, Body) || Range <- Ranges],
	case Parts of
		[OnePart] -> set_one_ranged_body(Req, State, OnePart);
		_ when is_tuple(Body) -> send_multipart_ranged_body(Req, State, Parts);
		_ -> set_multipart_ranged_body(Req, State, Parts)
	end.

ranged_partition(Range, {sendfile, Offset0, Bytes0, Path}) ->
	{From, To, Offset, Bytes} = case Range of
		{From0, infinity} -> {From0, Bytes0 - 1, Offset0 + From0, Bytes0 - From0};
		{From0, To0} -> {From0, To0, Offset0 + From0, 1 + To0 - From0};
		Neg -> {Bytes0 + Neg, Bytes0 - 1, Offset0 + Bytes0 + Neg, -Neg}
	end,
	{{From, To, Bytes0}, {sendfile, Offset, Bytes, Path}};
ranged_partition(Range, Data0) ->
	Total = iolist_size(Data0),
	{From, To, Data} = case Range of
		{From0, infinity} ->
			{_, Data1} = cow_iolists:split(From0, Data0),
			{From0, Total - 1, Data1};
		{From0, To0} ->
			{_, Data1} = cow_iolists:split(From0, Data0),
			{Data2, _} = cow_iolists:split(To0 - From0 + 1, Data1),
			{From0, To0, Data2};
		Neg ->
			{_, Data1} = cow_iolists:split(Total + Neg, Data0),
			{Total + Neg, Total - 1, Data1}
	end,
	{{From, To, Total}, Data}.

-ifdef(TEST).
ranged_partition_test_() ->
	Tests = [
		%% Sendfile with open-ended range.
		{{0, infinity}, {sendfile, 0, 12, "t"}, {{0, 11, 12}, {sendfile, 0, 12, "t"}}},
		{{6, infinity}, {sendfile, 0, 12, "t"}, {{6, 11, 12}, {sendfile, 6, 6, "t"}}},
		{{11, infinity}, {sendfile, 0, 12, "t"}, {{11, 11, 12}, {sendfile, 11, 1, "t"}}},
		%% Sendfile with open-ended range. Sendfile tuple has an offset originally.
		{{0, infinity}, {sendfile, 3, 12, "t"}, {{0, 11, 12}, {sendfile, 3, 12, "t"}}},
		{{6, infinity}, {sendfile, 3, 12, "t"}, {{6, 11, 12}, {sendfile, 9, 6, "t"}}},
		{{11, infinity}, {sendfile, 3, 12, "t"}, {{11, 11, 12}, {sendfile, 14, 1, "t"}}},
		%% Sendfile with a specific range.
		{{0, 11}, {sendfile, 0, 12, "t"}, {{0, 11, 12}, {sendfile, 0, 12, "t"}}},
		{{6, 11}, {sendfile, 0, 12, "t"}, {{6, 11, 12}, {sendfile, 6, 6, "t"}}},
		{{11, 11}, {sendfile, 0, 12, "t"}, {{11, 11, 12}, {sendfile, 11, 1, "t"}}},
		{{1, 10}, {sendfile, 0, 12, "t"}, {{1, 10, 12}, {sendfile, 1, 10, "t"}}},
		%% Sendfile with a specific range. Sendfile tuple has an offset originally.
		{{0, 11}, {sendfile, 3, 12, "t"}, {{0, 11, 12}, {sendfile, 3, 12, "t"}}},
		{{6, 11}, {sendfile, 3, 12, "t"}, {{6, 11, 12}, {sendfile, 9, 6, "t"}}},
		{{11, 11}, {sendfile, 3, 12, "t"}, {{11, 11, 12}, {sendfile, 14, 1, "t"}}},
		{{1, 10}, {sendfile, 3, 12, "t"}, {{1, 10, 12}, {sendfile, 4, 10, "t"}}},
		%% Sendfile with negative range.
		{-12, {sendfile, 0, 12, "t"}, {{0, 11, 12}, {sendfile, 0, 12, "t"}}},
		{-6, {sendfile, 0, 12, "t"}, {{6, 11, 12}, {sendfile, 6, 6, "t"}}},
		{-1, {sendfile, 0, 12, "t"}, {{11, 11, 12}, {sendfile, 11, 1, "t"}}},
		%% Sendfile with negative range. Sendfile tuple has an offset originally.
		{-12, {sendfile, 3, 12, "t"}, {{0, 11, 12}, {sendfile, 3, 12, "t"}}},
		{-6, {sendfile, 3, 12, "t"}, {{6, 11, 12}, {sendfile, 9, 6, "t"}}},
		{-1, {sendfile, 3, 12, "t"}, {{11, 11, 12}, {sendfile, 14, 1, "t"}}},
		%% Iodata with open-ended range.
		{{0, infinity}, <<"Hello world!">>, {{0, 11, 12}, <<"Hello world!">>}},
		{{6, infinity}, <<"Hello world!">>, {{6, 11, 12}, <<"world!">>}},
		{{11, infinity}, <<"Hello world!">>, {{11, 11, 12}, <<"!">>}},
		%% Iodata with a specific range. The resulting data is
		%% wrapped in a list because of how cow_iolists:split/2 works.
		{{0, 11}, <<"Hello world!">>, {{0, 11, 12}, [<<"Hello world!">>]}},
		{{6, 11}, <<"Hello world!">>, {{6, 11, 12}, [<<"world!">>]}},
		{{11, 11}, <<"Hello world!">>, {{11, 11, 12}, [<<"!">>]}},
		{{1, 10}, <<"Hello world!">>, {{1, 10, 12}, [<<"ello world">>]}},
		%% Iodata with negative range.
		{-12, <<"Hello world!">>, {{0, 11, 12}, <<"Hello world!">>}},
		{-6, <<"Hello world!">>, {{6, 11, 12}, <<"world!">>}},
		{-1, <<"Hello world!">>, {{11, 11, 12}, <<"!">>}}
	],
	[{iolist_to_binary(io_lib:format("range ~p data ~p", [VR, VD])),
		fun() -> R = ranged_partition(VR, VD) end} || {VR, VD, R} <- Tests].
-endif.

set_ranged_body_callback(Req, State=#state{handler=Handler}, Callback) ->
	try case call(Req, State, Callback) of
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		%% When we receive a single range, we send it directly.
		{[OneRange], Req2, State2} ->
			set_one_ranged_body(Req2, State2, OneRange);
		%% When we receive multiple ranges we have to send them as multipart/byteranges.
		%% This also applies to non-bytes units. (RFC7233 A) If users don't want to use
		%% this for non-bytes units they can always return a single range with a binary
		%% content-range information.
		{Ranges, Req2, State2} when length(Ranges) > 1 ->
			%% We have to check whether there are sendfile tuples in the
			%% ranges to be sent. If there are we must use stream_reply.
			HasSendfile = [] =/= [true || {_, {sendfile, _, _, _}} <- Ranges],
			case HasSendfile of
				true -> send_multipart_ranged_body(Req2, State2, Ranges);
				false -> set_multipart_ranged_body(Req2, State2, Ranges)
			end
	end catch Class:{case_clause, no_call}:Stacktrace ->
		error_terminate(Req, State, Class, {error, {missing_callback, {Handler, Callback, 2}},
			'A callback specified in ranges_provided/2 is not exported.'},
			Stacktrace)
	end.

set_one_ranged_body(Req0, State, OneRange) ->
	{ContentRange, Body} = prepare_range(Req0, OneRange),
	Req1 = cowboy_req:set_resp_header(<<"content-range">>, ContentRange, Req0),
	Req = cowboy_req:set_resp_body(Body, Req1),
	respond(Req, State, 206).

set_multipart_ranged_body(Req, State, [FirstRange|MoreRanges]) ->
	Boundary = cow_multipart:boundary(),
	ContentType = cowboy_req:resp_header(<<"content-type">>, Req),
	{FirstContentRange, FirstPartBody} = prepare_range(Req, FirstRange),
	FirstPartHead = cow_multipart:first_part(Boundary, [
		{<<"content-type">>, ContentType},
		{<<"content-range">>, FirstContentRange}
	]),
	MoreParts = [begin
		{NextContentRange, NextPartBody} = prepare_range(Req, NextRange),
		NextPartHead = cow_multipart:part(Boundary, [
			{<<"content-type">>, ContentType},
			{<<"content-range">>, NextContentRange}
		]),
		[NextPartHead, NextPartBody]
	end || NextRange <- MoreRanges],
	Body = [FirstPartHead, FirstPartBody, MoreParts, cow_multipart:close(Boundary)],
	Req2 = cowboy_req:set_resp_header(<<"content-type">>,
		[<<"multipart/byteranges; boundary=">>, Boundary], Req),
	Req3 = cowboy_req:set_resp_body(Body, Req2),
	respond(Req3, State, 206).

%% Similar to set_multipart_ranged_body except we have to stream
%% the data because the parts contain sendfile tuples.
send_multipart_ranged_body(Req, State, [FirstRange|MoreRanges]) ->
	Boundary = cow_multipart:boundary(),
	ContentType = cowboy_req:resp_header(<<"content-type">>, Req),
	Req2 = cowboy_req:set_resp_header(<<"content-type">>,
		[<<"multipart/byteranges; boundary=">>, Boundary], Req),
	Req3 = cowboy_req:stream_reply(206, Req2),
	{FirstContentRange, FirstPartBody} = prepare_range(Req, FirstRange),
	FirstPartHead = cow_multipart:first_part(Boundary, [
		{<<"content-type">>, ContentType},
		{<<"content-range">>, FirstContentRange}
	]),
	cowboy_req:stream_body(FirstPartHead, nofin, Req3),
	cowboy_req:stream_body(FirstPartBody, nofin, Req3),
	_ = [begin
		{NextContentRange, NextPartBody} = prepare_range(Req, NextRange),
		NextPartHead = cow_multipart:part(Boundary, [
			{<<"content-type">>, ContentType},
			{<<"content-range">>, NextContentRange}
		]),
		cowboy_req:stream_body(NextPartHead, nofin, Req3),
		cowboy_req:stream_body(NextPartBody, nofin, Req3),
		[NextPartHead, NextPartBody]
	end || NextRange <- MoreRanges],
	cowboy_req:stream_body(cow_multipart:close(Boundary), fin, Req3),
	terminate(Req3, State).

prepare_range(#{range := {RangeUnit, _}}, {{From, To, Total0}, Body}) ->
	Total = case Total0 of
		'*' -> <<"*">>;
		_ -> integer_to_binary(Total0)
	end,
	ContentRange = [RangeUnit, $\s, integer_to_binary(From),
		$-, integer_to_binary(To), $/, Total],
	{ContentRange, Body};
prepare_range(#{range := {RangeUnit, _}}, {RangeData, Body}) ->
	{[RangeUnit, $\s, RangeData], Body}.

%% We send the content-range header when we can on error.
range_not_satisfiable(Req, State, undefined) ->
	respond(Req, State, 416);
range_not_satisfiable(Req0=#{range := {RangeUnit, _}}, State, RangeData) ->
	Req = cowboy_req:set_resp_header(<<"content-range">>,
		[RangeUnit, $\s, RangeData], Req0),
	respond(Req, State, 416).

%% Set the response headers and call the callback found using
%% content_types_provided/2 to obtain the request body and add
%% it to the response.
set_resp_body(Req, State=#state{handler=Handler, content_type_a={_, Callback}}) ->
	try case call(Req, State, Callback) of
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{Body, Req2, State2} ->
			Req3 = cowboy_req:set_resp_body(Body, Req2),
			multiple_choices(Req3, State2)
	end catch Class:{case_clause, no_call}:Stacktrace ->
		error_terminate(Req, State, Class, {error, {missing_callback, {Handler, Callback, 2}},
			'A callback specified in content_types_provided/2 is not exported.'},
			Stacktrace)
	end.

multiple_choices(Req, State) ->
	expect(Req, State, multiple_choices, false, 200, 300).

%% Response utility functions.

set_resp_etag(Req, State) ->
	{Etag, Req2, State2} = generate_etag(Req, State),
	case Etag of
		undefined ->
			{Req2, State2};
		Etag ->
			Req3 = cowboy_req:set_resp_header(
				<<"etag">>, encode_etag(Etag), Req2),
			{Req3, State2}
	end.

-spec encode_etag({strong | weak, binary()}) -> iolist().
encode_etag({strong, Etag}) -> [$",Etag,$"];
encode_etag({weak, Etag}) -> ["W/\"",Etag,$"].

set_resp_expires(Req, State) ->
	{Expires, Req2, State2} = expires(Req, State),
	case Expires of
		Expires when is_atom(Expires) ->
			{Req2, State2};
		Expires when is_binary(Expires) ->
			Req3 = cowboy_req:set_resp_header(
				<<"expires">>, Expires, Req2),
			{Req3, State2};
		Expires ->
			ExpiresBin = cowboy_clock:rfc1123(Expires),
			Req3 = cowboy_req:set_resp_header(
				<<"expires">>, ExpiresBin, Req2),
			{Req3, State2}
	end.

%% Info retrieval. No logic.

generate_etag(Req, State=#state{etag=no_call}) ->
	{undefined, Req, State};
generate_etag(Req, State=#state{etag=undefined}) ->
	case unsafe_call(Req, State, generate_etag) of
		no_call ->
			{undefined, Req, State#state{etag=no_call}};
		%% We allow the callback to return 'undefined'
		%% to allow conditionally generating etags. We
		%% handle 'undefined' the same as if the function
		%% was not exported.
		{undefined, Req2, State2} ->
			{undefined, Req2, State2#state{etag=no_call}};
		{Etag, Req2, State2} when is_binary(Etag) ->
			Etag2 = cow_http_hd:parse_etag(Etag),
			{Etag2, Req2, State2#state{etag=Etag2}};
		{Etag, Req2, State2} ->
			{Etag, Req2, State2#state{etag=Etag}}
	end;
generate_etag(Req, State=#state{etag=Etag}) ->
	{Etag, Req, State}.

last_modified(Req, State=#state{last_modified=no_call}) ->
	{undefined, Req, State};
last_modified(Req, State=#state{last_modified=undefined}) ->
	case unsafe_call(Req, State, last_modified) of
		no_call ->
			{undefined, Req, State#state{last_modified=no_call}};
		{LastModified, Req2, State2} ->
			{LastModified, Req2, State2#state{last_modified=LastModified}}
	end;
last_modified(Req, State=#state{last_modified=LastModified}) ->
	{LastModified, Req, State}.

expires(Req, State=#state{expires=no_call}) ->
	{undefined, Req, State};
expires(Req, State=#state{expires=undefined}) ->
	case unsafe_call(Req, State, expires) of
		no_call ->
			{undefined, Req, State#state{expires=no_call}};
		{Expires, Req2, State2} ->
			{Expires, Req2, State2#state{expires=Expires}}
	end;
expires(Req, State=#state{expires=Expires}) ->
	{Expires, Req, State}.

%% REST primitives.

expect(Req, State, Callback, Expected, OnTrue, OnFalse) ->
	case call(Req, State, Callback) of
		no_call ->
			next(Req, State, OnTrue);
		{stop, Req2, State2} ->
			terminate(Req2, State2);
		{Switch, Req2, State2} when element(1, Switch) =:= switch_handler ->
			switch_handler(Switch, Req2, State2);
		{Expected, Req2, State2} ->
			next(Req2, State2, OnTrue);
		{_Unexpected, Req2, State2} ->
			next(Req2, State2, OnFalse)
	end.

call(Req0, State=#state{handler=Handler,
		handler_state=HandlerState0}, Callback) ->
	case erlang:function_exported(Handler, Callback, 2) of
		true ->
			try Handler:Callback(Req0, HandlerState0) of
				no_call ->
					no_call;
				{Result, Req, HandlerState} ->
					{Result, Req, State#state{handler_state=HandlerState}}
			catch Class:Reason:Stacktrace ->
				error_terminate(Req0, State, Class, Reason, Stacktrace)
			end;
		false ->
			no_call
	end.

unsafe_call(Req0, State=#state{handler=Handler,
		handler_state=HandlerState0}, Callback) ->
	case erlang:function_exported(Handler, Callback, 2) of
		false ->
			no_call;
		true ->
			case Handler:Callback(Req0, HandlerState0) of
				no_call ->
					no_call;
				{Result, Req, HandlerState} ->
					{Result, Req, State#state{handler_state=HandlerState}}
			end
	end.

next(Req, State, Next) when is_function(Next) ->
	Next(Req, State);
next(Req, State, StatusCode) when is_integer(StatusCode) ->
	respond(Req, State, StatusCode).

respond(Req0, State, StatusCode) ->
	%% We remove the content-type header when there is no body,
	%% except when the status code is 200 because it might have
	%% been intended (for example sending an empty file).
	Req = case cowboy_req:has_resp_body(Req0) of
		true when StatusCode =:= 200 -> Req0;
		true -> Req0;
		false -> cowboy_req:delete_resp_header(<<"content-type">>, Req0)
	end,
	terminate(cowboy_req:reply(StatusCode, Req), State).

switch_handler({switch_handler, Mod}, Req, #state{handler_state=HandlerState}) ->
	{Mod, Req, HandlerState};
switch_handler({switch_handler, Mod, Opts}, Req, #state{handler_state=HandlerState}) ->
	{Mod, Req, HandlerState, Opts}.

-spec error_terminate(cowboy_req:req(), #state{}, atom(), any(), any()) -> no_return().
error_terminate(Req, #state{handler=Handler, handler_state=HandlerState}, Class, Reason, Stacktrace) ->
	cowboy_handler:terminate({crash, Class, Reason}, Req, HandlerState, Handler),
	erlang:raise(Class, Reason, Stacktrace).

terminate(Req, #state{handler=Handler, handler_state=HandlerState}) ->
	%% @todo I don't think the result is used anywhere?
	Result = cowboy_handler:terminate(normal, Req, HandlerState, Handler),
	{ok, Req, Result}.
