%% Copyright (c) 2011-2014, Loïc Hoguin <essen@ninenines.eu>
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

-export([upgrade/6]).

-callback init(Req, any())
	-> {ok | module(), Req, any()}
	| {module(), Req, any(), hibernate}
	| {module(), Req, any(), timeout()}
	| {module(), Req, any(), timeout(), hibernate}
	when Req::cowboy_req:req().
%% @todo optional REST callbacks
%% @todo optional -callback terminate(terminate_reason(), cowboy_req:req(), state()) -> ok.

-record(state, {
	env :: cowboy_middleware:env(),
	method = undefined :: binary(),

	%% Handler.
	handler :: atom(),
	handler_state :: any(),

	%% Allowed methods. Only used for OPTIONS requests.
	allowed_methods :: [binary()],

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
	charsets_p = [] :: [binary()],
	charset_a :: undefined | binary(),

	%% Whether the resource exists.
	exists = false :: boolean(),

	%% Cached resource calls.
	etag :: undefined | no_call | {strong | weak, binary()},
	last_modified :: undefined | no_call | calendar:datetime(),
	expires :: undefined | no_call | calendar:datetime() | binary()
}).

-spec upgrade(Req, Env, module(), any(), infinity, run)
	-> {ok, Req, Env} when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade(Req, Env, Handler, HandlerState, infinity, run) ->
	Method = cowboy_req:method(Req),
	service_available(Req, #state{env=Env, method=Method,
		handler=Handler, handler_state=HandlerState}).

service_available(Req, State) ->
	expect(Req, State, service_available, true, fun known_methods/2, 503).

%% known_methods/2 should return a list of binary methods.
known_methods(Req, State=#state{method=Method}) ->
	case call(Req, State, known_methods) of
		no_call when Method =:= <<"HEAD">>; Method =:= <<"GET">>;
				Method =:= <<"POST">>; Method =:= <<"PUT">>;
				Method =:= <<"PATCH">>; Method =:= <<"DELETE">>;
				Method =:= <<"OPTIONS">> ->
			next(Req, State, fun uri_too_long/2);
		no_call ->
			next(Req, State, 501);
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{List, Req2, HandlerState} ->
			State2 = State#state{handler_state=HandlerState},
			case lists:member(Method, List) of
				true -> next(Req2, State2, fun uri_too_long/2);
				false -> next(Req2, State2, 501)
			end
	end.

uri_too_long(Req, State) ->
	expect(Req, State, uri_too_long, false, fun allowed_methods/2, 414).

%% allowed_methods/2 should return a list of binary methods.
allowed_methods(Req, State=#state{method=Method}) ->
	case call(Req, State, allowed_methods) of
		no_call when Method =:= <<"HEAD">>; Method =:= <<"GET">> ->
			next(Req, State, fun malformed_request/2);
		no_call when Method =:= <<"OPTIONS">> ->
			next(Req, State#state{allowed_methods=
				[<<"HEAD">>, <<"GET">>, <<"OPTIONS">>]},
				fun malformed_request/2);
		no_call ->
			method_not_allowed(Req, State,
				[<<"HEAD">>, <<"GET">>, <<"OPTIONS">>]);
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{List, Req2, HandlerState} ->
			State2 = State#state{handler_state=HandlerState},
			case lists:member(Method, List) of
				true when Method =:= <<"OPTIONS">> ->
					next(Req2, State2#state{allowed_methods=List},
						fun malformed_request/2);
				true ->
					next(Req2, State2, fun malformed_request/2);
				false ->
					method_not_allowed(Req2, State2, List)
			end
	end.

method_not_allowed(Req, State, []) ->
	Req2 = cowboy_req:set_resp_header(<<"allow">>, <<>>, Req),
	respond(Req2, State, 405);
method_not_allowed(Req, State, Methods) ->
	<< ", ", Allow/binary >> = << << ", ", M/binary >> || M <- Methods >>,
	Req2 = cowboy_req:set_resp_header(<<"allow">>, Allow, Req),
	respond(Req2, State, 405).

malformed_request(Req, State) ->
	expect(Req, State, malformed_request, false, fun is_authorized/2, 400).

%% is_authorized/2 should return true or {false, WwwAuthenticateHeader}.
is_authorized(Req, State) ->
	case call(Req, State, is_authorized) of
		no_call ->
			forbidden(Req, State);
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{true, Req2, HandlerState} ->
			forbidden(Req2, State#state{handler_state=HandlerState});
		{{false, AuthHead}, Req2, HandlerState} ->
			Req3 = cowboy_req:set_resp_header(
				<<"www-authenticate">>, AuthHead, Req2),
			respond(Req3, State#state{handler_state=HandlerState}, 401)
	end.

forbidden(Req, State) ->
	expect(Req, State, forbidden, false, fun valid_content_headers/2, 403).

valid_content_headers(Req, State) ->
	expect(Req, State, valid_content_headers, true,
		fun valid_entity_length/2, 501).

valid_entity_length(Req, State) ->
	expect(Req, State, valid_entity_length, true, fun options/2, 413).

%% If you need to add additional headers to the response at this point,
%% you should do it directly in the options/2 call using set_resp_headers.
options(Req, State=#state{allowed_methods=Methods, method= <<"OPTIONS">>}) ->
	case call(Req, State, options) of
		no_call when Methods =:= [] ->
			Req2 = cowboy_req:set_resp_header(<<"allow">>, <<>>, Req),
			respond(Req2, State, 200);
		no_call ->
			<< ", ", Allow/binary >>
				= << << ", ", M/binary >> || M <- Methods >>,
			Req2 = cowboy_req:set_resp_header(<<"allow">>, Allow, Req),
			respond(Req2, State, 200);
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{ok, Req2, HandlerState} ->
			respond(Req2, State#state{handler_state=HandlerState}, 200)
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
						cowboy_req:set_meta(media_type, {<<"text">>, <<"html">>, []}, Req),
						State2#state{content_type_a={{<<"text">>, <<"html">>, []}, to_html}});
				Accept ->
					choose_media_type(Req, State2, prioritize_accept(Accept))
			catch _:_ ->
				respond(Req, State2, 400)
			end;
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{[], Req2, HandlerState} ->
			not_acceptable(Req2, State#state{handler_state=HandlerState});
		{CTP, Req2, HandlerState} ->
			CTP2 = [normalize_content_types(P) || P <- CTP],
			State2 = State#state{
				handler_state=HandlerState, content_types_p=CTP2},
			try cowboy_req:parse_header(<<"accept">>, Req2) of
				undefined ->
					{PMT, _Fun} = HeadCTP = hd(CTP2),
					languages_provided(
						cowboy_req:set_meta(media_type, PMT, Req2),
						State2#state{content_type_a=HeadCTP});
				Accept ->
					choose_media_type(Req2, State2, prioritize_accept(Accept))
			catch _:_ ->
				respond(Req2, State2, 400)
			end
	end.

normalize_content_types({ContentType, Callback})
		when is_binary(ContentType) ->
	{cowboy_http:content_type(ContentType), Callback};
normalize_content_types(Normalized) ->
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

match_media_type_params(Req, State, _Accept,
		[Provided = {{TP, STP, '*'}, _Fun}|_Tail],
		{{_TA, _STA, Params_A}, _QA, _APA}) ->
	PMT = {TP, STP, Params_A},
	languages_provided(cowboy_req:set_meta(media_type, PMT, Req),
		State#state{content_type_a=Provided});
match_media_type_params(Req, State, Accept,
		[Provided = {PMT = {_TP, _STP, Params_P}, _Fun}|Tail],
		MediaType = {{_TA, _STA, Params_A}, _QA, _APA}) ->
	case lists:sort(Params_P) =:= lists:sort(Params_A) of
		true ->
			languages_provided(cowboy_req:set_meta(media_type, PMT, Req),
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
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{[], Req2, HandlerState} ->
			not_acceptable(Req2, State#state{handler_state=HandlerState});
		{LP, Req2, HandlerState} ->
			State2 = State#state{handler_state=HandlerState, languages_p=LP},
			case cowboy_req:parse_header(<<"accept-language">>, Req2) of
				undefined ->
					set_language(Req2, State2#state{language_a=hd(LP)});
				AcceptLanguage ->
					AcceptLanguage2 = prioritize_languages(AcceptLanguage),
					choose_language(Req2, State2, AcceptLanguage2)
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
	charsets_provided(cowboy_req:set_meta(language, Language, Req2), State).

%% charsets_provided should return a list of binary values indicating
%% which charsets are accepted by the resource.
charsets_provided(Req, State) ->
	case call(Req, State, charsets_provided) of
		no_call ->
			set_content_type(Req, State);
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{[], Req2, HandlerState} ->
			not_acceptable(Req2, State#state{handler_state=HandlerState});
		{CP, Req2, HandlerState} ->
			State2 = State#state{handler_state=HandlerState, charsets_p=CP},
			case cowboy_req:parse_header(<<"accept-charset">>, Req2) of
				undefined ->
					set_content_type(Req2, State2#state{charset_a=hd(CP)});
				AcceptCharset ->
					AcceptCharset2 = prioritize_charsets(AcceptCharset),
					choose_charset(Req2, State2, AcceptCharset2)
			end
	end.

%% The special value "*", if present in the Accept-Charset field,
%% matches every character set (including ISO-8859-1) which is not
%% mentioned elsewhere in the Accept-Charset field. If no "*" is present
%% in an Accept-Charset field, then all character sets not explicitly
%% mentioned get a quality value of 0, except for ISO-8859-1, which gets
%% a quality value of 1 if not explicitly mentioned.
prioritize_charsets(AcceptCharsets) ->
	AcceptCharsets2 = lists:sort(
		fun ({_CharsetA, QualityA}, {_CharsetB, QualityB}) ->
			QualityA > QualityB
		end, AcceptCharsets),
	case lists:keymember(<<"*">>, 1, AcceptCharsets2) of
		true -> AcceptCharsets2;
		false ->
			case lists:keymember(<<"iso-8859-1">>, 1, AcceptCharsets2) of
				true -> AcceptCharsets2;
				false -> [{<<"iso-8859-1">>, 1000}|AcceptCharsets2]
			end
	end.

choose_charset(Req, State, []) ->
	not_acceptable(Req, State);
choose_charset(Req, State=#state{charsets_p=CP}, [Charset|Tail]) ->
	match_charset(Req, State, Tail, CP, Charset).

match_charset(Req, State, Accept, [], _Charset) ->
	choose_charset(Req, State, Accept);
match_charset(Req, State, _Accept, [Provided|_], {Provided, _}) ->
	set_content_type(Req, State#state{charset_a=Provided});
match_charset(Req, State, Accept, [_|Tail], Charset) ->
	match_charset(Req, State, Accept, Tail, Charset).

set_content_type(Req, State=#state{
		content_type_a={{Type, SubType, Params}, _Fun},
		charset_a=Charset}) ->
	ParamsBin = set_content_type_build_params(Params, []),
	ContentType = [Type, <<"/">>, SubType, ParamsBin],
	ContentType2 = case Charset of
		undefined -> ContentType;
		Charset -> [ContentType, <<"; charset=">>, Charset]
	end,
	Req2 = cowboy_req:set_resp_header(<<"content-type">>, ContentType2, Req),
	encodings_provided(cowboy_req:set_meta(charset, Charset, Req2), State).

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
	variances(Req, State).

not_acceptable(Req, State) ->
	respond(Req, State, 406).

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
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason, variances)
	end.

variances(Req, State, Variances) ->
	case unsafe_call(Req, State, variances) of
		no_call ->
			{Variances, Req, State};
		{HandlerVariances, Req2, HandlerState} ->
			{Variances ++ HandlerVariances, Req2,
				State#state{handler_state=HandlerState}}
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
		{Etag, Req2, State2} ->
			case lists:member(Etag, EtagsList) of
				true -> if_unmodified_since_exists(Req2, State2);
				%% Etag may be `undefined' which cannot be a member.
				false -> precondition_failed(Req2, State2)
			end
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason, generate_etag)
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
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason, last_modified)
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
					case lists:member(Etag, EtagsList) of
						true -> precondition_is_head_get(Req2, State2);
						false -> if_modified_since_exists(Req2, State2)
					end
			end
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason, generate_etag)
	end.

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
		{no_call, Req2, State2} ->
			method(Req2, State2);
		{LastModified, Req2, State2} ->
			case LastModified > IfModifiedSince of
				true -> method(Req2, State2);
				false -> not_modified(Req2, State2)
			end
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason, last_modified)
	end.

not_modified(Req, State) ->
	Req2 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
	try set_resp_etag(Req2, State) of
		{Req3, State2} ->
			try set_resp_expires(Req3, State2) of
				{Req4, State3} ->
					respond(Req4, State3, 304)
			catch Class:Reason ->
				error_terminate(Req, State, Class, Reason, expires)
			end
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason, generate_etag)
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
		{{true, Location}, Req2, HandlerState} ->
			Req3 = cowboy_req:set_resp_header(
				<<"location">>, Location, Req2),
			respond(Req3, State#state{handler_state=HandlerState}, 301);
		{false, Req2, HandlerState} ->
			OnFalse(Req2, State#state{handler_state=HandlerState});
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
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
		{{true, Location}, Req2, HandlerState} ->
			Req3 = cowboy_req:set_resp_header(
				<<"location">>, Location, Req2),
			respond(Req3, State#state{handler_state=HandlerState}, 307);
		{false, Req2, HandlerState} ->
			is_post_to_missing_resource(Req2, State#state{handler_state=HandlerState}, 410);
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
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
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{CTA, Req2, HandlerState} ->
			CTA2 = [normalize_content_types(P) || P <- CTA],
			State2 = State#state{handler_state=HandlerState},
			try cowboy_req:parse_header(<<"content-type">>, Req2) of
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
		{halt, Req2, HandlerState2} ->
			terminate(Req2, State#state{handler_state=HandlerState2});
		{true, Req2, HandlerState2} when Exists ->
			State2 = State#state{handler_state=HandlerState2},
			next(Req2, State2, fun has_resp_body/2);
		{true, Req2, HandlerState2} ->
			State2 = State#state{handler_state=HandlerState2},
			next(Req2, State2, fun maybe_created/2);
		{false, Req2, HandlerState2} ->
			State2 = State#state{handler_state=HandlerState2},
			respond(Req2, State2, 400);
		{{true, ResURL}, Req2, HandlerState2} when Method =:= <<"POST">> ->
			State2 = State#state{handler_state=HandlerState2},
			Req3 = cowboy_req:set_resp_header(
				<<"location">>, ResURL, Req2),
			if
				Exists -> respond(Req3, State2, 303);
				true -> respond(Req3, State2, 201)
			end
	end catch Class:Reason = {case_clause, no_call} ->
		error_terminate(Req, State, Class, Reason, Fun)
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
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason, generate_etag)
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
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason, last_modified)
	end.

%% Set the Expires header if any for the response provided.
set_resp_body_expires(Req, State) ->
	try set_resp_expires(Req, State) of
		{Req2, State2} ->
			set_resp_body(Req2, State2)
	catch Class:Reason ->
		error_terminate(Req, State, Class, Reason, expires)
	end.

%% Set the response headers and call the callback found using
%% content_types_provided/2 to obtain the request body and add
%% it to the response.
set_resp_body(Req, State=#state{content_type_a={_, Callback}}) ->
	try case call(Req, State, Callback) of
		{halt, Req2, HandlerState2} ->
			terminate(Req2, State#state{handler_state=HandlerState2});
		{Body, Req2, HandlerState2} ->
			State2 = State#state{handler_state=HandlerState2},
			Req3 = case Body of
				{stream, StreamFun} ->
					cowboy_req:set_resp_body_fun(StreamFun, Req2);
				{stream, Len, StreamFun} ->
					cowboy_req:set_resp_body_fun(Len, StreamFun, Req2);
				{chunked, StreamFun} ->
					cowboy_req:set_resp_body_fun(chunked, StreamFun, Req2);
				_Contents ->
					cowboy_req:set_resp_body(Body, Req2)
			end,
			multiple_choices(Req3, State2)
	end catch Class:Reason = {case_clause, no_call} ->
		error_terminate(Req, State, Class, Reason, Callback)
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
		{Etag, Req2, HandlerState} when is_binary(Etag) ->
			[Etag2] = cowboy_http:entity_tag_match(Etag),
			{Etag2, Req2, State#state{handler_state=HandlerState, etag=Etag2}};
		{Etag, Req2, HandlerState} ->
			{Etag, Req2, State#state{handler_state=HandlerState, etag=Etag}}
	end;
generate_etag(Req, State=#state{etag=Etag}) ->
	{Etag, Req, State}.

last_modified(Req, State=#state{last_modified=no_call}) ->
	{undefined, Req, State};
last_modified(Req, State=#state{last_modified=undefined}) ->
	case unsafe_call(Req, State, last_modified) of
		no_call ->
			{undefined, Req, State#state{last_modified=no_call}};
		{LastModified, Req2, HandlerState} ->
			{LastModified, Req2, State#state{handler_state=HandlerState,
				last_modified=LastModified}}
	end;
last_modified(Req, State=#state{last_modified=LastModified}) ->
	{LastModified, Req, State}.

expires(Req, State=#state{expires=no_call}) ->
	{undefined, Req, State};
expires(Req, State=#state{expires=undefined}) ->
	case unsafe_call(Req, State, expires) of
		no_call ->
			{undefined, Req, State#state{expires=no_call}};
		{Expires, Req2, HandlerState} ->
			{Expires, Req2, State#state{handler_state=HandlerState,
				expires=Expires}}
	end;
expires(Req, State=#state{expires=Expires}) ->
	{Expires, Req, State}.

%% REST primitives.

expect(Req, State, Callback, Expected, OnTrue, OnFalse) ->
	case call(Req, State, Callback) of
		no_call ->
			next(Req, State, OnTrue);
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{Expected, Req2, HandlerState} ->
			next(Req2, State#state{handler_state=HandlerState}, OnTrue);
		{_Unexpected, Req2, HandlerState} ->
			next(Req2, State#state{handler_state=HandlerState}, OnFalse)
	end.

call(Req, State=#state{handler=Handler, handler_state=HandlerState},
		Callback) ->
	case erlang:function_exported(Handler, Callback, 2) of
		true ->
			try
				Handler:Callback(Req, HandlerState)
			catch Class:Reason ->
				error_terminate(Req, State, Class, Reason, Callback)
			end;
		false ->
			no_call
	end.

unsafe_call(Req, #state{handler=Handler, handler_state=HandlerState},
		Callback) ->
	case erlang:function_exported(Handler, Callback, 2) of
		true -> Handler:Callback(Req, HandlerState);
		false -> no_call
	end.

next(Req, State, Next) when is_function(Next) ->
	Next(Req, State);
next(Req, State, StatusCode) when is_integer(StatusCode) ->
	respond(Req, State, StatusCode).

respond(Req, State, StatusCode) ->
	terminate(cowboy_req:reply(StatusCode, Req), State).

-spec error_terminate(cowboy_req:req(), #state{}, exit | error | throw, any(),
		atom())
	-> no_return().
error_terminate(Req, #state{handler=Handler, handler_state=HandlerState},
		Class, Reason, Callback) ->
	Stacktrace = erlang:get_stacktrace(),
	cowboy_req:maybe_reply(Stacktrace, Req),
	cowboy_handler:terminate({crash, Class, Reason}, Req, HandlerState, Handler),
	exit([
		{class, Class},
		{reason, Reason},
		{mfa, {Handler, Callback, 2}},
		{stacktrace, Stacktrace},
		{req, cowboy_req:to_list(Req)},
		{state, HandlerState}
	]).

terminate(Req, #state{env=Env, handler=Handler, handler_state=HandlerState}) ->
	Result = cowboy_handler:terminate(normal, Req, HandlerState, Handler),
	{ok, Req, [{result, Result}|Env]}.
