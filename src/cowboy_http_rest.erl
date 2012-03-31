%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
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

%% @doc Experimental REST protocol implementation.
%%
%% Based on the Webmachine Diagram from Alan Dean and Justin Sheehy, which
%% can be found in the Webmachine source tree, and on the Webmachine
%% documentation available at http://wiki.basho.com/Webmachine.html
%% at the time of writing.
-module(cowboy_http_rest).
-export([upgrade/4]).

-record(state, {
	%% Handler.
	handler :: atom(),
	handler_state :: any(),

	%% Media type.
	content_types_p = [] ::
		[{{binary(), binary(), [{binary(), binary()}]}, atom()}],
	content_type_a :: undefined
		| {{binary(), binary(), [{binary(), binary()}]}, atom()},

	%% Language.
	languages_p = [] :: [binary()],
	language_a :: undefined | binary(),

	%% Charset.
	charsets_p = [] :: [binary()],
	charset_a :: undefined | binary(),

	%% Cached resource calls.
	etag :: undefined | no_call | {strong | weak, binary()},
	last_modified :: undefined | no_call | calendar:datetime(),
	expires :: undefined | no_call | calendar:datetime()
}).

-include("http.hrl").

%% @doc Upgrade a HTTP request to the REST protocol.
%%
%% You do not need to call this function manually. To upgrade to the REST
%% protocol, you simply need to return <em>{upgrade, protocol, {@module}}</em>
%% in your <em>cowboy_http_handler:init/3</em> handler function.
-spec upgrade(pid(), module(), any(), #http_req{})
	-> {ok, #http_req{}} | close.
upgrade(_ListenerPid, Handler, Opts, Req) ->
	try
		case erlang:function_exported(Handler, rest_init, 2) of
			true ->
				case Handler:rest_init(Req, Opts) of
					{ok, Req2, HandlerState} ->
						service_available(Req2, #state{handler=Handler,
							handler_state=HandlerState})
				end;
			false ->
				service_available(Req, #state{handler=Handler})
		end
	catch Class:Reason ->
		PLReq = lists:zip(record_info(fields, http_req), tl(tuple_to_list(Req))),
		error_logger:error_msg(
			"** Handler ~p terminating in rest_init/3~n"
			"   for the reason ~p:~p~n** Options were ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts, PLReq, erlang:get_stacktrace()]),
		{ok, _Req2} = cowboy_http_req:reply(500, Req),
		close
	end.

service_available(Req, State) ->
	expect(Req, State, service_available, true, fun known_methods/2, 503).

%% known_methods/2 should return a list of atoms or binary methods.
known_methods(Req=#http_req{method=Method}, State) ->
	case call(Req, State, known_methods) of
		no_call when Method =:= 'HEAD'; Method =:= 'GET'; Method =:= 'POST';
					Method =:= 'PUT'; Method =:= 'DELETE'; Method =:= 'TRACE';
					Method =:= 'CONNECT'; Method =:= 'OPTIONS' ->
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

%% allowed_methods/2 should return a list of atoms or binary methods.
allowed_methods(Req=#http_req{method=Method}, State) ->
	case call(Req, State, allowed_methods) of
		no_call when Method =:= 'HEAD'; Method =:= 'GET' ->
			next(Req, State, fun malformed_request/2);
		no_call ->
			method_not_allowed(Req, State, ['GET', 'HEAD']);
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{List, Req2, HandlerState} ->
			State2 = State#state{handler_state=HandlerState},
			case lists:member(Method, List) of
				true -> next(Req2, State2, fun malformed_request/2);
				false -> method_not_allowed(Req2, State2, List)
			end
	end.

method_not_allowed(Req, State, Methods) ->
	{ok, Req2} = cowboy_http_req:set_resp_header(
		<<"Allow">>, method_not_allowed_build(Methods, []), Req),
	respond(Req2, State, 405).

method_not_allowed_build([], []) ->
	<<>>;
method_not_allowed_build([], [_Ignore|Acc]) ->
	lists:reverse(Acc);
method_not_allowed_build([Method|Tail], Acc) when is_atom(Method) ->
	Method2 = list_to_binary(atom_to_list(Method)),
	method_not_allowed_build(Tail, [<<", ">>, Method2|Acc]);
method_not_allowed_build([Method|Tail], Acc) ->
	method_not_allowed_build(Tail, [<<", ">>, Method|Acc]).

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
			{ok, Req3} = cowboy_http_req:set_resp_header(
				<<"Www-Authenticate">>, AuthHead, Req2),
			respond(Req3, State#state{handler_state=HandlerState}, 401)
	end.

forbidden(Req, State) ->
	expect(Req, State, forbidden, false, fun valid_content_headers/2, 403).

valid_content_headers(Req, State) ->
	expect(Req, State, valid_content_headers, true,
		fun known_content_type/2, 501).

known_content_type(Req, State) ->
	expect(Req, State, known_content_type, true,
		fun valid_entity_length/2, 413).

valid_entity_length(Req, State) ->
	expect(Req, State, valid_entity_length, true, fun options/2, 413).

%% If you need to add additional headers to the response at this point,
%% you should do it directly in the options/2 call using set_resp_headers.
options(Req=#http_req{method='OPTIONS'}, State) ->
	case call(Req, State, options) of
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
%% resources a little more readable, this is a lot less efficient. An example
%% of such a return value would be:
%%    {<<"text/html">>, to_html}
content_types_provided(Req=#http_req{meta=Meta}, State) ->
	case call(Req, State, content_types_provided) of
		no_call ->
			not_acceptable(Req, State);
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{[], Req2, HandlerState} ->
			not_acceptable(Req2, State#state{handler_state=HandlerState});
		{CTP, Req2, HandlerState} ->
		    CTP2 = [normalize_content_types_provided(P) || P <- CTP],
			State2 = State#state{
				handler_state=HandlerState, content_types_p=CTP2},
			{Accept, Req3} = cowboy_http_req:parse_header('Accept', Req2),
			case Accept of
				undefined ->
					{PMT, _Fun} = HeadCTP = hd(CTP2),
					languages_provided(
						Req3#http_req{meta=[{media_type, PMT}|Meta]},
						State2#state{content_type_a=HeadCTP});
				Accept ->
					Accept2 = prioritize_accept(Accept),
					choose_media_type(Req3, State2, Accept2)
			end
	end.

normalize_content_types_provided({ContentType, Handler})
		when is_binary(ContentType) ->
    {cowboy_http:content_type(ContentType), Handler};
normalize_content_types_provided(Provided) ->
	Provided.

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

match_media_type_params(Req=#http_req{meta=Meta}, State, Accept,
		[Provided = {PMT = {_TP, _STP, Params_P}, _Fun}|Tail],
		MediaType = {{_TA, _STA, Params_A}, _QA, _APA}) ->
	case lists:sort(Params_P) =:= lists:sort(Params_A) of
		true ->
			languages_provided(Req#http_req{meta=[{media_type, PMT}|Meta]},
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
			{AcceptLanguage, Req3} =
				cowboy_http_req:parse_header('Accept-Language', Req2),
			case AcceptLanguage of
				undefined ->
					set_language(Req3, State2#state{language_a=hd(LP)});
				AcceptLanguage ->
					AcceptLanguage2 = prioritize_languages(AcceptLanguage),
					choose_language(Req3, State2, AcceptLanguage2)
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

set_language(Req=#http_req{meta=Meta}, State=#state{language_a=Language}) ->
	{ok, Req2} = cowboy_http_req:set_resp_header(
		<<"Content-Language">>, Language, Req),
	charsets_provided(Req2#http_req{meta=[{language, Language}|Meta]}, State).

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
			{AcceptCharset, Req3} =
				cowboy_http_req:parse_header('Accept-Charset', Req2),
			case AcceptCharset of
				undefined ->
					set_content_type(Req3, State2#state{charset_a=hd(CP)});
				AcceptCharset ->
					AcceptCharset2 = prioritize_charsets(AcceptCharset),
					choose_charset(Req3, State2, AcceptCharset2)
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
		false -> [{<<"iso-8859-1">>, 1000}|AcceptCharsets2]
	end.

choose_charset(Req, State, []) ->
	not_acceptable(Req, State);
choose_charset(Req, State=#state{charsets_p=CP}, [Charset|Tail]) ->
	match_charset(Req, State, Tail, CP, Charset).

match_charset(Req, State, Accept, [], _Charset) ->
	choose_charset(Req, State, Accept);
match_charset(Req, State, _Accept, [Provided|_Tail],
		{Provided, _Quality}) ->
	set_content_type(Req, State#state{charset_a=Provided});
match_charset(Req, State, Accept, [_Provided|Tail], Charset) ->
	match_charset(Req, State, Accept, Tail, Charset).

set_content_type(Req=#http_req{meta=Meta}, State=#state{
		content_type_a={{Type, SubType, Params}, _Fun},
		charset_a=Charset}) ->
	ParamsBin = set_content_type_build_params(Params, []),
	ContentType = [Type, <<"/">>, SubType, ParamsBin],
	ContentType2 = case Charset of
		undefined -> ContentType;
		Charset -> [ContentType, <<"; charset=">>, Charset]
	end,
	{ok, Req2} = cowboy_http_req:set_resp_header(
		<<"Content-Type">>, ContentType2, Req),
	encodings_provided(Req2#http_req{meta=[{charset, Charset}|Meta]}, State).

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
		[_|_] -> [<<"Accept">>]
	end,
	Variances2 = case LP of
		[] -> Variances;
		[_] -> Variances;
		[_|_] -> [<<"Accept-Language">>|Variances]
	end,
	Variances3 = case CP of
		[] -> Variances2;
		[_] -> Variances2;
		[_|_] -> [<<"Accept-Charset">>|Variances2]
	end,
	{Variances4, Req3, State2} = case call(Req, State, variances) of
		no_call ->
			{Variances3, Req, State};
		{HandlerVariances, Req2, HandlerState} ->
			{Variances3 ++ HandlerVariances, Req2,
				State#state{handler_state=HandlerState}}
	end,
	case [[<<", ">>, V] || V <- Variances4] of
		[] ->
			resource_exists(Req3, State2);
		[[<<", ">>, H]|Variances5] ->
			{ok, Req4} = cowboy_http_req:set_resp_header(
				<<"Variances">>, [H|Variances5], Req3),
			resource_exists(Req4, State2)
	end.

resource_exists(Req, State) ->
	expect(Req, State, resource_exists, true,
		fun if_match_exists/2, fun if_match_musnt_exist/2).

if_match_exists(Req, State) ->
	case cowboy_http_req:parse_header('If-Match', Req) of
		{undefined, Req2} ->
			if_unmodified_since_exists(Req2, State);
		{'*', Req2} ->
			if_unmodified_since_exists(Req2, State);
		{ETagsList, Req2} ->
			if_match(Req2, State, ETagsList)
	end.

if_match(Req, State, EtagsList) ->
	{Etag, Req2, State2} = generate_etag(Req, State),
	case lists:member(Etag, EtagsList) of
		true -> if_unmodified_since_exists(Req2, State2);
		%% Etag may be `undefined' which cannot be a member.
		false -> precondition_failed(Req2, State2)
	end.

if_match_musnt_exist(Req, State) ->
	case cowboy_http_req:header('If-Match', Req) of
		{undefined, Req2} -> is_put_to_missing_resource(Req2, State);
		{_Any, Req2} -> precondition_failed(Req2, State)
	end.

if_unmodified_since_exists(Req, State) ->
	case cowboy_http_req:parse_header('If-Unmodified-Since', Req) of
		{undefined, Req2} ->
			if_none_match_exists(Req2, State);
		{{error, badarg}, Req2} ->
			if_none_match_exists(Req2, State);
		{IfUnmodifiedSince, Req2} ->
			if_unmodified_since(Req2, State, IfUnmodifiedSince)
	end.

%% If LastModified is the atom 'no_call', we continue.
if_unmodified_since(Req, State, IfUnmodifiedSince) ->
	{LastModified, Req2, State2} = last_modified(Req, State),
	case LastModified > IfUnmodifiedSince of
		true -> precondition_failed(Req2, State2);
		false -> if_none_match_exists(Req2, State2)
	end.

if_none_match_exists(Req, State) ->
	case cowboy_http_req:parse_header('If-None-Match', Req) of
		{undefined, Req2} ->
			if_modified_since_exists(Req2, State);
		{'*', Req2} ->
			precondition_is_head_get(Req2, State);
		{EtagsList, Req2} ->
			if_none_match(Req2, State, EtagsList)
	end.

if_none_match(Req, State, EtagsList) ->
	{Etag, Req2, State2} = generate_etag(Req, State),
	case Etag of
		undefined ->
			precondition_failed(Req2, State2);
		Etag ->
			case lists:member(Etag, EtagsList) of
				true -> precondition_is_head_get(Req2, State2);
				false -> if_modified_since_exists(Req2, State2)
			end
	end.

precondition_is_head_get(Req=#http_req{method=Method}, State)
		when Method =:= 'HEAD'; Method =:= 'GET' ->
	not_modified(Req, State);
precondition_is_head_get(Req, State) ->
	precondition_failed(Req, State).

if_modified_since_exists(Req, State) ->
	case cowboy_http_req:parse_header('If-Modified-Since', Req) of
		{undefined, Req2} ->
			method(Req2, State);
		{{error, badarg}, Req2} ->
			method(Req2, State);
		{IfModifiedSince, Req2} ->
			if_modified_since_now(Req2, State, IfModifiedSince)
	end.

if_modified_since_now(Req, State, IfModifiedSince) ->
	case IfModifiedSince > erlang:universaltime() of
		true -> method(Req, State);
		false -> if_modified_since(Req, State, IfModifiedSince)
	end.

if_modified_since(Req, State, IfModifiedSince) ->
	{LastModified, Req2, State2} = last_modified(Req, State),
	case LastModified of
		no_call ->
			method(Req2, State2);
		LastModified ->
			case LastModified > IfModifiedSince of
				true -> method(Req2, State2);
				false -> not_modified(Req2, State2)
			end
	end.

not_modified(Req=#http_req{resp_headers=RespHeaders}, State) ->
	RespHeaders2 = lists:keydelete(<<"Content-Type">>, 1, RespHeaders),
	Req2 = Req#http_req{resp_headers=RespHeaders2},
	{Req3, State2} = set_resp_etag(Req2, State),
	{Req4, State3} = set_resp_expires(Req3, State2),
	respond(Req4, State3, 304).

precondition_failed(Req, State) ->
	respond(Req, State, 412).

is_put_to_missing_resource(Req=#http_req{method='PUT'}, State) ->
	moved_permanently(Req, State, fun is_conflict/2);
is_put_to_missing_resource(Req, State) ->
	previously_existed(Req, State).

%% moved_permanently/2 should return either false or {true, Location}
%% with Location the full new URI of the resource.
moved_permanently(Req, State, OnFalse) ->
	case call(Req, State, moved_permanently) of
		{{true, Location}, Req2, HandlerState} ->
			{ok, Req3} = cowboy_http_req:set_resp_header(
				<<"Location">>, Location, Req2),
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
			{ok, Req3} = cowboy_http_req:set_resp_header(
				<<"Location">>, Location, Req2),
			respond(Req3, State#state{handler_state=HandlerState}, 307);
		{false, Req2, HandlerState} ->
			is_post_to_missing_resource(Req2, State#state{handler_state=HandlerState}, 410);
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		no_call ->
			is_post_to_missing_resource(Req, State, 410)
	end.

is_post_to_missing_resource(Req=#http_req{method='POST'}, State, OnFalse) ->
	allow_missing_post(Req, State, OnFalse);
is_post_to_missing_resource(Req, State, OnFalse) ->
	respond(Req, State, OnFalse).

allow_missing_post(Req, State, OnFalse) ->
	expect(Req, State, allow_missing_post, true, fun post_is_create/2, OnFalse).

method(Req=#http_req{method='DELETE'}, State) ->
	delete_resource(Req, State);
method(Req=#http_req{method='POST'}, State) ->
	post_is_create(Req, State);
method(Req=#http_req{method='PUT'}, State) ->
	is_conflict(Req, State);
method(Req, State) ->
	set_resp_body(Req, State).

%% delete_resource/2 should start deleting the resource and return.
delete_resource(Req, State) ->
	expect(Req, State, delete_resource, false, 500, fun delete_completed/2).

%% delete_completed/2 indicates whether the resource has been deleted yet.
delete_completed(Req, State) ->
	expect(Req, State, delete_completed, true, fun has_resp_body/2, 202).

%% post_is_create/2 indicates whether the POST method can create new resources.
post_is_create(Req, State) ->
	expect(Req, State, post_is_create, false, fun process_post/2, fun create_path/2).

%% When the POST method can create new resources, create_path/2 will be called
%% and is expected to return the full path to the new resource
%% (including the leading /).
create_path(Req=#http_req{meta=Meta}, State) ->
	case call(Req, State, create_path) of
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{Path, Req2, HandlerState} ->
			Location = create_path_location(Req2, Path),
			State2 = State#state{handler_state=HandlerState},
			{ok, Req3} = cowboy_http_req:set_resp_header(
				<<"Location">>, Location, Req2),
			put_resource(Req3#http_req{meta=[{put_path, Path}|Meta]},
				State2, 303)
	end.

create_path_location(#http_req{transport=Transport, raw_host=Host,
		port=Port}, Path) ->
	TransportName = Transport:name(),
	<< (create_path_location_protocol(TransportName))/binary, "://",
		Host/binary, (create_path_location_port(TransportName, Port))/binary,
		Path/binary >>.

create_path_location_protocol(ssl) -> <<"https">>;
create_path_location_protocol(_) -> <<"http">>.

create_path_location_port(ssl, 443) ->
	<<>>;
create_path_location_port(tcp, 80) ->
	<<>>;
create_path_location_port(_, Port) ->
	<<":", (list_to_binary(integer_to_list(Port)))/binary>>.

%% process_post should return true when the POST body could be processed
%% and false when it hasn't, in which case a 500 error is sent.
process_post(Req, State) ->
	case call(Req, State, process_post) of
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{true, Req2, HandlerState} ->
			State2 = State#state{handler_state=HandlerState},
			next(Req2, State2, fun is_new_resource/2);
		{false, Req2, HandlerState} ->
			State2 = State#state{handler_state=HandlerState},
			respond(Req2, State2, 500)
	end.

is_conflict(Req, State) ->
	expect(Req, State, is_conflict, false, fun put_resource/2, 409).

put_resource(Req=#http_req{raw_path=RawPath, meta=Meta}, State) ->
	Req2 = Req#http_req{meta=[{put_path, RawPath}|Meta]},
	put_resource(Req2, State, fun is_new_resource/2).

%% content_types_accepted should return a list of media types and their
%% associated callback functions in the same format as content_types_provided.
%%
%% The callback will then be called and is expected to process the content
%% pushed to the resource in the request body. The path to the new resource
%% may be different from the request path, and is stored as request metadata.
%% It is always defined past this point. It can be retrieved as demonstrated:
%%     {PutPath, Req2} = cowboy_http_req:meta(put_path, Req)
put_resource(Req, State, OnTrue) ->
	case call(Req, State, content_types_accepted) of
		no_call ->
			respond(Req, State, 415);
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{CTA, Req2, HandlerState} ->
			State2 = State#state{handler_state=HandlerState},
			{ContentType, Req3}
				= cowboy_http_req:parse_header('Content-Type', Req2),
			choose_content_type(Req3, State2, OnTrue, ContentType, CTA)
	end.

%% The special content type '*' will always match. It can be used as a
%% catch-all content type for accepting any kind of request content.
%% Note that because it will always match, it should be the last of the
%% list of content types, otherwise it'll shadow the ones following.
choose_content_type(Req, State, _OnTrue, _ContentType, []) ->
	respond(Req, State, 415);
choose_content_type(Req, State, OnTrue, ContentType, [{Accepted, Fun}|_Tail])
		when Accepted =:= '*' orelse Accepted =:= ContentType ->
	case call(Req, State, Fun) of
		{halt, Req2, HandlerState} ->
			terminate(Req2, State#state{handler_state=HandlerState});
		{true, Req2, HandlerState} ->
			State2 = State#state{handler_state=HandlerState},
			next(Req2, State2, OnTrue);
		{false, Req2, HandlerState} ->
			State2 = State#state{handler_state=HandlerState},
			respond(Req2, State2, 500)
	end;
choose_content_type(Req, State, OnTrue, ContentType, [_Any|Tail]) ->
	choose_content_type(Req, State, OnTrue, ContentType, Tail).

%% Whether we created a new resource, either through PUT or POST.
%% This is easily testable because we would have set the Location
%% header by this point if we did so.
is_new_resource(Req, State) ->
	case cowboy_http_req:has_resp_header(<<"Location">>, Req) of
		true -> respond(Req, State, 201);
		false -> has_resp_body(Req, State)
	end.

has_resp_body(Req, State) ->
	case cowboy_http_req:has_resp_body(Req) of
		true -> multiple_choices(Req, State);
		false -> respond(Req, State, 204)
	end.

%% Set the response headers and call the callback found using
%% content_types_provided/2 to obtain the request body and add
%% it to the response.
set_resp_body(Req=#http_req{method=Method},
		State=#state{content_type_a={_Type, Fun}})
		when Method =:= 'GET'; Method =:= 'HEAD' ->
	{Req2, State2} = set_resp_etag(Req, State),
	{LastModified, Req3, State3} = last_modified(Req2, State2),
	case LastModified of
		LastModified when is_atom(LastModified) ->
			Req4 = Req3;
		LastModified ->
			LastModifiedStr = httpd_util:rfc1123_date(LastModified),
			{ok, Req4} = cowboy_http_req:set_resp_header(
				<<"Last-Modified">>, LastModifiedStr, Req3)
	end,
	{Req5, State4} = set_resp_expires(Req4, State3),
	case call(Req5, State4, Fun) of
		{halt, Req6, HandlerState} ->
			terminate(Req6, State4#state{handler_state=HandlerState});
		{Body, Req6, HandlerState} ->
			State5 = State4#state{handler_state=HandlerState},
			{ok, Req7} = case Body of
				{stream, Len, Fun1} ->
					cowboy_http_req:set_resp_body_fun(Len, Fun1, Req6);
				_Contents ->
					cowboy_http_req:set_resp_body(Body, Req6)
			end,
			multiple_choices(Req7, State5)
	end;
set_resp_body(Req, State) ->
	multiple_choices(Req, State).

multiple_choices(Req, State) ->
	expect(Req, State, multiple_choices, false, 200, 300).

%% Response utility functions.

set_resp_etag(Req, State) ->
	{Etag, Req2, State2} = generate_etag(Req, State),
	case Etag of
		undefined ->
			{Req2, State2};
		Etag ->
			{ok, Req3} = cowboy_http_req:set_resp_header(
				<<"ETag">>, encode_etag(Etag), Req2),
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
		Expires ->
			ExpiresStr = httpd_util:rfc1123_date(Expires),
			{ok, Req3} = cowboy_http_req:set_resp_header(
				<<"Expires">>, ExpiresStr, Req2),
			{Req3, State2}
	end.

%% Info retrieval. No logic.

generate_etag(Req, State=#state{etag=no_call}) ->
	{undefined, Req, State};
generate_etag(Req, State=#state{etag=undefined}) ->
	case call(Req, State, generate_etag) of
		no_call ->
			{undefined, Req, State#state{etag=no_call}};
		%% Previously the return value from the generate_etag/2 callback was set
		%% as the value of the ETag header in the response. Therefore the only
		%% valid return type was `binary()'. If a handler returns a `binary()'
		%% it must be mapped to the expected type or it'll always fail to
		%% compare equal to any entity tags present in the request headers.
		%% @todo Remove support for binary return values after 0.6.
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
	case call(Req, State, last_modified) of
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
	case call(Req, State, expires) of
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

call(Req, #state{handler=Handler, handler_state=HandlerState}, Fun) ->
	case erlang:function_exported(Handler, Fun, 2) of
		true -> Handler:Fun(Req, HandlerState);
		false -> no_call
	end.

next(Req, State, Next) when is_function(Next) ->
	Next(Req, State);
next(Req, State, StatusCode) when is_integer(StatusCode) ->
	respond(Req, State, StatusCode).

%% @todo Allow some sort of callback for custom error pages.
respond(Req, State, StatusCode) ->
	{ok, Req2} = cowboy_http_req:reply(StatusCode, Req),
	terminate(Req2, State).

terminate(Req, #state{handler=Handler, handler_state=HandlerState}) ->
	case erlang:function_exported(Handler, rest_terminate, 2) of
		true -> ok = Handler:rest_terminate(
			Req#http_req{resp_state=locked}, HandlerState);
		false -> ok
	end,
	{ok, Req}.
