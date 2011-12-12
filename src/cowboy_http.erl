%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
%% Copyright (c) 2011, Anthony Ramine <nox@dev-extend.eu>
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

%% @doc Core HTTP parsing API.
-module(cowboy_http).

%% Parsing.
-export([list/2, nonempty_list/2, content_type/1,
	media_range/2, conneg/2, language_range/2, entity_tag_match/1,
	http_date/1, rfc1123_date/1, rfc850_date/1, asctime_date/1,
	digits/1, token/2, token_ci/2, quoted_string/2]).

%% Interpretation.
-export([connection_to_atom/1, urldecode/1, urldecode/2, urlencode/1,
	urlencode/2]).

-include("include/http.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Parsing.

%% @doc Parse a non-empty list of the given type.
-spec nonempty_list(binary(), fun()) -> [any(), ...] | {error, badarg}.
nonempty_list(Data, Fun) ->
	case list(Data, Fun, []) of
		{error, badarg} -> {error, badarg};
		[] -> {error, badarg};
		L -> lists:reverse(L)
	end.

%% @doc Parse a list of the given type.
-spec list(binary(), fun()) -> list() | {error, badarg}.
list(Data, Fun) ->
	case list(Data, Fun, []) of
		{error, badarg} -> {error, badarg};
		L -> lists:reverse(L)
	end.

-spec list(binary(), fun(), [binary()]) -> [any()] | {error, badarg}.
%% From the RFC:
%% <blockquote>Wherever this construct is used, null elements are allowed,
%% but do not contribute to the count of elements present.
%% That is, "(element), , (element) " is permitted, but counts
%% as only two elements. Therefore, where at least one element is required,
%% at least one non-null element MUST be present.</blockquote>
list(Data, Fun, Acc) ->
	whitespace(Data,
		fun (<<>>) -> Acc;
			(<< $,, Rest/binary >>) -> list(Rest, Fun, Acc);
			(Rest) -> Fun(Rest,
				fun (D, I) -> whitespace(D,
						fun (<<>>) -> [I|Acc];
							(<< $,, R/binary >>) -> list(R, Fun, [I|Acc]);
							(_Any) -> {error, badarg}
						end)
				end)
		end).

%% @doc Parse a content type.
-spec content_type(binary()) -> any().
content_type(Data) ->
	media_type(Data,
		fun (Rest, Type, SubType) ->
				content_type_params(Rest,
					fun (Params) -> {Type, SubType, Params} end, [])
		end).

-spec content_type_params(binary(), fun(), list({binary(), binary()}))
	-> any().
content_type_params(Data, Fun, Acc) ->
	whitespace(Data,
		fun (<< $;, Rest/binary >>) -> content_type_param(Rest, Fun, Acc);
			(<<>>) -> Fun(lists:reverse(Acc));
			(_Rest) -> {error, badarg}
		end).

-spec content_type_param(binary(), fun(), list({binary(), binary()}))
	-> any().
content_type_param(Data, Fun, Acc) ->
	whitespace(Data,
		fun (Rest) ->
				token_ci(Rest,
					fun (_Rest2, <<>>) -> {error, badarg};
						(<< $=, Rest2/binary >>, Attr) ->
							word(Rest2,
								fun (Rest3, Value) ->
										content_type_params(Rest3, Fun,
											[{Attr, Value}|Acc])
								end);
						(_Rest2, _Attr) -> {error, badarg}
					end)
		end).

%% @doc Parse a media range.
-spec media_range(binary(), fun()) -> any().
media_range(Data, Fun) ->
	media_type(Data,
		fun (Rest, Type, SubType) ->
			media_range_params(Rest, Fun, Type, SubType, [])
		end).

-spec media_range_params(binary(), fun(), binary(), binary(),
	[{binary(), binary()}]) -> any().
media_range_params(Data, Fun, Type, SubType, Acc) ->
	whitespace(Data,
		fun (<< $;, Rest/binary >>) ->
				whitespace(Rest,
					fun (Rest2) ->
						media_range_param_attr(Rest2, Fun, Type, SubType, Acc)
					end);
			(Rest) -> Fun(Rest, {{Type, SubType, lists:reverse(Acc)}, 1000, []})
		end).

-spec media_range_param_attr(binary(), fun(), binary(), binary(),
	[{binary(), binary()}]) -> any().
media_range_param_attr(Data, Fun, Type, SubType, Acc) ->
	token_ci(Data,
		fun (_Rest, <<>>) -> {error, badarg};
			(<< $=, Rest/binary >>, Attr) ->
				media_range_param_value(Rest, Fun, Type, SubType, Acc, Attr)
		end).

-spec media_range_param_value(binary(), fun(), binary(), binary(),
	[{binary(), binary()}], binary()) -> any().
media_range_param_value(Data, Fun, Type, SubType, Acc, <<"q">>) ->
	qvalue(Data,
		fun (Rest, Quality) ->
			accept_ext(Rest, Fun, Type, SubType, Acc, Quality, [])
		end);
media_range_param_value(Data, Fun, Type, SubType, Acc, Attr) ->
	word(Data,
		fun (Rest, Value) ->
			media_range_params(Rest, Fun,
				Type, SubType, [{Attr, Value}|Acc])
		end).

%% @doc Parse a media type.
-spec media_type(binary(), fun()) -> any().
media_type(Data, Fun) ->
	token_ci(Data,
		fun (_Rest, <<>>) -> {error, badarg};
			(<< $/, Rest/binary >>, Type) ->
				token_ci(Rest,
					fun (_Rest2, <<>>) -> {error, badarg};
						(Rest2, SubType) -> Fun(Rest2, Type, SubType)
					end);
			(_Rest, _Type) -> {error, badarg}
		end).

-spec accept_ext(binary(), fun(), binary(), binary(),
	[{binary(), binary()}], 0..1000,
	[{binary(), binary()} | binary()]) -> any().
accept_ext(Data, Fun, Type, SubType, Params, Quality, Acc) ->
	whitespace(Data,
		fun (<< $;, Rest/binary >>) ->
				whitespace(Rest,
					fun (Rest2) ->
						accept_ext_attr(Rest2, Fun,
							Type, SubType, Params, Quality, Acc)
					end);
			(Rest) ->
				Fun(Rest, {{Type, SubType, lists:reverse(Params)},
					Quality, lists:reverse(Acc)})
		end).

-spec accept_ext_attr(binary(), fun(), binary(), binary(),
	[{binary(), binary()}], 0..1000,
	[{binary(), binary()} | binary()]) -> any().
accept_ext_attr(Data, Fun, Type, SubType, Params, Quality, Acc) ->
	token_ci(Data,
		fun (_Rest, <<>>) -> {error, badarg};
			(<< $=, Rest/binary >>, Attr) ->
				accept_ext_value(Rest, Fun, Type, SubType, Params,
					Quality, Acc, Attr);
			(Rest, Attr) ->
				accept_ext(Rest, Fun, Type, SubType, Params,
					Quality, [Attr|Acc])
		end).

-spec accept_ext_value(binary(), fun(), binary(), binary(),
	[{binary(), binary()}], 0..1000,
	[{binary(), binary()} | binary()], binary()) -> any().
accept_ext_value(Data, Fun, Type, SubType, Params, Quality, Acc, Attr) ->
	word(Data,
		fun (Rest, Value) ->
				accept_ext(Rest, Fun,
					Type, SubType, Params, Quality, [{Attr, Value}|Acc])
		end).

%% @doc Parse a conneg header (Accept-Charset, Accept-Encoding),
%% followed by an optional quality value.
-spec conneg(binary(), fun()) -> any().
conneg(Data, Fun) ->
	token_ci(Data,
		fun (_Rest, <<>>) -> {error, badarg};
			(Rest, Conneg) ->
				maybe_qparam(Rest,
					fun (Rest2, Quality) ->
						Fun(Rest2, {Conneg, Quality})
					end)
		end).

%% @doc Parse a language range, followed by an optional quality value.
-spec language_range(binary(), fun()) -> any().
language_range(<< $*, Rest/binary >>, Fun) ->
	language_range_ret(Rest, Fun, '*');
language_range(Data, Fun) ->
	language_tag(Data,
		fun (Rest, LanguageTag) ->
				language_range_ret(Rest, Fun, LanguageTag)
		end).

-spec language_range_ret(binary(), fun(), '*' | {binary(), [binary()]}) -> any().
language_range_ret(Data, Fun, LanguageTag) ->
	maybe_qparam(Data,
		fun (Rest, Quality) ->
				Fun(Rest, {LanguageTag, Quality})
		end).

-spec language_tag(binary(), fun()) -> any().
language_tag(Data, Fun) ->
	alpha(Data,
		fun (_Rest, Tag) when byte_size(Tag) =:= 0; byte_size(Tag) > 8 ->
				{error, badarg};
			(<< $-, Rest/binary >>, Tag) ->
				language_subtag(Rest, Fun, Tag, []);
			(Rest, Tag) ->
				Fun(Rest, Tag)
		end).

-spec language_subtag(binary(), fun(), binary(), [binary()]) -> any().
language_subtag(Data, Fun, Tag, Acc) ->
	alpha(Data,
		fun (_Rest, SubTag) when byte_size(SubTag) =:= 0;
				byte_size(SubTag) > 8 -> {error, badarg};
			(<< $-, Rest/binary >>, SubTag) ->
				language_subtag(Rest, Fun, Tag, [SubTag|Acc]);
			(Rest, SubTag) ->
				%% Rebuild the full tag now that we know it's correct
				Sub = << << $-, S/binary >> || S <- lists:reverse([SubTag|Acc]) >>,
				Fun(Rest, << Tag/binary, Sub/binary >>)
		end).

-spec maybe_qparam(binary(), fun()) -> any().
maybe_qparam(Data, Fun) ->
	whitespace(Data,
		fun (<< $;, Rest/binary >>) ->
			whitespace(Rest,
				fun (Rest2) ->
					qparam(Rest2, Fun)
				end);
			(Rest) ->
				Fun(Rest, 1000)
		end).

%% @doc Parse a quality parameter string (for example q=0.500).
-spec qparam(binary(), fun()) -> any().
qparam(<< Q, $=, Data/binary >>, Fun) when Q =:= $q; Q =:= $Q ->
	qvalue(Data, Fun).

%% @doc Parse either a list of entity tags or a "*".
-spec entity_tag_match(binary()) -> any().
entity_tag_match(<< $*, Rest/binary >>) ->
	whitespace(Rest,
		fun (<<>>) -> '*';
			(_Any) -> {error, badarg}
		end);
entity_tag_match(Data) ->
	nonempty_list(Data, fun entity_tag/2).

%% @doc Parse an entity-tag.
-spec entity_tag(binary(), fun()) -> any().
entity_tag(<< "W/", Rest/binary >>, Fun) ->
	opaque_tag(Rest, Fun, weak);
entity_tag(Data, Fun) ->
	opaque_tag(Data, Fun, strong).

-spec opaque_tag(binary(), fun(), weak | strong) -> any().
opaque_tag(Data, Fun, Strength) ->
	quoted_string(Data,
		fun (_Rest, <<>>) -> {error, badarg};
			(Rest, OpaqueTag) -> Fun(Rest, {Strength, OpaqueTag})
		end).

%% @doc Parse an HTTP date (RFC1123, RFC850 or asctime date).
%% @end
%%
%% While this may not be the most efficient date parsing we can do,
%% it should work fine for our purposes because all HTTP dates should
%% be sent as RFC1123 dates in HTTP/1.1.
-spec http_date(binary()) -> any().
http_date(Data) ->
	case rfc1123_date(Data) of
		{error, badarg} ->
			case rfc850_date(Data) of
				{error, badarg} ->
					case asctime_date(Data) of
						{error, badarg} ->
							{error, badarg};
						HTTPDate ->
							HTTPDate
					end;
				HTTPDate ->
					HTTPDate
			end;
		HTTPDate ->
			HTTPDate
	end.

%% @doc Parse an RFC1123 date.
-spec rfc1123_date(binary()) -> any().
rfc1123_date(Data) ->
	wkday(Data,
		fun (<< ", ", Rest/binary >>, _WkDay) ->
				date1(Rest,
					fun (<< " ", Rest2/binary >>, Date) ->
							time(Rest2,
								fun (<< " GMT", Rest3/binary >>, Time) ->
										http_date_ret(Rest3, {Date, Time});
									(_Any, _Time) ->
										{error, badarg}
								end);
						(_Any, _Date) ->
							{error, badarg}
					end);
			(_Any, _WkDay) ->
				{error, badarg}
		end).

%% @doc Parse an RFC850 date.
-spec rfc850_date(binary()) -> any().
%% From the RFC:
%% HTTP/1.1 clients and caches SHOULD assume that an RFC-850 date
%% which appears to be more than 50 years in the future is in fact
%% in the past (this helps solve the "year 2000" problem).
rfc850_date(Data) ->
	weekday(Data,
		fun (<< ", ", Rest/binary >>, _WeekDay) ->
				date2(Rest,
					fun (<< " ", Rest2/binary >>, Date) ->
							time(Rest2,
								fun (<< " GMT", Rest3/binary >>, Time) ->
										http_date_ret(Rest3, {Date, Time});
									(_Any, _Time) ->
										{error, badarg}
								end);
						(_Any, _Date) ->
							{error, badarg}
					end);
			(_Any, _WeekDay) ->
				{error, badarg}
		end).

%% @doc Parse an asctime date.
-spec asctime_date(binary()) -> any().
asctime_date(Data) ->
	wkday(Data,
		fun (<< " ", Rest/binary >>, _WkDay) ->
				date3(Rest,
					fun (<< " ", Rest2/binary >>, PartialDate) ->
							time(Rest2,
								fun (<< " ", Rest3/binary >>, Time) ->
										asctime_year(Rest3,
											PartialDate, Time);
									(_Any, _Time) ->
										{error, badarg}
								end);
						(_Any, _PartialDate) ->
							{error, badarg}
					end);
			(_Any, _WkDay) ->
				{error, badarg1}
		end).

-spec asctime_year(binary(), tuple(), tuple()) -> any().
asctime_year(<< Y1, Y2, Y3, Y4, Rest/binary >>, {Month, Day}, Time)
		when Y1 >= $0, Y1 =< $9, Y2 >= $0, Y2 =< $9,
			 Y3 >= $0, Y3 =< $9, Y4 >= $0, Y4 =< $9 ->
	Year = (Y1 - $0) * 1000 + (Y2 - $0) * 100 + (Y3 - $0) * 10 + (Y4 - $0),
	http_date_ret(Rest, {{Year, Month, Day}, Time}).

-spec http_date_ret(binary(), tuple()) -> any().
http_date_ret(Data, DateTime = {Date, _Time}) ->
	whitespace(Data,
		fun (<<>>) ->
				case calendar:valid_date(Date) of
					true -> DateTime;
					false -> {error, badarg}
				end;
			(_Any) ->
				{error, badarg}
		end).

%% We never use it, pretty much just checks the wkday is right.
-spec wkday(binary(), fun()) -> any().
wkday(<< WkDay:3/binary, Rest/binary >>, Fun)
		when WkDay =:= <<"Mon">>; WkDay =:= <<"Tue">>; WkDay =:= <<"Wed">>;
			 WkDay =:= <<"Thu">>; WkDay =:= <<"Fri">>; WkDay =:= <<"Sat">>;
			 WkDay =:= <<"Sun">> ->
	Fun(Rest, WkDay);
wkday(_Any, _Fun) ->
	{error, badarg}.

%% We never use it, pretty much just checks the weekday is right.
-spec weekday(binary(), fun()) -> any().
weekday(<< "Monday", Rest/binary >>, Fun) ->
	Fun(Rest, <<"Monday">>);
weekday(<< "Tuesday", Rest/binary >>, Fun) ->
	Fun(Rest, <<"Tuesday">>);
weekday(<< "Wednesday", Rest/binary >>, Fun) ->
	Fun(Rest, <<"Wednesday">>);
weekday(<< "Thursday", Rest/binary >>, Fun) ->
	Fun(Rest, <<"Thursday">>);
weekday(<< "Friday", Rest/binary >>, Fun) ->
	Fun(Rest, <<"Friday">>);
weekday(<< "Saturday", Rest/binary >>, Fun) ->
	Fun(Rest, <<"Saturday">>);
weekday(<< "Sunday", Rest/binary >>, Fun) ->
	Fun(Rest, <<"Sunday">>);
weekday(_Any, _Fun) ->
	{error, badarg}.

-spec date1(binary(), fun()) -> any().
date1(<< D1, D2, " ", M:3/binary, " ", Y1, Y2, Y3, Y4, Rest/binary >>, Fun)
		when D1 >= $0, D1 =< $9, D2 >= $0, D2 =< $9,
			 Y1 >= $0, Y1 =< $9, Y2 >= $0, Y2 =< $9,
			 Y3 >= $0, Y3 =< $9, Y4 >= $0, Y4 =< $9 ->
	case month(M) of
		{error, badarg} ->
			{error, badarg};
		Month ->
			Fun(Rest, {
				(Y1 - $0) * 1000 + (Y2 - $0) * 100 + (Y3 - $0) * 10 + (Y4 - $0),
				Month,
				(D1 - $0) * 10 + (D2 - $0)
			})
	end;
date1(_Data, _Fun) ->
	{error, badarg}.

-spec date2(binary(), fun()) -> any().
date2(<< D1, D2, "-", M:3/binary, "-", Y1, Y2, Rest/binary >>, Fun)
		when D1 >= $0, D1 =< $9, D2 >= $0, D2 =< $9,
			 Y1 >= $0, Y1 =< $9, Y2 >= $0, Y2 =< $9 ->
	case month(M) of
		{error, badarg} ->
			{error, badarg};
		Month ->
			Year = (Y1 - $0) * 10 + (Y2 - $0),
			Year2 = case Year > 50 of
				true -> Year + 1900;
				false -> Year + 2000
			end,
			Fun(Rest, {
				Year2,
				Month,
				(D1 - $0) * 10 + (D2 - $0)
			})
	end;
date2(_Data, _Fun) ->
	{error, badarg}.

-spec date3(binary(), fun()) -> any().
date3(<< M:3/binary, " ", D1, D2, Rest/binary >>, Fun)
		when (D1 >= $0 andalso D1 =< $3) orelse D1 =:= $\s,
			 D2 >= $0, D2 =< $9 ->
	case month(M) of
		{error, badarg} ->
			{error, badarg};
		Month ->
			Day = case D1 of
				$\s -> D2 - $0;
				D1 -> (D1 - $0) * 10 + (D2 - $0)
			end,
			Fun(Rest, {Month, Day})
	end;
date3(_Data, _Fun) ->
	{error, badarg}.

-spec month(<< _:24 >>) -> 1..12 | {error, badarg}.
month(<<"Jan">>) -> 1;
month(<<"Feb">>) -> 2;
month(<<"Mar">>) -> 3;
month(<<"Apr">>) -> 4;
month(<<"May">>) -> 5;
month(<<"Jun">>) -> 6;
month(<<"Jul">>) -> 7;
month(<<"Aug">>) -> 8;
month(<<"Sep">>) -> 9;
month(<<"Oct">>) -> 10;
month(<<"Nov">>) -> 11;
month(<<"Dec">>) -> 12;
month(_Any) -> {error, badarg}.

-spec time(binary(), fun()) -> any().
time(<< H1, H2, ":", M1, M2, ":", S1, S2, Rest/binary >>, Fun)
		when H1 >= $0, H1 =< $2, H2 >= $0, H2 =< $9,
			 M1 >= $0, M1 =< $5, M2 >= $0, M2 =< $9,
			 S1 >= $0, S1 =< $5, S2 >= $0, S2 =< $9 ->
	Hour = (H1 - $0) * 10 + (H2 - $0),
	case Hour < 24 of
		true ->
			Time = {
				Hour,
				(M1 - $0) * 10 + (M2 - $0),
				(S1 - $0) * 10 + (S2 - $0)
			},
			Fun(Rest, Time);
		false ->
			{error, badarg}
	end.

%% @doc Skip whitespace.
-spec whitespace(binary(), fun()) -> any().
whitespace(<< C, Rest/binary >>, Fun)
		when C =:= $\s; C =:= $\t ->
	whitespace(Rest, Fun);
whitespace(Data, Fun) ->
	Fun(Data).

%% @doc Parse a list of digits as a non negative integer.
-spec digits(binary()) -> non_neg_integer() | {error, badarg}.
digits(Data) ->
	digits(Data,
		fun (Rest, I) ->
			whitespace(Rest,
				fun (<<>>) ->
						I;
					(_Rest2) ->
						{error, badarg}
				end)
		end).

-spec digits(binary(), fun()) -> any().
digits(<< C, Rest/binary >>, Fun)
		when C >= $0, C =< $9 ->
	digits(Rest, Fun, C - $0);
digits(_Data, _Fun) ->
	{error, badarg}.

-spec digits(binary(), fun(), non_neg_integer()) -> any().
digits(<< C, Rest/binary >>, Fun, Acc)
		when C >= $0, C =< $9 ->
	digits(Rest, Fun, Acc * 10 + (C - $0));
digits(Data, Fun, Acc) ->
	Fun(Data, Acc).

%% @doc Parse a list of case-insensitive alpha characters.
%%
%% Changes all characters to lowercase.
-spec alpha(binary(), fun()) -> any().
alpha(Data, Fun) ->
	alpha(Data, Fun, <<>>).

-spec alpha(binary(), fun(), binary()) -> any().
alpha(<<>>, Fun, Acc) ->
	Fun(<<>>, Acc);
alpha(<< C, Rest/binary >>, Fun, Acc)
		when C >= $a andalso C =< $z;
			 C >= $A andalso C =< $Z ->
	C2 = cowboy_bstr:char_to_lower(C),
	alpha(Rest, Fun, << Acc/binary, C2 >>);
alpha(Data, Fun, Acc) ->
	Fun(Data, Acc).

%% @doc Parse either a token or a quoted string.
-spec word(binary(), fun()) -> any().
word(Data = << $", _/binary >>, Fun) ->
	quoted_string(Data, Fun);
word(Data, Fun) ->
	token(Data,
		fun (_Rest, <<>>) -> {error, badarg};
			(Rest, Token) -> Fun(Rest, Token)
		end).

%% @doc Parse a case-insensitive token.
%%
%% Changes all characters to lowercase.
-spec token_ci(binary(), fun()) -> any().
token_ci(Data, Fun) ->
	token(Data, Fun, ci, <<>>).

%% @doc Parse a token.
-spec token(binary(), fun()) -> any().
token(Data, Fun) ->
	token(Data, Fun, cs, <<>>).

-spec token(binary(), fun(), ci | cs, binary()) -> any().
token(<<>>, Fun, _Case, Acc) ->
	Fun(<<>>, Acc);
token(Data = << C, _Rest/binary >>, Fun, _Case, Acc)
		when C =:= $(; C =:= $); C =:= $<; C =:= $>; C =:= $@;
			 C =:= $,; C =:= $;; C =:= $:; C =:= $\\; C =:= $";
			 C =:= $/; C =:= $[; C =:= $]; C =:= $?; C =:= $=;
			 C =:= ${; C =:= $}; C =:= $\s; C =:= $\t;
			 C < 32; C =:= 127 ->
	Fun(Data, Acc);
token(<< C, Rest/binary >>, Fun, Case = ci, Acc) ->
	C2 = cowboy_bstr:char_to_lower(C),
	token(Rest, Fun, Case, << Acc/binary, C2 >>);
token(<< C, Rest/binary >>, Fun, Case, Acc) ->
	token(Rest, Fun, Case, << Acc/binary, C >>).

%% @doc Parse a quoted string.
-spec quoted_string(binary(), fun()) -> any().
quoted_string(<< $", Rest/binary >>, Fun) ->
	quoted_string(Rest, Fun, <<>>).

-spec quoted_string(binary(), fun(), binary()) -> any().
quoted_string(<<>>, _Fun, _Acc) ->
	{error, badarg};
quoted_string(<< $", Rest/binary >>, Fun, Acc) ->
	Fun(Rest, Acc);
quoted_string(<< $\\, C, Rest/binary >>, Fun, Acc) ->
	quoted_string(Rest, Fun, << Acc/binary, C >>);
quoted_string(<< C, Rest/binary >>, Fun, Acc) ->
	quoted_string(Rest, Fun, << Acc/binary, C >>).

%% @doc Parse a quality value.
-spec qvalue(binary(), fun()) -> any().
qvalue(<< $0, $., Rest/binary >>, Fun) ->
	qvalue(Rest, Fun, 0, 100);
qvalue(<< $0, Rest/binary >>, Fun) ->
	Fun(Rest, 0);
qvalue(<< $1, $., $0, $0, $0, Rest/binary >>, Fun) ->
	Fun(Rest, 1000);
qvalue(<< $1, $., $0, $0, Rest/binary >>, Fun) ->
	Fun(Rest, 1000);
qvalue(<< $1, $., $0, Rest/binary >>, Fun) ->
	Fun(Rest, 1000);
qvalue(<< $1, Rest/binary >>, Fun) ->
	Fun(Rest, 1000);
qvalue(_Data, _Fun) ->
	{error, badarg}.

-spec qvalue(binary(), fun(), integer(), 1 | 10 | 100) -> any().
qvalue(Data, Fun, Q, 0) ->
	Fun(Data, Q);
qvalue(<< C, Rest/binary >>, Fun, Q, M)
		when C >= $0, C =< $9 ->
	qvalue(Rest, Fun, Q + (C - $0) * M, M div 10);
qvalue(Data, Fun, Q, _M) ->
	Fun(Data, Q).


%% Interpretation.

%% @doc Walk through a tokens list and return whether
%% the connection is keepalive or closed.
%%
%% The connection token is expected to be lower-case.
-spec connection_to_atom([binary()]) -> keepalive | close.
connection_to_atom([]) ->
	keepalive;
connection_to_atom([<<"keep-alive">>|_Tail]) ->
	keepalive;
connection_to_atom([<<"close">>|_Tail]) ->
	close;
connection_to_atom([_Any|Tail]) ->
	connection_to_atom(Tail).

%% @doc Decode a URL encoded binary.
%% @equiv urldecode(Bin, crash)
-spec urldecode(binary()) -> binary().
urldecode(Bin) when is_binary(Bin) ->
	urldecode(Bin, <<>>, crash).

%% @doc Decode a URL encoded binary.
%% The second argument specifies how to handle percent characters that are not
%% followed by two valid hex characters. Use `skip' to ignore such errors,
%% if `crash' is used the function will fail with the reason `badarg'.
-spec urldecode(binary(), crash | skip) -> binary().
urldecode(Bin, OnError) when is_binary(Bin) ->
	urldecode(Bin, <<>>, OnError).

-spec urldecode(binary(), binary(), crash | skip) -> binary().
urldecode(<<$%, H, L, Rest/binary>>, Acc, OnError) ->
	G = unhex(H),
	M = unhex(L),
	if	G =:= error; M =:= error ->
		case OnError of skip -> ok; crash -> erlang:error(badarg) end,
		urldecode(<<H, L, Rest/binary>>, <<Acc/binary, $%>>, OnError);
		true ->
		urldecode(Rest, <<Acc/binary, (G bsl 4 bor M)>>, OnError)
	end;
urldecode(<<$%, Rest/binary>>, Acc, OnError) ->
	case OnError of skip -> ok; crash -> erlang:error(badarg) end,
	urldecode(Rest, <<Acc/binary, $%>>, OnError);
urldecode(<<$+, Rest/binary>>, Acc, OnError) ->
	urldecode(Rest, <<Acc/binary, $ >>, OnError);
urldecode(<<C, Rest/binary>>, Acc, OnError) ->
	urldecode(Rest, <<Acc/binary, C>>, OnError);
urldecode(<<>>, Acc, _OnError) ->
	Acc.

-spec unhex(byte()) -> byte() | error.
unhex(C) when C >= $0, C =< $9 -> C - $0;
unhex(C) when C >= $A, C =< $F -> C - $A + 10;
unhex(C) when C >= $a, C =< $f -> C - $a + 10;
unhex(_) -> error.


%% @doc URL encode a string binary.
%% @equiv urlencode(Bin, [])
-spec urlencode(binary()) -> binary().
urlencode(Bin) ->
	urlencode(Bin, []).

%% @doc URL encode a string binary.
%% The `noplus' option disables the default behaviour of quoting space
%% characters, `\s', as `+'. The `upper' option overrides the default behaviour
%% of writing hex numbers using lowecase letters to using uppercase letters
%% instead.
-spec urlencode(binary(), [noplus|upper]) -> binary().
urlencode(Bin, Opts) ->
	Plus = not proplists:get_value(noplus, Opts, false),
	Upper = proplists:get_value(upper, Opts, false),
	urlencode(Bin, <<>>, Plus, Upper).

-spec urlencode(binary(), binary(), boolean(), boolean()) -> binary().
urlencode(<<C, Rest/binary>>, Acc, P=Plus, U=Upper) ->
	if	C >= $0, C =< $9 -> urlencode(Rest, <<Acc/binary, C>>, P, U);
		C >= $A, C =< $Z -> urlencode(Rest, <<Acc/binary, C>>, P, U);
		C >= $a, C =< $z -> urlencode(Rest, <<Acc/binary, C>>, P, U);
		C =:= $.; C =:= $-; C =:= $~; C =:= $_ ->
		urlencode(Rest, <<Acc/binary, C>>, P, U);
		C =:= $ , Plus ->
		urlencode(Rest, <<Acc/binary, $+>>, P, U);
		true ->
		H = C band 16#F0 bsr 4, L = C band 16#0F,
		H1 = if Upper -> tohexu(H); true -> tohexl(H) end,
		L1 = if Upper -> tohexu(L); true -> tohexl(L) end,
		urlencode(Rest, <<Acc/binary, $%, H1, L1>>, P, U)
	end;
urlencode(<<>>, Acc, _Plus, _Upper) ->
	Acc.

-spec tohexu(byte()) -> byte().
tohexu(C) when C < 10 -> $0 + C;
tohexu(C) when C < 17 -> $A + C - 10.

-spec tohexl(byte()) -> byte().
tohexl(C) when C < 10 -> $0 + C;
tohexl(C) when C < 17 -> $a + C - 10.


%% Tests.

-ifdef(TEST).

nonempty_charset_list_test_() ->
	%% {Value, Result}
	Tests = [
		{<<>>, {error, badarg}},
		{<<"iso-8859-5, unicode-1-1;q=0.8">>, [
			{<<"iso-8859-5">>, 1000},
			{<<"unicode-1-1">>, 800}
		]}
	],
	[{V, fun() -> R = nonempty_list(V, fun conneg/2) end} || {V, R} <- Tests].

nonempty_language_range_list_test_() ->
	%% {Value, Result}
	Tests = [
		{<<"da, en-gb;q=0.8, en;q=0.7">>, [
			{<<"da">>, 1000},
			{<<"en-gb">>, 800},
			{<<"en">>, 700}
		]},
		{<<"en, en-US, en-cockney, i-cherokee, x-pig-latin">>, [
			{<<"en">>, 1000},
			{<<"en-us">>, 1000},
			{<<"en-cockney">>, 1000},
			{<<"i-cherokee">>, 1000},
			{<<"x-pig-latin">>, 1000}
		]}
	],
	[{V, fun() -> R = nonempty_list(V, fun language_range/2) end}
		|| {V, R} <- Tests].

nonempty_token_list_test_() ->
	%% {Value, Result}
	Tests = [
		{<<>>, {error, badarg}},
		{<<" ">>, {error, badarg}},
		{<<" , ">>, {error, badarg}},
		{<<",,,">>, {error, badarg}},
		{<<"a b">>, {error, badarg}},
		{<<"a , , , ">>, [<<"a">>]},
		{<<" , , , a">>, [<<"a">>]},
		{<<"a, , b">>, [<<"a">>, <<"b">>]},
		{<<"close">>, [<<"close">>]},
		{<<"keep-alive, upgrade">>, [<<"keep-alive">>, <<"upgrade">>]}
	],
	[{V, fun() -> R = nonempty_list(V, fun token/2) end} || {V, R} <- Tests].

media_range_list_test_() ->
	%% {Tokens, Result}
	Tests = [
		{<<"audio/*; q=0.2, audio/basic">>, [
			{{<<"audio">>, <<"*">>, []}, 200, []},
			{{<<"audio">>, <<"basic">>, []}, 1000, []}
		]},
		{<<"text/plain; q=0.5, text/html, "
		   "text/x-dvi; q=0.8, text/x-c">>, [
		   {{<<"text">>, <<"plain">>, []}, 500, []},
		   {{<<"text">>, <<"html">>, []}, 1000, []},
		   {{<<"text">>, <<"x-dvi">>, []}, 800, []},
		   {{<<"text">>, <<"x-c">>, []}, 1000, []}
		]},
		{<<"text/*, text/html, text/html;level=1, */*">>, [
			{{<<"text">>, <<"*">>, []}, 1000, []},
			{{<<"text">>, <<"html">>, []}, 1000, []},
			{{<<"text">>, <<"html">>, [{<<"level">>, <<"1">>}]}, 1000, []},
			{{<<"*">>, <<"*">>, []}, 1000, []}
		]},
		{<<"text/*;q=0.3, text/html;q=0.7, text/html;level=1, "
		   "text/html;level=2;q=0.4, */*;q=0.5">>, [
		   {{<<"text">>, <<"*">>, []}, 300, []},
		   {{<<"text">>, <<"html">>, []}, 700, []},
		   {{<<"text">>, <<"html">>, [{<<"level">>, <<"1">>}]}, 1000, []},
		   {{<<"text">>, <<"html">>, [{<<"level">>, <<"2">>}]}, 400, []},
		   {{<<"*">>, <<"*">>, []}, 500, []}
		]},
		{<<"text/html;level=1;quoted=\"hi hi hi\";"
		   "q=0.123;standalone;complex=gits, text/plain">>, [
			{{<<"text">>, <<"html">>,
				[{<<"level">>, <<"1">>}, {<<"quoted">>, <<"hi hi hi">>}]}, 123,
				[<<"standalone">>, {<<"complex">>, <<"gits">>}]},
			{{<<"text">>, <<"plain">>, []}, 1000, []}
		]}
	],
	[{V, fun() -> R = list(V, fun media_range/2) end} || {V, R} <- Tests].

entity_tag_match_test_() ->
	%% {Tokens, Result}
	Tests = [
		{<<"\"xyzzy\"">>, [{strong, <<"xyzzy">>}]},
		{<<"\"xyzzy\", W/\"r2d2xxxx\", \"c3piozzzz\"">>,
			[{strong, <<"xyzzy">>},
			 {weak, <<"r2d2xxxx">>},
			 {strong, <<"c3piozzzz">>}]},
		{<<"*">>, '*'}
	],
	[{V, fun() -> R = entity_tag_match(V) end} || {V, R} <- Tests].

http_date_test_() ->
	%% {Tokens, Result}
	Tests = [
		{<<"Sun, 06 Nov 1994 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}},
		{<<"Sunday, 06-Nov-94 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}},
		{<<"Sun Nov  6 08:49:37 1994">>, {{1994, 11, 6}, {8, 49, 37}}}
	],
	[{V, fun() -> R = http_date(V) end} || {V, R} <- Tests].

rfc1123_date_test_() ->
	%% {Tokens, Result}
	Tests = [
		{<<"Sun, 06 Nov 1994 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}}
	],
	[{V, fun() -> R = rfc1123_date(V) end} || {V, R} <- Tests].

rfc850_date_test_() ->
	%% {Tokens, Result}
	Tests = [
		{<<"Sunday, 06-Nov-94 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}}
	],
	[{V, fun() -> R = rfc850_date(V) end} || {V, R} <- Tests].

asctime_date_test_() ->
	%% {Tokens, Result}
	Tests = [
		{<<"Sun Nov  6 08:49:37 1994">>, {{1994, 11, 6}, {8, 49, 37}}}
	],
	[{V, fun() -> R = asctime_date(V) end} || {V, R} <- Tests].

connection_to_atom_test_() ->
	%% {Tokens, Result}
	Tests = [
		{[<<"close">>], close},
		{[<<"keep-alive">>], keepalive},
		{[<<"keep-alive">>, <<"upgrade">>], keepalive}
	],
	[{lists:flatten(io_lib:format("~p", [T])),
		fun() -> R = connection_to_atom(T) end} || {T, R} <- Tests].

content_type_test_() ->
	%% {ContentType, Result}
	Tests = [
		{<<"text/plain; charset=iso-8859-4">>,
			{<<"text">>, <<"plain">>, [{<<"charset">>, <<"iso-8859-4">>}]}},
		{<<"multipart/form-data  \t;Boundary=\"MultipartIsUgly\"">>,
			{<<"multipart">>, <<"form-data">>, [
				{<<"boundary">>, <<"MultipartIsUgly">>}
			]}},
		{<<"foo/bar; one=FirstParam; two=SecondParam">>,
			{<<"foo">>, <<"bar">>, [
				{<<"one">>, <<"FirstParam">>},
				{<<"two">>, <<"SecondParam">>}
			]}}
	],
	[{V, fun () -> R = content_type(V) end} || {V, R} <- Tests].

digits_test_() ->
	%% {Digits, Result}
	Tests = [
		{<<"42    ">>, 42},
		{<<"69\t">>, 69},
		{<<"1337">>, 1337}
	],
	[{V, fun() -> R = digits(V) end} || {V, R} <- Tests].

urldecode_test_() ->
	U = fun urldecode/2,
	[?_assertEqual(<<" ">>, U(<<"%20">>, crash)),
	 ?_assertEqual(<<" ">>, U(<<"+">>, crash)),
	 ?_assertEqual(<<0>>, U(<<"%00">>, crash)),
	 ?_assertEqual(<<255>>, U(<<"%fF">>, crash)),
	 ?_assertEqual(<<"123">>, U(<<"123">>, crash)),
	 ?_assertEqual(<<"%i5">>, U(<<"%i5">>, skip)),
	 ?_assertEqual(<<"%5">>, U(<<"%5">>, skip)),
	 ?_assertError(badarg, U(<<"%i5">>, crash)),
	 ?_assertError(badarg, U(<<"%5">>, crash))
	].

urlencode_test_() ->
	U = fun urlencode/2,
	[?_assertEqual(<<"%ff%00">>, U(<<255,0>>, [])),
	 ?_assertEqual(<<"%FF%00">>, U(<<255,0>>, [upper])),
	 ?_assertEqual(<<"+">>, U(<<" ">>, [])),
	 ?_assertEqual(<<"%20">>, U(<<" ">>, [noplus])),
	 ?_assertEqual(<<"aBc">>, U(<<"aBc">>, [])),
	 ?_assertEqual(<<".-~_">>, U(<<".-~_">>, [])),
	 ?_assertEqual(<<"%ff+">>, urlencode(<<255, " ">>))
	].

-endif.
