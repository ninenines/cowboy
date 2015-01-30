%% Copyright (c) 2011-2014, Loïc Hoguin <essen@ninenines.eu>
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

%% Deprecated HTTP parsing API.
-module(cowboy_http).

%% Parsing.
-export([list/2]).
-export([nonempty_list/2]).
-export([content_type/1]).
-export([media_range/2]).
-export([conneg/2]).
-export([language_range/2]).
-export([entity_tag_match/1]).
-export([expectation/2]).
-export([params/2]).
-export([http_date/1]).
-export([rfc1123_date/1]).
-export([rfc850_date/1]).
-export([asctime_date/1]).
-export([whitespace/2]).
-export([digits/1]).
-export([token/2]).
-export([token_ci/2]).
-export([quoted_string/2]).
-export([authorization/2]).
-export([range/1]).
-export([parameterized_tokens/1]).

%% Decoding.
-export([ce_identity/1]).

%% Parsing.

-spec nonempty_list(binary(), fun()) -> [any(), ...] | {error, badarg}.
nonempty_list(Data, Fun) ->
	case list(Data, Fun, []) of
		{error, badarg} -> {error, badarg};
		[] -> {error, badarg};
		L -> lists:reverse(L)
	end.

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

%% We lowercase the charset header as we know it's case insensitive.
-spec content_type(binary()) -> any().
content_type(Data) ->
	media_type(Data,
		fun (Rest, Type, SubType) ->
			params(Rest,
				fun (<<>>, Params) ->
						case lists:keyfind(<<"charset">>, 1, Params) of
							false ->
								{Type, SubType, Params};
							{_, Charset} ->
								Charset2 = cowboy_bstr:to_lower(Charset),
								Params2 = lists:keyreplace(<<"charset">>,
									1, Params, {<<"charset">>, Charset2}),
								{Type, SubType, Params2}
						end;
					(_Rest2, _) ->
						{error, badarg}
				end)
		end).

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

-spec media_type(binary(), fun()) -> any().
media_type(Data, Fun) ->
	token_ci(Data,
		fun (_Rest, <<>>) -> {error, badarg};
			(<< $/, Rest/binary >>, Type) ->
				token_ci(Rest,
					fun (_Rest2, <<>>) -> {error, badarg};
						(Rest2, SubType) -> Fun(Rest2, Type, SubType)
					end);
			%% This is a non-strict parsing clause required by some user agents
			%% that use * instead of */* in the list of media types.
			(Rest, <<"*">> = Type) ->
				token_ci(<<"*", Rest/binary>>,
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
	alphanumeric(Data,
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
					%% This is a non-strict parsing clause required by some user agents
					%% that use the wrong delimiter putting a charset where a qparam is
					%% expected.
					try qparam(Rest2, Fun) of
						Result -> Result
					catch
						error:function_clause ->
							Fun(<<",", Rest2/binary>>, 1000)
					end
				end);
			(Rest) ->
				Fun(Rest, 1000)
		end).

-spec qparam(binary(), fun()) -> any().
qparam(<< Q, $=, Data/binary >>, Fun) when Q =:= $q; Q =:= $Q ->
	qvalue(Data, Fun).

-spec entity_tag_match(binary()) -> any().
entity_tag_match(<< $*, Rest/binary >>) ->
	whitespace(Rest,
		fun (<<>>) -> '*';
			(_Any) -> {error, badarg}
		end);
entity_tag_match(Data) ->
	nonempty_list(Data, fun entity_tag/2).

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

-spec expectation(binary(), fun()) -> any().
expectation(Data, Fun) ->
	token_ci(Data,
		fun (_Rest, <<>>) -> {error, badarg};
			(<< $=, Rest/binary >>, Expectation) ->
				word(Rest,
					fun (Rest2, ExtValue) ->
						params(Rest2, fun (Rest3, ExtParams) ->
							Fun(Rest3, {Expectation, ExtValue, ExtParams})
						end)
					end);
			(Rest, Expectation) ->
				Fun(Rest, Expectation)
		end).

-spec params(binary(), fun()) -> any().
params(Data, Fun) ->
	params(Data, Fun, []).

-spec params(binary(), fun(), [{binary(), binary()}]) -> any().
params(Data, Fun, Acc) ->
	whitespace(Data,
		fun (<< $;, Rest/binary >>) ->
				param(Rest,
					fun (Rest2, Attr, Value) ->
							params(Rest2, Fun, [{Attr, Value}|Acc])
					end);
			(Rest) ->
				Fun(Rest, lists:reverse(Acc))
		end).

-spec param(binary(), fun()) -> any().
param(Data, Fun) ->
	whitespace(Data,
		fun (Rest) ->
				token_ci(Rest,
					fun (_Rest2, <<>>) -> {error, badarg};
						(<< $=, Rest2/binary >>, Attr) ->
							word(Rest2,
								fun (Rest3, Value) ->
										Fun(Rest3, Attr, Value)
								end);
						(_Rest2, _Attr) -> {error, badarg}
					end)
		end).

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

-spec whitespace(binary(), fun()) -> any().
whitespace(<< C, Rest/binary >>, Fun)
		when C =:= $\s; C =:= $\t ->
	whitespace(Rest, Fun);
whitespace(Data, Fun) ->
	Fun(Data).

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

-spec alphanumeric(binary(), fun()) -> any().
alphanumeric(Data, Fun) ->
	alphanumeric(Data, Fun, <<>>).

-spec alphanumeric(binary(), fun(), binary()) -> any().
alphanumeric(<<>>, Fun, Acc) ->
	Fun(<<>>, Acc);
alphanumeric(<< C, Rest/binary >>, Fun, Acc)
		when C >= $a andalso C =< $z;
			 C >= $A andalso C =< $Z;
			 C >= $0 andalso C =< $9 ->
	C2 = cowboy_bstr:char_to_lower(C),
	alphanumeric(Rest, Fun, << Acc/binary, C2 >>);
alphanumeric(Data, Fun, Acc) ->
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

%% Changes all characters to lowercase.
-spec token_ci(binary(), fun()) -> any().
token_ci(Data, Fun) ->
	token(Data, Fun, ci, <<>>).

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

-spec quoted_string(binary(), fun()) -> any().
quoted_string(<< $", Rest/binary >>, Fun) ->
	quoted_string(Rest, Fun, <<>>);
quoted_string(_, _Fun) ->
    {error, badarg}.

-spec quoted_string(binary(), fun(), binary()) -> any().
quoted_string(<<>>, _Fun, _Acc) ->
	{error, badarg};
quoted_string(<< $", Rest/binary >>, Fun, Acc) ->
	Fun(Rest, Acc);
quoted_string(<< $\\, C, Rest/binary >>, Fun, Acc) ->
	quoted_string(Rest, Fun, << Acc/binary, C >>);
quoted_string(<< C, Rest/binary >>, Fun, Acc) ->
	quoted_string(Rest, Fun, << Acc/binary, C >>).

-spec qvalue(binary(), fun()) -> any().
qvalue(<< $0, $., Rest/binary >>, Fun) ->
	qvalue(Rest, Fun, 0, 100);
%% Some user agents use q=.x instead of q=0.x
qvalue(<< $., Rest/binary >>, Fun) ->
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

%% Only RFC2617 Basic authorization is supported so far.
-spec authorization(binary(), binary()) -> {binary(), any()} | {error, badarg}.
authorization(UserPass, Type = <<"basic">>) ->
	whitespace(UserPass,
		fun(D) ->
			authorization_basic_userid(base64:mime_decode(D),
				fun(Rest, Userid) ->
					authorization_basic_password(Rest,
						fun(Password) ->
							{Type, {Userid, Password}}
						end)
				end)
		end);
authorization(String, Type) ->
	whitespace(String, fun(Rest) -> {Type, Rest} end).

-spec authorization_basic_userid(binary(), fun()) -> any().
authorization_basic_userid(Data, Fun) ->
	authorization_basic_userid(Data, Fun, <<>>).

authorization_basic_userid(<<>>, _Fun, _Acc) ->
	{error, badarg};
authorization_basic_userid(<<C, _Rest/binary>>, _Fun, Acc)
		when C < 32; C =:= 127; (C =:=$: andalso Acc =:= <<>>) ->
	{error, badarg};
authorization_basic_userid(<<$:, Rest/binary>>, Fun, Acc) ->
	Fun(Rest, Acc);
authorization_basic_userid(<<C, Rest/binary>>, Fun, Acc) ->
	authorization_basic_userid(Rest, Fun, <<Acc/binary, C>>).

-spec authorization_basic_password(binary(), fun()) -> any().
authorization_basic_password(Data, Fun) ->
	authorization_basic_password(Data, Fun, <<>>).

authorization_basic_password(<<C, _Rest/binary>>, _Fun, _Acc)
		when C < 32; C=:= 127 ->
	{error, badarg};
authorization_basic_password(<<>>, Fun, Acc) ->
	Fun(Acc);
authorization_basic_password(<<C, Rest/binary>>, Fun, Acc) ->
	authorization_basic_password(Rest, Fun, <<Acc/binary, C>>).

-spec range(binary()) -> {Unit, [Range]} | {error, badarg} when
		Unit :: binary(),
		Range :: {non_neg_integer(), non_neg_integer() | infinity} | neg_integer().
range(Data) ->
	token_ci(Data, fun range/2).

range(Data, Token) ->
	whitespace(Data,
		fun(<<"=", Rest/binary>>) ->
			case list(Rest, fun range_beginning/2) of
				{error, badarg} ->
					{error, badarg};
				Ranges ->
					{Token, Ranges}
			end;
		   (_) ->
			{error, badarg}
		end).

range_beginning(Data, Fun) ->
	range_digits(Data, suffix,
		fun(D, RangeBeginning) ->
			range_ending(D, Fun, RangeBeginning)
		end).

range_ending(Data, Fun, RangeBeginning) ->
	whitespace(Data,
		fun(<<"-", R/binary>>) ->
			case RangeBeginning of
				suffix ->
					range_digits(R, fun(D, RangeEnding) -> Fun(D, -RangeEnding) end);
				_ ->
					range_digits(R, infinity,
						fun(D, RangeEnding) ->
							Fun(D, {RangeBeginning, RangeEnding})
						end)
			end;
		   (_) ->
			{error, badarg}
		end).

-spec range_digits(binary(), fun()) -> any().
range_digits(Data, Fun) ->
	whitespace(Data,
		fun(D) ->
			digits(D, Fun)
		end).

-spec range_digits(binary(), any(), fun()) -> any().
range_digits(Data, Default, Fun) ->
	whitespace(Data,
		fun(<< C, Rest/binary >>) when C >= $0, C =< $9 ->
			digits(Rest, Fun, C - $0);
		   (_) ->
			Fun(Data, Default)
		end).

-spec parameterized_tokens(binary()) -> any().
parameterized_tokens(Data) ->
	nonempty_list(Data,
		fun (D, Fun) ->
			token(D,
				fun (_Rest, <<>>) -> {error, badarg};
					(Rest, Token) ->
						parameterized_tokens_params(Rest,
							fun (Rest2, Params) ->
								Fun(Rest2, {Token, Params})
							end, [])
				end)
		end).

-spec parameterized_tokens_params(binary(), fun(), [binary() | {binary(), binary()}]) -> any().
parameterized_tokens_params(Data, Fun, Acc) ->
	whitespace(Data,
		fun (<< $;, Rest/binary >>) ->
				parameterized_tokens_param(Rest,
					fun (Rest2, Param) ->
							parameterized_tokens_params(Rest2, Fun, [Param|Acc])
					end);
			(Rest) ->
				Fun(Rest, lists:reverse(Acc))
		end).

-spec parameterized_tokens_param(binary(), fun()) -> any().
parameterized_tokens_param(Data, Fun) ->
	whitespace(Data,
		fun (Rest) ->
				token(Rest,
					fun (_Rest2, <<>>) -> {error, badarg};
						(<< $=, Rest2/binary >>, Attr) ->
							word(Rest2,
								fun (Rest3, Value) ->
										Fun(Rest3, {Attr, Value})
								end);
						(Rest2, Attr) ->
							Fun(Rest2, Attr)
					end)
		end).

%% Decoding.

%% @todo Move this to cowlib too I suppose. :-)
-spec ce_identity(binary()) -> {ok, binary()}.
ce_identity(Data) ->
	{ok, Data}.

%% Tests.

-ifdef(TEST).
nonempty_charset_list_test_() ->
	Tests = [
		{<<>>, {error, badarg}},
		{<<"iso-8859-5, unicode-1-1;q=0.8">>, [
			{<<"iso-8859-5">>, 1000},
			{<<"unicode-1-1">>, 800}
		]},
		%% Some user agents send this invalid value for the Accept-Charset header
		{<<"ISO-8859-1;utf-8;q=0.7,*;q=0.7">>, [
			{<<"iso-8859-1">>, 1000},
			{<<"utf-8">>, 700},
			{<<"*">>, 700}
		]}
	],
	[{V, fun() -> R = nonempty_list(V, fun conneg/2) end} || {V, R} <- Tests].

nonempty_language_range_list_test_() ->
	Tests = [
		{<<"da, en-gb;q=0.8, en;q=0.7">>, [
			{<<"da">>, 1000},
			{<<"en-gb">>, 800},
			{<<"en">>, 700}
		]},
		{<<"en, en-US, en-cockney, i-cherokee, x-pig-latin, es-419">>, [
			{<<"en">>, 1000},
			{<<"en-us">>, 1000},
			{<<"en-cockney">>, 1000},
			{<<"i-cherokee">>, 1000},
			{<<"x-pig-latin">>, 1000},
			{<<"es-419">>, 1000}
		]}
	],
	[{V, fun() -> R = nonempty_list(V, fun language_range/2) end}
		|| {V, R} <- Tests].

nonempty_token_list_test_() ->
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
		]},
		{<<"text/html, image/gif, image/jpeg, *; q=.2, */*; q=.2">>, [
			{{<<"text">>, <<"html">>, []}, 1000, []},
			{{<<"image">>, <<"gif">>, []}, 1000, []},
			{{<<"image">>, <<"jpeg">>, []}, 1000, []},
			{{<<"*">>, <<"*">>, []}, 200, []},
			{{<<"*">>, <<"*">>, []}, 200, []}
		]}
	],
	[{V, fun() -> R = list(V, fun media_range/2) end} || {V, R} <- Tests].

entity_tag_match_test_() ->
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
	Tests = [
		{<<"Sun, 06 Nov 1994 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}},
		{<<"Sunday, 06-Nov-94 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}},
		{<<"Sun Nov  6 08:49:37 1994">>, {{1994, 11, 6}, {8, 49, 37}}}
	],
	[{V, fun() -> R = http_date(V) end} || {V, R} <- Tests].

rfc1123_date_test_() ->
	Tests = [
		{<<"Sun, 06 Nov 1994 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}}
	],
	[{V, fun() -> R = rfc1123_date(V) end} || {V, R} <- Tests].

rfc850_date_test_() ->
	Tests = [
		{<<"Sunday, 06-Nov-94 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}}
	],
	[{V, fun() -> R = rfc850_date(V) end} || {V, R} <- Tests].

asctime_date_test_() ->
	Tests = [
		{<<"Sun Nov  6 08:49:37 1994">>, {{1994, 11, 6}, {8, 49, 37}}}
	],
	[{V, fun() -> R = asctime_date(V) end} || {V, R} <- Tests].

content_type_test_() ->
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

parameterized_tokens_test_() ->
	Tests = [
		{<<"foo">>, [{<<"foo">>, []}]},
		{<<"bar; baz=2">>, [{<<"bar">>, [{<<"baz">>, <<"2">>}]}]},
		{<<"bar; baz=2;bat">>, [{<<"bar">>, [{<<"baz">>, <<"2">>}, <<"bat">>]}]},
		{<<"bar; baz=2;bat=\"z=1,2;3\"">>, [{<<"bar">>, [{<<"baz">>, <<"2">>}, {<<"bat">>, <<"z=1,2;3">>}]}]},
		{<<"foo, bar; baz=2">>, [{<<"foo">>, []}, {<<"bar">>, [{<<"baz">>, <<"2">>}]}]}
	],
	[{V, fun () -> R = parameterized_tokens(V) end} || {V, R} <- Tests].

digits_test_() ->
	Tests = [
		{<<"42    ">>, 42},
		{<<"69\t">>, 69},
		{<<"1337">>, 1337}
	],
	[{V, fun() -> R = digits(V) end} || {V, R} <- Tests].

http_authorization_test_() ->
	Tests = [
		{<<"basic">>, <<"QWxsYWRpbjpvcGVuIHNlc2FtZQ==">>,
			{<<"basic">>, {<<"Alladin">>, <<"open sesame">>}}},
		{<<"basic">>, <<"dXNlcm5hbWU6">>,
			{<<"basic">>, {<<"username">>, <<>>}}},
		{<<"basic">>, <<"dXNlcm5hbWUK">>,
			{error, badarg}},
		{<<"basic">>, <<"_[]@#$%^&*()-AA==">>,
			{error, badarg}},
		{<<"basic">>, <<"dXNlcjpwYXNzCA==">>,
			{error, badarg}},
		{<<"bearer">>, <<" some_secret_key">>,
			{<<"bearer">>,<<"some_secret_key">>}}
	],
	[{V, fun() -> R = authorization(V,T) end} || {T, V, R} <- Tests].

http_range_test_() ->
	Tests = [
		{<<"bytes=1-20">>,
			{<<"bytes">>, [{1, 20}]}},
		{<<"bytes=-100">>,
			{<<"bytes">>, [-100]}},
		{<<"bytes=1-">>,
			{<<"bytes">>, [{1, infinity}]}},
		{<<"bytes=1-20,30-40,50-">>,
			{<<"bytes">>, [{1, 20}, {30, 40}, {50, infinity}]}},
		{<<"bytes = 1 - 20 , 50 - , - 300 ">>,
			{<<"bytes">>, [{1, 20}, {50, infinity}, -300]}},
		{<<"bytes=1-20,-500,30-40">>,
			{<<"bytes">>, [{1, 20}, -500, {30, 40}]}},
		{<<"test=1-20,-500,30-40">>,
			{<<"test">>, [{1, 20}, -500, {30, 40}]}},
		{<<"bytes=-">>,
			{error, badarg}},
		{<<"bytes=-30,-">>,
			{error, badarg}}
	],
	[fun() -> R = range(V) end ||{V, R} <- Tests].
-endif.
