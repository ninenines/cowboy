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

%% @doc Multipart parser.
-module(cowboy_multipart).

-export([parser/1]).
-export([content_disposition/1]).

-type part_parser() :: parser(more(part_result())).
-type parser(T) :: fun((binary()) -> T).
-type more(T) :: T | {more, parser(T)}.
-type part_result() :: headers() | eof.
-type headers() :: {headers, http_headers(), body_cont()}.
-type http_headers() :: [{binary(), binary()}].
-type body_cont() :: cont(more(body_result())).
-type cont(T) :: fun(() -> T).
-type body_result() :: {body, binary(), body_cont()} | end_of_part().
-type end_of_part() :: {end_of_part, cont(more(part_result()))}.
-type disposition() :: {binary(), [{binary(), binary()}]}.

%% API.

%% @doc Return a multipart parser for the given boundary.
-spec parser(binary()) -> part_parser().
parser(Boundary) when is_binary(Boundary) ->
	fun (Bin) when is_binary(Bin) -> parse(Bin, Boundary) end.

%% @doc Parse a content disposition.
%% @todo Parse the MIME header instead of the HTTP one.
-spec content_disposition(binary()) -> disposition().
content_disposition(Data) ->
	cowboy_http:token_ci(Data,
		fun (_Rest, <<>>) -> {error, badarg};
			(Rest, Disposition) ->
				cowboy_http:params(Rest,
					fun (<<>>, Params) -> {Disposition, Params};
						(_Rest2, _) -> {error, badarg}
					end)
		end).

%% Internal.

%% @doc Entry point of the multipart parser, skips over the preamble if any.
-spec parse(binary(), binary()) -> more(part_result()).
parse(Bin, Boundary) when byte_size(Bin) >= byte_size(Boundary) + 2 ->
	BoundarySize = byte_size(Boundary),
	Pattern = pattern(Boundary),
	case Bin of
		<<"--", Boundary:BoundarySize/binary, Rest/binary>> ->
			% Data starts with initial boundary, skip preamble parsing.
			parse_boundary_tail(Rest, Pattern);
		_ ->
			% Parse preamble.
			skip(Bin, Pattern)
	end;
parse(Bin, Boundary) ->
	% Not enough data to know if the data begins with a boundary.
	more(Bin, fun (NewBin) -> parse(NewBin, Boundary) end).

-type pattern() :: {binary:cp(), non_neg_integer()}.
-type patterns() :: {pattern(), pattern()}.

%% @doc Return two compiled binary patterns with their sizes in bytes.
%% The boundary pattern is the boundary prepended with "\r\n--".
%% The boundary suffix pattern matches all prefixes of the boundary.
-spec pattern(binary()) -> patterns().
pattern(Boundary) ->
	MatchPattern = <<"\r\n--", Boundary/binary>>,
	MatchPrefixes = prefixes(MatchPattern),
	{{binary:compile_pattern(MatchPattern), byte_size(MatchPattern)},
	 {binary:compile_pattern(MatchPrefixes), byte_size(MatchPattern)}}.

%% @doc Return all prefixes of a binary string.
%% The list of prefixes includes the full string.
-spec prefixes(binary()) -> [binary()].
prefixes(<<C, Rest/binary>>) ->
	prefixes(Rest, <<C>>).

-spec prefixes(binary(), binary()) -> [binary()].
prefixes(<<C, Rest/binary>>, Acc) ->
	[Acc|prefixes(Rest, <<Acc/binary, C>>)];
prefixes(<<>>, Acc) ->
	[Acc].

%% @doc Test if a boundary is a possble suffix.
%% The patterns are expected to have been returned from `pattern/1'.
-spec suffix_match(binary(), patterns()) -> nomatch | {integer(), integer()}.
suffix_match(Bin, {_Boundary, {Pat, Len}}) ->
	Size = byte_size(Bin),
	suffix_match(Bin, Pat, Size, max(-Size, -Len)).

-spec suffix_match(binary(), binary:cp(), non_neg_integer(), 0|neg_integer()) ->
		nomatch | {integer(), integer()}.
suffix_match(_Bin, _Pat, _Size, _Match=0) ->
	nomatch;
suffix_match(Bin, Pat, Size, Match) when Match < 0 ->
	case binary:match(Bin, Pat, [{scope, {Size, Match}}]) of
		{Pos, Len}=Part when Pos + Len =:= Size -> Part;
		{_, Len} -> suffix_match(Bin, Pat, Size, Match + Len);
		nomatch -> nomatch
	end.

%% @doc Parse remaining characters of a line beginning with the boundary.
%% If followed by "--", <em>eof</em> is returned and parsing is finished.
-spec parse_boundary_tail(binary(), patterns()) -> more(part_result()).
parse_boundary_tail(Bin, Pattern) when byte_size(Bin) >= 2 ->
	case Bin of
		<<"--", _Rest/binary>> ->
			% Boundary is followed by "--", end parsing.
			eof;
		_ ->
			% No dash after boundary, proceed with unknown chars and lwsp
			% removal.
			parse_boundary_eol(Bin, Pattern)
	end;
parse_boundary_tail(Bin, Pattern) ->
	% Boundary may be followed by "--", need more data.
	more(Bin, fun (NewBin) -> parse_boundary_tail(NewBin, Pattern) end).

%% @doc Skip whitespace and unknown chars until CRLF.
-spec parse_boundary_eol(binary(), patterns()) -> more(part_result()).
parse_boundary_eol(Bin, Pattern) ->
	case binary:match(Bin, <<"\r\n">>) of
		{CrlfStart, _Length} ->
			% End of line found, remove optional whitespace.
			<<_:CrlfStart/binary, Rest/binary>> = Bin,
			Fun = fun (Rest2) -> parse_boundary_crlf(Rest2, Pattern) end,
			cowboy_http:whitespace(Rest, Fun);
		nomatch ->
			% CRLF not found in the given binary.
			RestStart = max(byte_size(Bin) - 1, 0),
			<<_:RestStart/binary, Rest/binary>> = Bin,
			more(Rest, fun (NewBin) -> parse_boundary_eol(NewBin, Pattern) end)
	end.

-spec parse_boundary_crlf(binary(), patterns()) -> more(part_result()).
parse_boundary_crlf(<<"\r\n", Rest/binary>>, Pattern) ->
	% The binary is at least 2 bytes long as this function is only called by
	% parse_boundary_eol/3 when CRLF has been found so a more tuple will never
	% be returned from here.
	parse_headers(Rest, Pattern);
parse_boundary_crlf(Bin, Pattern) ->
	% Unspecified behaviour here: RFC 2046 doesn't say what to do when LWSP is
	% not followed directly by a new line. In this implementation it is
	% considered part of the boundary so EOL needs to be searched again.
	parse_boundary_eol(Bin, Pattern).

-spec parse_headers(binary(), patterns()) -> more(part_result()).
parse_headers(Bin, Pattern) ->
  parse_headers(Bin, Pattern, []).

-spec parse_headers(binary(), patterns(), http_headers()) -> more(part_result()).
parse_headers(Bin, Pattern, Acc) ->
	case erlang:decode_packet(httph_bin, Bin, []) of
		{ok, {http_header, _, Name, _, Value}, Rest} ->
			Name2 = case is_atom(Name) of
				true -> cowboy_bstr:to_lower(atom_to_binary(Name, latin1));
				false -> cowboy_bstr:to_lower(Name)
			end,
			parse_headers(Rest, Pattern, [{Name2, Value} | Acc]);
		{ok, http_eoh, Rest} ->
			Headers = lists:reverse(Acc),
			{headers, Headers, fun () -> parse_body(Rest, Pattern) end};
		{ok, {http_error, _}, _} ->
			% Skip malformed parts.
			skip(Bin, Pattern);
		{more, _} ->
			more(Bin, fun (NewBin) -> parse_headers(NewBin, Pattern, Acc) end)
	end.

-spec parse_body(binary(), patterns()) -> more(body_result()).
parse_body(Bin, Pattern = {{P, PSize}, _}) when byte_size(Bin) >= PSize ->
	case binary:match(Bin, P) of
		{0, _Length} ->
			<<_:PSize/binary, Rest/binary>> = Bin,
			end_of_part(Rest, Pattern);
		{BoundaryStart, _Length} ->
			% Boundary found, this is the latest partial body that will be
			% returned for this part.
			<<PBody:BoundaryStart/binary, _:PSize/binary, Rest/binary>> = Bin,
			FResult = end_of_part(Rest, Pattern),
			{body, PBody, fun () -> FResult end};
		nomatch ->
			case suffix_match(Bin, Pattern) of
				nomatch ->
					%% Prefix of boundary not found at end of input. it's
					%% safe to return the whole binary. Saves copying of
					%% next input onto tail of current input binary.
					{body, Bin, fun () -> parse_body(<<>>, Pattern) end};
				{BoundaryStart, Len} ->
					PBody = binary:part(Bin, 0, BoundaryStart),
					Rest = binary:part(Bin, BoundaryStart, Len),
					{body, PBody, fun () -> parse_body(Rest, Pattern) end}
			end
	end;
parse_body(Bin, Pattern) ->
	more(Bin, fun (NewBin) -> parse_body(NewBin, Pattern) end).

-spec end_of_part(binary(), patterns()) -> end_of_part().
end_of_part(Bin, Pattern) ->
	{end_of_part, fun () -> parse_boundary_tail(Bin, Pattern) end}.

-spec skip(binary(), patterns()) -> more(part_result()).
skip(Bin, Pattern = {{P, PSize}, _}) ->
	case binary:match(Bin, P) of
		{BoundaryStart, _Length} ->
			% Boundary found, proceed with parsing of the next part.
			RestStart = BoundaryStart + PSize,
			<<_:RestStart/binary, Rest/binary>> = Bin,
			parse_boundary_tail(Rest, Pattern);
		nomatch ->
			% Boundary not found, need more data.
			RestStart = max(byte_size(Bin) - PSize + 1, 0),
			<<_:RestStart/binary, Rest/binary>> = Bin,
			more(Rest, fun (NewBin) -> skip(NewBin, Pattern) end)
	end.

-spec more(binary(), parser(T)) -> {more, parser(T)}.
more(<<>>, F) ->
	{more, F};
more(Bin, InnerF) ->
	F = fun (NewData) when is_binary(NewData) ->
				InnerF(<<Bin/binary, NewData/binary>>)
		end,
	{more, F}.

%% Tests.

-ifdef(TEST).

multipart_test_() ->
	%% {Body, Result}
	Tests = [
		{<<"--boundary--">>, []},
		{<<"preamble\r\n--boundary--">>, []},
		{<<"--boundary--\r\nepilogue">>, []},
		{<<"\r\n--boundary\r\nA:b\r\nC:d\r\n\r\n\r\n--boundary--">>,
			[{[{<<"a">>, <<"b">>}, {<<"c">>, <<"d">>}], <<>>}]},
		{
			<<
				"--boundary\r\nX-Name:answer\r\n\r\n42"
				"\r\n--boundary\r\nServer:Cowboy\r\n\r\nIt rocks!\r\n"
				"\r\n--boundary--"
			>>,
			[
				{[{<<"x-name">>, <<"answer">>}], <<"42">>},
				{[{<<"server">>, <<"Cowboy">>}], <<"It rocks!\r\n">>}
			]
		}
	],
	[{title(V), fun () -> R = acc_multipart(V) end} || {V, R} <- Tests].

acc_multipart(V) ->
	acc_multipart((parser(<<"boundary">>))(V), []).

acc_multipart({headers, Headers, Cont}, Acc) ->
	acc_multipart(Cont(), [{Headers, []}|Acc]);
acc_multipart({body, Body, Cont}, [{Headers, BodyAcc}|Acc]) ->
	acc_multipart(Cont(), [{Headers, [Body|BodyAcc]}|Acc]);
acc_multipart({end_of_part, Cont}, [{Headers, BodyAcc}|Acc]) ->
	Body = list_to_binary(lists:reverse(BodyAcc)),
	acc_multipart(Cont(), [{Headers, Body}|Acc]);
acc_multipart(eof, Acc) ->
	lists:reverse(Acc).

content_disposition_test_() ->
	%% {Disposition, Result}
	Tests = [
		{<<"form-data; name=id">>, {<<"form-data">>, [{<<"name">>, <<"id">>}]}},
		{<<"inline">>, {<<"inline">>, []}},
		{<<"attachment; \tfilename=brackets-slides.pdf">>,
			{<<"attachment">>, [{<<"filename">>, <<"brackets-slides.pdf">>}]}}
	],
	[{title(V), fun () -> R = content_disposition(V) end} || {V, R} <- Tests].

title(Bin) ->
	Title = lists:foldl(
		fun ({T, R}, V) -> re:replace(V, T, R, [global]) end,
		Bin,
		[{"\t", "\\\\t"}, {"\r", "\\\\r"}, {"\n", "\\\\n"}]
	),
	iolist_to_binary(Title).

suffix_test_() ->
	Tests = [
		{nomatch, <<>>, <<"ABC">>},
		{{0, 1}, <<"\r">>, <<"ABC">>},
		{{0, 2}, <<"\r\n">>, <<"ABC">>},
		{{0, 4}, <<"\r\n--">>, <<"ABC">>},
		{{0, 5}, <<"\r\n--A">>, <<"ABC">>},
		{{0, 6}, <<"\r\n--AB">>, <<"ABC">>},
		{{0, 7}, <<"\r\n--ABC">>, <<"ABC">>},
		{nomatch, <<"\r\n--AB1">>, <<"ABC">>},
		{{1, 1}, <<"1\r">>, <<"ABC">>},
		{{2, 2}, <<"12\r\n">>, <<"ABC">>},
		{{3, 4}, <<"123\r\n--">>, <<"ABC">>}
	],
	[fun() -> Part = suffix_match(Packet, pattern(Boundary)) end ||
		{Part, Packet, Boundary} <- Tests].

-endif.
