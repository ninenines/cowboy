%% Copyright (c) 2016, Loïc Hoguin <essen@ninenines.eu>
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

-module(cow_uri).

-export([urldecode/1]).
-export([urlencode/1]).

%% @doc Decode a percent encoded string. (RFC3986 2.1)

-spec urldecode(B) -> B when B::binary().
urldecode(B) ->
	urldecode(B, <<>>).

urldecode(<< $%, H, L, Rest/bits >>, Acc) ->
	C = (unhex(H) bsl 4 bor unhex(L)),
	urldecode(Rest, << Acc/bits, C >>);
urldecode(<< $!, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $! >>);
urldecode(<< $$, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $$ >>);
urldecode(<< $&, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $& >>);
urldecode(<< $', Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $' >>);
urldecode(<< $(, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $( >>);
urldecode(<< $), Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $) >>);
urldecode(<< $*, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $* >>);
urldecode(<< $+, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $+ >>);
urldecode(<< $,, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $, >>);
urldecode(<< $-, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $- >>);
urldecode(<< $., Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $. >>);
urldecode(<< $0, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $0 >>);
urldecode(<< $1, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $1 >>);
urldecode(<< $2, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $2 >>);
urldecode(<< $3, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $3 >>);
urldecode(<< $4, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $4 >>);
urldecode(<< $5, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $5 >>);
urldecode(<< $6, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $6 >>);
urldecode(<< $7, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $7 >>);
urldecode(<< $8, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $8 >>);
urldecode(<< $9, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $9 >>);
urldecode(<< $:, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $: >>);
urldecode(<< $;, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $; >>);
urldecode(<< $=, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $= >>);
urldecode(<< $@, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $@ >>);
urldecode(<< $A, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $A >>);
urldecode(<< $B, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $B >>);
urldecode(<< $C, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $C >>);
urldecode(<< $D, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $D >>);
urldecode(<< $E, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $E >>);
urldecode(<< $F, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $F >>);
urldecode(<< $G, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $G >>);
urldecode(<< $H, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $H >>);
urldecode(<< $I, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $I >>);
urldecode(<< $J, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $J >>);
urldecode(<< $K, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $K >>);
urldecode(<< $L, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $L >>);
urldecode(<< $M, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $M >>);
urldecode(<< $N, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $N >>);
urldecode(<< $O, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $O >>);
urldecode(<< $P, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $P >>);
urldecode(<< $Q, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $Q >>);
urldecode(<< $R, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $R >>);
urldecode(<< $S, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $S >>);
urldecode(<< $T, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $T >>);
urldecode(<< $U, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $U >>);
urldecode(<< $V, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $V >>);
urldecode(<< $W, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $W >>);
urldecode(<< $X, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $X >>);
urldecode(<< $Y, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $Y >>);
urldecode(<< $Z, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $Z >>);
urldecode(<< $_, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $_ >>);
urldecode(<< $a, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $a >>);
urldecode(<< $b, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $b >>);
urldecode(<< $c, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $c >>);
urldecode(<< $d, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $d >>);
urldecode(<< $e, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $e >>);
urldecode(<< $f, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $f >>);
urldecode(<< $g, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $g >>);
urldecode(<< $h, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $h >>);
urldecode(<< $i, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $i >>);
urldecode(<< $j, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $j >>);
urldecode(<< $k, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $k >>);
urldecode(<< $l, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $l >>);
urldecode(<< $m, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $m >>);
urldecode(<< $n, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $n >>);
urldecode(<< $o, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $o >>);
urldecode(<< $p, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $p >>);
urldecode(<< $q, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $q >>);
urldecode(<< $r, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $r >>);
urldecode(<< $s, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $s >>);
urldecode(<< $t, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $t >>);
urldecode(<< $u, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $u >>);
urldecode(<< $v, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $v >>);
urldecode(<< $w, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $w >>);
urldecode(<< $x, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $x >>);
urldecode(<< $y, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $y >>);
urldecode(<< $z, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $z >>);
urldecode(<< $~, Rest/bits >>, Acc) -> urldecode(Rest, << Acc/bits, $~ >>);
urldecode(<<>>, Acc) -> Acc.

unhex($0) ->  0;
unhex($1) ->  1;
unhex($2) ->  2;
unhex($3) ->  3;
unhex($4) ->  4;
unhex($5) ->  5;
unhex($6) ->  6;
unhex($7) ->  7;
unhex($8) ->  8;
unhex($9) ->  9;
unhex($A) -> 10;
unhex($B) -> 11;
unhex($C) -> 12;
unhex($D) -> 13;
unhex($E) -> 14;
unhex($F) -> 15;
unhex($a) -> 10;
unhex($b) -> 11;
unhex($c) -> 12;
unhex($d) -> 13;
unhex($e) -> 14;
unhex($f) -> 15.

-ifdef(TEST).
urldecode_test_() ->
	Tests = [
		{<<"%20">>, <<" ">>},
		{<<"+">>, <<"+">>},
		{<<"%00">>, <<0>>},
		{<<"%fF">>, <<255>>},
		{<<"123">>, <<"123">>},
		{<<"%i5">>, error},
		{<<"%5">>, error}
	],
	[{Qs, fun() ->
		E = try urldecode(Qs) of
			R -> R
		catch _:_ ->
			error
		end
	end} || {Qs, E} <- Tests].

urldecode_identity_test_() ->
	Tests = [
		<<"%20">>,
		<<"+">>,
		<<"nothingnothingnothingnothing">>,
		<<"Small+fast+modular+HTTP+server">>,
		<<"Small%20fast%20modular%20HTTP%20server">>,
		<<"Small%2F+fast%2F+modular+HTTP+server.">>,
		<<"%E3%83%84%E3%82%A4%E3%83%B3%E3%82%BD%E3%82%A6%E3%83"
			"%AB%E3%80%9C%E8%BC%AA%E5%BB%BB%E3%81%99%E3%82%8B%E6%97%8B%E5"
			"%BE%8B%E3%80%9C">>
	],
	[{V, fun() -> V = urlencode(urldecode(V)) end} || V <- Tests].

horse_urldecode() ->
	horse:repeat(100000,
		urldecode(<<"nothingnothingnothingnothing">>)
	).

horse_urldecode_hex() ->
	horse:repeat(100000,
		urldecode(<<"Small%2C%20fast%2C%20modular%20HTTP%20server.">>)
	).

horse_urldecode_jp_hex() ->
	horse:repeat(100000,
		urldecode(<<"%E3%83%84%E3%82%A4%E3%83%B3%E3%82%BD%E3%82%A6%E3%83"
			"%AB%E3%80%9C%E8%BC%AA%E5%BB%BB%E3%81%99%E3%82%8B%E6%97%8B%E5"
			"%BE%8B%E3%80%9C">>)
	).
-endif.

%% @doc Percent encode a string. (RFC3986 2.1)
%%
%% This function is meant to be used for path components.

-spec urlencode(B) -> B when B::binary().
urlencode(B) ->
	urlencode(B, <<>>).

urlencode(<< $!, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $! >>);
urlencode(<< $$, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $$ >>);
urlencode(<< $&, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $& >>);
urlencode(<< $', Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $' >>);
urlencode(<< $(, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $( >>);
urlencode(<< $), Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $) >>);
urlencode(<< $*, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $* >>);
urlencode(<< $+, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $+ >>);
urlencode(<< $,, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $, >>);
urlencode(<< $-, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $- >>);
urlencode(<< $., Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $. >>);
urlencode(<< $0, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $0 >>);
urlencode(<< $1, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $1 >>);
urlencode(<< $2, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $2 >>);
urlencode(<< $3, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $3 >>);
urlencode(<< $4, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $4 >>);
urlencode(<< $5, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $5 >>);
urlencode(<< $6, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $6 >>);
urlencode(<< $7, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $7 >>);
urlencode(<< $8, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $8 >>);
urlencode(<< $9, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $9 >>);
urlencode(<< $:, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $: >>);
urlencode(<< $;, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $; >>);
urlencode(<< $=, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $= >>);
urlencode(<< $@, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $@ >>);
urlencode(<< $A, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $A >>);
urlencode(<< $B, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $B >>);
urlencode(<< $C, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $C >>);
urlencode(<< $D, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $D >>);
urlencode(<< $E, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $E >>);
urlencode(<< $F, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $F >>);
urlencode(<< $G, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $G >>);
urlencode(<< $H, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $H >>);
urlencode(<< $I, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $I >>);
urlencode(<< $J, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $J >>);
urlencode(<< $K, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $K >>);
urlencode(<< $L, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $L >>);
urlencode(<< $M, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $M >>);
urlencode(<< $N, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $N >>);
urlencode(<< $O, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $O >>);
urlencode(<< $P, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $P >>);
urlencode(<< $Q, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Q >>);
urlencode(<< $R, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $R >>);
urlencode(<< $S, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $S >>);
urlencode(<< $T, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $T >>);
urlencode(<< $U, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $U >>);
urlencode(<< $V, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $V >>);
urlencode(<< $W, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $W >>);
urlencode(<< $X, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $X >>);
urlencode(<< $Y, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Y >>);
urlencode(<< $Z, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $Z >>);
urlencode(<< $_, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $_ >>);
urlencode(<< $a, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $a >>);
urlencode(<< $b, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $b >>);
urlencode(<< $c, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $c >>);
urlencode(<< $d, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $d >>);
urlencode(<< $e, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $e >>);
urlencode(<< $f, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $f >>);
urlencode(<< $g, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $g >>);
urlencode(<< $h, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $h >>);
urlencode(<< $i, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $i >>);
urlencode(<< $j, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $j >>);
urlencode(<< $k, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $k >>);
urlencode(<< $l, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $l >>);
urlencode(<< $m, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $m >>);
urlencode(<< $n, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $n >>);
urlencode(<< $o, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $o >>);
urlencode(<< $p, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $p >>);
urlencode(<< $q, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $q >>);
urlencode(<< $r, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $r >>);
urlencode(<< $s, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $s >>);
urlencode(<< $t, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $t >>);
urlencode(<< $u, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $u >>);
urlencode(<< $v, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $v >>);
urlencode(<< $w, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $w >>);
urlencode(<< $x, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $x >>);
urlencode(<< $y, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $y >>);
urlencode(<< $z, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $z >>);
urlencode(<< $~, Rest/bits >>, Acc) -> urlencode(Rest, << Acc/bits, $~ >>);
urlencode(<< C, Rest/bits >>, Acc) ->
	H = hex(C bsr 4),
	L = hex(C band 16#0f),
	urlencode(Rest, << Acc/bits, $%, H, L >>);
urlencode(<<>>, Acc) ->
	Acc.

hex( 0) -> $0;
hex( 1) -> $1;
hex( 2) -> $2;
hex( 3) -> $3;
hex( 4) -> $4;
hex( 5) -> $5;
hex( 6) -> $6;
hex( 7) -> $7;
hex( 8) -> $8;
hex( 9) -> $9;
hex(10) -> $A;
hex(11) -> $B;
hex(12) -> $C;
hex(13) -> $D;
hex(14) -> $E;
hex(15) -> $F.

-ifdef(TEST).
urlencode_test_() ->
	Tests = [
		{<<255, 0>>, <<"%FF%00">>},
		{<<255, " ">>, <<"%FF%20">>},
		{<<"+">>, <<"+">>},
		{<<"aBc123">>, <<"aBc123">>},
		{<<"!$&'()*+,:;=@-._~">>, <<"!$&'()*+,:;=@-._~">>}
	],
	[{V, fun() -> E = urlencode(V) end} || {V, E} <- Tests].

urlencode_identity_test_() ->
	Tests = [
		<<"+">>,
		<<"nothingnothingnothingnothing">>,
		<<"Small fast modular HTTP server">>,
		<<"Small, fast, modular HTTP server.">>,
		<<227,131,132,227,130,164,227,131,179,227,130,189,227,
			130,166,227,131,171,227,128,156,232,188,170,229,187,187,227,
			129,153,227,130,139,230,151,139,229,190,139,227,128,156>>
	],
	[{V, fun() -> V = urldecode(urlencode(V)) end} || V <- Tests].

horse_urlencode() ->
	horse:repeat(100000,
		urlencode(<<"nothingnothingnothingnothing">>)
	).

horse_urlencode_spaces() ->
	horse:repeat(100000,
		urlencode(<<"Small fast modular HTTP server">>)
	).

horse_urlencode_jp() ->
	horse:repeat(100000,
		urlencode(<<227,131,132,227,130,164,227,131,179,227,130,189,227,
			130,166,227,131,171,227,128,156,232,188,170,229,187,187,227,
			129,153,227,130,139,230,151,139,229,190,139,227,128,156>>)
	).

horse_urlencode_mix() ->
	horse:repeat(100000,
		urlencode(<<"Small, fast, modular HTTP server.">>)
	).
-endif.
