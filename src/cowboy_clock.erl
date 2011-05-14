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

-module(cowboy_clock).
-behaviour(gen_server).

-export([start_link/0, stop/0, rfc1123/0]). %% API.
-export([init/1, handle_call/3, handle_cast/2,
	handle_info/2, terminate/2, code_change/3]). %% gen_server.

%% @todo Use calendar types whenever they get exported.
-type year()   :: non_neg_integer().
-type month()  :: 1..12.
-type day()    :: 1..31.
-type hour()   :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59.
-type daynum() :: 1..7.

-type date() :: {year(), month(), day()}.
-type time() :: {hour(), minute(), second()}.

-type datetime() :: {date(), time()}.

-record(state, {
	universaltime = undefined :: undefined | datetime(),
	rfc1123 = <<>> :: binary(),
	tref = undefined :: undefined | timer:tref()
}).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-include_lib("eunit/include/eunit.hrl").

%% API.

-spec start_link() -> {ok, Pid::pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> stopped.
stop() ->
	gen_server:call(?SERVER, stop).

-spec rfc1123() -> binary().
rfc1123() ->
	ets:lookup_element(?TABLE, rfc1123, 2).

%% gen_server.

init([]) ->
	?TABLE = ets:new(?TABLE, [set, protected,
		named_table, {read_concurrency, true}]),
	T = erlang:universaltime(),
	B = update_rfc1123(undefined, T, <<>>),
	{ok, TRef} = timer:send_interval(10, update),
	ets:insert(?TABLE, {rfc1123, B}),
	{ok, #state{universaltime=T, rfc1123=B, tref=TRef}}.

handle_call(stop, _From, State=#state{tref=TRef}) ->
	{ok, cancel} = timer:cancel(TRef),
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(update, #state{universaltime=Prev, rfc1123=B1, tref=TRef}) ->
	T = erlang:universaltime(),
	B2 = update_rfc1123(Prev, T, B1),
	ets:insert(?TABLE, {rfc1123, B2}),
	{noreply, #state{universaltime=T, rfc1123=B2, tref=TRef}};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

-spec update_rfc1123(Prev::undefined | datetime(), Now::datetime(),
	Bin::binary()) -> binary().
update_rfc1123(Now, Now, Bin) ->
	Bin;
update_rfc1123({Date, {H, M, _}}, {Date, {H, M, S}},
		<< Keep:23/binary, _/bits >>) ->
	<< Keep/binary, (pad_int(S))/binary, " GMT" >>;
update_rfc1123({Date, {H, _, _}}, {Date, {H, M, S}},
		<< Keep:20/binary, _/bits >>) ->
	<< Keep/binary, (pad_int(M))/binary, $:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123({Date, _}, {Date, {H, M, S}}, << Keep:17/binary, _/bits >>) ->
	<< Keep/binary, (pad_int(H))/binary, $:, (pad_int(M))/binary,
		$:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123({{Y, Mo, _}, _}, {Date = {Y, Mo, D}, {H, M, S}},
		<< _:7/binary, Keep:10/binary, _/bits >>) ->
	Wday = calendar:day_of_the_week(Date),
	<< (weekday(Wday))/binary, ", ", (pad_int(D))/binary, Keep/binary,
		(pad_int(H))/binary, $:, (pad_int(M))/binary,
		$:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123({{Y, _, _}, _}, {Date = {Y, Mo, D}, {H, M, S}},
		<< _:11/binary, Keep:6/binary, _/bits >>) ->
	Wday = calendar:day_of_the_week(Date),
	<< (weekday(Wday))/binary, ", ", (pad_int(D))/binary, " ",
		(month(Mo))/binary, Keep/binary,
		(pad_int(H))/binary, $:, (pad_int(M))/binary,
		$:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(_, {Date = {Y, Mo, D}, {H, M, S}}, _) ->
	Wday = calendar:day_of_the_week(Date),
	<< (weekday(Wday))/binary, ", ", (pad_int(D))/binary, " ",
		(month(Mo))/binary, " ", (list_to_binary(integer_to_list(Y)))/binary,
		" ", (pad_int(H))/binary, $:, (pad_int(M))/binary,
		$:, (pad_int(S))/binary, " GMT" >>.

%% Following suggestion by MononcQc on #erlounge.
-spec pad_int(0..59) -> binary().
pad_int(X) when X < 10 ->
	<< $0, ($0 + X) >>;
pad_int(X) ->
	list_to_binary(integer_to_list(X)).

-spec weekday(daynum()) -> binary().
weekday(1) -> <<"Mon">>;
weekday(2) -> <<"Tue">>;
weekday(3) -> <<"Wed">>;
weekday(4) -> <<"Thu">>;
weekday(5) -> <<"Fri">>;
weekday(6) -> <<"Sat">>;
weekday(7) -> <<"Sun">>.

-spec month(month()) -> binary().
month( 1) -> <<"Jan">>;
month( 2) -> <<"Feb">>;
month( 3) -> <<"Mar">>;
month( 4) -> <<"Apr">>;
month( 5) -> <<"May">>;
month( 6) -> <<"Jun">>;
month( 7) -> <<"Jul">>;
month( 8) -> <<"Aug">>;
month( 9) -> <<"Sep">>;
month(10) -> <<"Oct">>;
month(11) -> <<"Nov">>;
month(12) -> <<"Dec">>.

%% Tests.

-ifdef(TEST).

update_rfc1123_test_() ->
	Tests = [
		{<<"Sat, 14 May 2011 14:25:33 GMT">>, undefined,
			{{2011, 5, 14}, {14, 25, 33}}, <<>>},
		{<<"Sat, 14 May 2011 14:25:33 GMT">>, {{2011, 5, 14}, {14, 25, 33}},
			{{2011, 5, 14}, {14, 25, 33}}, <<"Sat, 14 May 2011 14:25:33 GMT">>},
		{<<"Sat, 14 May 2011 14:25:34 GMT">>, {{2011, 5, 14}, {14, 25, 33}},
			{{2011, 5, 14}, {14, 25, 34}}, <<"Sat, 14 May 2011 14:25:33 GMT">>},
		{<<"Sat, 14 May 2011 14:26:00 GMT">>, {{2011, 5, 14}, {14, 25, 59}},
			{{2011, 5, 14}, {14, 26,  0}}, <<"Sat, 14 May 2011 14:25:59 GMT">>},
		{<<"Sat, 14 May 2011 15:00:00 GMT">>, {{2011, 5, 14}, {14, 59, 59}},
			{{2011, 5, 14}, {15,  0,  0}}, <<"Sat, 14 May 2011 14:59:59 GMT">>},
		{<<"Sun, 15 May 2011 00:00:00 GMT">>, {{2011, 5, 14}, {23, 59, 59}},
			{{2011, 5, 15}, { 0,  0,  0}}, <<"Sat, 14 May 2011 23:59:59 GMT">>},
		{<<"Wed, 01 Jun 2011 00:00:00 GMT">>, {{2011, 5, 31}, {23, 59, 59}},
			{{2011, 6,  1}, { 0,  0,  0}}, <<"Tue, 31 May 2011 23:59:59 GMT">>},
		{<<"Sun, 01 Jan 2012 00:00:00 GMT">>, {{2011, 5, 31}, {23, 59, 59}},
			{{2012, 1,  1}, { 0,  0,  0}}, <<"Sat, 31 Dec 2011 23:59:59 GMT">>}
	],
	[{R, fun() -> R = update_rfc1123(P, N, B) end} || {R, P, N, B} <- Tests].

pad_int_test_() ->
	Tests = [
		{ 0, <<"00">>}, { 1, <<"01">>}, { 2, <<"02">>}, { 3, <<"03">>},
		{ 4, <<"04">>}, { 5, <<"05">>}, { 6, <<"06">>}, { 7, <<"07">>},
		{ 8, <<"08">>}, { 9, <<"09">>}, {10, <<"10">>}, {11, <<"11">>},
		{12, <<"12">>}, {13, <<"13">>}, {14, <<"14">>}, {15, <<"15">>},
		{16, <<"16">>}, {17, <<"17">>}, {18, <<"18">>}, {19, <<"19">>},
		{20, <<"20">>}, {21, <<"21">>}, {22, <<"22">>}, {23, <<"23">>},
		{24, <<"24">>}, {25, <<"25">>}, {26, <<"26">>}, {27, <<"27">>},
		{28, <<"28">>}, {29, <<"29">>}, {30, <<"30">>}, {31, <<"31">>},
		{32, <<"32">>}, {33, <<"33">>}, {34, <<"34">>}, {35, <<"35">>},
		{36, <<"36">>}, {37, <<"37">>}, {38, <<"38">>}, {39, <<"39">>},
		{40, <<"40">>}, {41, <<"41">>}, {42, <<"42">>}, {43, <<"43">>},
		{44, <<"44">>}, {45, <<"45">>}, {46, <<"46">>}, {47, <<"47">>},
		{48, <<"48">>}, {49, <<"49">>}, {50, <<"50">>}, {51, <<"51">>},
		{52, <<"52">>}, {53, <<"53">>}, {54, <<"54">>}, {55, <<"55">>},
		{56, <<"56">>}, {57, <<"57">>}, {58, <<"58">>}, {59, <<"59">>}
	],
	[{I, fun() -> O = pad_int(I) end} || {I, O} <- Tests].

-endif.
