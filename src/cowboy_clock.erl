%% Copyright (c) 2011-2024, Loïc Hoguin <essen@ninenines.eu>
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

%% While a gen_server process runs in the background to update
%% the cache of formatted dates every second, all API calls are
%% local and directly read from the ETS cache table, providing
%% fast time and date computations.
-module(cowboy_clock).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([stop/0]).
-export([rfc1123/0]).
-export([rfc1123/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	universaltime = undefined :: undefined | calendar:datetime(),
	rfc1123 = <<>> :: binary(),
	tref = undefined :: undefined | reference()
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> stopped.
stop() ->
	gen_server:call(?MODULE, stop).

%% When the ets table doesn't exist, either because of a bug
%% or because Cowboy is being restarted, we perform in a
%% slightly degraded state and build a new timestamp for
%% every request.
-spec rfc1123() -> binary().
rfc1123() ->
	try
		ets:lookup_element(?MODULE, rfc1123, 2)
	catch error:badarg ->
		rfc1123(erlang:universaltime())
	end.

-spec rfc1123(calendar:datetime()) -> binary().
rfc1123(DateTime) ->
	update_rfc1123(<<>>, undefined, DateTime).

%% gen_server.

-spec init([]) -> {ok, #state{}}.
init([]) ->
	?MODULE = ets:new(?MODULE, [set, protected,
		named_table, {read_concurrency, true}]),
	T = erlang:universaltime(),
	B = update_rfc1123(<<>>, undefined, T),
	TRef = erlang:send_after(1000, self(), update),
	ets:insert(?MODULE, {rfc1123, B}),
	{ok, #state{universaltime=T, rfc1123=B, tref=TRef}}.

-type from() :: {pid(), term()}.
-spec handle_call
	(stop, from(), State) -> {stop, normal, stopped, State}
	when State::#state{}.
handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

-spec handle_cast(_, State) -> {noreply, State} when State::#state{}.
handle_cast(_Msg, State) ->
	{noreply, State}.

-spec handle_info(any(), State) -> {noreply, State} when State::#state{}.
handle_info(update, #state{universaltime=Prev, rfc1123=B1, tref=TRef0}) ->
	%% Cancel the timer in case an external process sent an update message.
	_ = erlang:cancel_timer(TRef0),
	T = erlang:universaltime(),
	B2 = update_rfc1123(B1, Prev, T),
	ets:insert(?MODULE, {rfc1123, B2}),
	TRef = erlang:send_after(1000, self(), update),
	{noreply, #state{universaltime=T, rfc1123=B2, tref=TRef}};
handle_info(_Info, State) ->
	{noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
	ok.

-spec code_change(_, State, _) -> {ok, State} when State::#state{}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

-spec update_rfc1123(binary(), undefined | calendar:datetime(),
	calendar:datetime()) -> binary().
update_rfc1123(Bin, Now, Now) ->
	Bin;
update_rfc1123(<< Keep:23/binary, _/bits >>,
		{Date, {H, M, _}}, {Date, {H, M, S}}) ->
	<< Keep/binary, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< Keep:20/binary, _/bits >>,
		{Date, {H, _, _}}, {Date, {H, M, S}}) ->
	<< Keep/binary, (pad_int(M))/binary, $:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< Keep:17/binary, _/bits >>, {Date, _}, {Date, {H, M, S}}) ->
	<< Keep/binary, (pad_int(H))/binary, $:, (pad_int(M))/binary,
		$:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< _:7/binary, Keep:10/binary, _/bits >>,
		{{Y, Mo, _}, _}, {Date = {Y, Mo, D}, {H, M, S}}) ->
	Wday = calendar:day_of_the_week(Date),
	<< (weekday(Wday))/binary, ", ", (pad_int(D))/binary, Keep/binary,
		(pad_int(H))/binary, $:, (pad_int(M))/binary,
		$:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< _:11/binary, Keep:6/binary, _/bits >>,
		{{Y, _, _}, _}, {Date = {Y, Mo, D}, {H, M, S}}) ->
	Wday = calendar:day_of_the_week(Date),
	<< (weekday(Wday))/binary, ", ", (pad_int(D))/binary, " ",
		(month(Mo))/binary, Keep/binary,
		(pad_int(H))/binary, $:, (pad_int(M))/binary,
		$:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(_, _, {Date = {Y, Mo, D}, {H, M, S}}) ->
	Wday = calendar:day_of_the_week(Date),
	<< (weekday(Wday))/binary, ", ", (pad_int(D))/binary, " ",
		(month(Mo))/binary, " ", (integer_to_binary(Y))/binary,
		" ", (pad_int(H))/binary, $:, (pad_int(M))/binary,
		$:, (pad_int(S))/binary, " GMT" >>.

%% Following suggestion by MononcQc on #erlounge.
-spec pad_int(0..59) -> binary().
pad_int(X) when X < 10 ->
	<< $0, ($0 + X) >>;
pad_int(X) ->
	integer_to_binary(X).

-spec weekday(1..7) -> <<_:24>>.
weekday(1) -> <<"Mon">>;
weekday(2) -> <<"Tue">>;
weekday(3) -> <<"Wed">>;
weekday(4) -> <<"Thu">>;
weekday(5) -> <<"Fri">>;
weekday(6) -> <<"Sat">>;
weekday(7) -> <<"Sun">>.

-spec month(1..12) -> <<_:24>>.
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
	[{R, fun() -> R = update_rfc1123(B, P, N) end} || {R, P, N, B} <- Tests].

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
