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
	tref = undefined :: undefined | timer:tref()
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> stopped.
stop() ->
	gen_server:call(?MODULE, stop).

-spec rfc1123() -> binary().
rfc1123() ->
	ets:lookup_element(?MODULE, rfc1123, 2).

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
	{ok, TRef} = timer:send_interval(1000, update),
	ets:insert(?MODULE, {rfc1123, B}),
	{ok, #state{universaltime=T, rfc1123=B, tref=TRef}}.

-spec handle_call(any(), _, State)
	-> {reply, ignored, State} | {stop, normal, stopped, State}
	when State::#state{}.
handle_call(stop, _From, State=#state{tref=TRef}) ->
	{ok, cancel} = timer:cancel(TRef),
	{stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

-spec handle_cast(_, State) -> {noreply, State} when State::#state{}.
handle_cast(_Msg, State) ->
	{noreply, State}.

-spec handle_info(any(), State) -> {noreply, State} when State::#state{}.
handle_info(update, #state{universaltime=Prev, rfc1123=B1, tref=TRef}) ->
	T = erlang:universaltime(),
	B2 = update_rfc1123(B1, Prev, T),
	ets:insert(?MODULE, {rfc1123, B2}),
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
	<< Keep/binary, (cow_date:pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< Keep:20/binary, _/bits >>,
		{Date, {H, _, _}}, {Date, {H, M, S}}) ->
	<< Keep/binary, (cow_date:pad_int(M))/binary, $:, (cow_date:pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< Keep:17/binary, _/bits >>, {Date, _}, {Date, {H, M, S}}) ->
	<< Keep/binary, (cow_date:pad_int(H))/binary, $:, (cow_date:pad_int(M))/binary,
		$:, (cow_date:pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< _:7/binary, Keep:10/binary, _/bits >>,
		{{Y, Mo, _}, _}, {Date = {Y, Mo, D}, {H, M, S}}) ->
	Wday = calendar:day_of_the_week(Date),
	<< (cow_date:weekday(Wday))/binary, ", ", (cow_date:pad_int(D))/binary, Keep/binary,
		(cow_date:pad_int(H))/binary, $:, (cow_date:pad_int(M))/binary,
		$:, (cow_date:pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< _:11/binary, Keep:6/binary, _/bits >>,
		{{Y, _, _}, _}, {Date = {Y, Mo, D}, {H, M, S}}) ->
	Wday = calendar:day_of_the_week(Date),
	<< (cow_date:weekday(Wday))/binary, ", ", (cow_date:pad_int(D))/binary, " ",
		(cow_date:month(Mo))/binary, Keep/binary,
		(cow_date:pad_int(H))/binary, $:, (cow_date:pad_int(M))/binary,
		$:, (cow_date:pad_int(S))/binary, " GMT" >>;
update_rfc1123(_, _, {Date = {Y, Mo, D}, {H, M, S}}) ->
	Wday = calendar:day_of_the_week(Date),
	<< (cow_date:weekday(Wday))/binary, ", ", (cow_date:pad_int(D))/binary, " ",
		(cow_date:month(Mo))/binary, " ", (list_to_binary(integer_to_list(Y)))/binary,
		" ", (cow_date:pad_int(H))/binary, $:, (cow_date:pad_int(M))/binary,
		$:, (cow_date:pad_int(S))/binary, " GMT" >>.

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
-endif.
