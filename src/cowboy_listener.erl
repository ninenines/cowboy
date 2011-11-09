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

%% @doc Public API for managing listeners.
-module(cowboy_listener).
-behaviour(gen_server).

-export([start_link/0, stop/1,
	add_connection/3, move_connection/3, remove_connection/2, wait/3]). %% API.
-export([init/1, handle_call/3, handle_cast/2,
	handle_info/2, terminate/2, code_change/3]). %% gen_server.

-record(state, {
	req_pools = [] :: [{atom(), non_neg_integer()}],
	reqs_table :: ets:tid(),
	queue = [] :: [{pid(), reference()}]
}).

%% API.

%% @private
%%
%% We set the process priority to high because cowboy_listener is the central
%% gen_server in Cowboy and is used to manage all the incoming connections.
%% Setting the process priority to high ensures the connection-related code
%% will always be executed when a connection needs it, allowing Cowboy to
%% scale far beyond what it would with a normal priority.
-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], [{spawn_opt, [{priority, high}]}]).

%% @private
-spec stop(pid()) -> stopped.
stop(ServerPid) ->
	gen_server:call(ServerPid, stop).

%% @doc Add a connection to the given pool in the listener.
%%
%% Pools of connections are used to restrict the maximum number of connections
%% depending on their type. By default, Cowboy add all connections to the
%% pool <em>default</em>. It also checks for the maximum number of connections
%% in that pool before accepting again.
%%
%% When a process managing a connection dies, the process is removed from the
%% pool. If the socket has been sent to another process, it is up to the
%% protocol code to inform the listener of the new <em>ConnPid</em> by removing
%% the previous and adding the new one.
-spec add_connection(pid(), atom(), pid()) -> {ok, non_neg_integer()}.
add_connection(ServerPid, Pool, ConnPid) ->
	gen_server:call(ServerPid, {add_connection, Pool, ConnPid}).

%% @doc Move a connection from one pool to another.
-spec move_connection(pid(), atom(), pid()) -> ok.
move_connection(ServerPid, DestPool, ConnPid) ->
	gen_server:cast(ServerPid, {move_connection, DestPool, ConnPid}).

%% @doc Remove the given connection from its pool.
-spec remove_connection(pid(), pid()) -> ok.
remove_connection(ServerPid, ConnPid) ->
	gen_server:cast(ServerPid, {remove_connection, ConnPid}).

%% @doc Wait until the number of connections in the given pool gets below
%% the given threshold.
%%
%% This function will not return until the number of connections in the pool
%% gets below <em>MaxConns</em>. It makes use of <em>gen_server:reply/2</em>
%% to make the process wait for a reply indefinitely.
-spec wait(pid(), atom(), non_neg_integer()) -> ok.
wait(ServerPid, Pool, MaxConns) ->
	gen_server:call(ServerPid, {wait, Pool, MaxConns}, infinity).

%% gen_server.

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
	ReqsTablePid = ets:new(requests_table, [set, private]),
	{ok, #state{reqs_table=ReqsTablePid}}.

%% @private
-spec handle_call(_, _, State)
	-> {reply, ignored, State} | {stop, normal, stopped, State}.
handle_call({add_connection, Pool, ConnPid}, _From, State=#state{
		req_pools=Pools, reqs_table=ReqsTable}) ->
	MonitorRef = erlang:monitor(process, ConnPid),
	{NbConnsRet, Pools2} = case lists:keyfind(Pool, 1, Pools) of
		false ->
			{1, [{Pool, 1}|Pools]};
		{Pool, NbConns} ->
			NbConns2 = NbConns + 1,
			{NbConns2, [{Pool, NbConns2}|lists:keydelete(Pool, 1, Pools)]}
	end,
	ets:insert(ReqsTable, {ConnPid, {MonitorRef, Pool}}),
	{reply, {ok, NbConnsRet}, State#state{req_pools=Pools2}};
handle_call({wait, Pool, MaxConns}, From, State=#state{
		req_pools=Pools, queue=Queue}) ->
	case lists:keyfind(Pool, 1, Pools) of
		{Pool, NbConns} when NbConns > MaxConns ->
			{noreply, State#state{queue=[From|Queue]}};
		_Any ->
			{reply, ok, State}
	end;
handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

%% @private
-spec handle_cast(_, State) -> {noreply, State}.
handle_cast({move_connection, DestPool, ConnPid}, State=#state{
		req_pools=Pools, reqs_table=ReqsTable}) ->
	{MonitorRef, SrcPool} = ets:lookup_element(ReqsTable, ConnPid, 2),
	ets:insert(ReqsTable, {ConnPid, {MonitorRef, DestPool}}),
	{SrcPool, SrcNbConns} = lists:keyfind(SrcPool, 1, Pools),
	DestNbConns = case lists:keyfind(DestPool, 1, Pools) of
		false -> 1;
		{DestPool, NbConns} -> NbConns + 1
	end,
	Pools2 = lists:keydelete(SrcPool, 1, lists:keydelete(DestPool, 1, Pools)),
	Pools3 = [{SrcPool, SrcNbConns - 1}, {DestPool, DestNbConns}|Pools2],
	{noreply, State#state{req_pools=Pools3}};
handle_cast({remove_connection, ConnPid}, State) ->
	State2 = remove_pid(ConnPid, State),
	{noreply, State2};
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
-spec handle_info(_, State) -> {noreply, State}.
handle_info({'DOWN', _Ref, process, Pid, _Info}, State) ->
	State2 = remove_pid(Pid, State),
	{noreply, State2};
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
	ok.

%% @private
-spec code_change(_, State, _) -> {ok, State}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

%% @private
-spec remove_pid(pid(), State) -> State.
remove_pid(Pid, State=#state{
		req_pools=Pools, reqs_table=ReqsTable, queue=Queue}) ->
	{MonitorRef, Pool} = ets:lookup_element(ReqsTable, Pid, 2),
	erlang:demonitor(MonitorRef, [flush]),
	{Pool, NbConns} = lists:keyfind(Pool, 1, Pools),
	Pools2 = [{Pool, NbConns - 1}|lists:keydelete(Pool, 1, Pools)],
	ets:delete(ReqsTable, Pid),
	case Queue of
		[] ->
			State#state{req_pools=Pools2};
		[Client|Queue2] ->
			gen_server:reply(Client, ok),
			State#state{req_pools=Pools2, queue=Queue2}
	end.
