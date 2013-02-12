%% Feel free to use, reuse and abuse the code in this file.

%% @doc Streaming handler.
-module(toppage_handler).

-export([init/3]).
-export([rest_init/2]).
-export([content_types_provided/2]).
-export([streaming_csv/2]).

init(_Transport, _Req, _Table) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, Table) ->
	{ok, Req, Table}.

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"csv">>, []}, streaming_csv}
	], Req, State}.

streaming_csv(Req, Table) ->
	{N, Req1} = cowboy_req:binding(v1, Req, 1),
	MS = [{{'$1', '$2', '$3'}, [{'==', '$2', N}], ['$$']}],

	{{stream, result_streamer(Table, MS)}, Req1, Table}.

result_streamer(Table, MS) ->
	fun (Socket, Transport) ->
			send_records(Socket, Transport, ets:select(Table, MS, 1))
	end.

send_records(Socket, Transport, {[Rec], Cont}) ->
	timer:sleep(500),
	send_line(Socket, Transport, Rec),
	send_records(Socket, Transport, ets:select(Cont));
send_records(_Socket, _Transport, '$end_of_table') ->
	ok.

send_line(Socket, Transport, [Key, V1, V2]) ->
	Transport:send(Socket,
		[Key, $,, integer_to_list(V1), $,, integer_to_list(V2), $\r, $\n]).
