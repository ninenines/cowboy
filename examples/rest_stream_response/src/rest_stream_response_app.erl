%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(rest_stream_response_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Table = ets:new(stream_tab, []),
	generate_rows(Table, 1000),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/[:v1]", [{v1, int}], toppage_handler, Table}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
	rest_stream_response_sup:start_link().

stop(_State) ->
	ok.

generate_rows(_Table, 0) ->
	ok;
generate_rows(Table, N) ->
	ets:insert(Table, {key(), val(), val()}),
	generate_rows(Table, N - 1).

key() -> key(10).
key(N) -> key(<< (random:uniform(26) - 1) >>, N - 1).
key(Acc, 0) -> binary_part(base64:encode(Acc), 0, 8);
key(Acc, N) -> key(<< Acc/binary, (random:uniform(26) - 1) >>, N - 1).
val() -> random:uniform(50).
