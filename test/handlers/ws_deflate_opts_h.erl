%% This module enables compression and returns deflate
%% options depending on the query string.

-module(ws_deflate_opts_h).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req=#{qs := Qs}, State) ->
	{Name, Value} = case Qs of
		<<"server_context_takeover">> -> {server_context_takeover, takeover};
		<<"server_no_context_takeover">> -> {server_context_takeover, no_takeover};
		<<"client_context_takeover">> -> {client_context_takeover, takeover};
		<<"client_no_context_takeover">> -> {client_context_takeover, no_takeover};
		<<"server_max_window_bits">> -> {server_max_window_bits, 9};
		<<"client_max_window_bits">> -> {client_max_window_bits, 9};
		<<"level">> -> {level, best_speed};
		<<"mem_level">> -> {mem_level, 1};
		<<"strategy">> -> {strategy, rle}
	end,
	{cowboy_websocket, Req, State, #{
		compress => true,
		deflate_opts => #{Name => Value}
	}}.

websocket_handle({text, Data}, State) ->
	{reply, {text, Data}, State};
websocket_handle({binary, Data}, State) ->
	{reply, {binary, Data}, State};
websocket_handle(_, State) ->
	{ok, State}.

websocket_info(_, State) ->
	{ok, State}.
