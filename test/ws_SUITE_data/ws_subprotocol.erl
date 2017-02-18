%% Feel free to use, reuse and abuse the code in this file.

-module(ws_subprotocol).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	[Protocol | _] = cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req),
	Req2 = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, Protocol, Req),
	{cowboy_websocket, Req2, Opts, #{
		idle_timeout => 1000
	}}.

websocket_handle(_Frame, State) ->
	{ok, State}.

websocket_info(_Info, State) ->
	{ok, State}.
