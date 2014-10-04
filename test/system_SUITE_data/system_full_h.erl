-module(system_full_h).

-export([init/2]).
-export([terminate/3]).
-export([code_change/4]).
-export([format_status/2]).

init(Req, Upgrade) ->
	system_full_tester ! {?MODULE, self()},
	{Upgrade, Req, undefined, 500}.

terminate(_Reason, _Req, _State) ->
	ok.

code_change(_OldVsn, Req, _State, Extra) ->
	{ok, Req, Extra}.

format_status(normal, [_PDict, _Req, State]) ->
	[{data, [{"Handler state", {formatted, State}}]}];
format_status(terminate, [_PDict, _Req, State]) ->
	State.
