%% Feel free to use, reuse and abuse the code in this file.

%% @doc Cookie handler.
-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	NewValue = integer_to_list(random:uniform(1000000)),
	Req2 = cowboy_req:set_resp_cookie(
		<<"server">>, NewValue, [{path, <<"/">>}], Req),
	#{client := ClientCookie, server := ServerCookie}
		= cowboy_req:match_cookies(Req2, [client, server]),
	{ok, Body} = toppage_dtl:render([
		{client, ClientCookie},
		{server, ServerCookie}
	]),
	Req3 = cowboy_req:reply(200,
		[{<<"content-type">>, <<"text/html">>}],
		Body, Req2),
	{ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.
