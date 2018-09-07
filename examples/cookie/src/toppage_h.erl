%% Feel free to use, reuse and abuse the code in this file.

%% @doc Cookie handler.
-module(toppage_h).

-export([init/2]).

init(Req0, Opts) ->
	NewValue = integer_to_list(rand:uniform(1000000)),
	Req1 = cowboy_req:set_resp_cookie(<<"server">>, NewValue,
		Req0, #{path => <<"/">>}),
	#{client := ClientCookie, server := ServerCookie}
		= cowboy_req:match_cookies([{client, [], <<>>}, {server, [], <<>>}], Req1),
	{ok, Body} = toppage_dtl:render([
		{client, ClientCookie},
		{server, ServerCookie}
	]),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/html">>
	}, Body, Req1),
	{ok, Req, Opts}.
