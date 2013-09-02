%% Feel free to use, reuse and abuse the code in this file.

-module(http_req_attr).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({_, http}, Req, _) ->
	{Attr, Req2} = cowboy_req:qs_val(<<"attr">>, Req),
	{ok, Req2, Attr}.

handle(Req, <<"host_and_port">> = Attr) ->
	{Host, Req2} = cowboy_req:host(Req),
	{Port, Req3} = cowboy_req:port(Req2),
	Value = [Host, "\n", integer_to_list(Port)],
	{ok, Req4} = cowboy_req:reply(200, [], Value, Req3),
	{ok, Req4, Attr}.

terminate(_, _, _) ->
	ok.
