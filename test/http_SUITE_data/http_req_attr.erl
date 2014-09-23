%% Feel free to use, reuse and abuse the code in this file.

-module(http_req_attr).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({_, http}, Req, _) ->
	#{attr := Attr} = cowboy_req:match_qs(Req, [attr]),
	{ok, Req, Attr}.

handle(Req, <<"host_and_port">> = Attr) ->
	Host = cowboy_req:host(Req),
	Port = cowboy_req:port(Req),
	Value = [Host, "\n", integer_to_list(Port)],
	{ok, cowboy_req:reply(200, [], Value, Req), Attr}.

terminate(_, _, _) ->
	ok.
