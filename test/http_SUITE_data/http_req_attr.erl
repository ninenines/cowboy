%% Feel free to use, reuse and abuse the code in this file.

%% @todo That module was clearly meant to do more than one
%% thing and yet doesn't.
-module(http_req_attr).

-export([init/2]).

init(Req, Opts) ->
	#{attr := Attr} = cowboy_req:match_qs([attr], Req),
	<<"host_and_port">> = Attr,
	Host = cowboy_req:host(Req),
	Port = cowboy_req:port(Req),
	Value = [Host, "\n", integer_to_list(Port)],
	{ok, cowboy_req:reply(200, #{}, Value, Req), Opts}.
