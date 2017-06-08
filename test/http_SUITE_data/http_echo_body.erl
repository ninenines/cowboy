%% Feel free to use, reuse and abuse the code in this file.

-module(http_echo_body).

-export([init/2]).

init(Req, Opts) ->
	true = cowboy_req:has_body(Req),
	Req3 = case cowboy_req:read_body(Req, [{length, 1000000}]) of
		{ok, Body, Req2} -> handle_body(Req2, Body);
		{more, _, Req2} -> handle_badlength(Req2)
	end,
	{ok, Req3, Opts}.

handle_badlength(Req) ->
	cowboy_req:reply(413, #{}, <<"Request entity too large">>, Req).

handle_body(Req, Body) ->
	Size = cowboy_req:body_length(Req),
	Size = byte_size(Body),
	cowboy_req:reply(200, #{}, Body, Req).
