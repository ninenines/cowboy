%% Feel free to use, reuse and abuse the code in this file.

-module(http_handler_set_resp).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Transport, http}, Req, Opts) ->
	Headers = proplists:get_value(headers, Opts, []),
	Body = proplists:get_value(body, Opts, <<"http_handler_set_resp">>),
	{ok, Req2} = lists:foldl(fun({Name, Value}, {ok, R}) ->
		cowboy_http_req:set_resp_header(Name, Value, R)
	end, {ok, Req}, Headers),
	{ok, Req3} = cowboy_http_req:set_resp_body(Body, Req2),
	{ok, Req4} = cowboy_http_req:set_resp_header(
		<<"X-Cowboy-Test">>, <<"ok">>, Req3),
	{ok, Req5} = cowboy_http_req:set_resp_cookie(
		<<"cake">>, <<"lie">>, [], Req4),
	{ok, Req5, undefined}.

handle(Req, State) ->
	case cowboy_http_req:has_resp_header(<<"X-Cowboy-Test">>, Req) of
		false -> {ok, Req, State};
		true ->
			case cowboy_http_req:has_resp_body(Req) of
				false -> {ok, Req, State};
				true ->
					{ok, Req2} = cowboy_http_req:reply(200, Req),
					{ok, Req2, State}
			end
	end.

terminate(_Req, _State) ->
	ok.
