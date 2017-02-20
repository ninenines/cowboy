%% Feel free to use, reuse and abuse the code in this file.

-module(http_set_resp).

-export([init/2]).

init(Req, Opts) ->
	Headers = proplists:get_value(headers, Opts, #{}),
	Body = proplists:get_value(body, Opts, <<"http_handler_set_resp">>),
	Req2 = lists:foldl(fun({Name, Value}, R) ->
		cowboy_req:set_resp_header(Name, Value, R)
	end, Req, maps:to_list(Headers)),
	Req3 = cowboy_req:set_resp_body(Body, Req2),
	Req4 = cowboy_req:set_resp_header(<<"x-cowboy-test">>, <<"ok">>, Req3),
	Req5 = cowboy_req:set_resp_cookie(<<"cake">>, <<"lie">>, Req4),
	case cowboy_req:has_resp_header(<<"x-cowboy-test">>, Req5) of
		false -> {ok, Req5, Opts};
		true ->
			case cowboy_req:has_resp_body(Req5) of
				false ->
					{ok, Req5, Opts};
				true ->
					{ok, cowboy_req:reply(200, Req5), Opts}
			end
	end.
