%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(cors_hello_world_middleware).

%% API.
-export([execute/2]).

%% API.

-spec execute(Req, Env) -> {ok | stop, Req, Env} when Req :: cowboy_req:req(), Env :: any().
execute(#{headers := #{<<"origin">> := HeaderVal}} = Req, Env) ->
	%%AllowedOrigins = '*',
	AllowedOrigins = [
		{<<"http">>, <<"example.org">>, 80},
		{<<"https">>, <<"example.org">>, 443}
	],

	%% NOTE: we assume we always deal with single origin
	[Val] = cow_http_hd:parse_origin(HeaderVal),
	case check_origin(Val, AllowedOrigins) of
		true -> handle_cors_request(HeaderVal, Req, Env);
		_ -> {ok, Req, Env}
	end;
execute(Req, Env) ->
	{ok, Req, Env}.

-spec handle_cors_request(binary(), cowboy_req:req(), any()) -> cowboy_req:req().
handle_cors_request(Origin, #{method := Method} = Req, Env) ->
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req),
	Req3 = cowboy_req:set_resp_header(<<"vary">>, <<"Origin">>, Req2),
	case Method of
		<<"OPTIONS">> ->
			Req4 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, PUT">>, Req3),
			Req5 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"Authorization">>, Req4),
			Req6 = cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"0">>, Req5),
			Req7 = cowboy_req:reply(200, Req6),
			{stop, Req7};
		_ ->
			{ok, Req3, Env}
	end.

-spec check_origin(Origin, [Origin] | '*') -> boolean() when Origin :: {binary(), binary(), 0..65535} | reference().
check_origin(Val, '*') when is_reference(Val) -> true;
check_origin(_, '*') -> true;
check_origin(Val, Val) -> true;
check_origin(Val, L) when is_list(L) -> lists:member(Val, L);
check_origin(_, _) -> false.
