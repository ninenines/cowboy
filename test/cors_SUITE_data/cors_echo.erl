%% Feel free to use, reuse and abuse the code in this file.

-module(cors_echo).

-export([init/2]).

init(Req, Opts) ->
	{_, Hs} = lists:keyfind(hs, 1, Opts),
	{_, PHs} = lists:keyfind(phs, 1, Opts),
	Req2 =
		case cowboy_req:method(Req) of
			<<"OPTIONS">> -> cowboy_req:set_cors_preflight_headers(PHs, Req);
			_ -> cowboy_req:set_cors_headers(Hs, Req)
		end,
	{ok, cowboy_req:reply(200, Req2), Opts}.

