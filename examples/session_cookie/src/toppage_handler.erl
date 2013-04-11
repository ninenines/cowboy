%% Feel free to use, reuse and abuse the code in this file.

-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Session, Req2} = cowboy_req:meta(session, Req),
	Visits = session_cookie_util:get_key(visits, Session, 0),
	Session2 = session_cookie_util:set_key(visits, Visits + 1, Session),
	Body = [<<"You have visited this page ">>, integer_to_list(Visits),
		" times this session."],
	Req3 = cowboy_req:set_meta(session, Session2, Req2),
	{ok, Req4} = cowboy_req:reply(200, [], Body, Req3),
	{ok, Req4, State}.

terminate(_Reason, _Req, _State) ->
	ok.
