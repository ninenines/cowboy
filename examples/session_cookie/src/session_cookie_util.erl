%% Feel free to use, reuse and abuse the code in this file.

-module(session_cookie_util).
-behaviour(cowboy_middleware).

% Cowboy interface
-export([execute/2]).
-export([set_cookie/4]).

% General purpose
-export([get_key/2]).
-export([get_key/3]).
-export([set_key/3]).

execute(Req, Env) ->
	{Val, Req2} = cowboy_req:cookie(<<"mycowboysession">>, Req),
	Session = (try parse_cookie(Val) catch _:_ -> [] end),
	Req3 = cowboy_req:set_meta(session, Session, Req2),
	{ok, Req3, Env}.

set_cookie(Code, Headers, Body, Req) ->
	{Session, Req2} = cowboy_req:meta(session, Req, []),
	Session2 = set_key(last_action, os:timestamp(), Session),
	Cookie = generate_cookie(Session2),
	Headers2 = [{<<"set-cookie">>, Cookie}|Headers],
	{ok, Req3} = cowboy_req:reply(Code, Headers2, Body, Req2),
	Req3.

get_key(Key, Session) ->
	get_key(Key, Session, undefined).
get_key(Key, Session, Default) ->
	case lists:keyfind(Key, 1, Session) of
		false -> Default;
		{_, Val} -> Val
	end.

set_key(Key, Val, Session) ->
	[{Key, Val} | lists:keydelete(Key, 1, Session)].

generate_cookie(Session) ->
	Expires = base64:encode(<< (expires(os:timestamp(), timeout())):64 >>),
	SessionPack = [Expires, base64:encode(term_to_binary(Session))],
	SignedSession = [signature(SessionPack), SessionPack],
	cowboy_http:cookie_to_iodata(<<"mycowboysession">>, SignedSession,
		[{max_age, timeout()}, {http_only, true}]).

parse_cookie(<< Sig:44/binary, Expires:12/binary, Data/binary >>) ->
	Sig = signature([Expires, Data]),
	<< TS:64/integer >> = base64:decode(Expires),
	true = TS > expires(os:timestamp(), 0),
	binary_to_term(base64:decode(Data)).

expires({Me, S, _Mi}, T) ->
	Me * 1000000 + S + T.

signature(Data) ->
	base64:encode(crypto:hmac(sha256, secret(), Data)).

secret() ->
	<<"myverlongapplicationsecretusedtosaltthehashofmycookies">>.

timeout() ->
	120.
