%% This module sends invalid response headers
%% using various reply functions.

-module(resp_invalid_headers_h).

-export([init/2]).
-export([upgrade/4]).
-export([takeover/7]).

init(Req0, Opts) ->
	case cowboy_req:path(Req0) of
		<<"/reply">> ->
			Req = cowboy_req:reply(200, #{
				<<"x-test">> => <<"bad\r\nvalue">>,
				<<"x-list">> => "good value as a list"
			}, <<"OK">>, Req0),
			{ok, Req, Opts};
		<<"/reply_name">> ->
			Req = cowboy_req:reply(200, #{
				<<"X-Test">> => <<"value">>
			}, <<"OK">>, Req0),
			{ok, Req, Opts};
		<<"/stream_reply">> ->
			Req = cowboy_req:stream_reply(200, #{
				<<"x-test">> => "bad\r\nvalue"
			}, Req0),
			cowboy_req:stream_body(<<"OK">>, fin, Req),
			{ok, Req, Opts};
		<<"/stream_trailers">> ->
			Req = cowboy_req:stream_reply(200, #{
				<<"trailer">> => <<"x-test">>
			}, Req0),
			timer:sleep(100),
			cowboy_req:stream_body(<<"OK">>, nofin, Req),
			timer:sleep(100),
			cowboy_req:stream_trailers(#{<<"x-test">> => <<"bad\r\nvalue">>}, Req),
			{ok, Req, Opts};
		<<"/inform">> ->
			ok = cowboy_req:inform(100, #{<<"x-test">> => ["bad", $\r, $\n, <<"value">>]}, Req0),
			timer:sleep(100),
			Req = cowboy_req:reply(200, #{}, <<"OK">>, Req0),
			{ok, Req, Opts};
		<<"/switch_protocol">> ->
			{resp_invalid_headers_h, Req0, Opts};
		<<"/push">> ->
			cowboy_req:push("/static/style.css", #{<<"x-test">> => <<"bad\r\nvalue">>}, Req0),
			Req = cowboy_req:reply(200, Req0),
			{ok, Req, Opts}
	end.

upgrade(Req=#{pid := Pid, streamid := StreamID}, Env, _Handler, _State) ->
	Headers = #{<<"x-test">> => <<"bad\r\nvalue">>},
	Pid ! {{Pid, StreamID}, {switch_protocol, Headers, ?MODULE, undefined}},
	{ok, Req, Env}.

takeover(_, invalid_response_headers_switch_protocol_ignore,
		_, _, _, _, undefined) ->
	exit({shutdown, expected}).
