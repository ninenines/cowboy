%% This module sends a response body of varying sizes to test
%% the cowboy_compress_h stream handler.

-module(compress_h).

-export([init/2]).

init(Req0, State=reply) ->
	Req = case cowboy_req:binding(what, Req0) of
		<<"small">> ->
			cowboy_req:reply(200, #{}, lists:duplicate(100, $a), Req0);
		<<"large">> ->
			cowboy_req:reply(200, #{}, lists:duplicate(100000, $a), Req0);
		<<"content-encoding">> ->
			cowboy_req:reply(200, #{<<"content-encoding">> => <<"compress">>},
				lists:duplicate(100000, $a), Req0);
		<<"sendfile">> ->
			AppFile = code:where_is_file("cowboy.app"),
			Size = filelib:file_size(AppFile),
			cowboy_req:reply(200, #{}, {sendfile, 0, Size, AppFile}, Req0)
	end,
	{ok, Req, State};
init(Req0, State=stream_reply) ->
	Req = case cowboy_req:binding(what, Req0) of
		<<"large">> ->
			stream_reply(#{}, Req0);
		<<"content-encoding">> ->
			stream_reply(#{<<"content-encoding">> => <<"compress">>}, Req0)
	end,
	{ok, Req, State}.

stream_reply(Headers, Req0) ->
	Data = lists:duplicate(10000, $a),
	Req = cowboy_req:stream_reply(200, Headers, Req0),
	_ = [cowboy_req:stream_body(Data, nofin, Req) || _ <- lists:seq(1,9)],
	cowboy_req:stream_body(Data, fin, Req),
	Req.
