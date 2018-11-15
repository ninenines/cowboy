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
		<<"over-threshold">> ->
			cowboy_req:reply(200, #{}, lists:duplicate(200, $a), Req0);
		<<"content-encoding">> ->
			cowboy_req:reply(200, #{<<"content-encoding">> => <<"compress">>},
				lists:duplicate(100000, $a), Req0);
		<<"sendfile">> ->
			AppFile = code:where_is_file("cowboy.app"),
			Size = filelib:file_size(AppFile),
			cowboy_req:reply(200, #{}, {sendfile, 0, Size, AppFile}, Req0);
		<<"set_options_threshold0">> ->
			%% @todo This should be replaced by a cowboy_req:cast/cowboy_stream:cast.
			#{pid := Pid, streamid := StreamID} = Req0,
			Pid ! {{Pid, StreamID}, {set_options, #{compress_threshold => 0}}},
			cowboy_req:reply(200, #{}, lists:duplicate(100, $a), Req0)
	end,
	{ok, Req, State};
init(Req0, State=stream_reply) ->
	Req = case cowboy_req:binding(what, Req0) of
		<<"large">> ->
			stream_reply(#{}, Req0);
		<<"content-encoding">> ->
			stream_reply(#{<<"content-encoding">> => <<"compress">>}, Req0);
		<<"sendfile">> ->
			Data = lists:duplicate(10000, $a),
			AppFile = code:where_is_file("cowboy.app"),
			Size = filelib:file_size(AppFile),
			Req1 = cowboy_req:stream_reply(200, Req0),
			%% We send a few files interspersed into other data.
			cowboy_req:stream_body(Data, nofin, Req1),
			cowboy_req:stream_body({sendfile, 0, Size, AppFile}, nofin, Req1),
			cowboy_req:stream_body(Data, nofin, Req1),
			cowboy_req:stream_body({sendfile, 0, Size, AppFile}, nofin, Req1),
			cowboy_req:stream_body(Data, fin, Req1),
			Req1;
		<<"sendfile_fin">> ->
			Data = lists:duplicate(10000, $a),
			AppFile = code:where_is_file("cowboy.app"),
			Size = filelib:file_size(AppFile),
			Req1 = cowboy_req:stream_reply(200, Req0),
			%% We send a few files interspersed into other data.
			cowboy_req:stream_body(Data, nofin, Req1),
			cowboy_req:stream_body({sendfile, 0, Size, AppFile}, nofin, Req1),
			cowboy_req:stream_body(Data, nofin, Req1),
			cowboy_req:stream_body({sendfile, 0, Size, AppFile}, fin, Req1),
			Req1;
		<<"delayed">> ->
			stream_delayed(Req0);
		<<"set_options_buffering_false">> ->
			%% @todo This should be replaced by a cowboy_req:cast/cowboy_stream:cast.
			#{pid := Pid, streamid := StreamID} = Req0,
			Pid ! {{Pid, StreamID}, {set_options, #{compress_buffering => false}}},
			stream_delayed(Req0);
		<<"set_options_buffering_true">> ->
			%% @todo This should be replaced by a cowboy_req:cast/cowboy_stream:cast.
			#{pid := Pid, streamid := StreamID} = Req0,
			Pid ! {{Pid, StreamID}, {set_options, #{compress_buffering => true}}},
			stream_delayed(Req0)
	end,
	{ok, Req, State}.

stream_reply(Headers, Req0) ->
	Data = lists:duplicate(10000, $a),
	Req = cowboy_req:stream_reply(200, Headers, Req0),
	_ = [cowboy_req:stream_body(Data, nofin, Req) || _ <- lists:seq(1,9)],
	cowboy_req:stream_body(Data, fin, Req),
	Req.

stream_delayed(Req0) ->
	Req = cowboy_req:stream_reply(200, Req0),
	cowboy_req:stream_body(<<"data: Hello!\r\n\r\n">>, nofin, Req),
	timer:sleep(1000),
	cowboy_req:stream_body(<<"data: World!\r\n\r\n">>, nofin, Req),
	timer:sleep(1000),
	cowboy_req:stream_body(<<"data: Closing!\r\n\r\n">>, fin, Req),
	Req.
