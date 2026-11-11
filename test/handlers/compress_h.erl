%% This module sends a response body of varying sizes to test
%% the cowboy_compress_h stream handler.

-module(compress_h).

-export([init/2]).
-export([level_chunks/0]).
-export([small_chunks/0]).

init(Req0, State=reply) ->
	Req = case cowboy_req:binding(what, Req0) of
		<<"small">> ->
			cowboy_req:reply(200, #{}, lists:duplicate(100, $a), Req0);
		<<"large">> ->
			cowboy_req:reply(200, #{}, lists:duplicate(100000, $a), Req0);
		<<"vary">> ->
			Vary = cowboy_req:header(<<"x-test-vary">>, Req0),
			cowboy_req:reply(200, #{<<"vary">> => Vary}, lists:duplicate(100000, $a), Req0);
		<<"over-threshold">> ->
			cowboy_req:reply(200, #{}, lists:duplicate(200, $a), Req0);
		<<"content-encoding">> ->
			cowboy_req:reply(200, #{<<"content-encoding">> => <<"compress">>},
				lists:duplicate(100000, $a), Req0);
		<<"etag">> ->
			cowboy_req:reply(200, #{<<"etag">> => <<"\"STRONK\"">>},
				lists:duplicate(100000, $a), Req0);
		<<"sendfile">> ->
			AppFile = code:where_is_file("cowboy.app"),
			Size = filelib:file_size(AppFile),
			cowboy_req:reply(200, #{}, {sendfile, 0, Size, AppFile}, Req0);
		<<"set_options_threshold0">> ->
			cowboy_req:cast({set_options, #{compress_threshold => 0}}, Req0),
			cowboy_req:reply(200, #{}, lists:duplicate(100, $a), Req0);
		<<"level">> ->
			cowboy_req:reply(200, #{}, iolist_to_binary(level_chunks(Req0)), Req0);
		<<"set_options_level">> ->
			cowboy_req:cast({set_options, #{compress_level => qs_level(Req0)}}, Req0),
			cowboy_req:reply(200, #{}, iolist_to_binary(level_chunks(Req0)), Req0)
	end,
	{ok, Req, State};
init(Req0, State=stream_reply) ->
	Req = case cowboy_req:binding(what, Req0) of
		<<"large">> ->
			stream_reply(#{}, Req0);
		<<"content-encoding">> ->
			stream_reply(#{<<"content-encoding">> => <<"compress">>}, Req0);
		<<"etag">> ->
			stream_reply(#{<<"etag">> => <<"\"STRONK\"">>}, Req0);
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
			cowboy_req:cast({set_options, #{compress_buffering => false}}, Req0),
			stream_delayed(Req0);
		<<"set_options_buffering_true">> ->
			cowboy_req:cast({set_options, #{compress_buffering => true}}, Req0),
			stream_delayed(Req0);
		<<"level">> ->
			Req1 = cowboy_req:stream_reply(200, Req0),
			{Chunks, [Last]} = lists:split(9, level_chunks(Req0)),
			_ = [cowboy_req:stream_body(C, nofin, Req1) || C <- Chunks],
			cowboy_req:stream_body(Last, fin, Req1),
			Req1;
		<<"set_options_level">> ->
			Req1 = cowboy_req:stream_reply(200, Req0),
			{Chunks1, Chunks2} = lists:split(5, level_chunks(Req0)),
			{Chunks3, [Last]} = lists:split(4, Chunks2),
			_ = [cowboy_req:stream_body(C, nofin, Req1) || C <- Chunks1],
			cowboy_req:cast({set_options, #{compress_level => qs_level(Req0)}}, Req1),
			_ = [cowboy_req:stream_body(C, nofin, Req1) || C <- Chunks3],
			cowboy_req:stream_body(Last, fin, Req1),
			Req1
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

%% Every compression level produces a different output for this data.
level_chunks(Req) ->
	case lists:keymember(<<"small">>, 1, cowboy_req:parse_qs(Req)) of
		true -> small_chunks();
		false -> level_chunks()
	end.

%% Used when the stream handler is expected to crash.
small_chunks() ->
	[iolist_to_binary(lists:duplicate(100, $0 + N)) || N <- lists:seq(0, 9)].

level_chunks() ->
	[iolist_to_binary([integer_to_list(N * N) || N <- lists:seq(From, From + 1999)])
		|| From <- lists:seq(1, 20000, 2000)].

qs_level(Req) ->
	#{level := Level} = cowboy_req:match_qs([level], Req),
	try
		binary_to_integer(Level)
	catch error:badarg ->
		binary_to_existing_atom(Level)
	end.
