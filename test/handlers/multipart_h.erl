%% This module reads a multipart body and echoes it back as an Erlang term.

-module(multipart_h).

-export([init/2]).

init(Req0, State) ->
	{Result, Req} = case cowboy_req:binding(key, Req0) of
		undefined -> acc_multipart(Req0, []);
		<<"skip_body">> -> skip_body_multipart(Req0, []);
		<<"read_part2">> -> read_part2_multipart(Req0, []);
		<<"read_part_body2">> -> read_part_body2_multipart(Req0, [])
	end,
	{ok, cowboy_req:reply(200, #{}, term_to_binary(Result), Req), State}.

acc_multipart(Req0, Acc) ->
	case cowboy_req:read_part(Req0) of
		{ok, Headers, Req1} ->
			{ok, Body, Req} = stream_body(Req1, <<>>),
			acc_multipart(Req, [{Headers, Body}|Acc]);
		{done, Req} ->
			{lists:reverse(Acc), Req}
	end.

stream_body(Req0, Acc) ->
	case cowboy_req:read_part_body(Req0) of
		{more, Data, Req} ->
			stream_body(Req, << Acc/binary, Data/binary >>);
		{ok, Data, Req} ->
			{ok, << Acc/binary, Data/binary >>, Req}
	end.

skip_body_multipart(Req0, Acc) ->
	case cowboy_req:read_part(Req0) of
		{ok, Headers, Req} ->
			skip_body_multipart(Req, [Headers|Acc]);
		{done, Req} ->
			{lists:reverse(Acc), Req}
	end.

read_part2_multipart(Req0, Acc) ->
	case cowboy_req:read_part(Req0, #{length => 1, period => 1}) of
		{ok, Headers, Req1} ->
			{ok, Body, Req} = stream_body(Req1, <<>>),
			acc_multipart(Req, [{Headers, Body}|Acc]);
		{done, Req} ->
			{lists:reverse(Acc), Req}
	end.

read_part_body2_multipart(Req0, Acc) ->
	case cowboy_req:read_part(Req0) of
		{ok, Headers, Req1} ->
			{ok, Body, Req} = stream_body2(Req1, <<>>),
			acc_multipart(Req, [{Headers, Body}|Acc]);
		{done, Req} ->
			{lists:reverse(Acc), Req}
	end.

stream_body2(Req0, Acc) ->
	case cowboy_req:read_part_body(Req0, #{length => 1, period => 1}) of
		{more, Data, Req} ->
			stream_body(Req, << Acc/binary, Data/binary >>);
		{ok, Data, Req} ->
			{ok, << Acc/binary, Data/binary >>, Req}
	end.
