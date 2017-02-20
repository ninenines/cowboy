%% This module echoes back the value the test is interested in.

-module(echo_h).

-export([init/2]).

init(Req, Opts) ->
	case cowboy_req:binding(arg, Req) of
		undefined ->
			echo(cowboy_req:binding(key, Req), Req, Opts);
		Arg ->
			echo_arg(Arg, Req, Opts)
	end.

echo(<<"read_body">>, Req0, Opts) ->
	case Opts of
		#{crash := true} -> ct_helper:ignore(cowboy_req, read_body, 2);
		_ -> ok
	end,
	{_, Body, Req} = case cowboy_req:path(Req0) of
		<<"/full", _/bits>> -> read_body(Req0, <<>>);
		<<"/opts", _/bits>> -> cowboy_req:read_body(Req0, Opts);
		_ -> cowboy_req:read_body(Req0)
	end,
	{ok, cowboy_req:reply(200, #{}, Body, Req), Opts};
echo(<<"read_urlencoded_body">>, Req0, Opts) ->
	Path = cowboy_req:path(Req0),
	case {Path, Opts} of
		{<<"/opts", _/bits>>, #{crash := true}} -> ct_helper:ignore(cowboy_req, read_body, 2);
		{_, #{crash := true}} -> ct_helper:ignore(cowboy_req, read_urlencoded_body, 2);
		_ -> ok
	end,
	{ok, Body, Req} = case Path of
		<<"/opts", _/bits>> -> cowboy_req:read_urlencoded_body(Req0, Opts);
		<<"/crash", _/bits>> -> cowboy_req:read_urlencoded_body(Req0, Opts);
		_ -> cowboy_req:read_urlencoded_body(Req0)
	end,
	{ok, cowboy_req:reply(200, #{}, value_to_iodata(Body), Req), Opts};
echo(<<"uri">>, Req, Opts) ->
	Value = case cowboy_req:path_info(Req) of
		[<<"origin">>] -> cowboy_req:uri(Req, #{host => undefined});
		[<<"protocol-relative">>] -> cowboy_req:uri(Req, #{scheme => undefined});
		[<<"no-qs">>] -> cowboy_req:uri(Req, #{qs => undefined});
		[<<"no-path">>] -> cowboy_req:uri(Req, #{path => undefined, qs => undefined});
		[<<"set-port">>] -> cowboy_req:uri(Req, #{port => 123});
		[] -> cowboy_req:uri(Req)
	end,
	{ok, cowboy_req:reply(200, #{}, Value, Req), Opts};
echo(<<"match">>, Req, Opts) ->
	[Type|Fields0] = cowboy_req:path_info(Req),
	Fields = [binary_to_atom(F, latin1) || F <- Fields0],
	Value = case Type of
		<<"qs">> -> cowboy_req:match_qs(Fields, Req);
		<<"cookies">> -> cowboy_req:match_cookies(Fields, Req)
	end,
	{ok, cowboy_req:reply(200, #{}, value_to_iodata(Value), Req), Opts};
echo(What, Req, Opts) ->
	F = binary_to_atom(What, latin1),
	Value = cowboy_req:F(Req),
	{ok, cowboy_req:reply(200, #{}, value_to_iodata(Value), Req), Opts}.

echo_arg(Arg0, Req, Opts) ->
	F = binary_to_atom(cowboy_req:binding(key, Req), latin1),
	Arg = case F of
		binding -> binary_to_atom(Arg0, latin1);
		_ -> Arg0
	end,
	Value = case cowboy_req:binding(default, Req) of
		undefined -> cowboy_req:F(Arg, Req);
		Default -> cowboy_req:F(Arg, Req, Default)
	end,
	{ok, cowboy_req:reply(200, #{}, value_to_iodata(Value), Req), Opts}.

read_body(Req0, Acc) ->
	case cowboy_req:read_body(Req0) of
		{ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
		{more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
	end.

value_to_iodata(V) when is_integer(V) -> integer_to_binary(V);
value_to_iodata(V) when is_atom(V) -> atom_to_binary(V, latin1);
value_to_iodata(V) when is_list(V); is_tuple(V); is_map(V) -> io_lib:format("~999999p", [V]);
value_to_iodata(V) -> V.
