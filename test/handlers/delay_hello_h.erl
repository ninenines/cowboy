%% This module sends a hello world response after a delay.

-module(delay_hello_h).

-export([init/2]).

init(Req, Delay) when is_integer(Delay) ->
	init(Req, #{delay => Delay});
init(Req, Opts=#{delay := Delay}) ->
	Path = maps:get(path, Req),
	_ = case Opts of
		#{notify_received := Pid, wait_for_go := true} ->
			Pid ! {request_received, Path, self()},
			receive go -> ok end;
		#{notify_received := Pid} ->
			Pid ! {request_received, Path};
		_ ->
			ok
	end,
	timer:sleep(Delay),
	{ok, cowboy_req:reply(200, #{}, <<"Hello world!">>, Req), Delay}.
