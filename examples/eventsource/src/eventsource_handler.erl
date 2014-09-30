%% Feel free to use, reuse and abuse the code in this file.

%% @doc EventSource emitter.
-module(eventsource_handler).

-export([init/2]).
-export([info/3]).

init(Req, Opts) ->
	Headers = [{<<"content-type">>, <<"text/event-stream">>}],
	Req2 = cowboy_req:chunked_reply(200, Headers, Req),
	erlang:send_after(1000, self(), {message, "Tick"}),
	{cowboy_loop, Req2, Opts, 5000}.

info({message, Msg}, Req, State) ->
	cowboy_req:chunk(["id: ", id(), "\ndata: ", Msg, "\n\n"], Req),
	erlang:send_after(1000, self(), {message, "Tick"}),
	{ok, Req, State}.

id() ->
	{Mega, Sec, Micro} = erlang:now(),
	Id = (Mega * 1000000 + Sec) * 1000000 + Micro,
	integer_to_list(Id, 16).
