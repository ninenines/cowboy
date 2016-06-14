%% Feel free to use, reuse and abuse the code in this file.

%% @doc EventSource emitter.
-module(eventsource_handler).

-export([init/2]).
-export([info/3]).

init(Req, Opts) ->
	Req2 = cowboy_req:chunked_reply(200, #{
		<<"content-type">> => <<"text/event-stream">>
	}, Req),
	erlang:send_after(1000, self(), {message, "Tick"}),
	{cowboy_loop, Req2, Opts, 5000}.

info({message, Msg}, Req, State) ->
	cowboy_req:chunk(["id: ", id(), "\ndata: ", Msg, "\n\n"], Req),
	erlang:send_after(1000, self(), {message, "Tick"}),
	{ok, Req, State}.

id() ->
	integer_to_list(erlang:unique_integer([positive, monotonic]), 16).
