%% Feel free to use, reuse and abuse the code in this file.

%% @doc EventSource emitter.
-module(eventsource_h).

-export([init/2]).
-export([info/3]).

init(Req0, Opts) ->
	Req = cowboy_req:stream_reply(200, #{
		<<"content-type">> => <<"text/event-stream">>
	}, Req0),
	erlang:send_after(1000, self(), {message, "Tick"}),
	{cowboy_loop, Req, Opts}.

info({message, Msg}, Req, State) ->
	cowboy_req:stream_events(#{
		id => id(),
		data => Msg
	}, nofin, Req),
	erlang:send_after(1000, self(), {message, "Tick"}),
	{ok, Req, State}.

id() ->
	integer_to_list(erlang:unique_integer([positive, monotonic]), 16).
