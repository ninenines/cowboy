-module(switch_handler_h).

-export([init/2]).
-export([content_types_provided/2]).
-export([provide/2]).
-export([info/3]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	{[{<<"text/plain">>, provide}], Req, State}.

provide(Req0, run) ->
	Req = cowboy_req:stream_reply(200, Req0),
	send_after(0),
	{{switch_handler, cowboy_loop}, Req, 0};
provide(Req0, hibernate) ->
	Req = cowboy_req:stream_reply(200, Req0),
	send_after(0),
	{{switch_handler, cowboy_loop, hibernate}, Req, 0}.

send_after(N) ->
	erlang:send_after(100, self(), {stream, msg(N)}).

msg(0) -> <<"Hello\n">>;
msg(1) -> <<"streamed\n">>;
msg(2) -> <<"world!\n">>;
msg(3) -> stop.

info({stream, stop}, Req, State) ->
	{stop, Req, State};
info({stream, What}, Req, State) ->
	cowboy_req:stream_body(What, nofin, Req),
	send_after(State + 1),
	{ok, Req, State + 1}.
