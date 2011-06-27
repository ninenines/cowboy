%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(cowboy_http_websocket).
-export([upgrade/3]). %% API.
-export([handler_loop/4]). %% Internal.

-include("include/http.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {
	handler :: module(),
	opts :: any(),
	origin = undefined :: undefined | binary(),
	challenge = undefined :: undefined | binary(),
	timeout = infinity :: timeout(),
	messages = undefined :: undefined | {atom(), atom(), atom()},
	eop :: tuple(),
	hibernate = false :: boolean()
}).

-spec upgrade(module(), any(), #http_req{}) -> ok.
upgrade(Handler, Opts, Req) ->
	EOP = binary:compile_pattern(<< 255 >>),
	case catch websocket_upgrade(#state{handler=Handler, opts=Opts, eop=EOP}, Req) of
		{ok, State, Req2} -> handler_init(State, Req2);
		{'EXIT', _Reason} -> upgrade_error(Req)
	end.

-spec websocket_upgrade(#state{}, #http_req{}) -> {ok, #state{}, #http_req{}}.
websocket_upgrade(State, Req) ->
	{<<"Upgrade">>, Req2} = cowboy_http_req:header('Connection', Req),
	{<<"WebSocket">>, Req3} = cowboy_http_req:header('Upgrade', Req2),
	{Origin, Req4} = cowboy_http_req:header(<<"Origin">>, Req3),
	{Key1, Req5} = cowboy_http_req:header(<<"Sec-Websocket-Key1">>, Req4),
	{Key2, Req6} = cowboy_http_req:header(<<"Sec-Websocket-Key2">>, Req5),
	false = lists:member(undefined, [Origin, Key1, Key2]),
	{ok, Key3, Req7} = cowboy_http_req:body(8, Req6),
	Challenge = challenge(Key1, Key2, Key3),
	{ok, State#state{origin=Origin, challenge=Challenge}, Req7}.

-spec challenge(binary(), binary(), binary()) -> binary().
challenge(Key1, Key2, Key3) ->
	IntKey1 = key_to_integer(Key1),
	IntKey2 = key_to_integer(Key2),
	erlang:md5(<< IntKey1:32, IntKey2:32, Key3/binary >>).

-spec key_to_integer(binary()) -> integer().
key_to_integer(Key) ->
	Number = list_to_integer([C || << C >> <= Key, C >= $0, C =< $9]),
	Spaces = length([C || << C >> <= Key, C =:= 32]),
	Number div Spaces.

-spec handler_init(#state{}, #http_req{}) -> ok.
handler_init(State=#state{handler=Handler, opts=Opts},
		Req=#http_req{transport=Transport}) ->
	try Handler:websocket_init(Transport:name(), Req, Opts) of
		{ok, Req2, HandlerState} ->
			websocket_handshake(State, Req2, HandlerState);
		{ok, Req2, HandlerState, Timeout} ->
			websocket_handshake(State#state{timeout=Timeout},
				Req2, HandlerState)
	catch Class:Reason ->
		upgrade_error(Req),
		error_logger:error_msg(
			"** Handler ~p terminating in websocket_init/3~n"
			"   for the reason ~p:~p~n** Options were ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts, Req, erlang:get_stacktrace()])
	end.

-spec upgrade_error(#http_req{}) -> ok.
upgrade_error(Req=#http_req{socket=Socket, transport=Transport}) ->
	{ok, _Req} = cowboy_http_req:reply(400, [], [],
		Req#http_req{resp_state=waiting}),
	Transport:close(Socket).

-spec websocket_handshake(#state{}, #http_req{}, any()) -> ok.
websocket_handshake(State=#state{origin=Origin, challenge=Challenge},
		Req=#http_req{transport=Transport, raw_host=Host, port=Port,
		raw_path=Path}, HandlerState) ->
	Location = websocket_location(Transport:name(), Host, Port, Path),
	{ok, Req2} = cowboy_http_req:reply(
		<<"101 WebSocket Protocol Handshake">>,
		[{<<"Connection">>, <<"Upgrade">>},
		 {<<"Upgrade">>, <<"WebSocket">>},
		 {<<"Sec-WebSocket-Location">>, Location},
		 {<<"Sec-WebSocket-Origin">>, Origin}],
		Challenge, Req#http_req{resp_state=waiting}),
	handler_before_loop(State#state{messages=Transport:messages()},
		Req2, HandlerState, <<>>).

-spec websocket_location(atom(), binary(), inet:ip_port(), binary())
	-> binary().
websocket_location(Protocol, Host, Port, Path) ->
  << (websocket_location_protocol(Protocol))/binary, "://", Host/binary,
    (websocket_location_port(ssl, Port))/binary, Path/binary >>.

-spec websocket_location_protocol(atom()) -> binary().
websocket_location_protocol(ssl) -> <<"wss">>;
websocket_location_protocol(_)   -> <<"ws">>.

-spec websocket_location_port(atom(), inet:ip_port()) -> binary().
websocket_location_port(ssl, 443) -> <<"">>;
websocket_location_port(_, 80)    -> <<"">>;
websocket_location_port(_, Port)  -> <<":", (list_to_binary(integer_to_list(Port)))/binary>>.

-spec handler_before_loop(#state{}, #http_req{}, any(), binary()) -> ok.
handler_before_loop(State=#state{hibernate=true},
		Req=#http_req{socket=Socket, transport=Transport},
		HandlerState, SoFar) ->
	Transport:setopts(Socket, [{active, once}]),
	erlang:hibernate(?MODULE, handler_loop, [State#state{hibernate=false},
		Req, HandlerState, SoFar]);
handler_before_loop(State, Req=#http_req{socket=Socket, transport=Transport},
		HandlerState, SoFar) ->
	Transport:setopts(Socket, [{active, once}]),
	handler_loop(State, Req, HandlerState, SoFar).

-spec handler_loop(#state{}, #http_req{}, any(), binary()) -> ok.
handler_loop(State=#state{messages={OK, Closed, Error}, timeout=Timeout},
		Req=#http_req{socket=Socket}, HandlerState, SoFar) ->
	receive
		{OK, Socket, Data} ->
			websocket_data(State, Req, HandlerState,
				<< SoFar/binary, Data/binary >>);
		{Closed, Socket} ->
			handler_terminate(State, Req, HandlerState, {error, closed});
		{Error, Socket, Reason} ->
			handler_terminate(State, Req, HandlerState, {error, Reason});
		Message ->
			handler_call(State, Req, HandlerState,
				SoFar, Message, fun handler_before_loop/4)
	after Timeout ->
		websocket_close(State, Req, HandlerState, {normal, timeout})
	end.

-spec websocket_data(#state{}, #http_req{}, any(), binary()) -> ok.
websocket_data(State, Req, HandlerState, << 255, 0, _Rest/bits >>) ->
	websocket_close(State, Req, HandlerState, {normal, closed});
websocket_data(State, Req, HandlerState, <<>>) ->
	handler_before_loop(State, Req, HandlerState, <<>>);
websocket_data(State, Req, HandlerState, Data) ->
	websocket_frame(State, Req, HandlerState, Data, binary:first(Data)).

%% We do not support any frame type other than 0 yet. Just like the specs.
-spec websocket_frame(#state{}, #http_req{}, any(), binary(), byte()) -> ok.
websocket_frame(State=#state{eop=EOP}, Req, HandlerState, Data, 0) ->
	case binary:match(Data, EOP) of
		{Pos, 1} ->
			Pos2 = Pos - 1,
			<< 0, Frame:Pos2/binary, 255, Rest/bits >> = Data,
			handler_call(State, Req, HandlerState,
				Rest, {websocket, Frame}, fun websocket_data/4);
		nomatch ->
			%% @todo We probably should allow limiting frame length.
			handler_before_loop(State, Req, HandlerState, Data)
	end;
websocket_frame(State, Req, HandlerState, _Data, _FrameType) ->
	websocket_close(State, Req, HandlerState, {error, badframe}).

-spec handler_call(#state{}, #http_req{}, any(), binary(), any(), fun()) -> ok.
handler_call(State=#state{handler=Handler, opts=Opts}, Req, HandlerState,
		RemainingData, Message, NextState) ->
	try Handler:websocket_handle(Message, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			NextState(State, Req2, HandlerState2, RemainingData);
		{ok, Req2, HandlerState2, hibernate} ->
			NextState(State#state{hibernate=true},
				Req2, HandlerState2, RemainingData);
		{reply, Data, Req2, HandlerState2} ->
			websocket_send(Data, Req2),
			NextState(State, Req2, HandlerState2, RemainingData);
		{reply, Data, Req2, HandlerState2, hibernate} ->
			websocket_send(Data, Req2),
			NextState(State#state{hibernate=true},
				Req2, HandlerState2, RemainingData);
		{shutdown, Req2, HandlerState2} ->
			websocket_close(State, Req2, HandlerState2, {normal, shutdown})
	catch Class:Reason ->
		websocket_close(State, Req, HandlerState, {error, handler}),
		error_logger:error_msg(
			"** Handler ~p terminating in websocket_handle/3~n"
			"   for the reason ~p:~p~n** Message was ~p~n"
			"** Options were ~p~n** Handler state was ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Message, Opts,
			 HandlerState, Req, erlang:get_stacktrace()])
	end.

-spec websocket_send(binary(), #http_req{}) -> ok.
websocket_send(Data, #http_req{socket=Socket, transport=Transport}) ->
	Transport:send(Socket, << 0, Data/binary, 255 >>).

-spec websocket_close(#state{}, #http_req{}, any(), {atom(), atom()}) -> ok.
websocket_close(State, Req=#http_req{socket=Socket, transport=Transport},
		HandlerState, Reason) ->
	Transport:send(Socket, << 255, 0 >>),
	Transport:close(Socket),
	handler_terminate(State, Req, HandlerState, Reason).

-spec handler_terminate(#state{}, #http_req{},
	any(), atom() | {atom(), atom()}) -> ok.
handler_terminate(#state{handler=Handler, opts=Opts},
		Req, HandlerState, TerminateReason) ->
	try
		Handler:websocket_terminate(TerminateReason, Req, HandlerState)
	catch Class:Reason ->
		error_logger:error_msg(
			"** Handler ~p terminating in websocket_terminate/3~n"
			"   for the reason ~p:~p~n** Initial reason was ~p~n"
			"** Options were ~p~n** Handler state was ~p~n"
			"** Request was ~p~n** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, TerminateReason, Opts,
			 HandlerState, Req, erlang:get_stacktrace()])
	end.

%% eunit

-ifdef(TEST).

websocket_location_test() ->
  ?assertEqual(<<"ws://localhost/path">>, websocket_location(other, <<"localhost">>, 80, <<"/path">>)),
  ?assertEqual(<<"ws://localhost:8080/path">>, websocket_location(other, <<"localhost">>, 8080, <<"/path">>)),
  ?assertEqual(<<"wss://localhost/path">>, websocket_location(ssl, <<"localhost">>, 443, <<"/path">>)),
  ?assertEqual(<<"wss://localhost:8443/path">>, websocket_location(ssl, <<"localhost">>, 8443, <<"/path">>)),
  ok.

-endif.

