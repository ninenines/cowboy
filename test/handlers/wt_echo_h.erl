%% This module echoes client events back,
%% including creating new streams.

-module(wt_echo_h).
-behavior(cowboy_webtransport).

-export([init/2]).
-export([webtransport_handle/2]).
-export([webtransport_info/2]).
-export([terminate/3]).

%% -define(DEBUG, 1).
-ifdef(DEBUG).
-define(LOG(Fmt, Args), ct:pal(Fmt, Args)).
-else.
-define(LOG(Fmt, Args), _ = Fmt, _ = Args, ok).
-endif.

init(Req0, _) ->
	?LOG("WT init ~p~n", [Req0]),
	Req = case cowboy_req:parse_header(<<"wt-available-protocols">>, Req0) of
		undefined ->
			Req0;
		[Protocol|_] ->
			cowboy_req:set_resp_header(<<"wt-protocol">>, cow_http_hd:wt_protocol(Protocol), Req0)
	end,
	{cowboy_webtransport, Req, #{}}.

webtransport_handle(Event = {stream_open, StreamID, bidi}, Streams) ->
	?LOG("WT handle ~p~n", [Event]),
	{[], Streams#{StreamID => bidi}};
webtransport_handle(Event = {stream_open, StreamID, unidi}, Streams) ->
	?LOG("WT handle ~p~n", [Event]),
	OpenStreamRef = make_ref(),
	{[{open_stream, OpenStreamRef, unidi, <<>>}], Streams#{
		StreamID => {unidi_remote, OpenStreamRef},
		OpenStreamRef => {unidi_local, StreamID}}};
webtransport_handle(Event = {opened_stream_id, OpenStreamRef, OpenStreamID}, Streams) ->
	?LOG("WT handle ~p~n", [Event]),
	case Streams of
		#{OpenStreamRef := bidi} ->
			{[], maps:remove(OpenStreamRef, Streams#{
				OpenStreamID => bidi
			})};
		#{OpenStreamRef := {unidi_local, RemoteStreamID}} ->
			#{RemoteStreamID := {unidi_remote, OpenStreamRef}} = Streams,
			{[], maps:remove(OpenStreamRef, Streams#{
				RemoteStreamID => {unidi_remote, OpenStreamID},
				OpenStreamID => {unidi_local, RemoteStreamID}
			})}
	end;
webtransport_handle(Event = {stream_data, StreamID, _IsFin, <<"TEST:", Test/bits>>}, Streams) ->
	?LOG("WT handle ~p~n", [Event]),
	case Test of
		<<"open_bidi">> ->
			OpenStreamRef = make_ref(),
			{[{open_stream, OpenStreamRef, bidi, <<>>}],
				Streams#{OpenStreamRef => bidi}};
		<<"initiate_close">> ->
			{[initiate_close], Streams};
		<<"close">> ->
			{[close], Streams};
		<<"close_app_code">> ->
			{[{close, 1234567890}], Streams};
		<<"close_app_code_msg">> ->
			{[{close, 1234567890, <<"onetwothreefourfivesixseveneightnineten">>}], Streams};
		<<"event_pid:", EventPidBin/bits>> ->
			{[{send, StreamID, nofin, <<"event_pid_received">>}],
				Streams#{event_pid => binary_to_term(EventPidBin)}}
	end;
webtransport_handle(Event = {stream_data, StreamID, IsFin, Data}, Streams) ->
	?LOG("WT handle ~p~n", [Event]),
	case Streams of
		#{StreamID := bidi} ->
			{[{send, StreamID, IsFin, Data}], Streams};
		#{StreamID := {unidi_remote, Ref}} when is_reference(Ref) ->
			%% The stream isn't ready. We try again later.
			erlang:send_after(100, self(), {try_again, Event}),
			{[], Streams};
		#{StreamID := {unidi_remote, LocalStreamID}} ->
			{[{send, LocalStreamID, IsFin, Data}], Streams}
	end;
webtransport_handle(Event = {datagram, Data}, Streams) ->
	?LOG("WT handle ~p~n", [Event]),
	{[{send, datagram, Data}], Streams};
webtransport_handle(Event = close_initiated, Streams) ->
	?LOG("WT handle ~p~n", [Event]),
	{[{send, datagram, <<"TEST:close_initiated">>}], Streams};
webtransport_handle(Event, Streams) ->
	?LOG("WT handle ignore ~p~n", [Event]),
	{[], Streams}.

webtransport_info({try_again, Event}, Streams) ->
	?LOG("WT try_again ~p", [Event]),
	webtransport_handle(Event, Streams).

terminate(Reason, Req, State=#{event_pid := EventPid}) ->
	?LOG("WT terminate ~0p~n~0p~n~0p", [Reason, Req, State]),
	EventPid ! {'$wt_echo_h', terminate, Reason, Req, State},
	ok;
terminate(Reason, Req, State) ->
	?LOG("WT terminate ~0p~n~0p~n~0p", [Reason, Req, State]),
	ok.
