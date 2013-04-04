%% Copyright (c) 2013, Dave Peticolas <dave@krondo.com>
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

%% @doc EventSource (Server-Sent Events) protocol API.
%%
%% Use the functions in this module in a cowboy loop
%% handler to serve Server-Sent Events.
%%
%% Call <em>cowboy_eventsource:init/1</em> from the loop handler's
%% <em>init/3</em> function to accept a client connection. You will
%% need to return one of the <em>loop</em> options to keep the
%% connection open so that subsequent events may be sent.
%%
%% Alternatively, return the <em>shutdown</em> option for
%% <em>init/3</em> to close the connection and tell the client to stop
%% reconnecting.
%%
%% Use <em>cowboy_eventsource:last_event_id/1</em> to obtain the
%% last event id seen by the client in order to send events it has
%% missed.
%%
%% Use <em>cowboy_eventsource:send/2</em> to send events from the
%% <em>init/3</em> and <em>info/3</em> loop handler callbacks.
-module(cowboy_eventsource).

%% API.
-export([init/1]).
-export([last_event_id/1]).
-export([send/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type message() :: {event, event()}
	| {comment, iodataline()}
	| {retry, milliseconds()}.
-type event() :: [event_field()].
-type event_field() :: {data, iodata()}
	| {dataline, iodataline()}
	| {id, iodataline()}
	| {type, iodataline()}.
-type iodataline() :: iodata(). % no \r or \n allowed
-type milliseconds() :: integer().


%% @doc Start a Server-Sent Event stream connection.
%%
%% This function should be called from the <em>init/3</em> function of
%% a cowboy loop handler to accept an EventSource connection.
-spec init(Req) -> {ok, Req} when Req::cowboy_req:req().
init(Req) ->
	Headers = [{<<"content-type">>, <<"text/event-stream">>}],
	cowboy_req:chunked_reply(200, Headers, Req).

%% @doc Return the last event id sent by the client, if present.
%%
%% The last event id, if present, can be used to send events the
%% client has missed.
-spec last_event_id(Req)
	-> {binary() | undefined, Req} when Req::cowboy_req:req().
last_event_id(Req) ->
	cowboy_req:header(<<"last-event-id">>, Req).

%% @doc Send a message or list of messages to an EventSource client.
%%
%% Messages take one of three forms:
%%
%%   {retry, 1000}
%%       Tell the client to delay for 1000 milliseconds
%%       when reconnecting.
%%   {comment, <<"ping">>}
%%       Send the text "ping" in a comment field. Comments
%%       are ignored by clients but can be used as an application
%%       level keep-alive mechanism. Comments must not contain
%%       newline characters.
%%   {event, Event}
%%       Send an event to the client.
%%
%% Events are given as a list of fields, which take one of
%% four forms:
%%
%%    {id, <<"message-id">>}
%%        The ID of the message. This is optional and may not
%%        contain newline characters.
%%    {type, <<"message-type">>}
%%        The type of the message. This is optional and may not
%%        contain newline characters.
%%    {data, <<"line 1\nline 2">>}
%%        The data for the message. This is required (otherwise
%%        the client will ignore the event). Newlines of the form
%%        <<"\r\n">> and <<"\r">>, will be converted to <<"\n">>.
%%    {dataline, <<"data">>}
%%        If you know your data contains no newline characters,
%%        you can use data-line to avoid some processing overhead.
%%
%% Multiple <em>data</em> and <em>data-line</em> fields in a single
%% event will be concatenated together by the client. If multiple
%% <<em>id</em> or <em>type</em> fields are present, the client will
%% ignore all but the last.
-spec send(message() | [message()], cowboy_req:req()) -> ok.
send(Reply, Req) ->
	cowboy_req:chunk(eventsource_iodata(Reply), Req).

eventsource_iodata(Messages) when is_list(Messages)->
	lists:map(fun eventsource_message_iodata/1, Messages);
eventsource_iodata(Message) ->
	eventsource_message_iodata(Message).

eventsource_message_iodata({event, Event}) ->
	eventsource_event_iodata(Event, []);
eventsource_message_iodata({comment, Comment}) ->
	[<<": ">>, Comment, <<"\n">>];
eventsource_message_iodata({retry, Timeout}) ->
	[<<"retry: ">>, integer_to_list(Timeout), <<"\n">>].

eventsource_event_iodata([], Accum) ->
	lists:reverse(Accum, [<<"\n">>]);
eventsource_event_iodata([{data, Data}|Fields], Accum) ->
	Accum2 = [eventsource_data_iodata(iolist_to_binary(Data))|Accum],
	eventsource_event_iodata(Fields, Accum2);
eventsource_event_iodata([{dataline, Line}|Fields], Accum) ->
	Accum2 = [[<<"data: ">>, Line, <<"\n">>]|Accum],
	eventsource_event_iodata(Fields, Accum2);
eventsource_event_iodata([{id, Line}|Fields], Accum) ->
	Accum2 = [[<<"id: ">>, Line, <<"\n">>]|Accum],
	eventsource_event_iodata(Fields, Accum2);
eventsource_event_iodata([{type, Line}|Fields], Accum) ->
	Accum2 = [[<<"event: ">>, Line, <<"\n">>]|Accum],
	eventsource_event_iodata(Fields, Accum2).

eventsource_data_iodata(Bin) ->
	[[<<"data: ">>, Line, <<"\n">>] ||
		Line <- binary:split(Bin, [<<"\r\n">>, <<"\r">>, <<"\n">>], [global])].

%% Tests.

-ifdef(TEST).

eventsource_message_iodata_event_test() ->
	?assertMatch(
	[[<<"id: ">>, <<"id">>, <<"\n">>], <<"\n">>],
	eventsource_message_iodata({event, [{id, <<"id">>}]})),
	ok.
	
eventsource_message_iodata_comment_test() ->
	?assertMatch(
	[<<": ">>, <<"ping">>, <<"\n">>],
	eventsource_message_iodata({comment, <<"ping">>})),
	ok.
	
eventsource_message_iodata_retry_test() ->
	?assertMatch(
	[<<"retry: ">>, "1000", <<"\n">>],
	eventsource_message_iodata({retry, 1000})),
	ok.
	
eventsource_event_iodata_reverse_test() ->
	?assertMatch(
	[<<"a">>, <<"b">>, <<"\n">>],
	eventsource_event_iodata([], [<<"b">>, <<"a">>])),
	ok.

eventsource_event_iodata_data_test() ->
	?assertMatch(
	[[[<<"data: ">>, <<"1">>, <<"\n">>],
	  [<<"data: ">>, <<"2">>, <<"\n">>]], <<"\n">>],
	eventsource_event_iodata([{data, <<"1\r2">>}], [])),
	ok.

eventsource_event_iodata_dataline_test() ->
	?assertMatch(
	[[<<"data: ">>, <<"1">>, <<"\n">>], <<"\n">>],
	eventsource_event_iodata([{dataline, <<"1">>}], [])),
	ok.

eventsource_event_iodata_id_test() ->
	?assertMatch(
	[[<<"id: ">>, <<"id">>, <<"\n">>], <<"\n">>],
	eventsource_event_iodata([{id, <<"id">>}], [])),
	ok.

eventsource_event_iodata_type_test() ->
	?assertMatch(
	[[<<"event: ">>, <<"type">>, <<"\n">>], <<"\n">>],
	eventsource_event_iodata([{type, <<"type">>}], [])),
	ok.

eventsource_data_iodata_test() ->
	?assertMatch(
	[[<<"data: ">>, <<"line 1">>, <<"\n">>],
	 [<<"data: ">>, <<"line 2">>, <<"\n">>]],
	eventsource_data_iodata(<<"line 1\r\nline 2">>)),
	ok.

-endif.
