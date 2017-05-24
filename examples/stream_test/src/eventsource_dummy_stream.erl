-module(eventsource_dummy_stream).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).


-record(state, {next,
                req}).

init(StreamID, Req, Opts) ->
  error_logger:info_msg("init stream: ~p~nreq: ~p~n~n", [StreamID, Req]),
  {Commands0, Next} = cowboy_stream:init(StreamID, Req, Opts),
  fold(Commands0, #state{req=Req, next=Next}).

data(StreamID, IsFin, Data, State0=#state{next=Next0}) ->
  error_logger:info_msg("stream ~p got data ~p~p", [StreamID, IsFin, Data]),

	{Commands0, Next} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
  fold(Commands0, State0#state{next=Next}).

info(StreamID, Info, State0=#state{next=Next0}) ->
  error_logger:info_msg("stream ~p got info: ~p", [StreamID, Info]),
	{Commands0, Next} = cowboy_stream:info(StreamID, Info, Next0),
  fold(Commands0, State0#state{next=Next}).

terminate(StreamID, Reason, #state{next=Next}) ->
  error_logger:info_msg("stream ~p got terminate: ~p", [StreamID, Reason]),
  cowboy_stream:terminate(StreamID, Reason, Next).

early_error(_StreamID, _Reason, _PartialReq, Resp, _Opts) ->
  Resp.

fold(Commands, State) ->
  fold(Commands, State, []).

fold([], State, Acc) ->
  {lists:reverse(Acc), State};

fold([Command|Tail], State, Acc) ->
  fold(Tail, State, [Command|Acc]).
