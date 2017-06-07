%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(error_logger_lager_hook_h).
-behaviour(gen_event).

%% API
-export([replace/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
	 handle_info/2, terminate/2, code_change/3]).

%% default lager handler

-define(LAGER_HANDLER, error_logger_lager_h).

%% API

replace() ->
	HighWaterMark = case application:get_env(lager, error_logger_hwm) of
		{ok, HwmVal} when is_integer(HwmVal), HwmVal > 0 ->
			HwmVal;
		{ok, BadVal} ->
			_ = lager:log(warning, self(), "Invalid error_logger high water mark: ~p, disabling", [BadVal]),
			undefined;
		undefined ->
			undefined
	end,
	gen_event:delete_handler(error_logger, ?LAGER_HANDLER, []),
	gen_event:add_handler(error_logger, ?MODULE, [HighWaterMark]).

%% gen_event callbacks

init(Args) ->
	?LAGER_HANDLER:init(Args).

handle_event(Event = {error, GL, {Pid, Fmt, Args}}, State) ->
	Event2 = case Fmt of
			 "** Cowboy "++_ ->
				 case Args of
					 [M, F, A, Class, Reason, _HandlerState, _ReqList, StackTrace] ->
						 StackTrace2 = pretty_stacktrace(Class, Reason, StackTrace),
						 {error, GL, {Pid, "** Cowboy exeption in ~p:~p/~p: ~n~s",
							      [M, F, A, StackTrace2]}};
					 _ ->
						 Event
				 end;
			 _ ->
				 Event
		 end,
	?LAGER_HANDLER:handle_event(Event2, State);
handle_event(Event, State) ->
	?LAGER_HANDLER:handle_event(Event, State).

handle_call(Request, State) ->
	?LAGER_HANDLER:handle_call(Request, State).

handle_info(Info, State) ->
	?LAGER_HANDLER:handle_info(Info, State).

terminate(Reason, State) ->
	?LAGER_HANDLER:terminate(Reason, State).

code_change(OldVsn, State, Extra) ->
	?LAGER_HANDLER:code_change(OldVsn, State, Extra).


%% Internal functions

pretty_stacktrace(Class, Reason, StackTrace) ->
	_ = code:ensure_loaded(eunit_lib),
	case erlang:function_exported(eunit_lib, format_exception, 1) of
		false ->
			StackTrace;
		true ->
			eunit_lib:format_exception({Class, Reason, StackTrace})
	end.
