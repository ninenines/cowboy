%% Copyright (c) 2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_metrics_h).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

-type proc_metrics() :: #{pid() => #{
	%% Time at which the process spawned.
	spawn := integer(),

	%% Time at which the process exited.
	exit => integer(),

	%% Reason for the process exit.
	reason => any()
}}.

-type informational_metrics() :: #{
	%% Informational response status.
	status := cowboy:http_status(),

	%% Headers sent with the informational response.
	headers := cowboy:http_headers(),

	%% Time when the informational response was sent.
	time := integer()
}.

-type metrics() :: #{
	%% The identifier for this listener.
	ref := ranch:ref(),

	%% The pid for this connection.
	pid := pid(),

	%% The streamid also indicates the total number of requests on
	%% this connection (StreamID div 2 + 1).
	streamid := cowboy_stream:streamid(),

	%% The terminate reason is always useful.
	reason := cowboy_stream:reason(),

	%% A filtered Req object or a partial Req object
	%% depending on how far the request got to.
	req => cowboy_req:req(),
	partial_req => cowboy_stream:partial_req(),

	%% Response status.
	resp_status := cowboy:http_status(),

	%% Filtered response headers.
	resp_headers := cowboy:http_headers(),

	%% Start/end of the processing of the request.
	%%
	%% This represents the time from this stream handler's init
	%% to terminate.
	req_start => integer(),
	req_end => integer(),

	%% Start/end of the receiving of the request body.
	%% Begins when the first packet has been received.
	req_body_start => integer(),
	req_body_end => integer(),

	%% Start/end of the sending of the response.
	%% Begins when we send the headers and ends on the final
	%% packet of the response body. If everything is sent at
	%% once these values are identical.
	resp_start => integer(),
	resp_end => integer(),

	%% For early errors all we get is the time we received it.
	early_error_time => integer(),

	%% Start/end of spawned processes. This is where most of
	%% the user code lies, excluding stream handlers. On a
	%% default Cowboy configuration there should be only one
	%% process: the request process.
	procs => proc_metrics(),

	%% Informational responses sent before the final response.
	informational => [informational_metrics()],

	%% Length of the request and response bodies. This does
	%% not include the framing.
	req_body_length => non_neg_integer(),
	resp_body_length => non_neg_integer(),

	%% Additional metadata set by the user.
	user_data => map()
}.
-export_type([metrics/0]).

-type metrics_callback() :: fun((metrics()) -> any()).
-export_type([metrics_callback/0]).

-record(state, {
	next :: any(),
	callback :: fun((metrics()) -> any()),
	resp_headers_filter :: undefined | fun((cowboy:http_headers()) -> cowboy:http_headers()),
	req :: map(),
	resp_status :: undefined | cowboy:http_status(),
	resp_headers :: undefined | cowboy:http_headers(),
	ref :: ranch:ref(),
	req_start :: integer(),
	req_end :: undefined | integer(),
	req_body_start :: undefined | integer(),
	req_body_end :: undefined | integer(),
	resp_start :: undefined | integer(),
	resp_end :: undefined | integer(),
	procs = #{} :: proc_metrics(),
	informational = [] :: [informational_metrics()],
	req_body_length = 0 :: non_neg_integer(),
	resp_body_length = 0 :: non_neg_integer(),
	user_data = #{} :: map()
}).

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
	-> {[{spawn, pid(), timeout()}], #state{}}.
init(StreamID, Req=#{ref := Ref}, Opts=#{metrics_callback := Fun}) ->
	ReqStart = erlang:monotonic_time(),
	{Commands, Next} = cowboy_stream:init(StreamID, Req, Opts),
	FilteredReq = case maps:get(metrics_req_filter, Opts, undefined) of
		undefined -> Req;
		ReqFilter -> ReqFilter(Req)
	end,
	RespHeadersFilter = maps:get(metrics_resp_headers_filter, Opts, undefined),
	{Commands, fold(Commands, #state{
		next=Next,
		callback=Fun,
		resp_headers_filter=RespHeadersFilter,
		req=FilteredReq,
		ref=Ref,
		req_start=ReqStart
	})}.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State)
	-> {cowboy_stream:commands(), State} when State::#state{}.
data(StreamID, IsFin=fin, Data, State=#state{req_body_start=undefined}) ->
	ReqBody = erlang:monotonic_time(),
	do_data(StreamID, IsFin, Data, State#state{
		req_body_start=ReqBody,
		req_body_end=ReqBody,
		req_body_length=byte_size(Data)
	});
data(StreamID, IsFin=fin, Data, State=#state{req_body_length=ReqBodyLen}) ->
	ReqBodyEnd = erlang:monotonic_time(),
	do_data(StreamID, IsFin, Data, State#state{
		req_body_end=ReqBodyEnd,
		req_body_length=ReqBodyLen + byte_size(Data)
	});
data(StreamID, IsFin, Data, State=#state{req_body_start=undefined}) ->
	ReqBodyStart = erlang:monotonic_time(),
	do_data(StreamID, IsFin, Data, State#state{
		req_body_start=ReqBodyStart,
		req_body_length=byte_size(Data)
	});
data(StreamID, IsFin, Data, State=#state{req_body_length=ReqBodyLen}) ->
	do_data(StreamID, IsFin, Data, State#state{
		req_body_length=ReqBodyLen + byte_size(Data)
	}).

do_data(StreamID, IsFin, Data, State0=#state{next=Next0}) ->
	{Commands, Next} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
	{Commands, fold(Commands, State0#state{next=Next})}.

-spec info(cowboy_stream:streamid(), any(), State)
	-> {cowboy_stream:commands(), State} when State::#state{}.
info(StreamID, Info={'EXIT', Pid, Reason}, State0=#state{procs=Procs}) ->
	ProcEnd = erlang:monotonic_time(),
	P = maps:get(Pid, Procs),
	State = State0#state{procs=Procs#{Pid => P#{
		exit => ProcEnd,
		reason => Reason
	}}},
	do_info(StreamID, Info, State);
info(StreamID, Info, State) ->
	do_info(StreamID, Info, State).

do_info(StreamID, Info, State0=#state{next=Next0}) ->
	{Commands, Next} = cowboy_stream:info(StreamID, Info, Next0),
	{Commands, fold(Commands, State0#state{next=Next})}.

fold([], State) ->
	State;
fold([{spawn, Pid, _}|Tail], State0=#state{procs=Procs}) ->
	ProcStart = erlang:monotonic_time(),
	State = State0#state{procs=Procs#{Pid => #{spawn => ProcStart}}},
	fold(Tail, State);
fold([{inform, Status, Headers}|Tail],
		State=#state{informational=Infos}) ->
	Time = erlang:monotonic_time(),
	fold(Tail, State#state{informational=[#{
		status => Status,
		headers => Headers,
		time => Time
	}|Infos]});
fold([{response, Status, Headers, Body}|Tail],
		State=#state{resp_headers_filter=RespHeadersFilter}) ->
	Resp = erlang:monotonic_time(),
	fold(Tail, State#state{
		resp_status=Status,
		resp_headers=case RespHeadersFilter of
			undefined -> Headers;
			_ -> RespHeadersFilter(Headers)
		end,
		resp_start=Resp,
		resp_end=Resp,
		resp_body_length=resp_body_length(Body)
	});
fold([{error_response, Status, Headers, Body}|Tail],
		State=#state{resp_status=RespStatus}) ->
	%% The error_response command only results in a response
	%% if no response was sent before.
	case RespStatus of
		undefined ->
			fold([{response, Status, Headers, Body}|Tail], State);
		_ ->
			fold(Tail, State)
	end;
fold([{headers, Status, Headers}|Tail],
		State=#state{resp_headers_filter=RespHeadersFilter}) ->
	RespStart = erlang:monotonic_time(),
	fold(Tail, State#state{
		resp_status=Status,
		resp_headers=case RespHeadersFilter of
			undefined -> Headers;
			_ -> RespHeadersFilter(Headers)
		end,
		resp_start=RespStart
	});
%% @todo It might be worthwhile to keep the sendfile information around,
%% especially if these frames ultimately result in a sendfile syscall.
fold([{data, nofin, Data}|Tail], State=#state{resp_body_length=RespBodyLen}) ->
	fold(Tail, State#state{
		resp_body_length=RespBodyLen + resp_body_length(Data)
	});
fold([{data, fin, Data}|Tail], State=#state{resp_body_length=RespBodyLen}) ->
	RespEnd = erlang:monotonic_time(),
	fold(Tail, State#state{
		resp_end=RespEnd,
		resp_body_length=RespBodyLen + resp_body_length(Data)
	});
fold([{set_options, SetOpts}|Tail], State0=#state{user_data=OldUserData}) ->
	State = case SetOpts of
		#{metrics_user_data := NewUserData} ->
			State0#state{user_data=maps:merge(OldUserData, NewUserData)};
		_ ->
			State0
	end,
	fold(Tail, State);
fold([_|Tail], State) ->
	fold(Tail, State).

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), #state{}) -> any().
terminate(StreamID, Reason, #state{next=Next, callback=Fun,
		req=Req, resp_status=RespStatus, resp_headers=RespHeaders, ref=Ref,
		req_start=ReqStart, req_body_start=ReqBodyStart,
		req_body_end=ReqBodyEnd, resp_start=RespStart, resp_end=RespEnd,
		procs=Procs, informational=Infos, user_data=UserData,
		req_body_length=ReqBodyLen, resp_body_length=RespBodyLen}) ->
	Res = cowboy_stream:terminate(StreamID, Reason, Next),
	ReqEnd = erlang:monotonic_time(),
	Metrics = #{
		ref => Ref,
		pid => self(),
		streamid => StreamID,
		reason => Reason,
		req => Req,
		resp_status => RespStatus,
		resp_headers => RespHeaders,
		req_start => ReqStart,
		req_end => ReqEnd,
		req_body_start => ReqBodyStart,
		req_body_end => ReqBodyEnd,
		resp_start => RespStart,
		resp_end => RespEnd,
		procs => Procs,
		informational => lists:reverse(Infos),
		req_body_length => ReqBodyLen,
		resp_body_length => RespBodyLen,
		user_data => UserData
	},
	Fun(Metrics),
	Res.

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(),
	cowboy_stream:partial_req(), Resp, cowboy:opts()) -> Resp
	when Resp::cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq=#{ref := Ref}, Resp0, Opts=#{metrics_callback := Fun}) ->
	Time = erlang:monotonic_time(),
	Resp = {response, RespStatus, RespHeaders, RespBody}
		= cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp0, Opts),
	%% As far as metrics go we are limited in what we can provide
	%% in this case.
	Metrics = #{
		ref => Ref,
		pid => self(),
		streamid => StreamID,
		reason => Reason,
		partial_req => PartialReq,
		resp_status => RespStatus,
		resp_headers => RespHeaders,
		early_error_time => Time,
		resp_body_length => resp_body_length(RespBody)
	},
	Fun(Metrics),
	Resp.

resp_body_length({sendfile, _, Len, _}) ->
	Len;
resp_body_length(Data) ->
	iolist_size(Data).
