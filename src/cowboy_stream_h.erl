%% Copyright (c) 2016-2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_stream_h).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

-export([request_process/3]).
-export([execute/3]).
-export([resume/5]).

%% @todo Need to call subsequent handlers.

-record(state, {
	ref = undefined :: ranch:ref(),
	pid = undefined :: pid(),
	expect = undefined :: undefined | continue,
	read_body_ref = undefined :: reference() | undefined,
	read_body_timer_ref = undefined :: reference() | undefined,
	read_body_length = 0 :: non_neg_integer() | infinity | auto,
	read_body_is_fin = nofin :: nofin | {fin, non_neg_integer()},
	read_body_buffer = <<>> :: binary(),
	body_length = 0 :: non_neg_integer()
}).

%% @todo For shutting down children we need to have a timeout before we terminate
%% the stream like supervisors do. So here just send a message to yourself first,
%% and then decide what to do when receiving this message.

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
	-> {[{spawn, pid(), timeout()}], #state{}}.
init(_StreamID, Req=#{ref := Ref}, Opts) ->
	Env = maps:get(env, Opts, #{}),
	Middlewares = maps:get(middlewares, Opts, [cowboy_router, cowboy_handler]),
	Shutdown = maps:get(shutdown_timeout, Opts, 5000),
	Pid = proc_lib:spawn_link(?MODULE, request_process, [Req, Env, Middlewares]),
	Expect = expect(Req),
	{[{spawn, Pid, Shutdown}], #state{ref=Ref, pid=Pid, expect=Expect}}.

%% Ignore the expect header in HTTP/1.0.
expect(#{version := 'HTTP/1.0'}) ->
	undefined;
expect(Req) ->
	try cowboy_req:parse_header(<<"expect">>, Req) of
		Expect ->
			Expect
	catch _:_ ->
		undefined
	end.

%% If we receive data and stream is waiting for data:
%%   If we accumulated enough data or IsFin=fin, send it.
%%   If we are in auto mode, send it and update flow control.
%%   If not, buffer it.
%% If not, buffer it.
%%
%% We always reset the expect field when we receive data,
%% since the client started sending the request body before
%% we could send a 100 continue response.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State)
	-> {cowboy_stream:commands(), State} when State::#state{}.
%% Stream isn't waiting for data.
data(_StreamID, IsFin, Data, State=#state{
		read_body_ref=undefined, read_body_buffer=Buffer, body_length=BodyLen}) ->
	{[], State#state{
		expect=undefined,
		read_body_is_fin=IsFin,
		read_body_buffer= << Buffer/binary, Data/binary >>,
		body_length=BodyLen + byte_size(Data)}};
%% Stream is waiting for data using auto mode.
%%
%% There is no buffering done in auto mode.
data(_StreamID, IsFin, Data, State=#state{pid=Pid, read_body_ref=Ref,
		read_body_length=auto, body_length=BodyLen}) ->
	send_request_body(Pid, Ref, IsFin, BodyLen, Data),
	{[{flow, byte_size(Data)}], State#state{
		read_body_ref=undefined,
		body_length=BodyLen}};
%% Stream is waiting for data but we didn't receive enough to send yet.
data(_StreamID, nofin, Data, State=#state{
		read_body_length=ReadLen, read_body_buffer=Buffer, body_length=BodyLen})
		when byte_size(Data) + byte_size(Buffer) < ReadLen ->
	{[], State#state{
		expect=undefined,
		read_body_buffer= << Buffer/binary, Data/binary >>,
		body_length=BodyLen + byte_size(Data)}};
%% Stream is waiting for data and we received enough to send.
data(_StreamID, IsFin, Data, State=#state{pid=Pid, read_body_ref=Ref,
		read_body_timer_ref=TRef, read_body_buffer=Buffer, body_length=BodyLen0}) ->
	BodyLen = BodyLen0 + byte_size(Data),
	%% @todo Handle the infinity case where no TRef was defined.
	ok = erlang:cancel_timer(TRef, [{async, true}, {info, false}]),
	send_request_body(Pid, Ref, IsFin, BodyLen, <<Buffer/binary, Data/binary>>),
	{[], State#state{
		expect=undefined,
		read_body_ref=undefined,
		read_body_timer_ref=undefined,
		read_body_buffer= <<>>,
		body_length=BodyLen}}.

-spec info(cowboy_stream:streamid(), any(), State)
	-> {cowboy_stream:commands(), State} when State::#state{}.
info(_StreamID, {'EXIT', Pid, normal}, State=#state{pid=Pid}) ->
	{[stop], State};
info(_StreamID, {'EXIT', Pid, {{request_error, Reason, _HumanReadable}, _}}, State=#state{pid=Pid}) ->
	%% @todo Optionally report the crash to help debugging.
	%%report_crash(Ref, StreamID, Pid, Reason, Stacktrace),
	Status = case Reason of
		timeout -> 408;
		payload_too_large -> 413;
		_ -> 400
	end,
	%% @todo Headers? Details in body? More stuff in debug only?
	{[{error_response, Status, #{<<"content-length">> => <<"0">>}, <<>>}, stop], State};
info(StreamID, Exit = {'EXIT', Pid, {Reason, Stacktrace}}, State=#state{ref=Ref, pid=Pid}) ->
	report_crash(Ref, StreamID, Pid, Reason, Stacktrace),
	{[
		{error_response, 500, #{<<"content-length">> => <<"0">>}, <<>>},
		{internal_error, Exit, 'Stream process crashed.'}
	], State};
%% Request body, auto mode, no body buffered.
info(_StreamID, {read_body, Ref, auto, infinity}, State=#state{read_body_buffer= <<>>}) ->
	{[], State#state{
		read_body_ref=Ref,
		read_body_length=auto}};
%% Request body, auto mode, body buffered or complete.
info(_StreamID, {read_body, Ref, auto, infinity}, State=#state{pid=Pid,
		read_body_is_fin=IsFin, read_body_buffer=Buffer, body_length=BodyLen}) ->
	send_request_body(Pid, Ref, IsFin, BodyLen, Buffer),
	{[{flow, byte_size(Buffer)}], State#state{read_body_buffer= <<>>}};
%% Request body, body buffered large enough or complete.
%%
%% We do not send a 100 continue response if the client
%% already started sending the body.
info(_StreamID, {read_body, Ref, Length, _}, State=#state{pid=Pid,
		read_body_is_fin=IsFin, read_body_buffer=Buffer, body_length=BodyLen})
		when IsFin =:= fin; byte_size(Buffer) >= Length ->
	send_request_body(Pid, Ref, IsFin, BodyLen, Buffer),
	{[], State#state{read_body_buffer= <<>>}};
%% Request body, not enough to send yet.
info(StreamID, {read_body, Ref, Length, Period}, State=#state{expect=Expect}) ->
	Commands = case Expect of
		continue -> [{inform, 100, #{}}, {flow, Length}];
		undefined -> [{flow, Length}]
	end,
	%% @todo Handle the case where Period =:= infinity.
	TRef = erlang:send_after(Period, self(), {{self(), StreamID}, {read_body_timeout, Ref}}),
	{Commands, State#state{
		read_body_ref=Ref,
		read_body_timer_ref=TRef,
		read_body_length=Length}};
%% Request body reading timeout; send what we got.
info(_StreamID, {read_body_timeout, Ref}, State=#state{pid=Pid, read_body_ref=Ref,
		read_body_is_fin=IsFin, read_body_buffer=Buffer, body_length=BodyLen}) ->
	send_request_body(Pid, Ref, IsFin, BodyLen, Buffer),
	{[], State#state{read_body_ref=undefined, read_body_timer_ref=undefined, read_body_buffer= <<>>}};
info(_StreamID, {read_body_timeout, _}, State) ->
	{[], State};
%% Response.
%%
%% We reset the expect field when a 100 continue response
%% is sent or when any final response is sent.
info(_StreamID, Inform = {inform, Status, _}, State0) ->
	State = case Status of
		100 -> State0#state{expect=undefined};
		<<"100">> -> State0#state{expect=undefined};
		<<"100 ", _/bits>> -> State0#state{expect=undefined};
		_ -> State0
	end,
	{[Inform], State};
info(_StreamID, Response = {response, _, _, _}, State) ->
	{[Response], State#state{expect=undefined}};
info(_StreamID, Headers = {headers, _, _}, State) ->
	{[Headers], State#state{expect=undefined}};
info(_StreamID, Data = {data, _, _}, State) ->
	{[Data], State};
info(_StreamID, Trailers = {trailers, _}, State) ->
	{[Trailers], State};
info(_StreamID, Push = {push, _, _, _, _, _, _, _}, State) ->
	{[Push], State};
info(_StreamID, SwitchProtocol = {switch_protocol, _, _, _}, State) ->
	{[SwitchProtocol], State#state{expect=undefined}};
%% Stray message.
info(_StreamID, _Info, State) ->
	{[], State}.

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), #state{}) -> ok.
terminate(_StreamID, _Reason, _State) ->
	ok.

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(),
	cowboy_stream:partial_req(), Resp, cowboy:opts()) -> Resp
	when Resp::cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
	cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

send_request_body(Pid, Ref, nofin, _, Data) ->
	Pid ! {request_body, Ref, nofin, Data},
	ok;
send_request_body(Pid, Ref, fin, BodyLen, Data) ->
	Pid ! {request_body, Ref, fin, BodyLen, Data},
	ok.

%% We use ~999999p here instead of ~w because the latter doesn't
%% support printable strings.
report_crash(_, _, _, normal, _) ->
	ok;
report_crash(_, _, _, shutdown, _) ->
	ok;
report_crash(_, _, _, {shutdown, _}, _) ->
	ok;
report_crash(Ref, StreamID, Pid, Reason, Stacktrace) ->
	error_logger:error_msg(
		"Ranch listener ~p, connection process ~p, stream ~p "
		"had its request process ~p exit with reason "
		"~999999p and stacktrace ~999999p~n",
		[Ref, self(), StreamID, Pid, Reason, Stacktrace]).

%% Request process.

%% We catch all exceptions in order to add the stacktrace to
%% the exit reason as it is not propagated by proc_lib otherwise
%% and therefore not present in the 'EXIT' message. We want
%% the stacktrace in order to simplify debugging of errors.
%%
%% This + the behavior in proc_lib means that we will get a
%% {Reason, Stacktrace} tuple for every exceptions, instead of
%% just for errors and throws.
%%
%% @todo Better spec.
-spec request_process(_, _, _) -> _.
request_process(Req, Env, Middlewares) ->
	OTP = erlang:system_info(otp_release),
	try
		execute(Req, Env, Middlewares)
	catch
		exit:Reason ->
			Stacktrace = erlang:get_stacktrace(),
			erlang:raise(exit, {Reason, Stacktrace}, Stacktrace);
		%% OTP 19 does not propagate any exception stacktraces,
		%% we therefore add it for every class of exception.
		_:Reason when OTP =:= "19" ->
			Stacktrace = erlang:get_stacktrace(),
			erlang:raise(exit, {Reason, Stacktrace}, Stacktrace);
		%% @todo I don't think this clause is necessary.
		Class:Reason ->
			erlang:raise(Class, Reason, erlang:get_stacktrace())
	end.

%% @todo
%-spec execute(cowboy_req:req(), #state{}, cowboy_middleware:env(), [module()])
%	-> ok.
-spec execute(_, _, _) -> _.
execute(_, _, []) ->
	ok; %% @todo Maybe error reason should differ here and there.
execute(Req, Env, [Middleware|Tail]) ->
	case Middleware:execute(Req, Env) of
		{ok, Req2, Env2} ->
			execute(Req2, Env2, Tail);
		{suspend, Module, Function, Args} ->
			proc_lib:hibernate(?MODULE, resume, [Env, Tail, Module, Function, Args]);
		{stop, _Req2} ->
			ok %% @todo Maybe error reason should differ here and there.
	end.

-spec resume(cowboy_middleware:env(), [module()],
	module(), module(), [any()]) -> ok.
resume(Env, Tail, Module, Function, Args) ->
	case apply(Module, Function, Args) of
		{ok, Req2, Env2} ->
			execute(Req2, Env2, Tail);
		{suspend, Module2, Function2, Args2} ->
			proc_lib:hibernate(?MODULE, resume, [Env, Tail, Module2, Function2, Args2]);
		{stop, _Req2} ->
			ok %% @todo Maybe error reason should differ here and there.
	end.
