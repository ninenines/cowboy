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

-export([proc_lib_hack/3]).
-export([execute/3]).
-export([resume/5]).

%% @todo Need to call subsequent handlers.

-record(state, {
	ref = undefined :: ranch:ref(),
	pid = undefined :: pid(),
	read_body_ref = undefined :: reference() | undefined,
	read_body_timer_ref = undefined :: reference() | undefined,
	read_body_length = 0 :: non_neg_integer() | infinity,
	read_body_is_fin = nofin :: nofin | {fin, non_neg_integer()},
	read_body_buffer = <<>> :: binary()
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
	Pid = proc_lib:spawn_link(?MODULE, proc_lib_hack, [Req, Env, Middlewares]),
	{[{spawn, Pid, Shutdown}], #state{ref=Ref, pid=Pid}}.

%% If we receive data and stream is waiting for data:
%%	If we accumulated enough data or IsFin=fin, send it.
%%	If not, buffer it.
%% If not, buffer it.
-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State)
	-> {cowboy_stream:commands(), State} when State::#state{}.
data(_StreamID, IsFin, Data, State=#state{read_body_ref=undefined, read_body_buffer=Buffer}) ->
	{[], State#state{read_body_is_fin=IsFin, read_body_buffer= << Buffer/binary, Data/binary >>}};
data(_StreamID, nofin, Data, State=#state{read_body_length=Length, read_body_buffer=Buffer}) when byte_size(Data) + byte_size(Buffer) < Length ->
	{[], State#state{read_body_buffer= << Buffer/binary, Data/binary >>}};
data(_StreamID, IsFin, Data, State=#state{pid=Pid, read_body_ref=Ref,
		read_body_timer_ref=TRef, read_body_buffer=Buffer}) ->
	ok = erlang:cancel_timer(TRef, [{async, true}, {info, false}]),
	Pid ! {request_body, Ref, IsFin, << Buffer/binary, Data/binary >>},
	{[], State#state{read_body_ref=undefined, read_body_timer_ref=undefined, read_body_buffer= <<>>}}.

-spec info(cowboy_stream:streamid(), any(), State)
	-> {cowboy_stream:commands(), State} when State::#state{}.
info(_StreamID, {'EXIT', Pid, normal}, State=#state{pid=Pid}) ->
	%% @todo Do we even reach this clause?
	{[stop], State};
info(_StreamID, {'EXIT', Pid, {_Reason, [_, {cow_http_hd, _, _, _}|_]}}, State=#state{pid=Pid}) ->
	%% @todo Have an option to enable/disable this specific crash report?
	%%report_crash(Ref, StreamID, Pid, Reason, Stacktrace),
	%% @todo Headers? Details in body? More stuff in debug only?
	{[{error_response, 400, #{<<"content-length">> => <<"0">>}, <<>>}, stop], State};
info(StreamID, Exit = {'EXIT', Pid, {Reason, Stacktrace}}, State=#state{ref=Ref, pid=Pid}) ->
	report_crash(Ref, StreamID, Pid, Reason, Stacktrace),
	{[
		{error_response, 500, #{<<"content-length">> => <<"0">>}, <<>>},
		{internal_error, Exit, 'Stream process crashed.'}
	], State};
%% Request body, no body buffer but IsFin=fin.
%info(_StreamID, {read_body, Ref, _, _}, State=#state{pid=Pid, read_body_is_fin=fin, read_body_buffer= <<>>}) ->
%	Pid ! {request_body, Ref, fin, <<>>},
%	{[], State};
%% Request body, body buffered large enough or complete.
info(_StreamID, {read_body, Ref, Length, _},
		State=#state{pid=Pid, read_body_is_fin=IsFin, read_body_buffer=Data})
		when element(1, IsFin) =:= fin; byte_size(Data) >= Length ->
	Pid ! {request_body, Ref, IsFin, Data},
	{[], State#state{read_body_buffer= <<>>}};
%% Request body, not enough to send yet.
info(StreamID, {read_body, Ref, Length, Period}, State) ->
	TRef = erlang:send_after(Period, self(), {{self(), StreamID}, {read_body_timeout, Ref}}),
	{[{flow, Length}], State#state{read_body_ref=Ref, read_body_timer_ref=TRef, read_body_length=Length}};
%% Request body reading timeout; send what we got.
info(_StreamID, {read_body_timeout, Ref}, State=#state{pid=Pid, read_body_ref=Ref,
		read_body_is_fin=IsFin, read_body_buffer=Buffer}) ->
	Pid ! {request_body, Ref, IsFin, Buffer},
	{[], State#state{read_body_ref=undefined, read_body_timer_ref=undefined, read_body_buffer= <<>>}};
info(_StreamID, {read_body_timeout, _}, State) ->
	{[], State};
%% Response.
info(_StreamID, Response = {response, _, _, _}, State) ->
	{[Response], State};
info(_StreamID, Headers = {headers, _, _}, State) ->
	{[Headers], State};
info(_StreamID, Data = {data, _, _}, State) ->
	{[Data], State};
info(_StreamID, Push = {push, _, _, _, _, _, _, _}, State) ->
	{[Push], State};
info(_StreamID, SwitchProtocol = {switch_protocol, _, _, _}, State) ->
	{[SwitchProtocol], State};
%% Stray message.
info(_StreamID, _Info, State) ->
	%% @todo Cleanup if no reply was sent when stream ends.
	{[], State}.

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), #state{}) -> ok.
terminate(_StreamID, _Reason, _State) ->
	ok.

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(),
	cowboy_stream:partial_req(), Resp, cowboy:opts()) -> Resp
	when Resp::cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
	cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

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

%% @todo This should wrap with try/catch to get the full error
%% in the stream handler. Only then can we decide what to do
%% about it. This means that we should remove any other try/catch
%% in the request process.

%% This hack is necessary because proc_lib does not propagate
%% stacktraces by default. This is ugly because we end up
%% having two try/catch instead of one (the one in proc_lib),
%% just to add the stacktrace information.
%%
%% @todo Remove whenever proc_lib propagates stacktraces.
-spec proc_lib_hack(_, _, _) -> _.
proc_lib_hack(Req, Env, Middlewares) ->
	try
		execute(Req, Env, Middlewares)
	catch
		_:Reason when element(1, Reason) =:= cowboy_handler ->
			exit(Reason);
		_:Reason ->
			exit({Reason, erlang:get_stacktrace()})
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
