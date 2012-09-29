%% Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>
%% Copyright (c) 2011, Anthony Ramine <nox@dev-extend.eu>
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

%% @doc HTTP protocol handler.
%%
%% The available options are:
%% <dl>
%%  <dt>dispatch</dt><dd>The dispatch list for this protocol.</dd>
%%  <dt>max_empty_lines</dt><dd>Max number of empty lines before a request.
%%   Defaults to 5.</dd>
%%  <dt>max_header_name_length</dt><dd>Max length allowed for header names.
%%   Defaults to 64.</dd>
%%  <dt>max_header_value_length</dt><dd>Max length allowed for header values.
%%   Defaults to 4096.</dd>
%%  <dt>max_headers</dt><dd>Max number of headers allowed.
%%   Defaults to 100.</dd>
%%  <dt>max_keepalive</dt><dd>Max number of requests allowed in a single
%%   keep-alive session. Defaults to infinity.</dd>
%%  <dt>max_request_line_length</dt><dd>Max length allowed for the request
%%   line. Defaults to 4096.</dd>
%%  <dt>onrequest</dt><dd>Optional fun that allows Req interaction before
%%   any dispatching is done. Host info, path info and bindings are thus
%%   not available at this point.</dd>
%%  <dt>onresponse</dt><dd>Optional fun that allows replacing a response
%%   sent by the application based on its status code or headers.</dd>
%%  <dt>timeout</dt><dd>Time in milliseconds before an idle
%%   connection is closed. Defaults to 5000 milliseconds.</dd>
%% </dl>
%%
%% Note that there is no need to monitor these processes when using Cowboy as
%% an application as it already supervises them under the listener supervisor.
%%
%% @see cowboy_dispatcher
%% @see cowboy_http_handler
-module(cowboy_protocol).

%% API.
-export([start_link/4]).

%% Internal.
-export([init/4]).
-export([parse_request/3]).
-export([handler_loop/4]).

-type onrequest_fun() :: fun((Req) -> Req).
-type onresponse_fun() ::
	fun((cowboy_http:status(), cowboy_http:headers(), Req) -> Req).

-export_type([onrequest_fun/0]).
-export_type([onresponse_fun/0]).

-record(state, {
	listener :: pid(),
	socket :: inet:socket(),
	transport :: module(),
	dispatch :: cowboy_dispatcher:dispatch_rules(),
	onrequest :: undefined | onrequest_fun(),
	onresponse = undefined :: undefined | onresponse_fun(),
	max_empty_lines :: non_neg_integer(),
	req_keepalive = 1 :: non_neg_integer(),
	max_keepalive :: non_neg_integer(),
	max_request_line_length :: non_neg_integer(),
	max_header_name_length :: non_neg_integer(),
	max_header_value_length :: non_neg_integer(),
	max_headers :: non_neg_integer(),
	timeout :: timeout(),
	hibernate = false :: boolean(),
	loop_timeout = infinity :: timeout(),
	loop_timeout_ref :: undefined | reference()
}).

%% API.

%% @doc Start an HTTP protocol process.
-spec start_link(pid(), inet:socket(), module(), any()) -> {ok, pid()}.
start_link(ListenerPid, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
	{ok, Pid}.

%% Internal.

%% @doc Faster alternative to proplists:get_value/3.
%% @private
get_value(Key, Opts, Default) ->
	case lists:keyfind(Key, 1, Opts) of
		{_, Value} -> Value;
		_ -> Default
	end.

%% @private
-spec init(pid(), inet:socket(), module(), any()) -> ok.
init(ListenerPid, Socket, Transport, Opts) ->
	Dispatch = get_value(dispatch, Opts, []),
	MaxEmptyLines = get_value(max_empty_lines, Opts, 5),
	MaxHeaderNameLength = get_value(max_header_name_length, Opts, 64),
	MaxHeaderValueLength = get_value(max_header_value_length, Opts, 4096),
	MaxHeaders = get_value(max_headers, Opts, 100),
	MaxKeepalive = get_value(max_keepalive, Opts, infinity),
	MaxRequestLineLength = get_value(max_request_line_length, Opts, 4096),
	OnRequest = get_value(onrequest, Opts, undefined),
	OnResponse = get_value(onresponse, Opts, undefined),
	Timeout = get_value(timeout, Opts, 5000),
	ok = ranch:accept_ack(ListenerPid),
	wait_request(<<>>, #state{listener=ListenerPid, socket=Socket,
		transport=Transport, dispatch=Dispatch,
		max_empty_lines=MaxEmptyLines, max_keepalive=MaxKeepalive,
		max_request_line_length=MaxRequestLineLength,
		max_header_name_length=MaxHeaderNameLength,
		max_header_value_length=MaxHeaderValueLength, max_headers=MaxHeaders,
		timeout=Timeout, onrequest=OnRequest, onresponse=OnResponse}, 0).

%% Request parsing.
%%
%% The next set of functions is the request parsing code. All of it
%% runs using a single binary match context. This optimization ends
%% right after the header parsing is finished and the code becomes
%% more interesting past that point.

-spec wait_request(binary(), #state{}, non_neg_integer()) -> ok.
wait_request(Buffer, State=#state{socket=Socket, transport=Transport,
		timeout=Timeout}, ReqEmpty) ->
	case Transport:recv(Socket, 0, Timeout) of
		{ok, Data} ->
			parse_request(<< Buffer/binary, Data/binary >>, State, ReqEmpty);
		{error, _} ->
			terminate(State)
	end.

%% @private
-spec parse_request(binary(), #state{}, non_neg_integer()) -> ok.
%% Empty lines must be using \r\n.
parse_request(<< $\n, _/binary >>, State, _) ->
	error_terminate(400, State);
%% We limit the length of the Request-line to MaxLength to avoid endlessly
%% reading from the socket and eventually crashing.
parse_request(Buffer, State=#state{max_request_line_length=MaxLength,
		max_empty_lines=MaxEmpty}, ReqEmpty) ->
	case binary:match(Buffer, <<"\n">>) of
		nomatch when byte_size(Buffer) > MaxLength ->
			error_terminate(414, State);
		nomatch ->
			wait_request(Buffer, State, ReqEmpty);
		{1, _} when ReqEmpty =:= MaxEmpty ->
			error_terminate(400, State);
		{1, _} ->
			<< _:16, Rest/binary >> = Buffer,
			parse_request(Rest, State, ReqEmpty + 1);
		{_, _} ->
			parse_method(Buffer, State, <<>>)
	end.

parse_method(<< C, Rest/bits >>, State, SoFar) ->
	case C of
		$\r -> error_terminate(400, State);
		$\s -> parse_uri(Rest, State, SoFar);
		_ -> parse_method(Rest, State, << SoFar/binary, C >>)
	end.

parse_uri(<< $\r, _/bits >>, State, _) ->
	error_terminate(400, State);
parse_uri(<< "* ", Rest/bits >>, State, Method) ->
	parse_version(Rest, State, Method, <<"*">>, <<>>, <<>>);
parse_uri(<< "http://", Rest/bits >>, State, Method) ->
	parse_uri_skip_host(Rest, State, Method);
parse_uri(<< "https://", Rest/bits >>, State, Method) ->
	parse_uri_skip_host(Rest, State, Method);
parse_uri(Buffer, State, Method) ->
	parse_uri_path(Buffer, State, Method, <<>>).

parse_uri_skip_host(<< C, Rest/bits >>, State, Method) ->
	case C of
		$\r -> error_terminate(400, State);
		$/ -> parse_uri_path(Rest, State, Method, <<"/">>);
		_ -> parse_uri_skip_host(Rest, State, Method)
	end.

parse_uri_path(<< C, Rest/bits >>, State, Method, SoFar) ->
	case C of
		$\r -> error_terminate(400, State);
		$\s -> parse_version(Rest, State, Method, SoFar, <<>>, <<>>);
		$? -> parse_uri_query(Rest, State, Method, SoFar, <<>>);
		$# -> parse_uri_fragment(Rest, State, Method, SoFar, <<>>, <<>>);
		_ -> parse_uri_path(Rest, State, Method, << SoFar/binary, C >>)
	end.

parse_uri_query(<< C, Rest/bits >>, S, M, P, SoFar) ->
	case C of
		$\r -> error_terminate(400, S);
		$\s -> parse_version(Rest, S, M, P, SoFar, <<>>);
		$# -> parse_uri_fragment(Rest, S, M, P, SoFar, <<>>);
		_ -> parse_uri_query(Rest, S, M, P, << SoFar/binary, C >>)
	end.

parse_uri_fragment(<< C, Rest/bits >>, S, M, P, Q, SoFar) ->
	case C of
		$\r -> error_terminate(400, S);
		$\s -> parse_version(Rest, S, M, P, Q, SoFar);
		_ -> parse_uri_fragment(Rest, S, M, P, Q, << SoFar/binary, C >>)
	end.

parse_version(<< "HTTP/1.1\r\n", Rest/bits >>, S, M, P, Q, F) ->
	parse_header(Rest, S, M, P, Q, F, {1, 1}, []);
parse_version(<< "HTTP/1.0\r\n", Rest/bits >>, S, M, P, Q, F) ->
	parse_header(Rest, S, M, P, Q, F, {1, 0}, []);
parse_version(_, State, _, _, _, _) ->
	error_terminate(505, State).

%% Stop receiving data if we have more than allowed number of headers.
wait_header(_, State=#state{max_headers=MaxHeaders}, _, _, _, _, _, Headers)
		when length(Headers) >= MaxHeaders ->
	error_terminate(400, State);
wait_header(Buffer, State=#state{socket=Socket, transport=Transport,
		timeout=Timeout}, M, P, Q, F, V, H) ->
	case Transport:recv(Socket, 0, Timeout) of
		{ok, Data} ->
			parse_header(<< Buffer/binary, Data/binary >>,
				State, M, P, Q, F, V, H);
		{error, timeout} ->
			error_terminate(408, State);
		{error, _} ->
			terminate(State)
	end.

parse_header(<< $\r, $\n, Rest/bits >>, S, M, P, Q, F, V, Headers) ->
	request(Rest, S, M, P, Q, F, V, lists:reverse(Headers));
parse_header(Buffer, State=#state{max_header_name_length=MaxLength},
		M, P, Q, F, V, H) ->
	case binary:match(Buffer, <<":">>) of
		nomatch when byte_size(Buffer) > MaxLength ->
			error_terminate(400, State);
		nomatch ->
			wait_header(Buffer, State, M, P, Q, F, V, H);
		{_, _} ->
			parse_hd_name(Buffer, State, M, P, Q, F, V, H, <<>>)
	end.

%% I know, this isn't exactly pretty. But this is the most critical
%% code path and as such needs to be optimized to death.
%%
%% ... Sorry for your eyes.
%%
%% But let's be honest, that's still pretty readable.
parse_hd_name(<< C, Rest/bits >>, S, M, P, Q, F, V, H, SoFar) ->
	case C of
		$: -> parse_hd_before_value(Rest, S, M, P, Q, F, V, H, SoFar);
		$\s -> parse_hd_name_ws(Rest, S, M, P, Q, F, V, H, SoFar);
		$\t -> parse_hd_name_ws(Rest, S, M, P, Q, F, V, H, SoFar);
		$A -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $a >>);
		$B -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $b >>);
		$C -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $c >>);
		$D -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $d >>);
		$E -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $e >>);
		$F -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $f >>);
		$G -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $g >>);
		$H -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $h >>);
		$I -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $i >>);
		$J -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $j >>);
		$K -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $k >>);
		$L -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $l >>);
		$M -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $m >>);
		$N -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $n >>);
		$O -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $o >>);
		$P -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $p >>);
		$Q -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $q >>);
		$R -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $r >>);
		$S -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $s >>);
		$T -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $t >>);
		$U -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $u >>);
		$V -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $v >>);
		$W -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $w >>);
		$X -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $x >>);
		$Y -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $y >>);
		$Z -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, $z >>);
		C -> parse_hd_name(Rest, S, M, P, Q, F, V, H, << SoFar/binary, C >>)
	end.

parse_hd_name_ws(<< C, Rest/bits >>, S, M, P, Q, F, V, H, Name) ->
	case C of
		$\s -> parse_hd_name_ws(Rest, S, M, P, Q, F, V, H, Name);
		$\t -> parse_hd_name_ws(Rest, S, M, P, Q, F, V, H, Name);
		$: -> parse_hd_before_value(Rest, S, M, P, Q, F, V, H, Name)
	end.

wait_hd_before_value(Buffer, State=#state{
		socket=Socket, transport=Transport, timeout=Timeout},
		M, P, Q, F, V, H, N) ->
	case Transport:recv(Socket, 0, Timeout) of
		{ok, Data} ->
			parse_hd_before_value(<< Buffer/binary, Data/binary >>,
				State, M, P, Q, F, V, H, N);
		{error, timeout} ->
			error_terminate(408, State);
		{error, _} ->
			terminate(State)
	end.

parse_hd_before_value(<< $\s, Rest/bits >>, S, M, P, Q, F, V, H, N) ->
	parse_hd_before_value(Rest, S, M, P, Q, F, V, H, N);
parse_hd_before_value(<< $\t, Rest/bits >>, S, M, P, Q, F, V, H, N) ->
	parse_hd_before_value(Rest, S, M, P, Q, F, V, H, N);
parse_hd_before_value(Buffer, State=#state{
		max_header_value_length=MaxLength}, M, P, Q, F, V, H, N) ->
	case binary:match(Buffer, <<"\n">>) of
		nomatch when byte_size(Buffer) > MaxLength ->
			error_terminate(400, State);
		nomatch ->
			wait_hd_before_value(Buffer, State, M, P, Q, F, V, H, N);
		{_, _} ->
			parse_hd_value(Buffer, State, M, P, Q, F, V, H, N, <<>>)
	end.

%% We completely ignore the first argument which is always
%% the empty binary. We keep it there because we don't want
%% to change the other arguments' position and trigger costy
%% operations for no reasons.
wait_hd_value(_, State=#state{
		socket=Socket, transport=Transport, timeout=Timeout},
		M, P, Q, F, V, H, N, SoFar) ->
	case Transport:recv(Socket, 0, Timeout) of
		{ok, Data} ->
			parse_hd_value(Data, State, M, P, Q, F, V, H, N, SoFar);
		{error, timeout} ->
			error_terminate(408, State);
		{error, _} ->
			terminate(State)
	end.

%% Pushing back as much as we could the retrieval of new data
%% to check for multilines allows us to avoid a few tests in
%% the critical path, but forces us to have a special function.
wait_hd_value_nl(_, State=#state{
		socket=Socket, transport=Transport, timeout=Timeout},
		M, P, Q, F, V, Headers, Name, SoFar) ->
	case Transport:recv(Socket, 0, Timeout) of
		{ok, << C, Data/bits >>} when C =:= $\s; C =:= $\t  ->
			parse_hd_value(Data, State, M, P, Q, F, V, Headers, Name, SoFar);
		{ok, Data} ->
			parse_header(Data, State, M, P, Q, F, V, [{Name, SoFar}|Headers]);
		{error, timeout} ->
			error_terminate(408, State);
		{error, _} ->
			terminate(State)
	end.

parse_hd_value(<< $\r, Rest/bits >>, S, M, P, Q, F, V, Headers, Name, SoFar) ->
	case Rest of
		<< $\n >> ->
			wait_hd_value_nl(<<>>, S, M, P, Q, F, V, Headers, Name, SoFar);
		<< $\n, C, Rest2/bits >> when C =:= $\s; C =:= $\t ->
			parse_hd_value(Rest2, S, M, P, Q, F, V, Headers, Name, SoFar);
		<< $\n, Rest2/bits >> ->
			parse_header(Rest2, S, M, P, Q, F, V, [{Name, SoFar}|Headers])
	end;
parse_hd_value(<< C, Rest/bits >>, S, M, P, Q, F, V, H, N, SoFar) ->
	parse_hd_value(Rest, S, M, P, Q, F, V, H, N, << SoFar/binary, C >>);
parse_hd_value(<<>>, State=#state{max_header_value_length=MaxLength},
		_, _, _, _, _, _, _, SoFar) when byte_size(SoFar) > MaxLength ->
	error_terminate(400, State);
parse_hd_value(<<>>, S, M, P, Q, F, V, H, N, SoFar) ->
	wait_hd_value(<<>>, S, M, P, Q, F, V, H, N, SoFar).

request(B, State=#state{transport=Transport}, M, P, Q, F, Version, Headers) ->
	case lists:keyfind(<<"host">>, 1, Headers) of
		false when Version =:= {1, 1} ->
			error_terminate(400, State);
		false ->
			request(B, State, M, P, Q, F, Version, Headers,
				<<>>, default_port(Transport:name()));
		{_, RawHost} ->
			case parse_host(RawHost, <<>>) of
				{Host, undefined} ->
					request(B, State, M, P, Q, F, Version, Headers,
						Host, default_port(Transport:name()));
				{Host, Port} ->
					request(B, State, M, P, Q, F, Version, Headers,
						Host, Port)
			end
	end.

-spec default_port(atom()) -> 80 | 443.
default_port(ssl) -> 443;
default_port(_) -> 80.

%% Another hurtful block of code. :)
parse_host(<<>>, Acc) ->
	{Acc, undefined};
parse_host(<< $:, Rest/bits >>, Acc) ->
	{Acc, list_to_integer(binary_to_list(Rest))};
parse_host(<< C, Rest/bits >>, Acc) ->
	case C of
		$A -> parse_host(Rest, << Acc/binary, $a >>);
		$B -> parse_host(Rest, << Acc/binary, $b >>);
		$C -> parse_host(Rest, << Acc/binary, $c >>);
		$D -> parse_host(Rest, << Acc/binary, $d >>);
		$E -> parse_host(Rest, << Acc/binary, $e >>);
		$F -> parse_host(Rest, << Acc/binary, $f >>);
		$G -> parse_host(Rest, << Acc/binary, $g >>);
		$H -> parse_host(Rest, << Acc/binary, $h >>);
		$I -> parse_host(Rest, << Acc/binary, $i >>);
		$J -> parse_host(Rest, << Acc/binary, $j >>);
		$K -> parse_host(Rest, << Acc/binary, $k >>);
		$L -> parse_host(Rest, << Acc/binary, $l >>);
		$M -> parse_host(Rest, << Acc/binary, $m >>);
		$N -> parse_host(Rest, << Acc/binary, $n >>);
		$O -> parse_host(Rest, << Acc/binary, $o >>);
		$P -> parse_host(Rest, << Acc/binary, $p >>);
		$Q -> parse_host(Rest, << Acc/binary, $q >>);
		$R -> parse_host(Rest, << Acc/binary, $r >>);
		$S -> parse_host(Rest, << Acc/binary, $s >>);
		$T -> parse_host(Rest, << Acc/binary, $t >>);
		$U -> parse_host(Rest, << Acc/binary, $u >>);
		$V -> parse_host(Rest, << Acc/binary, $v >>);
		$W -> parse_host(Rest, << Acc/binary, $w >>);
		$X -> parse_host(Rest, << Acc/binary, $x >>);
		$Y -> parse_host(Rest, << Acc/binary, $y >>);
		$Z -> parse_host(Rest, << Acc/binary, $z >>);
		_ -> parse_host(Rest, << Acc/binary, C >>)
	end.

%% End of request parsing.
%%
%% We create the Req object and start handling the request.

request(Buffer, State=#state{socket=Socket, transport=Transport,
		req_keepalive=ReqKeepalive, max_keepalive=MaxKeepalive,
		onresponse=OnResponse},
		Method, Path, Query, Fragment, Version, Headers, Host, Port) ->
	Req = cowboy_req:new(Socket, Transport, Method, Path, Query, Fragment,
		Version, Headers, Host, Port, Buffer, ReqKeepalive < MaxKeepalive,
		OnResponse),
	onrequest(Req, State, Host, Path).

%% Call the global onrequest callback. The callback can send a reply,
%% in which case we consider the request handled and move on to the next
%% one. Note that since we haven't dispatched yet, we don't know the
%% handler, host_info, path_info or bindings yet.
-spec onrequest(cowboy_req:req(), #state{}, binary(), binary()) -> ok.
onrequest(Req, State=#state{onrequest=undefined}, Host, Path) ->
	dispatch(Req, State, Host, Path);
onrequest(Req, State=#state{onrequest=OnRequest}, Host, Path) ->
	Req2 = OnRequest(Req),
	case cowboy_req:get(resp_state, Req2) of
		waiting -> dispatch(Req2, State, Host, Path);
		_ -> next_request(Req2, State, ok)
	end.

-spec dispatch(cowboy_req:req(), #state{}, binary(), binary()) -> ok.
dispatch(Req, State=#state{dispatch=Dispatch}, Host, Path) ->
	case cowboy_dispatcher:match(Dispatch, Host, Path) of
		{ok, Handler, Opts, Bindings, HostInfo, PathInfo} ->
			Req2 = cowboy_req:set_bindings(HostInfo, PathInfo, Bindings, Req),
			handler_init(Req2, State, Handler, Opts);
		{error, notfound, host} ->
			error_terminate(400, State);
		{error, notfound, path} ->
			error_terminate(404, State)
	end.

-spec handler_init(cowboy_req:req(), #state{}, module(), any()) -> ok.
handler_init(Req, State=#state{transport=Transport}, Handler, Opts) ->
	try Handler:init({Transport:name(), http}, Req, Opts) of
		{ok, Req2, HandlerState} ->
			handler_handle(Req2, State, Handler, HandlerState);
		{loop, Req2, HandlerState} ->
			handler_before_loop(Req2, State#state{hibernate=false},
				Handler, HandlerState);
		{loop, Req2, HandlerState, hibernate} ->
			handler_before_loop(Req2, State#state{hibernate=true},
				Handler, HandlerState);
		{loop, Req2, HandlerState, Timeout} ->
			handler_before_loop(Req2, State#state{loop_timeout=Timeout},
				Handler, HandlerState);
		{loop, Req2, HandlerState, Timeout, hibernate} ->
			handler_before_loop(Req2, State#state{
				hibernate=true, loop_timeout=Timeout}, Handler, HandlerState);
		{shutdown, Req2, HandlerState} ->
			handler_terminate(Req2, Handler, HandlerState);
		%% @todo {upgrade, transport, Module}
		{upgrade, protocol, Module} ->
			upgrade_protocol(Req, State, Handler, Opts, Module)
	catch Class:Reason ->
		error_terminate(500, State),
		error_logger:error_msg(
			"** Handler ~p terminating in init/3~n"
			"   for the reason ~p:~p~n"
			"** Options were ~p~n"
			"** Request was ~p~n"
			"** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, Opts,
				cowboy_req:to_list(Req), erlang:get_stacktrace()])
	end.

-spec upgrade_protocol(cowboy_req:req(), #state{}, module(), any(), module())
	-> ok.
upgrade_protocol(Req, State=#state{listener=ListenerPid},
		Handler, Opts, Module) ->
	case Module:upgrade(ListenerPid, Handler, Opts, Req) of
		{UpgradeRes, Req2} -> next_request(Req2, State, UpgradeRes);
		_Any -> terminate(State)
	end.

-spec handler_handle(cowboy_req:req(), #state{}, module(), any()) -> ok.
handler_handle(Req, State, Handler, HandlerState) ->
	try Handler:handle(Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			terminate_request(Req2, State, Handler, HandlerState2)
	catch Class:Reason ->
		error_logger:error_msg(
			"** Handler ~p terminating in handle/2~n"
			"   for the reason ~p:~p~n"
			"** Handler state was ~p~n"
			"** Request was ~p~n"
			"** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, HandlerState,
				cowboy_req:to_list(Req), erlang:get_stacktrace()]),
		handler_terminate(Req, Handler, HandlerState),
		error_terminate(500, State)
	end.

%% We don't listen for Transport closes because that would force us
%% to receive data and buffer it indefinitely.
-spec handler_before_loop(cowboy_req:req(), #state{}, module(), any()) -> ok.
handler_before_loop(Req, State=#state{hibernate=true}, Handler, HandlerState) ->
	State2 = handler_loop_timeout(State),
	catch erlang:hibernate(?MODULE, handler_loop,
		[Req, State2#state{hibernate=false}, Handler, HandlerState]),
	ok;
handler_before_loop(Req, State, Handler, HandlerState) ->
	State2 = handler_loop_timeout(State),
	handler_loop(Req, State2, Handler, HandlerState).

%% Almost the same code can be found in cowboy_websocket.
-spec handler_loop_timeout(#state{}) -> #state{}.
handler_loop_timeout(State=#state{loop_timeout=infinity}) ->
	State#state{loop_timeout_ref=undefined};
handler_loop_timeout(State=#state{loop_timeout=Timeout,
		loop_timeout_ref=PrevRef}) ->
	_ = case PrevRef of undefined -> ignore; PrevRef ->
		erlang:cancel_timer(PrevRef) end,
	TRef = erlang:start_timer(Timeout, self(), ?MODULE),
	State#state{loop_timeout_ref=TRef}.

%% @private
-spec handler_loop(cowboy_req:req(), #state{}, module(), any()) -> ok.
handler_loop(Req, State=#state{loop_timeout_ref=TRef}, Handler, HandlerState) ->
	receive
		{timeout, TRef, ?MODULE} ->
			terminate_request(Req, State, Handler, HandlerState);
		{timeout, OlderTRef, ?MODULE} when is_reference(OlderTRef) ->
			handler_loop(Req, State, Handler, HandlerState);
		Message ->
			handler_call(Req, State, Handler, HandlerState, Message)
	end.

-spec handler_call(cowboy_req:req(), #state{}, module(), any(), any()) -> ok.
handler_call(Req, State, Handler, HandlerState, Message) ->
	try Handler:info(Message, Req, HandlerState) of
		{ok, Req2, HandlerState2} ->
			terminate_request(Req2, State, Handler, HandlerState2);
		{loop, Req2, HandlerState2} ->
			handler_before_loop(Req2, State, Handler, HandlerState2);
		{loop, Req2, HandlerState2, hibernate} ->
			handler_before_loop(Req2, State#state{hibernate=true},
				Handler, HandlerState2)
	catch Class:Reason ->
		error_logger:error_msg(
			"** Handler ~p terminating in info/3~n"
			"   for the reason ~p:~p~n"
			"** Handler state was ~p~n"
			"** Request was ~p~n"
			"** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, HandlerState,
				cowboy_req:to_list(Req), erlang:get_stacktrace()]),
		handler_terminate(Req, Handler, HandlerState),
		error_terminate(500, State)
	end.

-spec handler_terminate(cowboy_req:req(), module(), any()) -> ok.
handler_terminate(Req, Handler, HandlerState) ->
	try
		Handler:terminate(cowboy_req:lock(Req), HandlerState)
	catch Class:Reason ->
		error_logger:error_msg(
			"** Handler ~p terminating in terminate/2~n"
			"   for the reason ~p:~p~n"
			"** Handler state was ~p~n"
			"** Request was ~p~n"
			"** Stacktrace: ~p~n~n",
			[Handler, Class, Reason, HandlerState,
				cowboy_req:to_list(Req), erlang:get_stacktrace()])
	end.

-spec terminate_request(cowboy_req:req(), #state{}, module(), any()) -> ok.
terminate_request(Req, State, Handler, HandlerState) ->
	HandlerRes = handler_terminate(Req, Handler, HandlerState),
	next_request(Req, State, HandlerRes).

-spec next_request(cowboy_req:req(), #state{}, any()) -> ok.
next_request(Req, State=#state{req_keepalive=Keepalive}, HandlerRes) ->
	cowboy_req:ensure_response(Req, 204),
	{BodyRes, [Buffer, Connection]} = case cowboy_req:skip_body(Req) of
		{ok, Req2} -> {ok, cowboy_req:get([buffer, connection], Req2)};
		{error, _} -> {close, [<<>>, close]}
	end,
	%% Flush the resp_sent message before moving on.
	receive {cowboy_req, resp_sent} -> ok after 0 -> ok end,
	case {HandlerRes, BodyRes, Connection} of
		{ok, ok, keepalive} ->
			?MODULE:parse_request(Buffer, State#state{
				req_keepalive=Keepalive + 1}, 0);
		_Closed ->
			terminate(State)
	end.

%% Only send an error reply if there is no resp_sent message.
-spec error_terminate(cowboy_http:status(), #state{}) -> ok.
error_terminate(Code, State=#state{socket=Socket, transport=Transport,
		onresponse=OnResponse}) ->
	receive
		{cowboy_req, resp_sent} -> ok
	after 0 ->
		_ = cowboy_req:reply(Code, cowboy_req:new(Socket, Transport,
			<<"GET">>, <<>>, <<>>, <<>>, {1, 1}, [], <<>>, undefined,
			<<>>, false, OnResponse)),
		ok
	end,
	terminate(State).

-spec terminate(#state{}) -> ok.
terminate(#state{socket=Socket, transport=Transport}) ->
	Transport:close(Socket),
	ok.
