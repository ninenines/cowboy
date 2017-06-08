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

-module(cowboy_http).

-export([init/5]).

-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-type opts() :: #{
	connection_type => worker | supervisor,
	env => cowboy_middleware:env(),
	idle_timeout => timeout(),
	inactivity_timeout => timeout(),
	max_empty_lines => non_neg_integer(),
	max_header_name_length => non_neg_integer(),
	max_header_value_length => non_neg_integer(),
	max_headers => non_neg_integer(),
	max_keepalive => non_neg_integer(),
	max_method_length => non_neg_integer(),
	max_request_line_length => non_neg_integer(),
	middlewares => [module()],
	request_timeout => timeout(),
	shutdown_timeout => timeout(),
	stream_handlers => [module()]
}.
-export_type([opts/0]).

-record(ps_request_line, {
	empty_lines = 0 :: non_neg_integer()
}).

-record(ps_header, {
	method = undefined :: binary(),
	path = undefined :: binary(),
	qs = undefined :: binary(),
	version = undefined :: cowboy:http_version(),
	headers = undefined :: map() | undefined, %% @todo better type than map()
	name = undefined :: binary() | undefined
}).

%% @todo We need a state where we wait for the stream process to ask for the body.
%% OR DO WE

%% In HTTP/2 we start receiving data before the body asks for it, even if optionally
%% (and by default), so we need to be able to do the same for HTTP/1.1 too. This means
%% that when we receive data (up to a certain limit, we read from the socket and decode.
%% When we reach a limit, we stop reading from the socket momentarily until the stream
%% process asks for more or the stream ends.

%% This means that we need to keep a buffer in the stream handler (until the stream
%% process asks for it). And that we need the body state to indicate how much we have
%% left to read (and stop/start reading from the socket depending on value).

-record(ps_body, {
	%% @todo flow
	transfer_decode_fun :: fun(), %% @todo better type
	transfer_decode_state :: any() %% @todo better type
}).

-record(stream, {
	id = undefined :: cowboy_stream:streamid(),
	%% Stream handlers and their state.
	state = undefined :: {module(), any()},
	%% Client HTTP version for this stream.
	version = undefined :: cowboy:http_version(),
	%% Commands queued.
	queue = [] :: cowboy_stream:commands()
}).

-type stream() :: #stream{}.

-record(state, {
	parent :: pid(),
	ref :: ranch:ref(),
	socket :: inet:socket(),
	transport :: module(),
	opts = #{} :: map(),

	%% Remote address and port for the connection.
	peer = undefined :: {inet:ip_address(), inet:port_number()},

	timer = undefined :: undefined | reference(),

	%% Identifier for the stream currently being read (or waiting to be received).
	in_streamid = 1 :: pos_integer(),

	%% Parsing state for the current stream or stream-to-be.
	in_state = #ps_request_line{} :: #ps_request_line{} | #ps_header{} | #ps_body{},

	%% Identifier for the stream currently being written.
	%% Note that out_streamid =< in_streamid.
	out_streamid = 1 :: pos_integer(),

	%% Whether we finished writing data for the current stream.
	out_state = wait :: wait | headers | chunked | done,

	%% The connection will be closed after this stream.
	last_streamid = undefined :: pos_integer(),

	%% Currently active HTTP/1.1 streams.
	streams = [] :: [stream()],

	%% Children processes created by streams.
	children = [] :: [{pid(), cowboy_stream:streamid(), timeout()}]
}).

-include_lib("cowlib/include/cow_inline.hrl").
-include_lib("cowlib/include/cow_parse.hrl").

-spec init(pid(), ranch:ref(), inet:socket(), module(), cowboy:opts()) -> ok.
init(Parent, Ref, Socket, Transport, Opts) ->
	case Transport:peername(Socket) of
		{ok, Peer} ->
			LastStreamID = maps:get(max_keepalive, Opts, 100),
			before_loop(set_timeout(#state{
				parent=Parent, ref=Ref, socket=Socket,
				transport=Transport, opts=Opts,
				peer=Peer, last_streamid=LastStreamID}), <<>>);
		{error, Reason} ->
			%% Couldn't read the peer address; connection is gone.
			terminate(undefined, {socket_error, Reason, 'An error has occurred on the socket.'})
	end.

%% @todo Send a response depending on in_state and whether one was already sent.
%% @todo If we skip the body, skip for a specific duration.

before_loop(State=#state{socket=Socket, transport=Transport}, Buffer) ->
	%% @todo disable this when we get to the body, until the stream asks for it?
	%% Perhaps have a threshold for how much we're willing to read before waiting.
	Transport:setopts(Socket, [{active, once}]),
	loop(State, Buffer).

loop(State=#state{parent=Parent, socket=Socket, transport=Transport, opts=Opts,
		timer=TimerRef, children=Children, streams=Streams}, Buffer) ->
	{OK, Closed, Error} = Transport:messages(),
	InactivityTimeout = maps:get(inactivity_timeout, Opts, 300000),
	receive
		%% Socket messages.
		{OK, Socket, Data} ->
			%% Only reset the timeout if it is idle_timeout (active streams).
			State1 = case Streams of
				[] -> State;
				_ -> set_timeout(State)
			end,
			parse(<< Buffer/binary, Data/binary >>, State1);
		{Closed, Socket} ->
			terminate(State, {socket_error, closed, 'The socket has been closed.'});
		{Error, Socket, Reason} ->
			terminate(State, {socket_error, Reason, 'An error has occurred on the socket.'});
		%% Timeouts.
		{timeout, TimerRef, Reason} ->
			timeout(State, Reason);
		{timeout, _, _} ->
			loop(State, Buffer);
		%% System messages.
		{'EXIT', Parent, Reason} ->
			exit(Reason);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, [], {State, Buffer});
		%% Messages pertaining to a stream.
		{{Pid, StreamID}, Msg} when Pid =:= self() ->
			loop(info(State, StreamID, Msg), Buffer);
		%% Exit signal from children.
		Msg = {'EXIT', Pid, _} ->
			loop(down(State, Pid, Msg), Buffer);
		%% Calls from supervisor module.
		{'$gen_call', {From, Tag}, which_children} ->
			Workers = [{?MODULE, Pid, worker, [?MODULE]} || {Pid, _, _} <- Children],
			From ! {Tag, Workers},
			loop(State, Buffer);
		{'$gen_call', {From, Tag}, count_children} ->
			NbChildren = length(Children),
			Counts = [{specs, 1}, {active, NbChildren},
				{supervisors, 0}, {workers, NbChildren}],
			From ! {Tag, Counts},
			loop(State, Buffer);
		{'$gen_call', {From, Tag}, _} ->
			From ! {Tag, {error, ?MODULE}},
			loop(State, Buffer);
		%% Unknown messages.
		Msg ->
			error_logger:error_msg("Received stray message ~p.~n", [Msg]),
			loop(State, Buffer)
	after InactivityTimeout ->
		terminate(State, {internal_error, timeout, 'No message or data received before timeout.'})
	end.

%% We set request_timeout when there are no active streams,
%% and idle_timeout otherwise.
set_timeout(State0=#state{opts=Opts, streams=Streams}) ->
	State = cancel_timeout(State0),
	{Name, Default} = case Streams of
		[] -> {request_timeout, 5000};
		_ -> {idle_timeout, 60000}
	end,
	Timeout = maps:get(Name, Opts, Default),
	TimerRef = erlang:start_timer(Timeout, self(), Name),
	State#state{timer=TimerRef}.

cancel_timeout(State=#state{timer=TimerRef}) ->
	ok = case TimerRef of
		undefined -> ok;
		_ -> erlang:cancel_timer(TimerRef, [{async, true}, {info, false}])
	end,
	State#state{timer=undefined}.

-spec timeout(_, _) -> no_return().
timeout(State=#state{in_state=#ps_request_line{}}, request_timeout) ->
	terminate(State, {connection_error, timeout,
		'No request-line received before timeout.'});
timeout(State=#state{in_state=#ps_header{}}, request_timeout) ->
	error_terminate(408, State, {connection_error, timeout,
		'Request headers not received before timeout.'});
timeout(State, idle_timeout) ->
	terminate(State, {connection_error, timeout,
		'Connection idle longer than configuration allows.'}).

%% Request-line.
parse(<<>>, State) ->
	before_loop(State, <<>>);
parse(Buffer, State=#state{in_state=#ps_request_line{empty_lines=EmptyLines}}) ->
	after_parse(parse_request(Buffer, State, EmptyLines));
parse(Buffer, State=#state{in_state=PS=#ps_header{headers=Headers, name=undefined}}) ->
	after_parse(parse_header(Buffer,
		State#state{in_state=PS#ps_header{headers=undefined}},
		Headers));
parse(Buffer, State=#state{in_state=PS=#ps_header{headers=Headers, name=Name}}) ->
	after_parse(parse_hd_before_value(Buffer,
		State#state{in_state=PS#ps_header{headers=undefined, name=undefined}},
		Headers, Name));
parse(Buffer, State=#state{in_state=#ps_body{}}) ->
	%% @todo We do not want to get the body automatically if the request doesn't ask for it.
	%% We may want to get bodies that are below a threshold without waiting, and buffer them
	%% until the request asks, though.
	after_parse(parse_body(Buffer, State)).
%% @todo Don't parse if body is finished but request isn't. Let's not parallelize for now.

after_parse({request, Req=#{streamid := StreamID, headers := Headers, version := Version},
		State0=#state{opts=Opts, streams=Streams0}, Buffer}) ->
	try cowboy_stream:init(StreamID, Req, Opts) of
		{Commands, StreamState} ->
			Streams = [#stream{id=StreamID, state=StreamState, version=Version}|Streams0],
			State1 = case maybe_req_close(State0, Headers, Version) of
				close -> State0#state{streams=Streams, last_streamid=StreamID};
				keepalive -> State0#state{streams=Streams}
			end,
			State = set_timeout(State1),
			parse(Buffer, commands(State, StreamID, Commands))
	catch Class:Reason ->
		error_logger:error_msg("Exception occurred in "
			"cowboy_stream:init(~p, ~p, ~p) with reason ~p:~p.",
			[StreamID, Req, Opts, Class, Reason]),
		ok %% @todo send a proper response, etc. note that terminate must NOT be called
		%% @todo Status code.
%		stream_reset(State, StreamID, {internal_error, {Class, Reason},
%			'Exception occurred in StreamHandler:init/10 call.'}) %% @todo Check final arity.
	end;
%% Streams are sequential so the body is always about the last stream created
%% unless that stream has terminated.
after_parse({data, StreamID, IsFin, Data, State=#state{
		streams=Streams0=[Stream=#stream{id=StreamID, state=StreamState0}|_]}, Buffer}) ->
	try cowboy_stream:data(StreamID, IsFin, Data, StreamState0) of
		{Commands, StreamState} ->
			Streams = lists:keyreplace(StreamID, #stream.id, Streams0,
				Stream#stream{state=StreamState}),
			parse(Buffer, commands(State#state{streams=Streams}, StreamID, Commands))
	catch Class:Reason ->
		error_logger:error_msg("Exception occurred in "
			"cowboy_stream:data(~p, ~p, ~p, ~p) with reason ~p:~p.",
			[StreamID, IsFin, Data, StreamState0, Class, Reason]),
		%% @todo Bad value returned here. Crashes.
		ok
		%% @todo
%		stream_reset(State, StreamID, {internal_error, {Class, Reason},
%			'Exception occurred in StreamHandler:data/4 call.'})
	end;
%% No corresponding stream, skip.
after_parse({data, _, _, _, State, Buffer}) ->
	before_loop(State, Buffer);
after_parse({more, State, Buffer}) ->
	before_loop(State, Buffer).

%% Request-line.

-spec parse_request(Buffer, State, non_neg_integer())
	-> {request, cowboy_req:req(), State, Buffer}
	| {data, cowboy_stream:streamid(), cowboy_stream:fin(), binary(), State, Buffer}
	| {more, State, Buffer}
	when Buffer::binary(), State::#state{}.
%% Empty lines must be using \r\n.
parse_request(<< $\n, _/bits >>, State, _) ->
	error_terminate(400, State, {connection_error, protocol_error,
		'Empty lines between requests must use the CRLF line terminator. (RFC7230 3.5)'});
parse_request(<< $\s, _/bits >>, State, _) ->
	error_terminate(400, State, {connection_error, protocol_error,
		'The request-line must not begin with a space. (RFC7230 3.1.1, RFC7230 3.5)'});
%% We limit the length of the Request-line to MaxLength to avoid endlessly
%% reading from the socket and eventually crashing.
parse_request(Buffer, State=#state{opts=Opts, in_streamid=InStreamID}, EmptyLines) ->
	MaxLength = maps:get(max_request_line_length, Opts, 8000),
	MaxEmptyLines = maps:get(max_empty_lines, Opts, 5),
	case match_eol(Buffer, 0) of
		nomatch when byte_size(Buffer) > MaxLength ->
			error_terminate(414, State, {connection_error, limit_reached,
				'The request-line length is larger than configuration allows. (RFC7230 3.1.1)'});
		nomatch ->
			{more, State#state{in_state=#ps_request_line{empty_lines=EmptyLines}}, Buffer};
		1 when EmptyLines =:= MaxEmptyLines ->
			error_terminate(400, State, {connection_error, limit_reached,
				'More empty lines were received than configuration allows. (RFC7230 3.5)'});
		1 ->
			<< _:16, Rest/bits >> = Buffer,
			parse_request(Rest, State, EmptyLines + 1);
		_ ->
			case Buffer of
				%% @todo * is only for server-wide OPTIONS request (RFC7230 5.3.4); tests
				<< "OPTIONS * ", Rest/bits >> ->
					parse_version(Rest, State, <<"OPTIONS">>, <<"*">>, <<>>);
%				<< "CONNECT ", Rest/bits >> ->
%					parse_authority( %% @todo
				%% Accept direct HTTP/2 only at the beginning of the connection.
				<< "PRI * HTTP/2.0\r\n", _/bits >> when InStreamID =:= 1 ->
					%% @todo Might be worth throwing to get a clean stacktrace.
					http2_upgrade(State, Buffer);
				_ ->
					parse_method(Buffer, State, <<>>,
						maps:get(max_method_length, Opts, 32))
			end
	end.

match_eol(<< $\n, _/bits >>, N) ->
	N;
match_eol(<< _, Rest/bits >>, N) ->
	match_eol(Rest, N + 1);
match_eol(_, _) ->
	nomatch.

parse_method(_, State, _, 0) ->
	error_terminate(501, State, {connection_error, limit_reached,
		'The method name is longer than configuration allows. (RFC7230 3.1.1)'});
parse_method(<< C, Rest/bits >>, State, SoFar, Remaining) ->
	case C of
		$\r -> error_terminate(400, State, {connection_error, protocol_error,
			'The method name must not be followed with a line break. (RFC7230 3.1.1)'});
		$\s -> parse_uri(Rest, State, SoFar);
		_ when ?IS_TOKEN(C) -> parse_method(Rest, State, << SoFar/binary, C >>, Remaining - 1);
		_ -> error_terminate(400, State, {connection_error, protocol_error,
			'The method name must contain only valid token characters. (RFC7230 3.1.1)'})
	end.

parse_uri(<< H, T, T, P, "://", Rest/bits >>, State, Method)
		when H =:= $h orelse H =:= $H, T =:= $t orelse T =:= $T;
			P =:= $p orelse P =:= $P ->
	parse_uri_skip_host(Rest, State, Method);
parse_uri(<< H, T, T, P, S, "://", Rest/bits >>, State, Method)
		when H =:= $h orelse H =:= $H, T =:= $t orelse T =:= $T;
			P =:= $p orelse P =:= $P; S =:= $s orelse S =:= $S ->
	parse_uri_skip_host(Rest, State, Method);
parse_uri(<< $/, Rest/bits >>, State, Method) ->
	parse_uri_path(Rest, State, Method, << $/ >>);
parse_uri(_, State, _) ->
	error_terminate(400, State, {connection_error, protocol_error,
		'Invalid request-line or request-target. (RFC7230 3.1.1, RFC7230 5.3)'}).

parse_uri_skip_host(<< C, Rest/bits >>, State, Method) ->
	case C of
		$\r -> error_terminate(400, State, {connection_error, protocol_error,
			'The request-target must not be followed by a line break. (RFC7230 3.1.1)'});
		$/ -> parse_uri_path(Rest, State, Method, <<"/">>);
		$\s -> parse_version(Rest, State, Method, <<"/">>, <<>>);
		$? -> parse_uri_query(Rest, State, Method, <<"/">>, <<>>);
		$# -> skip_uri_fragment(Rest, State, Method, <<"/">>, <<>>);
		_ -> parse_uri_skip_host(Rest, State, Method)
	end.

parse_uri_path(<< C, Rest/bits >>, State, Method, SoFar) ->
	case C of
		$\r -> error_terminate(400, State, {connection_error, protocol_error,
			'The request-target must not be followed by a line break. (RFC7230 3.1.1)'});
		$\s -> parse_version(Rest, State, Method, SoFar, <<>>);
		$? -> parse_uri_query(Rest, State, Method, SoFar, <<>>);
		$# -> skip_uri_fragment(Rest, State, Method, SoFar, <<>>);
		_ -> parse_uri_path(Rest, State, Method, << SoFar/binary, C >>)
	end.

parse_uri_query(<< C, Rest/bits >>, State, M, P, SoFar) ->
	case C of
		$\r -> error_terminate(400, State, {connection_error, protocol_error,
			'The request-target must not be followed by a line break. (RFC7230 3.1.1)'});
		$\s -> parse_version(Rest, State, M, P, SoFar);
		$# -> skip_uri_fragment(Rest, State, M, P, SoFar);
		_ -> parse_uri_query(Rest, State, M, P, << SoFar/binary, C >>)
	end.

skip_uri_fragment(<< C, Rest/bits >>, State, M, P, Q) ->
	case C of
		$\r -> error_terminate(400, State, {connection_error, protocol_error,
			'The request-target must not be followed by a line break. (RFC7230 3.1.1)'});
		$\s -> parse_version(Rest, State, M, P, Q);
		_ -> skip_uri_fragment(Rest, State, M, P, Q)
	end.

parse_version(<< "HTTP/1.1\r\n", Rest/bits >>, State, M, P, Q) ->
	parse_headers(Rest, State, M, P, Q, 'HTTP/1.1');
parse_version(<< "HTTP/1.0\r\n", Rest/bits >>, State, M, P, Q) ->
	parse_headers(Rest, State, M, P, Q, 'HTTP/1.0');
parse_version(<< "HTTP/1.", _, C, _/bits >>, State, _, _, _) when C =:= $\s; C =:= $\t ->
	error_terminate(400, State, {connection_error, protocol_error,
		'Whitespace is not allowed after the HTTP version. (RFC7230 3.1.1)'});
parse_version(<< C, _/bits >>, State, _, _, _) when C =:= $\s; C =:= $\t ->
	error_terminate(400, State, {connection_error, protocol_error,
		'The separator between request target and version must be a single SP. (RFC7230 3.1.1)'});
parse_version(_, State, _, _, _) ->
	error_terminate(505, State, {connection_error, protocol_error,
		'Unsupported HTTP version. (RFC7230 2.6)'}).

parse_headers(Rest, State, M, P, Q, V) ->
	parse_header(Rest, State#state{in_state=#ps_header{
		method=M, path=P, qs=Q, version=V}}, #{}).

%% Headers.

%% We need two or more bytes in the buffer to continue.
parse_header(Rest, State=#state{in_state=PS}, Headers) when byte_size(Rest) < 2 ->
	{more, State#state{in_state=PS#ps_header{headers=Headers}}, Rest};
parse_header(<< $\r, $\n, Rest/bits >>, S, Headers) ->
	request(Rest, S, Headers);
parse_header(Buffer, State=#state{opts=Opts, in_state=PS}, Headers) ->
	MaxLength = maps:get(max_header_name_length, Opts, 64),
	MaxHeaders = maps:get(max_headers, Opts, 100),
	NumHeaders = maps:size(Headers),
	case match_colon(Buffer, 0) of
		nomatch when byte_size(Buffer) > MaxLength ->
			error_terminate(431, State#state{in_state=PS#ps_header{headers=Headers}},
				{connection_error, limit_reached,
					'A header name is larger than configuration allows. (RFC7230 3.2.5, RFC6585 5)'});
		nomatch when NumHeaders >= MaxHeaders ->
			error_terminate(431, State#state{in_state=PS#ps_header{headers=Headers}},
				{connection_error, limit_reached,
					'The number of headers is larger than configuration allows. (RFC7230 3.2.5, RFC6585 5)'});
		nomatch ->
			{more, State#state{in_state=PS#ps_header{headers=Headers}}, Buffer};
		_ ->
			parse_hd_name(Buffer, State, Headers, <<>>)
	end.

match_colon(<< $:, _/bits >>, N) ->
	N;
match_colon(<< _, Rest/bits >>, N) ->
	match_colon(Rest, N + 1);
match_colon(_, _) ->
	nomatch.

parse_hd_name(<< $:, Rest/bits >>, State, H, SoFar) ->
	parse_hd_before_value(Rest, State, H, SoFar);
parse_hd_name(<< C, _/bits >>, State=#state{in_state=PS}, H, <<>>) when ?IS_WS(C) ->
	error_terminate(400, State#state{in_state=PS#ps_header{headers=H}},
		{connection_error, protocol_error,
			'Whitespace is not allowed between the header name and the colon. (RFC7230 3.2)'});
parse_hd_name(<< C, Rest/bits >>, State, H, SoFar) when ?IS_WS(C) ->
	parse_hd_name_ws(Rest, State, H, SoFar);
parse_hd_name(<< C, Rest/bits >>, State, H, SoFar) ->
	?LOWER(parse_hd_name, Rest, State, H, SoFar).

parse_hd_name_ws(<< C, Rest/bits >>, S, H, Name) ->
	case C of
		$\s -> parse_hd_name_ws(Rest, S, H, Name);
		$\t -> parse_hd_name_ws(Rest, S, H, Name);
		$: -> parse_hd_before_value(Rest, S, H, Name)
	end.

parse_hd_before_value(<< $\s, Rest/bits >>, S, H, N) ->
	parse_hd_before_value(Rest, S, H, N);
parse_hd_before_value(<< $\t, Rest/bits >>, S, H, N) ->
	parse_hd_before_value(Rest, S, H, N);
parse_hd_before_value(Buffer, State=#state{opts=Opts, in_state=PS}, H, N) ->
	MaxLength = maps:get(max_header_value_length, Opts, 4096),
	case match_eol(Buffer, 0) of
		nomatch when byte_size(Buffer) > MaxLength ->
			error_terminate(431, State#state{in_state=PS#ps_header{headers=H}},
				{connection_error, limit_reached,
					'A header value is larger than configuration allows. (RFC7230 3.2.5, RFC6585 5)'});
		nomatch ->
			{more, State#state{in_state=PS#ps_header{headers=H, name=N}}, Buffer};
		_ ->
			parse_hd_value(Buffer, State, H, N, <<>>)
	end.

parse_hd_value(<< $\r, $\n, Rest/bits >>, S, Headers0, Name, SoFar) ->
	Value = clean_value_ws_end(SoFar, byte_size(SoFar) - 1),
	Headers = case maps:get(Name, Headers0, undefined) of
		undefined -> Headers0#{Name => Value};
		%% The cookie header does not use proper HTTP header lists.
		Value0 when Name =:= <<"cookie">> -> Headers0#{Name => << Value0/binary, "; ", Value/binary >>};
		Value0 -> Headers0#{Name => << Value0/binary, ", ", Value/binary >>}
	end,
	parse_header(Rest, S, Headers);
parse_hd_value(<< C, Rest/bits >>, S, H, N, SoFar) ->
	parse_hd_value(Rest, S, H, N, << SoFar/binary, C >>).

clean_value_ws_end(_, -1) ->
	<<>>;
clean_value_ws_end(Value, N) ->
	case binary:at(Value, N) of
		$\s -> clean_value_ws_end(Value, N - 1);
		$\t -> clean_value_ws_end(Value, N - 1);
		_ ->
			S = N + 1,
			<< Value2:S/binary, _/bits >> = Value,
			Value2
	end.

-ifdef(TEST).
clean_value_ws_end_test_() ->
	Tests = [
		{<<>>, <<>>},
		{<<"     ">>, <<>>},
		{<<"text/*;q=0.3, text/html;q=0.7, text/html;level=1, "
			"text/html;level=2;q=0.4, */*;q=0.5   \t   \t    ">>,
			<<"text/*;q=0.3, text/html;q=0.7, text/html;level=1, "
				"text/html;level=2;q=0.4, */*;q=0.5">>}
	],
	[{V, fun() -> R = clean_value_ws_end(V, byte_size(V) - 1) end} || {V, R} <- Tests].

horse_clean_value_ws_end() ->
	horse:repeat(200000,
		clean_value_ws_end(
			<<"text/*;q=0.3, text/html;q=0.7, text/html;level=1, "
				"text/html;level=2;q=0.4, */*;q=0.5          ">>,
			byte_size(<<"text/*;q=0.3, text/html;q=0.7, text/html;level=1, "
				"text/html;level=2;q=0.4, */*;q=0.5          ">>) - 1)
	).
-endif.

request(Buffer, State=#state{transport=Transport, in_streamid=StreamID,
		in_state=PS=#ps_header{version=Version}}, Headers) ->
	case maps:get(<<"host">>, Headers, undefined) of
		undefined when Version =:= 'HTTP/1.1' ->
			%% @todo Might want to not close the connection on this and next one.
			error_terminate(400, State#state{in_state=PS#ps_header{headers=Headers}},
				{stream_error, StreamID, protocol_error,
					'HTTP/1.1 requests must include a host header. (RFC7230 5.4)'});
		undefined ->
			request(Buffer, State, Headers, <<>>, default_port(Transport:secure()));
		RawHost ->
			try cow_http_hd:parse_host(RawHost) of
				{Host, undefined} ->
					request(Buffer, State, Headers, Host, default_port(Transport:secure()));
				{Host, Port} ->
					request(Buffer, State, Headers, Host, Port)
			catch _:_ ->
				error_terminate(400, State#state{in_state=PS#ps_header{headers=Headers}},
					{stream_error, StreamID, protocol_error,
						'The host header is invalid. (RFC7230 5.4)'})
			end
	end.

-spec default_port(boolean()) -> 80 | 443.
default_port(true) -> 443;
default_port(_) -> 80.

%% End of request parsing.

request(Buffer, State0=#state{ref=Ref, transport=Transport, peer=Peer, in_streamid=StreamID,
		in_state=PS=#ps_header{method=Method, path=Path, qs=Qs, version=Version}},
		Headers, Host, Port) ->
	Scheme = case Transport:secure() of
		true -> <<"https">>;
		false -> <<"http">>
	end,
	{HasBody, BodyLength, TDecodeFun, TDecodeState} = case Headers of
		#{<<"content-length">> := <<"0">>} ->
			{false, 0, undefined, undefined};
		#{<<"content-length">> := BinLength} ->
			Length = try
				cow_http_hd:parse_content_length(BinLength)
			catch _:_ ->
				error_terminate(400, State0#state{in_state=PS#ps_header{headers=Headers}},
					{stream_error, StreamID, protocol_error,
						'The content-length header is invalid. (RFC7230 3.3.2)'})
			end,
			{true, Length, fun cow_http_te:stream_identity/2, {0, Length}};
		%% @todo Better handling of transfer decoding.
		#{<<"transfer-encoding">> := <<"chunked">>} ->
			{true, undefined, fun cow_http_te:stream_chunked/2, {0, 0}};
		_ ->
			{false, 0, undefined, undefined}
	end,
	Req = #{
		ref => Ref,
		pid => self(),
		streamid => StreamID,
		peer => Peer,
		method => Method,
		scheme => Scheme,
		host => Host,
		port => Port,
		%% @todo The path component needs to be normalized.
		path => Path,
		qs => Qs,
		version => Version,
		%% We are transparently taking care of transfer-encodings so
		%% the user code has no need to know about it.
		headers => maps:remove(<<"transfer-encoding">>, Headers),
		has_body => HasBody,
		body_length => BodyLength
	},
	case is_http2_upgrade(Headers, Version) of
		false ->
			State = case HasBody of
				true ->
					State0#state{in_state=#ps_body{
						%% @todo Don't need length anymore?
						transfer_decode_fun = TDecodeFun,
						transfer_decode_state = TDecodeState
					}};
				false ->
					State0#state{in_streamid=StreamID + 1, in_state=#ps_request_line{}}
			end,
			{request, Req, State, Buffer};
		{true, HTTP2Settings} ->
			%% We save the headers in case the upgrade will fail
			%% and we need to pass them to cowboy_stream:early_error.
			http2_upgrade(State0#state{in_state=PS#ps_header{headers=Headers}},
				Buffer, HTTP2Settings, Req)
	end.

%% HTTP/2 upgrade.

%% @todo We must not upgrade to h2c over a TLS connection.
is_http2_upgrade(#{<<"connection">> := Conn, <<"upgrade">> := Upgrade,
		<<"http2-settings">> := HTTP2Settings}, 'HTTP/1.1') ->
	Conns = cow_http_hd:parse_connection(Conn),
	case {lists:member(<<"upgrade">>, Conns), lists:member(<<"http2-settings">>, Conns)} of
		{true, true} ->
			Protocols = cow_http_hd:parse_upgrade(Upgrade),
			case lists:member(<<"h2c">>, Protocols) of
				true ->
					{true, HTTP2Settings};
				false ->
					false
			end;
		_ ->
			false
	end;
is_http2_upgrade(_, _) ->
	false.

%% Upgrade through an HTTP/1.1 request.

%% Prior knowledge upgrade, without an HTTP/1.1 request.
http2_upgrade(State=#state{parent=Parent, ref=Ref, socket=Socket, transport=Transport,
		opts=Opts, peer=Peer}, Buffer) ->
	case Transport:secure() of
		false ->
			_ = cancel_timeout(State),
			cowboy_http2:init(Parent, Ref, Socket, Transport, Opts, Peer, Buffer);
		true ->
			error_terminate(400, State, {connection_error, protocol_error,
				'Clients that support HTTP/2 over TLS MUST use ALPN. (RFC7540 3.4)'})
	end.

http2_upgrade(State=#state{parent=Parent, ref=Ref, socket=Socket, transport=Transport,
		opts=Opts, peer=Peer}, Buffer, HTTP2Settings, Req) ->
	%% @todo
	%% However if the client sent a body, we need to read the body in full
	%% and if we can't do that, return a 413 response. Some options are in order.
	%% Always half-closed stream coming from this side.
	try cow_http_hd:parse_http2_settings(HTTP2Settings) of
		Settings ->
			_ = cancel_timeout(State),
			cowboy_http2:init(Parent, Ref, Socket, Transport, Opts, Peer, Buffer, Settings, Req)
	catch _:_ ->
		error_terminate(400, State, {connection_error, protocol_error,
			'The HTTP2-Settings header must contain a base64 SETTINGS payload. (RFC7540 3.2, RFC7540 3.2.1)'})
	end.

%% Request body parsing.

parse_body(Buffer, State=#state{in_streamid=StreamID, in_state=
		PS=#ps_body{transfer_decode_fun=TDecode, transfer_decode_state=TState0}}) ->
	%% @todo Proper trailers.
	case TDecode(Buffer, TState0) of
		more ->
			%% @todo Asks for 0 or more bytes.
			{more, State, Buffer};
		{more, Data, TState} ->
			%% @todo Asks for 0 or more bytes.
			{data, StreamID, nofin, Data, State#state{in_state=
				PS#ps_body{transfer_decode_state=TState}}, <<>>};
		{more, Data, _Length, TState} when is_integer(_Length) ->
			%% @todo Asks for Length more bytes.
			{data, StreamID, nofin, Data, State#state{in_state=
				PS#ps_body{transfer_decode_state=TState}}, <<>>};
		{more, Data, Rest, TState} ->
			%% @todo Asks for 0 or more bytes.
			{data, StreamID, nofin, Data, State#state{in_state=
				PS#ps_body{transfer_decode_state=TState}}, Rest};
		{done, TotalLength, Rest} ->
			{data, StreamID, {fin, TotalLength}, <<>>, set_timeout(
				State#state{in_streamid=StreamID + 1, in_state=#ps_request_line{}}), Rest};
		{done, Data, TotalLength, Rest} ->
			{data, StreamID, {fin, TotalLength}, Data, set_timeout(
				State#state{in_streamid=StreamID + 1, in_state=#ps_request_line{}}), Rest}
	end.

%% Message handling.

%% @todo There is a difference in behavior between HTTP/1.1 and HTTP/2
%% when an error or crash occurs after sending a 500 response. In HTTP/2
%% the error will be printed, in HTTP/1.1 the error will be ignored.
%% This is due to HTTP/1.1 disabling streams differently after both
%% requests and responses have been sent.
down(State=#state{children=Children0}, Pid, Msg) ->
	case lists:keytake(Pid, 1, Children0) of
		{value, {_, undefined, _}, Children} ->
			State#state{children=Children};
		{value, {_, StreamID, _}, Children} ->
			info(State#state{children=Children}, StreamID, Msg);
		false ->
			error_logger:error_msg("Received EXIT signal ~p for unknown process ~p.~n", [Msg, Pid]),
			State
	end.

info(State=#state{streams=Streams0}, StreamID, Msg) ->
	case lists:keyfind(StreamID, #stream.id, Streams0) of
		Stream = #stream{state=StreamState0} ->
			try cowboy_stream:info(StreamID, Msg, StreamState0) of
				{Commands, StreamState} ->
					Streams = lists:keyreplace(StreamID, #stream.id, Streams0,
						Stream#stream{state=StreamState}),
					commands(State#state{streams=Streams}, StreamID, Commands)
			catch Class:Reason ->
				error_logger:error_msg("Exception occurred in "
					"cowboy_stream:info(~p, ~p, ~p) with reason ~p:~p.",
					[StreamID, Msg, StreamState0, Class, Reason]),
				ok
%% @todo
%				stream_reset(State, StreamID, {internal_error, {Class, Reason},
%					'Exception occurred in StreamHandler:info/3 call.'})
			end;
		false ->
			error_logger:error_msg("Received message ~p for unknown stream ~p.~n", [Msg, StreamID]),
			State
	end.

%% Commands.

commands(State, _, []) ->
	State;
%% Supervise a child process.
commands(State=#state{children=Children}, StreamID, [{spawn, Pid, Shutdown}|Tail]) ->
	commands(State#state{children=[{Pid, StreamID, Shutdown}|Children]}, StreamID, Tail);
%% Error handling.
commands(State, StreamID, [Error = {internal_error, _, _}|Tail]) ->
	commands(stream_reset(State, StreamID, Error), StreamID, Tail);
%% Commands for a stream currently inactive.
commands(State=#state{out_streamid=Current, streams=Streams0}, StreamID, Commands)
		when Current =/= StreamID ->

	%% @todo We still want to handle some commands...

	Stream = #stream{queue=Queue} = lists:keyfind(StreamID, #stream.id, Streams0),
	Streams = lists:keyreplace(StreamID, #stream.id, Streams0,
		Stream#stream{queue=Queue ++ Commands}),
	State#state{streams=Streams};
%% Read the request body.
commands(State, StreamID, [{flow, _Length}|Tail]) ->
	%% @todo We only read from socket if buffer is empty, otherwise
	%% we decode the buffer.

	%% @todo Set the body reading length to min(Length, BodyLength)

	commands(State, StreamID, Tail);
%% Error responses are sent only if a response wasn't sent already.
commands(State=#state{out_state=wait}, StreamID, [{error_response, StatusCode, Headers, Body}|Tail]) ->
	commands(State, StreamID, [{response, StatusCode, Headers, Body}|Tail]);
commands(State, StreamID, [{error_response, _, _, _}|Tail]) ->
	commands(State, StreamID, Tail);
%% Send an informational response.
commands(State=#state{socket=Socket, transport=Transport, out_state=wait, streams=Streams},
		StreamID, [{inform, StatusCode, Headers}|Tail]) ->
	%% @todo I'm pretty sure the last stream in the list is the one we want
	%% considering all others are queued.
	#stream{version=Version} = lists:keyfind(StreamID, #stream.id, Streams),
	_ = case Version of
		'HTTP/1.1' ->
			Transport:send(Socket, cow_http:response(StatusCode, 'HTTP/1.1',
				headers_to_list(Headers)));
		%% Do not send informational responses to HTTP/1.0 clients. (RFC7231 6.2)
		'HTTP/1.0' ->
			ok
	end,
	commands(State, StreamID, Tail);
%% Send a full response.
%%
%% @todo Kill the stream if it sent a response when one has already been sent.
%% @todo Keep IsFin in the state.
%% @todo Same two things above apply to DATA, possibly promise too.
commands(State0=#state{socket=Socket, transport=Transport, out_state=wait, streams=Streams}, StreamID,
		[{response, StatusCode, Headers0, Body}|Tail]) ->
	%% @todo I'm pretty sure the last stream in the list is the one we want
	%% considering all others are queued.
	#stream{version=Version} = lists:keyfind(StreamID, #stream.id, Streams),
	{State, Headers} = connection(State0, Headers0, StreamID, Version),
	%% @todo Ensure content-length is set.
	Response = cow_http:response(StatusCode, 'HTTP/1.1', headers_to_list(Headers)),
	case Body of
		{sendfile, O, B, P} ->
			Transport:send(Socket, Response),
			commands(State#state{out_state=done}, StreamID, [{sendfile, fin, O, B, P}|Tail]);
		_ ->
			Transport:send(Socket, [Response, Body]),
			%% @todo If max number of requests, close connection.
			%% @todo If IsFin, maybe skip body of current request.
			maybe_terminate(State#state{out_state=done}, StreamID, Tail, fin)
	end;
%% Send response headers and initiate chunked encoding.
commands(State0=#state{socket=Socket, transport=Transport, streams=Streams}, StreamID,
		[{headers, StatusCode, Headers0}|Tail]) ->
	%% @todo Same as above.
	#stream{version=Version} = lists:keyfind(StreamID, #stream.id, Streams),
	{State1, Headers1} = case Version of
		'HTTP/1.1' ->
			{State0, Headers0#{<<"transfer-encoding">> => <<"chunked">>}};
		%% Close the connection after streaming the data to HTTP/1.0 client.
		%% @todo I'm guessing we need to differentiate responses with a content-length and others.
		'HTTP/1.0' ->
			{State0#state{last_streamid=StreamID}, Headers0}
	end,
	{State, Headers} = connection(State1, Headers1, StreamID, Version),
	Transport:send(Socket, cow_http:response(StatusCode, 'HTTP/1.1', headers_to_list(Headers))),
	commands(State#state{out_state=chunked}, StreamID, Tail);
%% Send a response body chunk.
%%
%% @todo WINDOW_UPDATE stuff require us to buffer some data.
%% @todo We probably want to allow Data to be the {sendfile, ...} tuple also.
commands(State=#state{socket=Socket, transport=Transport, streams=Streams}, StreamID,
		[{data, IsFin, Data}|Tail]) ->
	%% Do not send anything when the user asks to send an empty
	%% data frame, as that would break the protocol.
	Size = iolist_size(Data),
	case Size of
		0 -> ok;
		_ ->
			%% @todo We need to kill the stream if it tries to send data before headers.
			%% @todo Same as above.
			case lists:keyfind(StreamID, #stream.id, Streams) of
				#stream{version='HTTP/1.1'} ->
					Transport:send(Socket, [integer_to_binary(Size, 16), <<"\r\n">>, Data, <<"\r\n">>]);
				#stream{version='HTTP/1.0'} ->
					Transport:send(Socket, Data)
			end
	end,
	maybe_terminate(State, StreamID, Tail, IsFin);
%% Send a file.
commands(State=#state{socket=Socket, transport=Transport}, StreamID,
		[{sendfile, IsFin, Offset, Bytes, Path}|Tail]) ->
	Transport:sendfile(Socket, Path, Offset, Bytes),
	maybe_terminate(State, StreamID, Tail, IsFin);
%% Protocol takeover.
commands(State0=#state{ref=Ref, parent=Parent, socket=Socket, transport=Transport,
		opts=Opts, children=Children}, StreamID,
		[{switch_protocol, Headers, Protocol, InitialState}|_Tail]) ->
	%% @todo This should be the last stream running otherwise we need to wait before switching.
	%% @todo If there's streams opened after this one, fail instead of 101.
	State = cancel_timeout(State0),
	%% @todo When we actually do the upgrade, we only have the one stream left, plus
	%% possibly some processes terminating. We need a smart strategy for handling the
	%% children shutdown. We can start with brutal_kill and discarding the EXIT messages
	%% received before switching to Websocket. Something better would be to let the
	%% stream processes finish but that implies the Websocket module to know about
	%% them and filter the messages. For now, kill them all and discard all messages
	%% in the mailbox.
	_ = [exit(Pid, kill) || {Pid, _, _} <- Children],
	flush(),
	%% Everything good, upgrade!
	_ = commands(State, StreamID, [{inform, 101, Headers}]),
	%% @todo This is no good because commands return a state normally and here it doesn't
	%% we need to let this module go entirely. Perhaps it should be handled directly in
	%% cowboy_clear/cowboy_tls? Perhaps not. We do want that Buffer.
	Protocol:takeover(Parent, Ref, Socket, Transport, Opts, <<>>, InitialState);
%% Stream shutdown.
commands(State, StreamID, [stop|Tail]) ->
	%% @todo Do we want to run the commands after a stop?
%	commands(stream_terminate(State, StreamID, stop), StreamID, Tail).

	%% @todo I think that's where we need to terminate streams.

	maybe_terminate(State, StreamID, Tail, fin);
%% HTTP/1.1 does not support push; ignore.
commands(State, StreamID, [{push, _, _, _, _, _, _, _}|Tail]) ->
	commands(State, StreamID, Tail).

%% The set-cookie header is special; we can only send one cookie per header.
headers_to_list(Headers0=#{<<"set-cookie">> := SetCookies}) ->
	Headers1 = maps:to_list(maps:remove(<<"set-cookie">>, Headers0)),
	Headers1 ++ [{<<"set-cookie">>, Value} || Value <- SetCookies];
headers_to_list(Headers) ->
	maps:to_list(Headers).

flush() ->
	receive _ -> flush() after 0 -> ok end.

maybe_terminate(State, StreamID, Tail, nofin) ->
	commands(State, StreamID, Tail);
%% @todo In these cases I'm not sure if we should continue processing commands.
maybe_terminate(State=#state{last_streamid=StreamID}, StreamID, _Tail, fin) ->
	terminate(stream_terminate(State, StreamID, normal), normal); %% @todo Reason ok?
maybe_terminate(State, StreamID, _Tail, fin) ->
	stream_terminate(State, StreamID, normal).

stream_reset(State, StreamID, StreamError={internal_error, _, _}) ->
	%% @todo headers
	%% @todo Don't send this if there are no streams left.
%	Transport:send(Socket, cow_http:response(500, 'HTTP/1.1', [
%		{<<"content-length">>, <<"0">>}
%	])),
	%% @todo update IsFin local
%	stream_terminate(State#state{out_state=done}, StreamID, StreamError).
	stream_terminate(State, StreamID, StreamError).

stream_terminate(State0=#state{socket=Socket, transport=Transport,
		out_streamid=OutStreamID, out_state=OutState,
		streams=Streams0, children=Children0}, StreamID, Reason) ->
	{value, #stream{state=StreamState, version=Version}, Streams}
		= lists:keytake(StreamID, #stream.id, Streams0),
	State1 = case OutState of
		wait ->
			info(State0, StreamID, {response, 204, #{}, <<>>});
		chunked when Version =:= 'HTTP/1.1' ->
			_ = Transport:send(Socket, <<"0\r\n\r\n">>),
			State0;
		_ -> %% done or Version =:= 'HTTP/1.0'
			State0
	end,
	%% We reset the timeout if there are no active streams anymore.
	State = case Streams of
		[] -> set_timeout(State1);
		_ -> State1
	end,

	stream_call_terminate(StreamID, Reason, StreamState),
%% @todo initiate children shutdown
%			Children = stream_terminate_children(Children0, StreamID, []),
	Children = [case C of
		{Pid, StreamID, Shutdown} -> {Pid, undefined, Shutdown};
		_ -> C
	end || C <- Children0],

	%% @todo Skip the body, if any, or drop the connection if too large.

	%% @todo Only do this if Current =:= StreamID.
	NextOutStreamID = OutStreamID + 1,
	case lists:keyfind(NextOutStreamID, #stream.id, Streams) of
		false ->
			%% @todo This is clearly wrong, if the stream is gone we need to check if
			%% there used to be such a stream, and if there was to send an error.
			State#state{out_streamid=NextOutStreamID, out_state=wait, streams=Streams, children=Children};
		#stream{queue=Commands} ->
			%% @todo Remove queue from the stream.
			commands(State#state{out_streamid=NextOutStreamID, out_state=wait,
				streams=Streams, children=Children}, NextOutStreamID, Commands)
	end.

%% @todo Taken directly from _http2
stream_call_terminate(StreamID, Reason, StreamState) ->
	try
		cowboy_stream:terminate(StreamID, Reason, StreamState)
	catch Class:Reason ->
		error_logger:error_msg("Exception occurred in "
			"cowboy_stream:terminate(~p, ~p, ~p) with reason ~p:~p.",
			[StreamID, Reason, StreamState, Class, Reason])
	end.

%stream_terminate_children([], _, Acc) ->
%	Acc;
%stream_terminate_children([{Pid, StreamID}|Tail], StreamID, Acc) ->
%	exit(Pid, kill),
%	stream_terminate_children(Tail, StreamID, Acc);
%stream_terminate_children([Child|Tail], StreamID, Acc) ->
%	stream_terminate_children(Tail, StreamID, [Child|Acc]).


%% @todo max_reqs also
maybe_req_close(_, #{<<"connection">> := Conn}, 'HTTP/1.0') ->
	Conns = cow_http_hd:parse_connection(Conn),
	case lists:member(<<"keep-alive">>, Conns) of
		true -> keepalive;
		false -> close
	end;
maybe_req_close(_, _, 'HTTP/1.0') ->
	close;
maybe_req_close(_, #{<<"connection">> := Conn}, 'HTTP/1.1') ->
	case connection_hd_is_close(Conn) of
		true -> close;
		false -> keepalive
	end;
maybe_req_close(_State, _, _) ->
	keepalive.

connection(State=#state{last_streamid=StreamID}, Headers=#{<<"connection">> := Conn}, StreamID, _) ->
	case connection_hd_is_close(Conn) of
		true -> {State, Headers};
		%% @todo Here we need to remove keep-alive and add close, not just add close.
		false -> {State, Headers#{<<"connection">> => [<<"close, ">>, Conn]}}
	end;
connection(State=#state{last_streamid=StreamID}, Headers, StreamID, _) ->
	{State, Headers#{<<"connection">> => <<"close">>}};
connection(State, Headers=#{<<"connection">> := Conn}, StreamID, _) ->
	case connection_hd_is_close(Conn) of
		true -> {State#state{last_streamid=StreamID}, Headers};
		%% @todo Here we need to set keep-alive only if it wasn't set before.
		false -> {State, Headers}
	end;
connection(State, Headers, _, 'HTTP/1.0') ->
	{State, Headers#{<<"connection">> => <<"keep-alive">>}};
connection(State, Headers, _, _) ->
	{State, Headers}.

connection_hd_is_close(Conn) ->
	Conns = cow_http_hd:parse_connection(iolist_to_binary(Conn)),
	lists:member(<<"close">>, Conns).

%% This function is only called when an error occurs on a new stream.
-spec error_terminate(cowboy:http_status(), #state{}, _) -> no_return().
error_terminate(StatusCode0, State=#state{ref=Ref, socket=Socket, transport=Transport,
		opts=Opts, peer=Peer, in_streamid=StreamID, in_state=StreamState}, Reason) ->
	PartialReq = case StreamState of
		#ps_request_line{} ->
			#{};
		#ps_header{method=Method, path=Path, qs=Qs,
				version=Version, headers=ReqHeaders} -> #{
			ref => Ref,
			peer => Peer,
			method => Method,
			path => Path,
			qs => Qs,
			version => Version,
			headers => case ReqHeaders of
				undefined -> #{};
				_ -> ReqHeaders
			end
		}
	end,
	{response, StatusCode, RespHeaders, RespBody}
		= cowboy_stream:early_error(StreamID, Reason, PartialReq,
			{response, StatusCode0, #{
				<<"content-length">> => <<"0">>
			}, <<>>}, Opts),
	Transport:send(Socket, [
		cow_http:response(StatusCode, 'HTTP/1.1', maps:to_list(RespHeaders)),
		RespBody
	]),
	terminate(State, Reason).

-spec terminate(_, _) -> no_return().
terminate(undefined, Reason) ->
	exit({shutdown, Reason});
terminate(#state{streams=Streams, children=Children}, Reason) ->
	terminate_all_streams(Streams, Reason),
	%% @todo Leave them time to terminate.
	_ = [exit(Pid, kill) || {Pid, _, _} <- Children],
	exit(normal). %% @todo We probably don't want to exit normal on errors.

terminate_all_streams([], _) ->
	ok;
terminate_all_streams([#stream{id=StreamID, state=StreamState}|Tail], Reason) ->
	stream_call_terminate(StreamID, Reason, StreamState),
	terminate_all_streams(Tail, Reason).

%% System callbacks.

-spec system_continue(_, _, {#state{}, binary()}) -> ok.
system_continue(_, _, {State, Buffer}) ->
	loop(State, Buffer).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, _) ->
	exit(Reason).

-spec system_code_change(Misc, _, _, _) -> {ok, Misc} when Misc::{#state{}, binary()}.
system_code_change(Misc, _, _, _) ->
	{ok, Misc}.
