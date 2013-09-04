%% Copyright (c) 2011-2013, Loïc Hoguin <essen@ninenines.eu>
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
%%  <dt>compress</dt><dd>Whether to automatically compress the response
%%   body when the conditions are met. Disabled by default.</dd>
%%  <dt>env</dt><dd>The environment passed and optionally modified
%%   by middlewares.</dd>
%%  <dt>max_empty_lines</dt><dd>Max number of empty lines before a request.
%%   Defaults to 5.</dd>
%%  <dt>max_header_name_length</dt><dd>Max length allowed for header names.
%%   Defaults to 64.</dd>
%%  <dt>max_header_value_length</dt><dd>Max length allowed for header values.
%%   Defaults to 4096.</dd>
%%  <dt>max_headers</dt><dd>Max number of headers allowed.
%%   Defaults to 100.</dd>
%%  <dt>max_keepalive</dt><dd>Max number of requests allowed in a single
%%   keep-alive session. Defaults to 100.</dd>
%%  <dt>max_request_line_length</dt><dd>Max length allowed for the request
%%   line. Defaults to 4096.</dd>
%%  <dt>middlewares</dt><dd>The list of middlewares to execute when a
%%   request is received.</dd>
%%  <dt>onrequest</dt><dd>Optional fun that allows Req interaction before
%%   any dispatching is done. Host info, path info and bindings are thus
%%   not available at this point.</dd>
%%  <dt>onresponse</dt><dd>Optional fun that allows replacing a response
%%   sent by the application.</dd>
%%  <dt>timeout</dt><dd>Time in milliseconds a client has to send the
%%   full request line and headers. Defaults to 5000 milliseconds.</dd>
%% </dl>
%%
%% Note that there is no need to monitor these processes when using Cowboy as
%% an application as it already supervises them under the listener supervisor.
-module(cowboy_protocol).

%% API.
-export([start_link/4]).

%% Internal.
-export([init/4]).
-export([parse_request/3]).
-export([resume/6]).

-type opts() :: [{compress, boolean()}
	| {env, cowboy_middleware:env()}
	| {max_empty_lines, non_neg_integer()}
	| {max_header_name_length, non_neg_integer()}
	| {max_header_value_length, non_neg_integer()}
	| {max_headers, non_neg_integer()}
	| {max_keepalive, non_neg_integer()}
	| {max_request_line_length, non_neg_integer()}
	| {middlewares, [module()]}
	| {onrequest, cowboy:onrequest_fun()}
	| {onresponse, cowboy:onresponse_fun()}
	| {timeout, timeout()}].
-export_type([opts/0]).

-record(state, {
	socket :: inet:socket(),
	transport :: module(),
	middlewares :: [module()],
	compress :: boolean(),
	env :: cowboy_middleware:env(),
	onrequest :: undefined | cowboy:onrequest_fun(),
	onresponse = undefined :: undefined | cowboy:onresponse_fun(),
	max_empty_lines :: non_neg_integer(),
	req_keepalive = 1 :: non_neg_integer(),
	max_keepalive :: non_neg_integer(),
	max_request_line_length :: non_neg_integer(),
	max_header_name_length :: non_neg_integer(),
	max_header_value_length :: non_neg_integer(),
	max_headers :: non_neg_integer(),
	timeout :: timeout(),
	until :: non_neg_integer() | infinity
}).

%% API.

%% @doc Start an HTTP protocol process.
-spec start_link(ranch:ref(), inet:socket(), module(), opts()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
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
-spec init(ranch:ref(), inet:socket(), module(), opts()) -> ok.
init(Ref, Socket, Transport, Opts) ->
	Compress = get_value(compress, Opts, false),
	MaxEmptyLines = get_value(max_empty_lines, Opts, 5),
	MaxHeaderNameLength = get_value(max_header_name_length, Opts, 64),
	MaxHeaderValueLength = get_value(max_header_value_length, Opts, 4096),
	MaxHeaders = get_value(max_headers, Opts, 100),
	MaxKeepalive = get_value(max_keepalive, Opts, 100),
	MaxRequestLineLength = get_value(max_request_line_length, Opts, 4096),
	Middlewares = get_value(middlewares, Opts, [cowboy_router, cowboy_handler]),
	Env = [{listener, Ref}|get_value(env, Opts, [])],
	OnRequest = get_value(onrequest, Opts, undefined),
	OnResponse = get_value(onresponse, Opts, undefined),
	Timeout = get_value(timeout, Opts, 5000),
	ok = ranch:accept_ack(Ref),
	wait_request(<<>>, #state{socket=Socket, transport=Transport,
		middlewares=Middlewares, compress=Compress, env=Env,
		max_empty_lines=MaxEmptyLines, max_keepalive=MaxKeepalive,
		max_request_line_length=MaxRequestLineLength,
		max_header_name_length=MaxHeaderNameLength,
		max_header_value_length=MaxHeaderValueLength, max_headers=MaxHeaders,
		onrequest=OnRequest, onresponse=OnResponse,
		timeout=Timeout, until=until(Timeout)}, 0).

-spec until(timeout()) -> non_neg_integer() | infinity.
until(infinity) ->
	infinity;
until(Timeout) ->
	{Me, S, Mi} = os:timestamp(),
	Me * 1000000000 + S * 1000 + Mi div 1000 + Timeout.

%% Request parsing.
%%
%% The next set of functions is the request parsing code. All of it
%% runs using a single binary match context. This optimization ends
%% right after the header parsing is finished and the code becomes
%% more interesting past that point.

-spec recv(inet:socket(), module(), non_neg_integer() | infinity)
	-> {ok, binary()} | {error, closed | timeout | atom()}.
recv(Socket, Transport, infinity) ->
	Transport:recv(Socket, 0, infinity);
recv(Socket, Transport, Until) ->
	{Me, S, Mi} = os:timestamp(),
	Now = Me * 1000000000 + S * 1000 + Mi div 1000,
	Timeout = Until - Now,
	if	Timeout < 0 ->
			{error, timeout};
		true ->
			Transport:recv(Socket, 0, Timeout)
	end.

-spec wait_request(binary(), #state{}, non_neg_integer()) -> ok.
wait_request(Buffer, State=#state{socket=Socket, transport=Transport,
		until=Until}, ReqEmpty) ->
	case recv(Socket, Transport, Until) of
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
	case match_eol(Buffer, 0) of
		nomatch when byte_size(Buffer) > MaxLength ->
			error_terminate(414, State);
		nomatch ->
			wait_request(Buffer, State, ReqEmpty);
		1 when ReqEmpty =:= MaxEmpty ->
			error_terminate(400, State);
		1 ->
			<< _:16, Rest/binary >> = Buffer,
			parse_request(Rest, State, ReqEmpty + 1);
		_ ->
			parse_method(Buffer, State, <<>>)
	end.

match_eol(<< $\n, _/bits >>, N) ->
	N;
match_eol(<< _, Rest/bits >>, N) ->
	match_eol(Rest, N + 1);
match_eol(_, _) ->
	nomatch.

parse_method(<< C, Rest/bits >>, State, SoFar) ->
	case C of
		$\r -> error_terminate(400, State);
		$\s -> parse_uri(Rest, State, SoFar);
		_ -> parse_method(Rest, State, << SoFar/binary, C >>)
	end.

parse_uri(<< $\r, _/bits >>, State, _) ->
	error_terminate(400, State);
parse_uri(<< "* ", Rest/bits >>, State, Method) ->
	parse_version(Rest, State, Method, <<"*">>, <<>>);
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
		$\s -> parse_version(Rest, State, Method, SoFar, <<>>);
		$? -> parse_uri_query(Rest, State, Method, SoFar, <<>>);
		$# -> skip_uri_fragment(Rest, State, Method, SoFar, <<>>);
		_ -> parse_uri_path(Rest, State, Method, << SoFar/binary, C >>)
	end.

parse_uri_query(<< C, Rest/bits >>, S, M, P, SoFar) ->
	case C of
		$\r -> error_terminate(400, S);
		$\s -> parse_version(Rest, S, M, P, SoFar);
		$# -> skip_uri_fragment(Rest, S, M, P, SoFar);
		_ -> parse_uri_query(Rest, S, M, P, << SoFar/binary, C >>)
	end.

skip_uri_fragment(<< C, Rest/bits >>, S, M, P, Q) ->
	case C of
		$\r -> error_terminate(400, S);
		$\s -> parse_version(Rest, S, M, P, Q);
		_ -> skip_uri_fragment(Rest, S, M, P, Q)
	end.

parse_version(<< "HTTP/1.1\r\n", Rest/bits >>, S, M, P, Q) ->
	parse_header(Rest, S, M, P, Q, 'HTTP/1.1', []);
parse_version(<< "HTTP/1.0\r\n", Rest/bits >>, S, M, P, Q) ->
	parse_header(Rest, S, M, P, Q, 'HTTP/1.0', []);
parse_version(_, State, _, _, _) ->
	error_terminate(505, State).

%% Stop receiving data if we have more than allowed number of headers.
wait_header(_, State=#state{max_headers=MaxHeaders}, _, _, _, _, Headers)
		when length(Headers) >= MaxHeaders ->
	error_terminate(400, State);
wait_header(Buffer, State=#state{socket=Socket, transport=Transport,
		until=Until}, M, P, Q, V, H) ->
	case recv(Socket, Transport, Until) of
		{ok, Data} ->
			parse_header(<< Buffer/binary, Data/binary >>,
				State, M, P, Q, V, H);
		{error, timeout} ->
			error_terminate(408, State);
		{error, _} ->
			terminate(State)
	end.

parse_header(<< $\r, $\n, Rest/bits >>, S, M, P, Q, V, Headers) ->
	request(Rest, S, M, P, Q, V, lists:reverse(Headers));
parse_header(Buffer, State=#state{max_header_name_length=MaxLength},
		M, P, Q, V, H) ->
	case match_colon(Buffer, 0) of
		nomatch when byte_size(Buffer) > MaxLength ->
			error_terminate(400, State);
		nomatch ->
			wait_header(Buffer, State, M, P, Q, V, H);
		_ ->
			parse_hd_name(Buffer, State, M, P, Q, V, H, <<>>)
	end.

match_colon(<< $:, _/bits >>, N) ->
	N;
match_colon(<< _, Rest/bits >>, N) ->
	match_colon(Rest, N + 1);
match_colon(_, _) ->
	nomatch.

%% I know, this isn't exactly pretty. But this is the most critical
%% code path and as such needs to be optimized to death.
%%
%% ... Sorry for your eyes.
%%
%% But let's be honest, that's still pretty readable.
parse_hd_name(<< C, Rest/bits >>, S, M, P, Q, V, H, SoFar) ->
	case C of
		$: -> parse_hd_before_value(Rest, S, M, P, Q, V, H, SoFar);
		$\s -> parse_hd_name_ws(Rest, S, M, P, Q, V, H, SoFar);
		$\t -> parse_hd_name_ws(Rest, S, M, P, Q, V, H, SoFar);
		$A -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $a >>);
		$B -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $b >>);
		$C -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $c >>);
		$D -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $d >>);
		$E -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $e >>);
		$F -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $f >>);
		$G -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $g >>);
		$H -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $h >>);
		$I -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $i >>);
		$J -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $j >>);
		$K -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $k >>);
		$L -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $l >>);
		$M -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $m >>);
		$N -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $n >>);
		$O -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $o >>);
		$P -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $p >>);
		$Q -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $q >>);
		$R -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $r >>);
		$S -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $s >>);
		$T -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $t >>);
		$U -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $u >>);
		$V -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $v >>);
		$W -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $w >>);
		$X -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $x >>);
		$Y -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $y >>);
		$Z -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, $z >>);
		C -> parse_hd_name(Rest, S, M, P, Q, V, H, << SoFar/binary, C >>)
	end.

parse_hd_name_ws(<< C, Rest/bits >>, S, M, P, Q, V, H, Name) ->
	case C of
		$\s -> parse_hd_name_ws(Rest, S, M, P, Q, V, H, Name);
		$\t -> parse_hd_name_ws(Rest, S, M, P, Q, V, H, Name);
		$: -> parse_hd_before_value(Rest, S, M, P, Q, V, H, Name)
	end.

wait_hd_before_value(Buffer, State=#state{
		socket=Socket, transport=Transport, until=Until},
		M, P, Q, V, H, N) ->
	case recv(Socket, Transport, Until) of
		{ok, Data} ->
			parse_hd_before_value(<< Buffer/binary, Data/binary >>,
				State, M, P, Q, V, H, N);
		{error, timeout} ->
			error_terminate(408, State);
		{error, _} ->
			terminate(State)
	end.

parse_hd_before_value(<< $\s, Rest/bits >>, S, M, P, Q, V, H, N) ->
	parse_hd_before_value(Rest, S, M, P, Q, V, H, N);
parse_hd_before_value(<< $\t, Rest/bits >>, S, M, P, Q, V, H, N) ->
	parse_hd_before_value(Rest, S, M, P, Q, V, H, N);
parse_hd_before_value(Buffer, State=#state{
		max_header_value_length=MaxLength}, M, P, Q, V, H, N) ->
	case match_eol(Buffer, 0) of
		nomatch when byte_size(Buffer) > MaxLength ->
			error_terminate(400, State);
		nomatch ->
			wait_hd_before_value(Buffer, State, M, P, Q, V, H, N);
		_ ->
			parse_hd_value(Buffer, State, M, P, Q, V, H, N, <<>>)
	end.

%% We completely ignore the first argument which is always
%% the empty binary. We keep it there because we don't want
%% to change the other arguments' position and trigger costy
%% operations for no reasons.
wait_hd_value(_, State=#state{
		socket=Socket, transport=Transport, until=Until},
		M, P, Q, V, H, N, SoFar) ->
	case recv(Socket, Transport, Until) of
		{ok, Data} ->
			parse_hd_value(Data, State, M, P, Q, V, H, N, SoFar);
		{error, timeout} ->
			error_terminate(408, State);
		{error, _} ->
			terminate(State)
	end.

%% Pushing back as much as we could the retrieval of new data
%% to check for multilines allows us to avoid a few tests in
%% the critical path, but forces us to have a special function.
wait_hd_value_nl(_, State=#state{
		socket=Socket, transport=Transport, until=Until},
		M, P, Q, V, Headers, Name, SoFar) ->
	case recv(Socket, Transport, Until) of
		{ok, << C, Data/bits >>} when C =:= $\s; C =:= $\t  ->
			parse_hd_value(Data, State, M, P, Q, V, Headers, Name, SoFar);
		{ok, Data} ->
			parse_header(Data, State, M, P, Q, V, [{Name, SoFar}|Headers]);
		{error, timeout} ->
			error_terminate(408, State);
		{error, _} ->
			terminate(State)
	end.

parse_hd_value(<< $\r, Rest/bits >>, S, M, P, Q, V, Headers, Name, SoFar) ->
	case Rest of
		<< $\n >> ->
			wait_hd_value_nl(<<>>, S, M, P, Q, V, Headers, Name, SoFar);
		<< $\n, C, Rest2/bits >> when C =:= $\s; C =:= $\t ->
			parse_hd_value(Rest2, S, M, P, Q, V, Headers, Name, SoFar);
		<< $\n, Rest2/bits >> ->
			parse_header(Rest2, S, M, P, Q, V, [{Name, SoFar}|Headers])
	end;
parse_hd_value(<< C, Rest/bits >>, S, M, P, Q, V, H, N, SoFar) ->
	parse_hd_value(Rest, S, M, P, Q, V, H, N, << SoFar/binary, C >>);
parse_hd_value(<<>>, State=#state{max_header_value_length=MaxLength},
		_, _, _, _, _, _, SoFar) when byte_size(SoFar) > MaxLength ->
	error_terminate(400, State);
parse_hd_value(<<>>, S, M, P, Q, V, H, N, SoFar) ->
	wait_hd_value(<<>>, S, M, P, Q, V, H, N, SoFar).

request(B, State=#state{transport=Transport}, M, P, Q, Version, Headers) ->
	case lists:keyfind(<<"host">>, 1, Headers) of
		false when Version =:= 'HTTP/1.1' ->
			error_terminate(400, State);
		false ->
			request(B, State, M, P, Q, Version, Headers,
				<<>>, default_port(Transport:name()));
		{_, RawHost} ->
			case catch parse_host(RawHost, false, <<>>) of
				{'EXIT', _} ->
					error_terminate(400, State);
				{Host, undefined} ->
					request(B, State, M, P, Q, Version, Headers,
						Host, default_port(Transport:name()));
				{Host, Port} ->
					request(B, State, M, P, Q, Version, Headers,
						Host, Port)
			end
	end.

-spec default_port(atom()) -> 80 | 443.
default_port(ssl) -> 443;
default_port(_) -> 80.

%% Another hurtful block of code. :)
%%
%% Same code as cow_http:parse_fullhost/1, but inline because we
%% really want this to go fast.
parse_host(<< $[, Rest/bits >>, false, <<>>) ->
	parse_host(Rest, true, << $[ >>);
parse_host(<<>>, false, Acc) ->
	{Acc, undefined};
parse_host(<< $:, Rest/bits >>, false, Acc) ->
	{Acc, list_to_integer(binary_to_list(Rest))};
parse_host(<< $], Rest/bits >>, true, Acc) ->
	parse_host(Rest, false, << Acc/binary, $] >>);
parse_host(<< C, Rest/bits >>, E, Acc) ->
	case C of
		$A -> parse_host(Rest, E, << Acc/binary, $a >>);
		$B -> parse_host(Rest, E, << Acc/binary, $b >>);
		$C -> parse_host(Rest, E, << Acc/binary, $c >>);
		$D -> parse_host(Rest, E, << Acc/binary, $d >>);
		$E -> parse_host(Rest, E, << Acc/binary, $e >>);
		$F -> parse_host(Rest, E, << Acc/binary, $f >>);
		$G -> parse_host(Rest, E, << Acc/binary, $g >>);
		$H -> parse_host(Rest, E, << Acc/binary, $h >>);
		$I -> parse_host(Rest, E, << Acc/binary, $i >>);
		$J -> parse_host(Rest, E, << Acc/binary, $j >>);
		$K -> parse_host(Rest, E, << Acc/binary, $k >>);
		$L -> parse_host(Rest, E, << Acc/binary, $l >>);
		$M -> parse_host(Rest, E, << Acc/binary, $m >>);
		$N -> parse_host(Rest, E, << Acc/binary, $n >>);
		$O -> parse_host(Rest, E, << Acc/binary, $o >>);
		$P -> parse_host(Rest, E, << Acc/binary, $p >>);
		$Q -> parse_host(Rest, E, << Acc/binary, $q >>);
		$R -> parse_host(Rest, E, << Acc/binary, $r >>);
		$S -> parse_host(Rest, E, << Acc/binary, $s >>);
		$T -> parse_host(Rest, E, << Acc/binary, $t >>);
		$U -> parse_host(Rest, E, << Acc/binary, $u >>);
		$V -> parse_host(Rest, E, << Acc/binary, $v >>);
		$W -> parse_host(Rest, E, << Acc/binary, $w >>);
		$X -> parse_host(Rest, E, << Acc/binary, $x >>);
		$Y -> parse_host(Rest, E, << Acc/binary, $y >>);
		$Z -> parse_host(Rest, E, << Acc/binary, $z >>);
		_ -> parse_host(Rest, E, << Acc/binary, C >>)
	end.

%% End of request parsing.
%%
%% We create the Req object and start handling the request.

request(Buffer, State=#state{socket=Socket, transport=Transport,
		req_keepalive=ReqKeepalive, max_keepalive=MaxKeepalive,
		compress=Compress, onresponse=OnResponse},
		Method, Path, Query, Version, Headers, Host, Port) ->
	case Transport:peername(Socket) of
		{ok, Peer} ->
			Req = cowboy_req:new(Socket, Transport, Peer, Method, Path,
				Query, Version, Headers, Host, Port, Buffer,
				ReqKeepalive < MaxKeepalive, Compress, OnResponse),
			onrequest(Req, State);
		{error, _} ->
			%% Couldn't read the peer address; connection is gone.
			terminate(State)
	end.

%% Call the global onrequest callback. The callback can send a reply,
%% in which case we consider the request handled and move on to the next
%% one. Note that since we haven't dispatched yet, we don't know the
%% handler, host_info, path_info or bindings yet.
-spec onrequest(cowboy_req:req(), #state{}) -> ok.
onrequest(Req, State=#state{onrequest=undefined}) ->
	execute(Req, State);
onrequest(Req, State=#state{onrequest=OnRequest}) ->
	Req2 = OnRequest(Req),
	case cowboy_req:get(resp_state, Req2) of
		waiting -> execute(Req2, State);
		_ -> next_request(Req2, State, ok)
	end.

-spec execute(cowboy_req:req(), #state{}) -> ok.
execute(Req, State=#state{middlewares=Middlewares, env=Env}) ->
	execute(Req, State, Env, Middlewares).

-spec execute(cowboy_req:req(), #state{}, cowboy_middleware:env(), [module()])
	-> ok.
execute(Req, State, Env, []) ->
	next_request(Req, State, get_value(result, Env, ok));
execute(Req, State, Env, [Middleware|Tail]) ->
	case Middleware:execute(Req, Env) of
		{ok, Req2, Env2} ->
			execute(Req2, State, Env2, Tail);
		{suspend, Module, Function, Args} ->
			erlang:hibernate(?MODULE, resume,
				[State, Env, Tail, Module, Function, Args]);
		{halt, Req2} ->
			next_request(Req2, State, ok);
		{error, Code, Req2} ->
			error_terminate(Code, Req2, State)
	end.

%% @private
-spec resume(#state{}, cowboy_middleware:env(), [module()],
	module(), module(), [any()]) -> ok.
resume(State, Env, Tail, Module, Function, Args) ->
	case apply(Module, Function, Args) of
		{ok, Req2, Env2} ->
			execute(Req2, State, Env2, Tail);
		{suspend, Module2, Function2, Args2} ->
			erlang:hibernate(?MODULE, resume,
				[State, Env, Tail, Module2, Function2, Args2]);
		{halt, Req2} ->
			next_request(Req2, State, ok);
		{error, Code, Req2} ->
			error_terminate(Code, Req2, State)
	end.

-spec next_request(cowboy_req:req(), #state{}, any()) -> ok.
next_request(Req, State=#state{req_keepalive=Keepalive, timeout=Timeout},
		HandlerRes) ->
	cowboy_req:ensure_response(Req, 204),
	%% If we are going to close the connection,
	%% we do not want to attempt to skip the body.
	case cowboy_req:get(connection, Req) of
		close ->
			terminate(State);
		_ ->
			Buffer = case cowboy_req:skip_body(Req) of
				{ok, Req2} -> cowboy_req:get(buffer, Req2);
				_ -> close
			end,
			%% Flush the resp_sent message before moving on.
			receive {cowboy_req, resp_sent} -> ok after 0 -> ok end,
			if HandlerRes =:= ok, Buffer =/= close ->
					?MODULE:parse_request(Buffer,
						State#state{req_keepalive=Keepalive + 1,
						until=until(Timeout)}, 0);
				true ->
					terminate(State)
			end
	end.

-spec error_terminate(cowboy:http_status(), #state{}) -> ok.
error_terminate(Status, State=#state{socket=Socket, transport=Transport,
		compress=Compress, onresponse=OnResponse}) ->
	error_terminate(Status, cowboy_req:new(Socket, Transport,
		undefined, <<"GET">>, <<>>, <<>>, 'HTTP/1.1', [], <<>>,
		undefined, <<>>, false, Compress, OnResponse), State).

-spec error_terminate(cowboy:http_status(), cowboy_req:req(), #state{}) -> ok.
error_terminate(Status, Req, State) ->
	cowboy_req:maybe_reply(Status, Req),
	terminate(State).

-spec terminate(#state{}) -> ok.
terminate(#state{socket=Socket, transport=Transport}) ->
	Transport:close(Socket),
	ok.
