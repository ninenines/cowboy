%% Copyright (c) 2011-2014, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_req).

%% Request API.
-export([method/1]).
-export([version/1]).
-export([peer/1]).
-export([scheme/1]).
-export([host/1]).
-export([host_info/1]).
-export([port/1]).
-export([path/1]).
-export([path_info/1]).
-export([qs/1]).
-export([parse_qs/1]).
-export([match_qs/2]).
-export([uri/1]).
-export([uri/2]).
-export([binding/2]).
-export([binding/3]).
-export([bindings/1]).
-export([header/2]).
-export([header/3]).
-export([headers/1]).
-export([parse_header/2]).
-export([parse_header/3]).
-export([parse_cookies/1]).
-export([match_cookies/2]).

%% Request body API.
-export([has_body/1]).
-export([body_length/1]).
-export([read_body/1]).
-export([read_body/2]).

-export([body/1]).
-export([body/2]).
-export([body_qs/1]).
-export([body_qs/2]).

%% Multipart API.
-export([part/1]).
-export([part/2]).
-export([part_body/1]).
-export([part_body/2]).

%% Response API.
-export([set_resp_cookie/4]).
-export([set_resp_header/3]).
-export([set_resp_body/2]).
-export([set_resp_body_fun/2]).
-export([set_resp_body_fun/3]).
-export([has_resp_header/2]).
-export([has_resp_body/1]).
-export([delete_resp_header/2]).
-export([reply/2]).
-export([reply/3]).
-export([reply/4]).

-export([send_body/3]).

-export([chunked_reply/2]).
-export([chunked_reply/3]).
-export([chunk/2]).
-export([continue/1]).
-export([maybe_reply/2]).
-export([ensure_response/2]).

-type cookie_opts() :: cow_cookie:cookie_opts().
-export_type([cookie_opts/0]).

-type content_decode_fun() :: fun((binary()) -> binary()).
-type transfer_decode_fun() :: fun((binary(), any())
	-> cow_http_te:decode_ret()).

-type body_opts() :: [{continue, boolean()} %% doesn't apply
	| {length, non_neg_integer()}
	| {read_length, non_neg_integer()} %% to be added back later as optimization
	| {read_timeout, timeout()} %% same
	| {transfer_decode, transfer_decode_fun(), any()} %% doesn't apply
	| {content_decode, content_decode_fun()}]. %% does apply
-export_type([body_opts/0]).

-type resp_body_fun() :: fun((any(), module()) -> ok).
-type send_chunk_fun() :: fun((iodata()) -> ok).
-type resp_chunked_fun() :: fun((send_chunk_fun()) -> ok).

-record(http_req, {
	%% Transport.
	socket = undefined :: any(),
	transport = undefined :: undefined | module(),
	connection = keepalive :: keepalive | close,

	%% Request.
	pid = undefined :: pid(),
	method = <<"GET">> :: binary(),
	version = 'HTTP/1.1' :: cowboy:http_version(),
	peer = undefined :: undefined | {inet:ip_address(), inet:port_number()},
	host = undefined :: undefined | binary(),
	host_info = undefined :: undefined | cowboy_router:tokens(),
	port = undefined :: undefined | inet:port_number(),
	path = undefined :: binary(),
	path_info = undefined :: undefined | cowboy_router:tokens(),
	qs = undefined :: binary(),
	bindings = undefined :: undefined | cowboy_router:bindings(),
	headers = [] :: cowboy:http_headers(),
	meta = [] :: [{atom(), any()}],

	%% Request body.
	body_state = waiting :: waiting | done | {stream, non_neg_integer(),
		transfer_decode_fun(), any(), content_decode_fun()},
	buffer = <<>> :: binary(),
	multipart = undefined :: undefined | {binary(), binary()},

	%% Response.
	resp_compress = false :: boolean(),
	resp_state = waiting :: locked | waiting | waiting_stream
		| chunks | stream | done,
	resp_headers = [] :: cowboy:http_headers(),
	resp_body = <<>> :: iodata() | resp_body_fun()
		| {non_neg_integer(), resp_body_fun()}
		| {chunked, resp_chunked_fun()},

	%% Functions.
	onresponse = undefined :: undefined | already_called
		| cowboy:onresponse_fun()
}).

-opaque req() :: #http_req{}.
-export_type([req/0]).

%% Request API.

-spec method(req()) -> binary().
method(#{method := Method}) ->
	Method.

-spec version(req()) -> cowboy:http_version().
version(#{version := Version}) ->
	Version.

-spec peer(req()) -> {inet:ip_address(), inet:port_number()}.
peer(#{peer := Peer}) ->
	Peer.

-spec scheme(req()) -> binary().
scheme(#{scheme := Scheme}) ->
	Scheme.

-spec host(req()) -> binary().
host(#{host := Host}) ->
	Host.

-spec host_info(req()) -> cowboy_router:tokens() | undefined.
host_info(#{host_info := HostInfo}) ->
	HostInfo.

-spec port(req()) -> inet:port_number().
port(#{port := Port}) ->
	Port.

-spec path(req()) -> binary().
path(#{path := Path}) ->
	Path.

-spec path_info(req()) -> cowboy_router:tokens() | undefined.
path_info(#{path_info := PathInfo}) ->
	PathInfo.

-spec qs(req()) -> binary().
qs(#{qs := Qs}) ->
	Qs.

-spec parse_qs(req()) -> [{binary(), binary() | true}].
parse_qs(#{qs := Qs}) ->
	cow_qs:parse_qs(Qs).

-spec match_qs(cowboy:fields(), req()) -> map().
match_qs(Fields, Req) ->
	filter(Fields, kvlist_to_map(Fields, parse_qs(Req))).

-spec uri(req()) -> iodata().
uri(Req) ->
	uri(Req, #{}).

-spec uri(req(), map()) -> iodata().
uri(#{scheme := Scheme0, host := Host0, port := Port0,
		path := Path0, qs := Qs0}, Opts) ->
	Scheme = case maps:get(scheme, Opts, Scheme0) of
		S = undefined -> S;
		S -> iolist_to_binary(S)
	end,
	Host = maps:get(host, Opts, Host0),
	Port = maps:get(port, Opts, Port0),
	Path = maps:get(path, Opts, Path0),
	Qs = maps:get(qs, Opts, Qs0),
	Fragment = maps:get(fragment, Opts, undefined),
	[uri_host(Scheme, Scheme0, Port, Host), uri_path(Path), uri_qs(Qs), uri_fragment(Fragment)].

uri_host(_, _, _, undefined) -> <<>>;
uri_host(Scheme, Scheme0, Port, Host) ->
	case iolist_size(Host) of
		0 -> <<>>;
		_ -> [uri_scheme(Scheme), <<"//">>, Host, uri_port(Scheme, Scheme0, Port)]
	end.

uri_scheme(undefined) -> <<>>;
uri_scheme(Scheme) ->
	case iolist_size(Scheme) of
		0 -> Scheme;
		_ -> [Scheme, $:]
	end.

uri_port(_, _, undefined) -> <<>>;
uri_port(undefined, <<"http">>, 80) -> <<>>;
uri_port(undefined, <<"https">>, 443) -> <<>>;
uri_port(<<"http">>, _, 80) -> <<>>;
uri_port(<<"https">>, _, 443) -> <<>>;
uri_port(_, _, Port) ->
	[$:, integer_to_binary(Port)].

uri_path(undefined) -> <<>>;
uri_path(Path) -> Path.

uri_qs(undefined) -> <<>>;
uri_qs(Qs) ->
	case iolist_size(Qs) of
		0 -> Qs;
		_ -> [$?, Qs]
	end.

uri_fragment(undefined) -> <<>>;
uri_fragment(Fragment) ->
	case iolist_size(Fragment) of
		0 -> Fragment;
		_ -> [$#, Fragment]
	end.

-ifdef(TEST).
uri1_test() ->
	<<"http://localhost/path">> = iolist_to_binary(uri(#{
		scheme => <<"http">>, host => <<"localhost">>, port => 80,
		path => <<"/path">>, qs => <<>>})),
	<<"http://localhost:443/path">> = iolist_to_binary(uri(#{
		scheme => <<"http">>, host => <<"localhost">>, port => 443,
		path => <<"/path">>, qs => <<>>})),
	<<"http://localhost:8080/path">> = iolist_to_binary(uri(#{
		scheme => <<"http">>, host => <<"localhost">>, port => 8080,
		path => <<"/path">>, qs => <<>>})),
	<<"http://localhost:8080/path?dummy=2785">> = iolist_to_binary(uri(#{
		scheme => <<"http">>, host => <<"localhost">>, port => 8080,
		path => <<"/path">>, qs => <<"dummy=2785">>})),
	<<"https://localhost/path">> = iolist_to_binary(uri(#{
		scheme => <<"https">>, host => <<"localhost">>, port => 443,
		path => <<"/path">>, qs => <<>>})),
	<<"https://localhost:8443/path">> = iolist_to_binary(uri(#{
		scheme => <<"https">>, host => <<"localhost">>, port => 8443,
		path => <<"/path">>, qs => <<>>})),
	<<"https://localhost:8443/path?dummy=2785">> = iolist_to_binary(uri(#{
		scheme => <<"https">>, host => <<"localhost">>, port => 8443,
		path => <<"/path">>, qs => <<"dummy=2785">>})),
	ok.

uri2_test() ->
	Req = #{
		scheme => <<"http">>, host => <<"localhost">>, port => 8080,
		path => <<"/path">>, qs => <<"dummy=2785">>
	},
	<<"http://localhost:8080/path?dummy=2785">> = iolist_to_binary(uri(Req, #{})),
	%% Disable individual components.
	<<"//localhost:8080/path?dummy=2785">> = iolist_to_binary(uri(Req, #{scheme => undefined})),
	<<"/path?dummy=2785">> = iolist_to_binary(uri(Req, #{host => undefined})),
	<<"http://localhost/path?dummy=2785">> = iolist_to_binary(uri(Req, #{port => undefined})),
	<<"http://localhost:8080?dummy=2785">> = iolist_to_binary(uri(Req, #{path => undefined})),
	<<"http://localhost:8080/path">> = iolist_to_binary(uri(Req, #{qs => undefined})),
	<<"http://localhost:8080/path?dummy=2785">> = iolist_to_binary(uri(Req, #{fragment => undefined})),
	<<"http://localhost:8080">> = iolist_to_binary(uri(Req, #{path => undefined, qs => undefined})),
	<<>> = iolist_to_binary(uri(Req, #{host => undefined, path => undefined, qs => undefined})),
	%% Empty values.
	<<"//localhost:8080/path?dummy=2785">> = iolist_to_binary(uri(Req, #{scheme => <<>>})),
	<<"//localhost:8080/path?dummy=2785">> = iolist_to_binary(uri(Req, #{scheme => ""})),
	<<"//localhost:8080/path?dummy=2785">> = iolist_to_binary(uri(Req, #{scheme => [<<>>]})),
	<<"/path?dummy=2785">> = iolist_to_binary(uri(Req, #{host => <<>>})),
	<<"/path?dummy=2785">> = iolist_to_binary(uri(Req, #{host => ""})),
	<<"/path?dummy=2785">> = iolist_to_binary(uri(Req, #{host => [<<>>]})),
	<<"http://localhost:8080?dummy=2785">> = iolist_to_binary(uri(Req, #{path => <<>>})),
	<<"http://localhost:8080?dummy=2785">> = iolist_to_binary(uri(Req, #{path => ""})),
	<<"http://localhost:8080?dummy=2785">> = iolist_to_binary(uri(Req, #{path => [<<>>]})),
	<<"http://localhost:8080/path">> = iolist_to_binary(uri(Req, #{qs => <<>>})),
	<<"http://localhost:8080/path">> = iolist_to_binary(uri(Req, #{qs => ""})),
	<<"http://localhost:8080/path">> = iolist_to_binary(uri(Req, #{qs => [<<>>]})),
	<<"http://localhost:8080/path?dummy=2785">> = iolist_to_binary(uri(Req, #{fragment => <<>>})),
	<<"http://localhost:8080/path?dummy=2785">> = iolist_to_binary(uri(Req, #{fragment => ""})),
	<<"http://localhost:8080/path?dummy=2785">> = iolist_to_binary(uri(Req, #{fragment => [<<>>]})),
	%% Port is integer() | undefined.
	{'EXIT', _} = (catch iolist_to_binary(uri(Req, #{port => <<>>}))),
	{'EXIT', _} = (catch iolist_to_binary(uri(Req, #{port => ""}))),
	{'EXIT', _} = (catch iolist_to_binary(uri(Req, #{port => [<<>>]}))),
	%% Update components.
	<<"https://localhost:8080/path?dummy=2785">> = iolist_to_binary(uri(Req, #{scheme => "https"})),
	<<"http://example.org:8080/path?dummy=2785">> = iolist_to_binary(uri(Req, #{host => "example.org"})),
	<<"http://localhost:123/path?dummy=2785">> = iolist_to_binary(uri(Req, #{port => 123})),
	<<"http://localhost:8080/custom?dummy=2785">> = iolist_to_binary(uri(Req, #{path => "/custom"})),
	<<"http://localhost:8080/path?smart=42">> = iolist_to_binary(uri(Req, #{qs => "smart=42"})),
	<<"http://localhost:8080/path?dummy=2785#intro">> = iolist_to_binary(uri(Req, #{fragment => "intro"})),
	%% Interesting combinations.
	<<"http://localhost/path?dummy=2785">> = iolist_to_binary(uri(Req, #{port => 80})),
	<<"https://localhost/path?dummy=2785">> = iolist_to_binary(uri(Req, #{scheme => "https", port => 443})),
	ok.
-endif.

-spec binding(atom(), req()) -> any() | undefined.
binding(Name, Req) ->
	binding(Name, Req, undefined).

-spec binding(atom(), req(), Default) -> any() | Default when Default::any().
binding(Name, #{bindings := Bindings}, Default) when is_atom(Name) ->
	case lists:keyfind(Name, 1, Bindings) of
		{_, Value} -> Value;
		false -> Default
	end;
binding(Name, _, Default) when is_atom(Name) ->
	Default.

-spec bindings(req()) -> [{atom(), any()}].
bindings(#{bindings := Bindings}) ->
	Bindings;
bindings(_) ->
	[].

-spec header(binary(), req()) -> binary() | undefined.
header(Name, Req) ->
	header(Name, Req, undefined).

-spec header(binary(), req(), Default) -> binary() | Default when Default::any().
header(Name, #{headers := Headers}, Default) ->
	maps:get(Name, Headers, Default).

-spec headers(req()) -> cowboy:http_headers().
headers(#{headers := Headers}) ->
	Headers.

-spec parse_header(binary(), Req) -> any() when Req::req().
parse_header(Name = <<"content-length">>, Req) ->
	parse_header(Name, Req, 0, fun cow_http_hd:parse_content_length/1);
parse_header(Name = <<"cookie">>, Req) ->
	parse_header(Name, Req, [], fun cow_cookie:parse_cookie/1);
parse_header(Name = <<"transfer-encoding">>, Req) ->
	parse_header(Name, Req, [<<"identity">>], fun cow_http_hd:parse_transfer_encoding/1);
parse_header(Name, Req) ->
	parse_header(Name, Req, undefined).

-spec parse_header(binary(), Req, any()) -> any() when Req::req().
parse_header(Name, Req, Default) ->
	parse_header(Name, Req, Default, parse_header_fun(Name)).

parse_header_fun(<<"accept">>) -> fun cow_http_hd:parse_accept/1;
parse_header_fun(<<"accept-charset">>) -> fun cow_http_hd:parse_accept_charset/1;
parse_header_fun(<<"accept-encoding">>) -> fun cow_http_hd:parse_accept_encoding/1;
parse_header_fun(<<"accept-language">>) -> fun cow_http_hd:parse_accept_language/1;
parse_header_fun(<<"authorization">>) -> fun cow_http_hd:parse_authorization/1;
parse_header_fun(<<"connection">>) -> fun cow_http_hd:parse_connection/1;
parse_header_fun(<<"content-length">>) -> fun cow_http_hd:parse_content_length/1;
parse_header_fun(<<"content-type">>) -> fun cow_http_hd:parse_content_type/1;
parse_header_fun(<<"cookie">>) -> fun cow_cookie:parse_cookie/1;
parse_header_fun(<<"expect">>) -> fun cow_http_hd:parse_expect/1;
parse_header_fun(<<"if-match">>) -> fun cow_http_hd:parse_if_match/1;
parse_header_fun(<<"if-modified-since">>) -> fun cow_http_hd:parse_if_modified_since/1;
parse_header_fun(<<"if-none-match">>) -> fun cow_http_hd:parse_if_none_match/1;
parse_header_fun(<<"if-unmodified-since">>) -> fun cow_http_hd:parse_if_unmodified_since/1;
parse_header_fun(<<"range">>) -> fun cow_http_hd:parse_range/1;
parse_header_fun(<<"sec-websocket-extensions">>) -> fun cow_http_hd:parse_sec_websocket_extensions/1;
parse_header_fun(<<"sec-websocket-protocol">>) -> fun cow_http_hd:parse_sec_websocket_protocol_req/1;
parse_header_fun(<<"transfer-encoding">>) -> fun cow_http_hd:parse_transfer_encoding/1;
parse_header_fun(<<"upgrade">>) -> fun cow_http_hd:parse_upgrade/1;
parse_header_fun(<<"x-forwarded-for">>) -> fun cow_http_hd:parse_x_forwarded_for/1.

parse_header(Name, Req, Default, ParseFun) ->
	case header(Name, Req) of
		undefined -> Default;
		Value -> ParseFun(Value)
	end.

-spec parse_cookies(req()) -> [{binary(), binary()}].
parse_cookies(Req) ->
	parse_header(<<"cookie">>, Req).

-spec match_cookies(cowboy:fields(), req()) -> map().
match_cookies(Fields, Req) ->
	filter(Fields, kvlist_to_map(Fields, parse_cookies(Req))).

%% Request Body API.

-spec has_body(req()) -> boolean().
has_body(#{has_body := HasBody}) ->
	HasBody.

%% The length may not be known if Transfer-Encoding is not identity,
%% and the body hasn't been read at the time of the call.
-spec body_length(req()) -> undefined | non_neg_integer().
body_length(#{body_length := Length}) ->
	Length.

-spec body(Req) -> {ok, binary(), Req} | {more, binary(), Req} when Req::req().
body(Req) ->
	body(Req, []).

-spec read_body(Req) -> {ok, binary(), Req} | {more, binary(), Req} when Req::req().
read_body(Req) ->
	read_body(Req, []).

-spec read_body(Req, body_opts()) -> {ok, binary(), Req} | {more, binary(), Req} when Req::req().
read_body(Req=#{pid := Pid, streamid := StreamID}, Opts) ->
	%% @todo Opts should be a map
	Length = case lists:keyfind(length, 1, Opts) of
		false -> 8000000;
		{_, ChunkLen0} -> ChunkLen0
	end,
	ReadTimeout = case lists:keyfind(read_timeout, 1, Opts) of
		false -> 15000;
		{_, ReadTimeout0} -> ReadTimeout0
	end,
	Ref = make_ref(),
	Pid ! {{Pid, StreamID}, {read_body, Ref, Length}},
	receive
		{request_body, Ref, nofin, Body} ->
			{more, Body, Req};
		{request_body, Ref, {fin, BodyLength}, Body} ->
			{ok, Body, set_body_length(Req, BodyLength)}
	after ReadTimeout ->
		exit(read_body_timeout)
	end.

set_body_length(Req=#{headers := Headers}, BodyLength) ->
	Req#{
		headers => Headers#{<<"content-length">> => integer_to_binary(BodyLength)},
		body_length => BodyLength
	}.

-spec body(Req, body_opts()) -> {ok, binary(), Req} | {more, binary(), Req} when Req::req().
body(Req=#http_req{body_state=waiting}, Opts) ->
	%% Send a 100 continue if needed (enabled by default).
	case lists:keyfind(continue, 1, Opts) of
		{_, false} ->
			ok;
		_ ->
			ExpectHeader = parse_header(<<"expect">>, Req),
			ok = case ExpectHeader of
				continue -> continue(Req);
				_ -> ok
			end
	end,
	%% Initialize body streaming state.
	CFun = case lists:keyfind(content_decode, 1, Opts) of
		false ->
			fun body_content_decode_identity/1;
		{_, CFun0} ->
			CFun0
	end,
	case lists:keyfind(transfer_decode, 1, Opts) of
		false ->
			case parse_header(<<"transfer-encoding">>, Req) of
				[<<"chunked">>] ->
					body(Req#http_req{body_state={stream, 0,
						fun cow_http_te:stream_chunked/2, {0, 0}, CFun}}, Opts);
				[<<"identity">>] ->
					case body_length(Req) of
						0 ->
							{ok, <<>>, Req#http_req{body_state=done}};
						Len ->
							body(Req#http_req{body_state={stream, Len,
								fun cow_http_te:stream_identity/2, {0, Len},
								CFun}}, Opts)
					end
			end;
		{_, TFun, TState} ->
			body(Req#http_req{body_state={stream, 0,
				TFun, TState, CFun}}, Opts)
	end;
body(Req=#http_req{body_state=done}, _) ->
	{ok, <<>>, Req};
body(Req, Opts) ->
	ChunkLen = case lists:keyfind(length, 1, Opts) of
		false -> 8000000;
		{_, ChunkLen0} -> ChunkLen0
	end,
	ReadLen = case lists:keyfind(read_length, 1, Opts) of
		false -> 1000000;
		{_, ReadLen0} -> ReadLen0
	end,
	ReadTimeout = case lists:keyfind(read_timeout, 1, Opts) of
		false -> 15000;
		{_, ReadTimeout0} -> ReadTimeout0
	end,
	body_loop(Req, ReadTimeout, ReadLen, ChunkLen, <<>>).

%% Default identity function for content decoding.
%% @todo Move into cowlib when more content decode functions get implemented.
body_content_decode_identity(Data) -> Data.

body_loop(Req=#http_req{buffer=Buffer, body_state={stream, Length, _, _, _}},
		ReadTimeout, ReadLength, ChunkLength, Acc) ->
	{Tag, Res, Req2} = case Buffer of
		<<>> ->
			body_recv(Req, ReadTimeout, min(Length, ReadLength));
		_ ->
			body_decode(Req, ReadTimeout)
	end,
	case {Tag, Res} of
		{ok, Data} ->
			{ok, << Acc/binary, Data/binary >>, Req2};
		{more, Data} ->
			Acc2 = << Acc/binary, Data/binary >>,
			case byte_size(Acc2) >= ChunkLength of
				true -> {more, Acc2, Req2};
				false -> body_loop(Req2, ReadTimeout, ReadLength, ChunkLength, Acc2)
			end
	end.

body_recv(Req=#http_req{transport=Transport, socket=Socket, buffer=Buffer},
		ReadTimeout, ReadLength) ->
	{ok, Data} = Transport:recv(Socket, ReadLength, ReadTimeout),
	body_decode(Req#http_req{buffer= << Buffer/binary, Data/binary >>}, ReadTimeout).

%% Two decodings happen. First a decoding function is applied to the
%% transferred data, and then another is applied to the actual content.
%%
%% Transfer encoding is generally used for chunked bodies. The decoding
%% function uses a state to keep track of how much it has read, which is
%% also initialized through this function.
%%
%% Content encoding is generally used for compression.
%%
%% @todo Handle chunked after-the-facts headers.
%% @todo Depending on the length returned we might want to 0 or +5 it.
body_decode(Req=#http_req{buffer=Data, body_state={stream, _,
		TDecode, TState, CDecode}}, ReadTimeout) ->
	case TDecode(Data, TState) of
		more ->
			body_recv(Req#http_req{body_state={stream, 0,
				TDecode, TState, CDecode}}, ReadTimeout, 0);
		{more, Data2, TState2} ->
			{more, CDecode(Data2), Req#http_req{body_state={stream, 0,
				TDecode, TState2, CDecode}, buffer= <<>>}};
		{more, Data2, Length, TState2} when is_integer(Length) ->
			{more, CDecode(Data2), Req#http_req{body_state={stream, Length,
				TDecode, TState2, CDecode}, buffer= <<>>}};
		{more, Data2, Rest, TState2} ->
			{more, CDecode(Data2), Req#http_req{body_state={stream, 0,
				TDecode, TState2, CDecode}, buffer=Rest}};
		{done, TotalLength, Rest} ->
			{ok, <<>>, body_decode_end(Req, TotalLength, Rest)};
		{done, Data2, TotalLength, Rest} ->
			{ok, CDecode(Data2), body_decode_end(Req, TotalLength, Rest)}
	end.

body_decode_end(Req=#http_req{headers=Headers}, TotalLength, Rest) ->
	Headers2 = lists:keystore(<<"content-length">>, 1, Headers,
		{<<"content-length">>, integer_to_binary(TotalLength)}),
	%% At this point we just assume TEs were all decoded.
	Headers3 = lists:keydelete(<<"transfer-encoding">>, 1, Headers2),
	Req#http_req{buffer=Rest, body_state=done, headers=Headers3}.

-spec body_qs(Req) -> {ok, [{binary(), binary() | true}], Req}
	| {badlength, Req} when Req::req().
body_qs(Req) ->
	body_qs(Req, [
		{length, 64000},
		{read_length, 64000},
		{read_timeout, 5000}]).

-spec body_qs(Req, body_opts()) -> {ok, [{binary(), binary() | true}], Req}
	| {badlength, Req} when Req::req().
body_qs(Req, Opts) ->
	case read_body(Req, Opts) of
		{ok, Body, Req2} ->
			{ok, cow_qs:parse_qs(Body), Req2};
		{more, _, Req2} ->
			{badlength, Req2}
	end.

%% Multipart API.

-spec part(Req)
	-> {ok, cow_multipart:headers(), Req} | {done, Req}
	when Req::req().
part(Req) ->
	part(Req, [
		{length, 64000},
		{read_length, 64000},
		{read_timeout, 5000}]).

-spec part(Req, body_opts())
	-> {ok, cow_multipart:headers(), Req} | {done, Req}
	when Req::req().
part(Req, Opts) ->
	case maps:is_key(multipart, Req) of
		true ->
			{Data, Req2} = stream_multipart(Req, Opts),
			part(Data, Opts, Req2);
		false ->
			part(init_multipart(Req), Opts)
	end.

part(Buffer, Opts, Req=#{multipart := {Boundary, _}}) ->
	case cow_multipart:parse_headers(Buffer, Boundary) of
		more ->
			{Data, Req2} = stream_multipart(Req, Opts),
			part(<< Buffer/binary, Data/binary >>, Opts, Req2);
		{more, Buffer2} ->
			{Data, Req2} = stream_multipart(Req, Opts),
			part(<< Buffer2/binary, Data/binary >>, Opts, Req2);
		{ok, Headers, Rest} ->
			{ok, Headers, Req#{multipart => {Boundary, Rest}}};
		%% Ignore epilogue.
		{done, _} ->
			{done, Req#{multipart => done}}
	end.

-spec part_body(Req)
	-> {ok, binary(), Req} | {more, binary(), Req}
	when Req::req().
part_body(Req) ->
	part_body(Req, []).

-spec part_body(Req, body_opts())
	-> {ok, binary(), Req} | {more, binary(), Req}
	when Req::req().
part_body(Req, Opts) ->
	case maps:is_key(multipart, Req) of
		true ->
			part_body(<<>>, Opts, Req, <<>>);
		false ->
			part_body(init_multipart(Req), Opts)
	end.

part_body(Buffer, Opts, Req=#{multipart := {Boundary, _}}, Acc) ->
	ChunkLen = case lists:keyfind(length, 1, Opts) of
		false -> 8000000;
		{_, ChunkLen0} -> ChunkLen0
	end,
	case byte_size(Acc) > ChunkLen of
		true ->
			{more, Acc, Req#{multipart => {Boundary, Buffer}}};
		false ->
			{Data, Req2} = stream_multipart(Req, Opts),
			case cow_multipart:parse_body(<< Buffer/binary, Data/binary >>, Boundary) of
				{ok, Body} ->
					part_body(<<>>, Opts, Req2, << Acc/binary, Body/binary >>);
				{ok, Body, Rest} ->
					part_body(Rest, Opts, Req2, << Acc/binary, Body/binary >>);
				done ->
					{ok, Acc, Req2};
				{done, Body} ->
					{ok, << Acc/binary, Body/binary >>, Req2};
				{done, Body, Rest} ->
					{ok, << Acc/binary, Body/binary >>,
						Req2#{multipart => {Boundary, Rest}}}
			end
	end.

init_multipart(Req) ->
	{<<"multipart">>, _, Params} = parse_header(<<"content-type">>, Req),
	{_, Boundary} = lists:keyfind(<<"boundary">>, 1, Params),
	Req#{multipart => {Boundary, <<>>}}.

stream_multipart(Req=#{multipart := done}, _) ->
	{<<>>, Req};
stream_multipart(Req=#{multipart := {_, <<>>}}, Opts) ->
	{_, Data, Req2} = read_body(Req, Opts),
	{Data, Req2};
stream_multipart(Req=#{multipart := {Boundary, Buffer}}, _) ->
	{Buffer, Req#{multipart => {Boundary, <<>>}}}.

%% Response API.

%% The cookie name cannot contain any of the following characters:
%%   =,;\s\t\r\n\013\014
%%
%% The cookie value cannot contain any of the following characters:
%%   ,; \t\r\n\013\014
-spec set_resp_cookie(iodata(), iodata(), cookie_opts(), Req)
	-> Req when Req::req().
set_resp_cookie(Name, Value, Opts, Req) ->
	Cookie = cow_cookie:setcookie(Name, Value, Opts),
	%% @todo Nah, keep separate.
	set_resp_header(<<"set-cookie">>, Cookie, Req).

-spec set_resp_header(binary(), iodata(), Req)
	-> Req when Req::req().
set_resp_header(Name, Value, Req=#{resp_headers := RespHeaders}) ->
	Req#{resp_headers => RespHeaders#{Name => Value}};
set_resp_header(Name,Value, Req) ->
	Req#{resp_headers => #{Name => Value}}.

%% @todo {sendfile, Offset, Bytes, Path} tuple
-spec set_resp_body(iodata(), Req) -> Req when Req::req().
set_resp_body(Body, Req) ->
	Req#{resp_body => Body}.
%set_resp_body(Body, Req) ->
%	Req#http_req{resp_body=Body}.

-spec set_resp_body_fun(resp_body_fun(), Req) -> Req when Req::req().
set_resp_body_fun(StreamFun, Req) when is_function(StreamFun) ->
	Req#http_req{resp_body=StreamFun}.

%% If the body function crashes while writing the response body or writes
%% fewer bytes than declared the behaviour is undefined.
-spec set_resp_body_fun(non_neg_integer(), resp_body_fun(), Req)
	-> Req when Req::req();
	(chunked, resp_chunked_fun(), Req)
	-> Req when Req::req().
set_resp_body_fun(StreamLen, StreamFun, Req)
		when is_integer(StreamLen), is_function(StreamFun) ->
	Req#http_req{resp_body={StreamLen, StreamFun}};
set_resp_body_fun(chunked, StreamFun, Req)
		when is_function(StreamFun) ->
	Req#http_req{resp_body={chunked, StreamFun}}.

-spec has_resp_header(binary(), req()) -> boolean().
has_resp_header(Name, #{resp_headers := RespHeaders}) ->
	maps:is_key(Name, RespHeaders);
has_resp_header(_, _) ->
	false.

-spec has_resp_body(req()) -> boolean().
has_resp_body(#{resp_body := {sendfile, Len, _}}) ->
	Len > 0;
has_resp_body(#{resp_body := RespBody}) ->
	iolist_size(RespBody) > 0;
has_resp_body(_) ->
	false.

%has_resp_body(#http_req{resp_body=RespBody}) when is_function(RespBody) ->
%	true;
%has_resp_body(#http_req{resp_body={chunked, _}}) ->
%	true;
%has_resp_body(#http_req{resp_body={Length, _}}) ->
%	Length > 0;
%has_resp_body(#http_req{resp_body=RespBody}) ->
%	iolist_size(RespBody) > 0.

-spec delete_resp_header(binary(), Req)
	-> Req when Req::req().
delete_resp_header(Name, Req=#{resp_headers := RespHeaders}) ->
	Req#{resp_headers => maps:remove(Name, RespHeaders)}.

-spec reply(cowboy:http_status(), Req) -> Req when Req::req().
reply(Status, Req) ->
	reply(Status, #{}, Req).

-spec reply(cowboy:http_status(), cowboy:http_headers(), Req)
	-> Req when Req::req().
reply(Status, Headers, Req=#{resp_body := Body}) ->
	reply(Status, Headers, Body, Req);
reply(Status, Headers, Req) ->
	reply(Status, Headers, <<>>, Req).

-spec reply(cowboy:http_status(), cowboy:http_headers(),
	iodata() | resp_body_fun() | {non_neg_integer(), resp_body_fun()}
	| {chunked, resp_chunked_fun()}, Req)
	-> Req when Req::req().
reply(Status, Headers, Stream = {stream, undefined, _}, Req) ->
	do_stream_reply(Status, Headers, Stream, Req);
reply(Status, Headers, Stream = {stream, Len, _}, Req) ->
	do_stream_reply(Status, Headers#{
		<<"content-length">> => integer_to_binary(Len)
	}, Stream, Req);
reply(Status, Headers, SendFile = {sendfile, _, Len, _}, Req) ->
	do_reply(Status, Headers#{
		<<"content-length">> => integer_to_binary(Len)
	}, SendFile, Req);
reply(Status, Headers, Body, Req) ->
	do_reply(Status, Headers#{
		<<"content-length">> => integer_to_binary(iolist_size(Body))
	}, Body, Req).

do_stream_reply(Status, Headers, {stream, _, Fun}, Req=#{pid := Pid, streamid := StreamID}) ->
	Pid ! {{Pid, StreamID}, {headers, Status, response_headers(Headers, Req)}},
	Fun(),
	ok.

do_reply(Status, Headers, Body, Req=#{pid := Pid, streamid := StreamID}) ->
	Pid ! {{Pid, StreamID}, {response, Status, response_headers(Headers, Req), Body}},
	ok.

-spec send_body(iodata(), fin | nofin, req()) -> ok.
send_body(Data, IsFin, #{pid := Pid, streamid := StreamID}) ->
	Pid ! {{Pid, StreamID}, {data, IsFin, Data}},
	ok.

response_headers(Headers, Req) ->
	RespHeaders = maps:get(resp_headers, Req, #{}),
	maps:merge(#{
		<<"date">> => cowboy_clock:rfc1123(),
		<<"server">> => <<"Cowboy">>
	}, maps:merge(RespHeaders, Headers)).

%reply(Status, Headers, Body, Req=#http_req{
%		socket=Socket, transport=Transport,
%		version=Version, connection=Connection,
%		method=Method, resp_compress=Compress,
%		resp_state=RespState, resp_headers=RespHeaders})
%		when RespState =:= waiting; RespState =:= waiting_stream ->
%	Req3 = case Body of
%		BodyFun when is_function(BodyFun) ->
%			%% We stream the response body until we close the connection.
%			RespConn = close,
%			{RespType, Req2} = if
%				true ->
%					response(Status, Headers, RespHeaders, [
%						{<<"connection">>, <<"close">>},
%						{<<"date">>, cowboy_clock:rfc1123()},
%						{<<"server">>, <<"Cowboy">>},
%						{<<"transfer-encoding">>, <<"identity">>}
%					], <<>>, Req)
%			end,
%			if	RespType =/= hook, Method =/= <<"HEAD">> ->
%					BodyFun(Socket, Transport);
%				true -> ok
%			end,
%			Req2#http_req{connection=RespConn};
%		{chunked, BodyFun} ->
%			%% We stream the response body in chunks.
%			{RespType, Req2} = chunked_response(Status, Headers, Req),
%			if	RespType =/= hook, Method =/= <<"HEAD">> ->
%					ChunkFun = fun(IoData) -> chunk(IoData, Req2) end,
%					BodyFun(ChunkFun),
%					%% Send the last chunk if chunked encoding was used.
%					if
%						Version =:= 'HTTP/1.0'; RespState =:= waiting_stream ->
%							Req2;
%						true ->
%							last_chunk(Req2)
%					end;
%				true -> Req2
%			end;
%		{ContentLength, BodyFun} ->
%			%% We stream the response body for ContentLength bytes.
%			RespConn = response_connection(Headers, Connection),
%			{RespType, Req2} = response(Status, Headers, RespHeaders, [
%					{<<"content-length">>, integer_to_list(ContentLength)},
%					{<<"date">>, cowboy_clock:rfc1123()},
%					{<<"server">>, <<"Cowboy">>}
%				|HTTP11Headers], stream, Req),
%			if	RespType =/= hook, Method =/= <<"HEAD">> ->
%					BodyFun(Socket, Transport);
%				true -> ok
%			end,
%			Req2#http_req{connection=RespConn};
%		_ when Compress ->
%			RespConn = response_connection(Headers, Connection),
%			Req2 = reply_may_compress(Status, Headers, Body, Req,
%				RespHeaders, HTTP11Headers, Method),
%			Req2#http_req{connection=RespConn};
%		_ ->
%			RespConn = response_connection(Headers, Connection),
%			Req2 = reply_no_compress(Status, Headers, Body, Req,
%				RespHeaders, HTTP11Headers, Method, iolist_size(Body)),
%			Req2#http_req{connection=RespConn}
%	end,
%	Req3#http_req{resp_state=done, resp_headers=[], resp_body= <<>>}.

%reply_may_compress(Status, Headers, Body, Req,
%		RespHeaders, HTTP11Headers, Method) ->
%	BodySize = iolist_size(Body),
%	try parse_header(<<"accept-encoding">>, Req) of
%		Encodings ->
%			CanGzip = (BodySize > 300)
%				andalso (false =:= lists:keyfind(<<"content-encoding">>,
%					1, Headers))
%				andalso (false =:= lists:keyfind(<<"content-encoding">>,
%					1, RespHeaders))
%				andalso (false =:= lists:keyfind(<<"transfer-encoding">>,
%					1, Headers))
%				andalso (false =:= lists:keyfind(<<"transfer-encoding">>,
%					1, RespHeaders))
%				andalso (Encodings =/= undefined)
%				andalso (false =/= lists:keyfind(<<"gzip">>, 1, Encodings)),
%			case CanGzip of
%				true ->
%					GzBody = zlib:gzip(Body),
%					{_, Req2} = response(Status, Headers, RespHeaders, [
%							{<<"content-length">>, integer_to_list(byte_size(GzBody))},
%							{<<"content-encoding">>, <<"gzip">>},
%						|HTTP11Headers],
%						case Method of <<"HEAD">> -> <<>>; _ -> GzBody end,
%						Req),
%					Req2;
%				false ->
%					reply_no_compress(Status, Headers, Body, Req,
%						RespHeaders, HTTP11Headers, Method, BodySize)
%			end
%	catch _:_ ->
%		reply_no_compress(Status, Headers, Body, Req,
%			RespHeaders, HTTP11Headers, Method, BodySize)
%	end.
%
%reply_no_compress(Status, Headers, Body, Req,
%		RespHeaders, HTTP11Headers, Method, BodySize) ->
%	{_, Req2} = response(Status, Headers, RespHeaders, [
%			{<<"content-length">>, integer_to_list(BodySize)},
%		|HTTP11Headers],
%		case Method of <<"HEAD">> -> <<>>; _ -> Body end,
%		Req),
%	Req2.

-spec chunked_reply(cowboy:http_status(), Req) -> Req when Req::req().
chunked_reply(Status, Req) ->
	chunked_reply(Status, #{}, Req).

-spec chunked_reply(cowboy:http_status(), cowboy:http_headers(), Req)
	-> Req when Req::req().
chunked_reply(Status, Headers, Req=#{pid := Pid, streamid := StreamID}) ->
	Pid ! {{Pid, StreamID}, {headers, Status, response_headers(Headers, Req)}},
	Req. %% @todo return ok
%	ok.

-spec chunk(iodata(), req()) -> ok.
chunk(_Data, #{method := <<"HEAD">>}) ->
	ok;
chunk(Data, #{pid := Pid, streamid := StreamID}) ->
	case iolist_size(Data) of
		0 -> ok;
		_ ->
			Pid ! {{Pid, StreamID}, {data, nofin, Data}},
			ok
	end.

%% If ever made public, need to send nothing if HEAD.
-spec last_chunk(Req) -> Req when Req::req().
last_chunk(Req=#http_req{socket=Socket, transport=Transport}) ->
	_ = Transport:send(Socket, <<"0\r\n\r\n">>),
	Req#http_req{resp_state=done}.

-spec continue(req()) -> ok.
continue(#http_req{socket=Socket, transport=Transport,
		version=Version}) ->
	HTTPVer = atom_to_binary(Version, latin1),
	ok = Transport:send(Socket,
		<< HTTPVer/binary, " ", (status(100))/binary, "\r\n\r\n" >>).

%% Meant to be used internally for sending errors after crashes.
-spec maybe_reply([{module(), atom(), arity() | [term()], _}], req()) -> ok.
maybe_reply(Stacktrace, Req) ->
	receive
		{cowboy_req, resp_sent} -> ok
	after 0 ->
		_ = do_maybe_reply(Stacktrace, Req),
		ok
	end.

do_maybe_reply([{erlang, binary_to_integer, _, _}, {cow_http_hd, parse_content_length, _, _}|_], Req) ->
	cowboy_req:reply(400, Req);
do_maybe_reply([{cow_http_hd, _, _, _}|_], Req) ->
	cowboy_req:reply(400, Req);
do_maybe_reply(_, Req) ->
	cowboy_req:reply(500, Req).

-spec ensure_response(req(), cowboy:http_status()) -> ok.
%% The response has already been fully sent to the client.
ensure_response(#http_req{resp_state=done}, _) ->
	ok;
%% No response has been sent but everything apparently went fine.
%% Reply with the status code found in the second argument.
ensure_response(Req=#http_req{resp_state=RespState}, Status)
		when RespState =:= waiting; RespState =:= waiting_stream ->
	_ = reply(Status, [], [], Req),
	ok;
%% Terminate the chunked body for HTTP/1.1 only.
ensure_response(#http_req{method= <<"HEAD">>}, _) ->
	ok;
ensure_response(Req=#http_req{resp_state=chunks}, _) ->
	_ = last_chunk(Req),
	ok;
ensure_response(#http_req{}, _) ->
	ok.

%% Internal.

-spec status(cowboy:http_status()) -> binary().
status(100) -> <<"100 Continue">>;
status(101) -> <<"101 Switching Protocols">>;
status(102) -> <<"102 Processing">>;
status(200) -> <<"200 OK">>;
status(201) -> <<"201 Created">>;
status(202) -> <<"202 Accepted">>;
status(203) -> <<"203 Non-Authoritative Information">>;
status(204) -> <<"204 No Content">>;
status(205) -> <<"205 Reset Content">>;
status(206) -> <<"206 Partial Content">>;
status(207) -> <<"207 Multi-Status">>;
status(226) -> <<"226 IM Used">>;
status(300) -> <<"300 Multiple Choices">>;
status(301) -> <<"301 Moved Permanently">>;
status(302) -> <<"302 Found">>;
status(303) -> <<"303 See Other">>;
status(304) -> <<"304 Not Modified">>;
status(305) -> <<"305 Use Proxy">>;
status(306) -> <<"306 Switch Proxy">>;
status(307) -> <<"307 Temporary Redirect">>;
status(400) -> <<"400 Bad Request">>;
status(401) -> <<"401 Unauthorized">>;
status(402) -> <<"402 Payment Required">>;
status(403) -> <<"403 Forbidden">>;
status(404) -> <<"404 Not Found">>;
status(405) -> <<"405 Method Not Allowed">>;
status(406) -> <<"406 Not Acceptable">>;
status(407) -> <<"407 Proxy Authentication Required">>;
status(408) -> <<"408 Request Timeout">>;
status(409) -> <<"409 Conflict">>;
status(410) -> <<"410 Gone">>;
status(411) -> <<"411 Length Required">>;
status(412) -> <<"412 Precondition Failed">>;
status(413) -> <<"413 Request Entity Too Large">>;
status(414) -> <<"414 Request-URI Too Long">>;
status(415) -> <<"415 Unsupported Media Type">>;
status(416) -> <<"416 Requested Range Not Satisfiable">>;
status(417) -> <<"417 Expectation Failed">>;
status(418) -> <<"418 I'm a teapot">>;
status(422) -> <<"422 Unprocessable Entity">>;
status(423) -> <<"423 Locked">>;
status(424) -> <<"424 Failed Dependency">>;
status(425) -> <<"425 Unordered Collection">>;
status(426) -> <<"426 Upgrade Required">>;
status(428) -> <<"428 Precondition Required">>;
status(429) -> <<"429 Too Many Requests">>;
status(431) -> <<"431 Request Header Fields Too Large">>;
status(500) -> <<"500 Internal Server Error">>;
status(501) -> <<"501 Not Implemented">>;
status(502) -> <<"502 Bad Gateway">>;
status(503) -> <<"503 Service Unavailable">>;
status(504) -> <<"504 Gateway Timeout">>;
status(505) -> <<"505 HTTP Version Not Supported">>;
status(506) -> <<"506 Variant Also Negotiates">>;
status(507) -> <<"507 Insufficient Storage">>;
status(510) -> <<"510 Not Extended">>;
status(511) -> <<"511 Network Authentication Required">>;
status(B) when is_binary(B) -> B.

%% Create map, convert keys to atoms and group duplicate keys into lists.
%% Keys that are not found in the user provided list are entirely skipped.
%% @todo Can probably be done directly while parsing.
kvlist_to_map(Fields, KvList) ->
	Keys = [case K of
		{Key, _} -> Key;
		{Key, _, _} -> Key;
		Key -> Key
	end || K <- Fields],
	kvlist_to_map(Keys, KvList, #{}).

kvlist_to_map(_, [], Map) ->
	Map;
kvlist_to_map(Keys, [{Key, Value}|Tail], Map) ->
	try binary_to_existing_atom(Key, utf8) of
		Atom ->
			case lists:member(Atom, Keys) of
				true ->
					case maps:find(Atom, Map) of
						{ok, MapValue} when is_list(MapValue) ->
							kvlist_to_map(Keys, Tail,
								Map#{Atom => [Value|MapValue]});
						{ok, MapValue} ->
							kvlist_to_map(Keys, Tail,
								Map#{Atom => [Value, MapValue]});
						error ->
							kvlist_to_map(Keys, Tail,
								Map#{Atom => Value})
					end;
				false ->
					kvlist_to_map(Keys, Tail, Map)
			end
	catch error:badarg ->
		kvlist_to_map(Keys, Tail, Map)
	end.

%% Loop through fields, if value is missing and no default, crash;
%% else if value is missing and has a default, set default;
%% otherwise apply constraints. If constraint fails, crash.
filter([], Map) ->
	Map;
filter([{Key, Constraints}|Tail], Map) ->
	filter_constraints(Tail, Map, Key, maps:get(Key, Map), Constraints);
filter([{Key, Constraints, Default}|Tail], Map) ->
	case maps:find(Key, Map) of
		{ok, Value} ->
			filter_constraints(Tail, Map, Key, Value, Constraints);
		error ->
			filter(Tail, Map#{Key => Default})
	end;
filter([Key|Tail], Map) ->
	true = maps:is_key(Key, Map),
	filter(Tail, Map).

filter_constraints(Tail, Map, Key, Value, Constraints) ->
	case cowboy_constraints:validate(Value, Constraints) of
		true ->
			filter(Tail, Map);
		{true, Value2} ->
			filter(Tail, Map#{Key => Value2})
	end.
