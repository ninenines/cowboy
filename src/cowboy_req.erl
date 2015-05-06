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
-export([new/14]).
-export([method/1]).
-export([version/1]).
-export([peer/1]).
-export([host/1]).
-export([host_info/1]).
-export([port/1]).
-export([path/1]).
-export([path_info/1]).
-export([qs/1]).
-export([parse_qs/1]).
-export([match_qs/2]).
-export([host_url/1]).
-export([url/1]).
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
-export([meta/2]).
-export([meta/3]).
-export([set_meta/3]).

%% Request body API.
-export([has_body/1]).
-export([body_length/1]).
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
-export([chunked_reply/2]).
-export([chunked_reply/3]).
-export([chunk/2]).
-export([upgrade_reply/3]).
-export([continue/1]).
-export([maybe_reply/2]).
-export([ensure_response/2]).

%% Private setter/getter API.
-export([append_buffer/2]).
-export([get/2]).
-export([set/2]).
-export([set_bindings/4]).
-export([lock/1]).
-export([to_list/1]).

-type cookie_opts() :: cow_cookie:cookie_opts().
-export_type([cookie_opts/0]).

-type content_decode_fun() :: fun((binary()) -> binary()).
-type transfer_decode_fun() :: fun((binary(), any())
	-> cow_http_te:decode_ret()).

-type body_opts() :: [{continue, boolean()}
	| {length, non_neg_integer()}
	| {read_length, non_neg_integer()}
	| {read_timeout, timeout()}
	| {transfer_decode, transfer_decode_fun(), any()}
	| {content_decode, content_decode_fun()}].
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

-spec new(any(), module(),
	undefined | {inet:ip_address(), inet:port_number()},
	binary(), binary(), binary(),
	cowboy:http_version(), cowboy:http_headers(), binary(),
	inet:port_number() | undefined, binary(), boolean(), boolean(),
	undefined | cowboy:onresponse_fun())
	-> req().
new(Socket, Transport, Peer, Method, Path, Query,
		Version, Headers, Host, Port, Buffer, CanKeepalive,
		Compress, OnResponse) ->
	Req = #http_req{socket=Socket, transport=Transport, pid=self(), peer=Peer,
		method=Method, path=Path, qs=Query, version=Version,
		headers=Headers, host=Host, port=Port, buffer=Buffer,
		resp_compress=Compress, onresponse=OnResponse},
	case CanKeepalive of
		false ->
			Req#http_req{connection=close};
		true ->
			case parse_header(<<"connection">>, Req) of
				undefined ->
					case Version of
						'HTTP/1.1' -> Req; %% keepalive
						'HTTP/1.0' -> Req#http_req{connection=close}
					end;
				Tokens ->
					Connection = connection_to_atom(Tokens),
					Req#http_req{connection=Connection}
			end
	end.

-spec method(req()) -> binary().
method(Req) ->
	Req#http_req.method.

-spec version(req()) -> cowboy:http_version().
version(Req) ->
	Req#http_req.version.

-spec peer(req()) -> {inet:ip_address(), inet:port_number()} | undefined.
peer(Req) ->
	Req#http_req.peer.

-spec host(req()) -> binary().
host(Req) ->
	Req#http_req.host.

-spec host_info(req()) -> cowboy_router:tokens() | undefined.
host_info(Req) ->
	Req#http_req.host_info.

-spec port(req()) -> inet:port_number().
port(Req) ->
	Req#http_req.port.

-spec path(req()) -> binary().
path(Req) ->
	Req#http_req.path.

-spec path_info(req()) -> cowboy_router:tokens() | undefined.
path_info(Req) ->
	Req#http_req.path_info.

-spec qs(req()) -> binary().
qs(Req) ->
	Req#http_req.qs.

-spec parse_qs(req()) -> [{binary(), binary() | true}].
parse_qs(#http_req{qs=Qs}) ->
	cow_qs:parse_qs(Qs).

-spec match_qs(cowboy:fields(), req()) -> map().
match_qs(Fields, Req) ->
	filter(Fields, kvlist_to_map(Fields, parse_qs(Req))).

%% The URL includes the scheme, host and port only.
-spec host_url(req()) -> undefined | binary().
host_url(#http_req{port=undefined}) ->
	undefined;
host_url(#http_req{transport=Transport, host=Host, port=Port}) ->
	TransportName = Transport:name(),
	Secure = case TransportName of
		ssl -> <<"s">>;
		_ -> <<>>
	end,
	PortBin = case {TransportName, Port} of
		{ssl, 443} -> <<>>;
		{tcp, 80} -> <<>>;
		_ -> << ":", (integer_to_binary(Port))/binary >>
	end,
	<< "http", Secure/binary, "://", Host/binary, PortBin/binary >>.

%% The URL includes the scheme, host, port, path and query string.
-spec url(req()) -> undefined | binary().
url(Req=#http_req{}) ->
	HostURL = host_url(Req),
	url(Req, HostURL).

url(_, undefined) ->
	undefined;
url(#http_req{path=Path, qs=QS}, HostURL) ->
	QS2 = case QS of
		<<>> -> <<>>;
		_ -> << "?", QS/binary >>
	end,
	<< HostURL/binary, Path/binary, QS2/binary >>.

-spec binding(atom(), req()) -> any() | undefined.
binding(Name, Req) ->
	binding(Name, Req, undefined).

-spec binding(atom(), req(), Default) -> any() | Default when Default::any().
binding(Name, Req, Default) when is_atom(Name) ->
	case lists:keyfind(Name, 1, Req#http_req.bindings) of
		{_, Value} -> Value;
		false -> Default
	end.

-spec bindings(req()) -> [{atom(), any()}].
bindings(Req) ->
	Req#http_req.bindings.

-spec header(binary(), req()) -> binary() | undefined.
header(Name, Req) ->
	header(Name, Req, undefined).

-spec header(binary(), req(), Default) -> binary() | Default when Default::any().
header(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.headers) of
		{Name, Value} -> Value;
		false -> Default
	end.

-spec headers(req()) -> cowboy:http_headers().
headers(Req) ->
	Req#http_req.headers.

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

-spec meta(atom(), req()) -> any() | undefined.
meta(Name, Req) ->
	meta(Name, Req, undefined).

-spec meta(atom(), req(), any()) -> any().
meta(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.meta) of
		{Name, Value} -> Value;
		false -> Default
	end.

-spec set_meta(atom(), any(), Req) -> Req when Req::req().
set_meta(Name, Value, Req=#http_req{meta=Meta}) ->
	Req#http_req{meta=lists:keystore(Name, 1, Meta, {Name, Value})}.

%% Request Body API.

-spec has_body(req()) -> boolean().
has_body(Req) ->
	case lists:keyfind(<<"content-length">>, 1, Req#http_req.headers) of
		{_, <<"0">>} ->
			false;
		{_, _} ->
			true;
		_ ->
			lists:keymember(<<"transfer-encoding">>, 1, Req#http_req.headers)
	end.

%% The length may not be known if Transfer-Encoding is not identity,
%% and the body hasn't been read at the time of the call.
-spec body_length(req()) -> undefined | non_neg_integer().
body_length(Req) ->
	case parse_header(<<"transfer-encoding">>, Req) of
		[<<"identity">>] ->
			parse_header(<<"content-length">>, Req);
		_ ->
			undefined
	end.

-spec body(Req) -> {ok, binary(), Req} | {more, binary(), Req} when Req::req().
body(Req) ->
	body(Req, []).

-spec body(Req, body_opts()) -> {ok, binary(), Req} | {more, binary(), Req} when Req::req().
body(Req=#http_req{body_state=waiting}, Opts) ->
	%% Send a 100 continue if needed (enabled by default).
	case lists:keyfind(continue, 1, Opts) of
		{_, false} ->
			ok;
		_ ->
			ExpectHeader = parse_header(<<"expect">>, Req),
			ok = case ExpectHeader of
				[<<"100-continue">>] -> continue(Req);
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
	case body(Req, Opts) of
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
part(Req=#http_req{multipart=undefined}, Opts) ->
	part(init_multipart(Req), Opts);
part(Req, Opts) ->
	{Data, Req2} = stream_multipart(Req, Opts),
	part(Data, Opts, Req2).

part(Buffer, Opts, Req=#http_req{multipart={Boundary, _}}) ->
	case cow_multipart:parse_headers(Buffer, Boundary) of
		more ->
			{Data, Req2} = stream_multipart(Req, Opts),
			part(<< Buffer/binary, Data/binary >>, Opts, Req2);
		{more, Buffer2} ->
			{Data, Req2} = stream_multipart(Req, Opts),
			part(<< Buffer2/binary, Data/binary >>, Opts, Req2);
		{ok, Headers, Rest} ->
			{ok, Headers, Req#http_req{multipart={Boundary, Rest}}};
		%% Ignore epilogue.
		{done, _} ->
			{done, Req#http_req{multipart=undefined}}
	end.

-spec part_body(Req)
	-> {ok, binary(), Req} | {more, binary(), Req}
	when Req::req().
part_body(Req) ->
	part_body(Req, []).

-spec part_body(Req, body_opts())
	-> {ok, binary(), Req} | {more, binary(), Req}
	when Req::req().
part_body(Req=#http_req{multipart=undefined}, Opts) ->
	part_body(init_multipart(Req), Opts);
part_body(Req, Opts) ->
	part_body(<<>>, Opts, Req, <<>>).

part_body(Buffer, Opts, Req=#http_req{multipart={Boundary, _}}, Acc) ->
	ChunkLen = case lists:keyfind(length, 1, Opts) of
		false -> 8000000;
		{_, ChunkLen0} -> ChunkLen0
	end,
	case byte_size(Acc) > ChunkLen of
		true ->
			{more, Acc, Req#http_req{multipart={Boundary, Buffer}}};
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
						Req2#http_req{multipart={Boundary, Rest}}}
			end
	end.

init_multipart(Req) ->
	{<<"multipart">>, _, Params} = parse_header(<<"content-type">>, Req),
	{_, Boundary} = lists:keyfind(<<"boundary">>, 1, Params),
	Req#http_req{multipart={Boundary, <<>>}}.

stream_multipart(Req=#http_req{body_state=BodyState, multipart={_, <<>>}}, Opts) ->
	true = BodyState =/= done,
	{_, Data, Req2} = body(Req, Opts),
	{Data, Req2};
stream_multipart(Req=#http_req{multipart={Boundary, Buffer}}, _) ->
	{Buffer, Req#http_req{multipart={Boundary, <<>>}}}.

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
	set_resp_header(<<"set-cookie">>, Cookie, Req).

-spec set_resp_header(binary(), iodata(), Req)
	-> Req when Req::req().
set_resp_header(Name, Value, Req=#http_req{resp_headers=RespHeaders}) ->
	Req#http_req{resp_headers=[{Name, Value}|RespHeaders]}.

-spec set_resp_body(iodata(), Req) -> Req when Req::req().
set_resp_body(Body, Req) ->
	Req#http_req{resp_body=Body}.

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
has_resp_header(Name, #http_req{resp_headers=RespHeaders}) ->
	lists:keymember(Name, 1, RespHeaders).

-spec has_resp_body(req()) -> boolean().
has_resp_body(#http_req{resp_body=RespBody}) when is_function(RespBody) ->
	true;
has_resp_body(#http_req{resp_body={chunked, _}}) ->
	true;
has_resp_body(#http_req{resp_body={Length, _}}) ->
	Length > 0;
has_resp_body(#http_req{resp_body=RespBody}) ->
	iolist_size(RespBody) > 0.

-spec delete_resp_header(binary(), Req)
	-> Req when Req::req().
delete_resp_header(Name, Req=#http_req{resp_headers=RespHeaders}) ->
	RespHeaders2 = lists:keydelete(Name, 1, RespHeaders),
	Req#http_req{resp_headers=RespHeaders2}.

-spec reply(cowboy:http_status(), Req) -> Req when Req::req().
reply(Status, Req=#http_req{resp_body=Body}) ->
	reply(Status, [], Body, Req).

-spec reply(cowboy:http_status(), cowboy:http_headers(), Req)
	-> Req when Req::req().
reply(Status, Headers, Req=#http_req{resp_body=Body}) ->
	reply(Status, Headers, Body, Req).

-spec reply(cowboy:http_status(), cowboy:http_headers(),
	iodata() | resp_body_fun() | {non_neg_integer(), resp_body_fun()}
	| {chunked, resp_chunked_fun()}, Req)
	-> Req when Req::req().
reply(Status, Headers, Body, Req=#http_req{
		socket=Socket, transport=Transport,
		version=Version, connection=Connection,
		method=Method, resp_compress=Compress,
		resp_state=RespState, resp_headers=RespHeaders})
		when RespState =:= waiting; RespState =:= waiting_stream ->
	HTTP11Headers = if
		Transport =/= cowboy_spdy, Version =:= 'HTTP/1.1' ->
			[{<<"connection">>, atom_to_connection(Connection)}];
		true ->
			[]
	end,
	Req3 = case Body of
		BodyFun when is_function(BodyFun) ->
			%% We stream the response body until we close the connection.
			RespConn = close,
			{RespType, Req2} = if
				Transport =:= cowboy_spdy ->
					response(Status, Headers, RespHeaders, [
						{<<"date">>, cowboy_clock:rfc1123()},
						{<<"server">>, <<"Cowboy">>}
					], stream, Req);
				true ->
					response(Status, Headers, RespHeaders, [
						{<<"connection">>, <<"close">>},
						{<<"date">>, cowboy_clock:rfc1123()},
						{<<"server">>, <<"Cowboy">>},
						{<<"transfer-encoding">>, <<"identity">>}
					], <<>>, Req)
			end,
			if	RespType =/= hook, Method =/= <<"HEAD">> ->
					BodyFun(Socket, Transport);
				true -> ok
			end,
			Req2#http_req{connection=RespConn};
		{chunked, BodyFun} ->
			%% We stream the response body in chunks.
			{RespType, Req2} = chunked_response(Status, Headers, Req),
			if	RespType =/= hook, Method =/= <<"HEAD">> ->
					ChunkFun = fun(IoData) -> chunk(IoData, Req2) end,
					BodyFun(ChunkFun),
					%% Send the last chunk if chunked encoding was used.
					if
						Version =:= 'HTTP/1.0'; RespState =:= waiting_stream ->
							Req2;
						true ->
							last_chunk(Req2)
					end;
				true -> Req2
			end;
		{ContentLength, BodyFun} ->
			%% We stream the response body for ContentLength bytes.
			RespConn = response_connection(Headers, Connection),
			{RespType, Req2} = response(Status, Headers, RespHeaders, [
					{<<"content-length">>, integer_to_list(ContentLength)},
					{<<"date">>, cowboy_clock:rfc1123()},
					{<<"server">>, <<"Cowboy">>}
				|HTTP11Headers], stream, Req),
			if	RespType =/= hook, Method =/= <<"HEAD">> ->
					BodyFun(Socket, Transport);
				true -> ok
			end,
			Req2#http_req{connection=RespConn};
		_ when Compress ->
			RespConn = response_connection(Headers, Connection),
			Req2 = reply_may_compress(Status, Headers, Body, Req,
				RespHeaders, HTTP11Headers, Method),
			Req2#http_req{connection=RespConn};
		_ ->
			RespConn = response_connection(Headers, Connection),
			Req2 = reply_no_compress(Status, Headers, Body, Req,
				RespHeaders, HTTP11Headers, Method, iolist_size(Body)),
			Req2#http_req{connection=RespConn}
	end,
	Req3#http_req{resp_state=done, resp_headers=[], resp_body= <<>>}.

reply_may_compress(Status, Headers, Body, Req,
		RespHeaders, HTTP11Headers, Method) ->
	BodySize = iolist_size(Body),
	try parse_header(<<"accept-encoding">>, Req) of
		Encodings ->
			CanGzip = (BodySize > 300)
				andalso (false =:= lists:keyfind(<<"content-encoding">>,
					1, Headers))
				andalso (false =:= lists:keyfind(<<"content-encoding">>,
					1, RespHeaders))
				andalso (false =:= lists:keyfind(<<"transfer-encoding">>,
					1, Headers))
				andalso (false =:= lists:keyfind(<<"transfer-encoding">>,
					1, RespHeaders))
				andalso (Encodings =/= undefined)
				andalso (false =/= lists:keyfind(<<"gzip">>, 1, Encodings)),
			case CanGzip of
				true ->
					GzBody = zlib:gzip(Body),
					{_, Req2} = response(Status, Headers, RespHeaders, [
							{<<"content-length">>, integer_to_list(byte_size(GzBody))},
							{<<"content-encoding">>, <<"gzip">>},
							{<<"date">>, cowboy_clock:rfc1123()},
							{<<"server">>, <<"Cowboy">>}
						|HTTP11Headers],
						case Method of <<"HEAD">> -> <<>>; _ -> GzBody end,
						Req),
					Req2;
				false ->
					reply_no_compress(Status, Headers, Body, Req,
						RespHeaders, HTTP11Headers, Method, BodySize)
			end
	catch _:_ ->
		reply_no_compress(Status, Headers, Body, Req,
			RespHeaders, HTTP11Headers, Method, BodySize)
	end.

reply_no_compress(Status, Headers, Body, Req,
		RespHeaders, HTTP11Headers, Method, BodySize) ->
	{_, Req2} = response(Status, Headers, RespHeaders, [
			{<<"content-length">>, integer_to_list(BodySize)},
			{<<"date">>, cowboy_clock:rfc1123()},
			{<<"server">>, <<"Cowboy">>}
		|HTTP11Headers],
		case Method of <<"HEAD">> -> <<>>; _ -> Body end,
		Req),
	Req2.

-spec chunked_reply(cowboy:http_status(), Req) -> Req when Req::req().
chunked_reply(Status, Req) ->
	chunked_reply(Status, [], Req).

-spec chunked_reply(cowboy:http_status(), cowboy:http_headers(), Req)
	-> Req when Req::req().
chunked_reply(Status, Headers, Req) ->
	{_, Req2} = chunked_response(Status, Headers, Req),
	Req2.

-spec chunk(iodata(), req()) -> ok.
chunk(_Data, #http_req{method= <<"HEAD">>}) ->
	ok;
chunk(Data, #http_req{socket=Socket, transport=cowboy_spdy,
		resp_state=chunks}) ->
	cowboy_spdy:stream_data(Socket, Data);
chunk(Data, #http_req{socket=Socket, transport=Transport,
		resp_state=stream}) ->
	ok = Transport:send(Socket, Data);
chunk(Data, #http_req{socket=Socket, transport=Transport,
		resp_state=chunks}) ->
	case iolist_size(Data) of
		0 -> ok;
		Size -> Transport:send(Socket, [integer_to_list(Size, 16),
			<<"\r\n">>, Data, <<"\r\n">>])
	end.

%% If ever made public, need to send nothing if HEAD.
-spec last_chunk(Req) -> Req when Req::req().
last_chunk(Req=#http_req{socket=Socket, transport=cowboy_spdy}) ->
	_ = cowboy_spdy:stream_close(Socket),
	Req#http_req{resp_state=done};
last_chunk(Req=#http_req{socket=Socket, transport=Transport}) ->
	_ = Transport:send(Socket, <<"0\r\n\r\n">>),
	Req#http_req{resp_state=done}.

-spec upgrade_reply(cowboy:http_status(), cowboy:http_headers(), Req)
	-> Req when Req::req().
upgrade_reply(Status, Headers, Req=#http_req{transport=Transport,
		resp_state=waiting, resp_headers=RespHeaders})
		when Transport =/= cowboy_spdy ->
	{_, Req2} = response(Status, Headers, RespHeaders, [
		{<<"connection">>, <<"Upgrade">>}
	], <<>>, Req),
	Req2#http_req{resp_state=done, resp_headers=[], resp_body= <<>>}.

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

%% Private setter/getter API.

-spec append_buffer(binary(), Req) -> Req when Req::req().
append_buffer(Suffix, Req=#http_req{buffer=Buffer}) ->
	Req#http_req{buffer= << Buffer/binary, Suffix/binary >>}.

-spec get(atom(), req()) -> any(); ([atom()], req()) -> any().
get(List, Req) when is_list(List) ->
	[g(Atom, Req) || Atom <- List];
get(Atom, Req) when is_atom(Atom) ->
	g(Atom, Req).

g(bindings, #http_req{bindings=Ret}) -> Ret;
g(body_state, #http_req{body_state=Ret}) -> Ret;
g(buffer, #http_req{buffer=Ret}) -> Ret;
g(connection, #http_req{connection=Ret}) -> Ret;
g(headers, #http_req{headers=Ret}) -> Ret;
g(host, #http_req{host=Ret}) -> Ret;
g(host_info, #http_req{host_info=Ret}) -> Ret;
g(meta, #http_req{meta=Ret}) -> Ret;
g(method, #http_req{method=Ret}) -> Ret;
g(multipart, #http_req{multipart=Ret}) -> Ret;
g(onresponse, #http_req{onresponse=Ret}) -> Ret;
g(path, #http_req{path=Ret}) -> Ret;
g(path_info, #http_req{path_info=Ret}) -> Ret;
g(peer, #http_req{peer=Ret}) -> Ret;
g(pid, #http_req{pid=Ret}) -> Ret;
g(port, #http_req{port=Ret}) -> Ret;
g(qs, #http_req{qs=Ret}) -> Ret;
g(resp_body, #http_req{resp_body=Ret}) -> Ret;
g(resp_compress, #http_req{resp_compress=Ret}) -> Ret;
g(resp_headers, #http_req{resp_headers=Ret}) -> Ret;
g(resp_state, #http_req{resp_state=Ret}) -> Ret;
g(socket, #http_req{socket=Ret}) -> Ret;
g(transport, #http_req{transport=Ret}) -> Ret;
g(version, #http_req{version=Ret}) -> Ret.

-spec set([{atom(), any()}], Req) -> Req when Req::req().
set([], Req) -> Req;
set([{bindings, Val}|Tail], Req) -> set(Tail, Req#http_req{bindings=Val});
set([{body_state, Val}|Tail], Req) -> set(Tail, Req#http_req{body_state=Val});
set([{buffer, Val}|Tail], Req) -> set(Tail, Req#http_req{buffer=Val});
set([{connection, Val}|Tail], Req) -> set(Tail, Req#http_req{connection=Val});
set([{headers, Val}|Tail], Req) -> set(Tail, Req#http_req{headers=Val});
set([{host, Val}|Tail], Req) -> set(Tail, Req#http_req{host=Val});
set([{host_info, Val}|Tail], Req) -> set(Tail, Req#http_req{host_info=Val});
set([{meta, Val}|Tail], Req) -> set(Tail, Req#http_req{meta=Val});
set([{method, Val}|Tail], Req) -> set(Tail, Req#http_req{method=Val});
set([{multipart, Val}|Tail], Req) -> set(Tail, Req#http_req{multipart=Val});
set([{onresponse, Val}|Tail], Req) -> set(Tail, Req#http_req{onresponse=Val});
set([{path, Val}|Tail], Req) -> set(Tail, Req#http_req{path=Val});
set([{path_info, Val}|Tail], Req) -> set(Tail, Req#http_req{path_info=Val});
set([{peer, Val}|Tail], Req) -> set(Tail, Req#http_req{peer=Val});
set([{pid, Val}|Tail], Req) -> set(Tail, Req#http_req{pid=Val});
set([{port, Val}|Tail], Req) -> set(Tail, Req#http_req{port=Val});
set([{qs, Val}|Tail], Req) -> set(Tail, Req#http_req{qs=Val});
set([{resp_body, Val}|Tail], Req) -> set(Tail, Req#http_req{resp_body=Val});
set([{resp_headers, Val}|Tail], Req) -> set(Tail, Req#http_req{resp_headers=Val});
set([{resp_state, Val}|Tail], Req) -> set(Tail, Req#http_req{resp_state=Val});
set([{socket, Val}|Tail], Req) -> set(Tail, Req#http_req{socket=Val});
set([{transport, Val}|Tail], Req) -> set(Tail, Req#http_req{transport=Val});
set([{version, Val}|Tail], Req) -> set(Tail, Req#http_req{version=Val}).

-spec set_bindings(cowboy_router:tokens(), cowboy_router:tokens(),
	cowboy_router:bindings(), Req) -> Req when Req::req().
set_bindings(HostInfo, PathInfo, Bindings, Req) ->
	Req#http_req{host_info=HostInfo, path_info=PathInfo,
		bindings=Bindings}.

-spec lock(Req) -> Req when Req::req().
lock(Req) ->
	Req#http_req{resp_state=locked}.

-spec to_list(req()) -> [{atom(), any()}].
to_list(Req) ->
	lists:zip(record_info(fields, http_req), tl(tuple_to_list(Req))).

%% Internal.

-spec chunked_response(cowboy:http_status(), cowboy:http_headers(), Req) ->
	{normal | hook, Req} when Req::req().
chunked_response(Status, Headers, Req=#http_req{
		transport=cowboy_spdy, resp_state=waiting,
		resp_headers=RespHeaders}) ->
	{RespType, Req2} = response(Status, Headers, RespHeaders, [
		{<<"date">>, cowboy_clock:rfc1123()},
		{<<"server">>, <<"Cowboy">>}
	], stream, Req),
	{RespType, Req2#http_req{resp_state=chunks,
		resp_headers=[], resp_body= <<>>}};
chunked_response(Status, Headers, Req=#http_req{
		version=Version, connection=Connection,
		resp_state=RespState, resp_headers=RespHeaders})
		when RespState =:= waiting; RespState =:= waiting_stream ->
	RespConn = response_connection(Headers, Connection),
	HTTP11Headers = if
		Version =:= 'HTTP/1.0' -> [];
		true ->
			MaybeTE = if
				RespState =:= waiting_stream -> [];
				true -> [{<<"transfer-encoding">>, <<"chunked">>}]
			end,
			[{<<"connection">>, atom_to_connection(Connection)}|MaybeTE]
	end,
	RespState2 = if
		Version =:= 'HTTP/1.1', RespState =:= 'waiting' -> chunks;
		true -> stream
	end,
	{RespType, Req2} = response(Status, Headers, RespHeaders, [
		{<<"date">>, cowboy_clock:rfc1123()},
		{<<"server">>, <<"Cowboy">>}
	|HTTP11Headers], <<>>, Req),
	{RespType, Req2#http_req{connection=RespConn, resp_state=RespState2,
			resp_headers=[], resp_body= <<>>}}.

-spec response(cowboy:http_status(), cowboy:http_headers(),
	cowboy:http_headers(), cowboy:http_headers(), stream | iodata(), Req)
	-> {normal | hook, Req} when Req::req().
response(Status, Headers, RespHeaders, DefaultHeaders, Body, Req=#http_req{
		socket=Socket, transport=Transport, version=Version,
		pid=ReqPid, onresponse=OnResponse}) ->
	FullHeaders = case OnResponse of
		already_called -> Headers;
		_ -> response_merge_headers(Headers, RespHeaders, DefaultHeaders)
	end,
	Body2 = case Body of stream -> <<>>; _ -> Body end,
	{Status2, FullHeaders2, Req2} = case OnResponse of
		already_called -> {Status, FullHeaders, Req};
		undefined -> {Status, FullHeaders, Req};
		OnResponse ->
			case OnResponse(Status, FullHeaders, Body2,
					%% Don't call 'onresponse' from the hook itself.
					Req#http_req{resp_headers=[], resp_body= <<>>,
						onresponse=already_called}) of
				StHdReq = {_, _, _} ->
					StHdReq;
				Req1 ->
					{Status, FullHeaders, Req1}
			end
	end,
	ReplyType = case Req2#http_req.resp_state of
		waiting when Transport =:= cowboy_spdy, Body =:= stream ->
			cowboy_spdy:stream_reply(Socket, status(Status2), FullHeaders2),
			ReqPid ! {?MODULE, resp_sent},
			normal;
		waiting when Transport =:= cowboy_spdy ->
			cowboy_spdy:reply(Socket, status(Status2), FullHeaders2, Body),
			ReqPid ! {?MODULE, resp_sent},
			normal;
		RespState when RespState =:= waiting; RespState =:= waiting_stream ->
			HTTPVer = atom_to_binary(Version, latin1),
			StatusLine = << HTTPVer/binary, " ",
				(status(Status2))/binary, "\r\n" >>,
			HeaderLines = [[Key, <<": ">>, Value, <<"\r\n">>]
				|| {Key, Value} <- FullHeaders2],
			ok = Transport:send(Socket, [StatusLine, HeaderLines, <<"\r\n">>, Body2]),
			ReqPid ! {?MODULE, resp_sent},
			normal;
		_ ->
			hook
	end,
	{ReplyType, Req2}.

-spec response_connection(cowboy:http_headers(), keepalive | close)
	-> keepalive | close.
response_connection([], Connection) ->
	Connection;
response_connection([{Name, Value}|Tail], Connection) ->
	case Name of
		<<"connection">> ->
			Tokens = cow_http_hd:parse_connection(Value),
			connection_to_atom(Tokens);
		_ ->
			response_connection(Tail, Connection)
	end.

-spec response_merge_headers(cowboy:http_headers(), cowboy:http_headers(),
	cowboy:http_headers()) -> cowboy:http_headers().
response_merge_headers(Headers, RespHeaders, DefaultHeaders) ->
	Headers2 = [{Key, Value} || {Key, Value} <- Headers],
	merge_headers(
		merge_headers(Headers2, RespHeaders),
		DefaultHeaders).

-spec merge_headers(cowboy:http_headers(), cowboy:http_headers())
	-> cowboy:http_headers().

%% Merge headers by prepending the tuples in the second list to the
%% first list. It also handles Set-Cookie properly, which supports
%% duplicated entries. Notice that, while the RFC2109 does allow more
%% than one cookie to be set per Set-Cookie header, we are following
%% the implementation of common web servers and applications which
%% return many distinct headers per each Set-Cookie entry to avoid
%% issues with clients/browser which may not support it.
merge_headers(Headers, []) ->
	Headers;
merge_headers(Headers, [{<<"set-cookie">>, Value}|Tail]) ->
	merge_headers([{<<"set-cookie">>, Value}|Headers], Tail);
merge_headers(Headers, [{Name, Value}|Tail]) ->
	Headers2 = case lists:keymember(Name, 1, Headers) of
		true -> Headers;
		false -> [{Name, Value}|Headers]
	end,
	merge_headers(Headers2, Tail).

-spec atom_to_connection(keepalive) -> <<_:80>>;
						(close) -> <<_:40>>.
atom_to_connection(keepalive) ->
	<<"keep-alive">>;
atom_to_connection(close) ->
	<<"close">>.

%% We don't match on "keep-alive" since it is the default value.
-spec connection_to_atom([binary()]) -> keepalive | close.
connection_to_atom([]) ->
	keepalive;
connection_to_atom([<<"close">>|_]) ->
	close;
connection_to_atom([_|Tail]) ->
	connection_to_atom(Tail).

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
								maps:put(Atom, [Value|MapValue], Map));
						{ok, MapValue} ->
							kvlist_to_map(Keys, Tail,
								maps:put(Atom, [Value, MapValue], Map));
						error ->
							kvlist_to_map(Keys, Tail,
								maps:put(Atom, Value, Map))
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
			filter(Tail, maps:put(Key, Default, Map))
	end;
filter([Key|Tail], Map) ->
	true = maps:is_key(Key, Map),
	filter(Tail, Map).

filter_constraints(Tail, Map, Key, Value, Constraints) ->
	case cowboy_constraints:validate(Value, Constraints) of
		true ->
			filter(Tail, Map);
		{true, Value2} ->
			filter(Tail, maps:put(Key, Value2, Map))
	end.

%% Tests.

-ifdef(TEST).
url_test() ->
	undefined =
		url(#http_req{transport=ranch_tcp, host= <<>>, port= undefined,
			path= <<>>, qs= <<>>, pid=self()}),
	<<"http://localhost/path">> =
		url(#http_req{transport=ranch_tcp, host= <<"localhost">>, port=80,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	<<"http://localhost:443/path">> =
		url(#http_req{transport=ranch_tcp, host= <<"localhost">>, port=443,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	<<"http://localhost:8080/path">> =
		url(#http_req{transport=ranch_tcp, host= <<"localhost">>, port=8080,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	<<"http://localhost:8080/path?dummy=2785">> =
		url(#http_req{transport=ranch_tcp, host= <<"localhost">>, port=8080,
			path= <<"/path">>, qs= <<"dummy=2785">>, pid=self()}),
	<<"https://localhost/path">> =
		url(#http_req{transport=ranch_ssl, host= <<"localhost">>, port=443,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	<<"https://localhost:8443/path">> =
		url(#http_req{transport=ranch_ssl, host= <<"localhost">>, port=8443,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	<<"https://localhost:8443/path?dummy=2785">> =
		url(#http_req{transport=ranch_ssl, host= <<"localhost">>, port=8443,
			path= <<"/path">>, qs= <<"dummy=2785">>, pid=self()}),
	ok.

connection_to_atom_test_() ->
	Tests = [
		{[<<"close">>], close},
		{[<<"keep-alive">>], keepalive},
		{[<<"keep-alive">>, <<"upgrade">>], keepalive}
	],
	[{lists:flatten(io_lib:format("~p", [T])),
		fun() -> R = connection_to_atom(T) end} || {T, R} <- Tests].

merge_headers_test_() ->
	Tests = [
		{[{<<"content-length">>,<<"13">>},{<<"server">>,<<"Cowboy">>}],
		 [{<<"set-cookie">>,<<"foo=bar">>},{<<"content-length">>,<<"11">>}],
		 [{<<"set-cookie">>,<<"foo=bar">>},
		  {<<"content-length">>,<<"13">>},
		  {<<"server">>,<<"Cowboy">>}]},
		{[{<<"content-length">>,<<"13">>},{<<"server">>,<<"Cowboy">>}],
		 [{<<"set-cookie">>,<<"foo=bar">>},{<<"set-cookie">>,<<"bar=baz">>}],
		 [{<<"set-cookie">>,<<"bar=baz">>},
		  {<<"set-cookie">>,<<"foo=bar">>},
		  {<<"content-length">>,<<"13">>},
		  {<<"server">>,<<"Cowboy">>}]}
	],
	[fun() -> Res = merge_headers(L,R) end || {L, R, Res} <- Tests].
-endif.
