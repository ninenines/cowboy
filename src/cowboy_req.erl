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

%% @doc HTTP request manipulation API.
%%
%% The functions in this module try to follow this pattern for their
%% return types:
%% <dl>
%% <dt>access:</dt>
%%   <dd><em>{Value, Req}</em></dd>
%% <dt>action:</dt>
%%   <dd><em>{Result, Req} | {Result, Value, Req} | {error, atom()}</em></dd>
%% <dt>modification:</dt>
%%   <dd><em>Req</em></dd>
%% <dt>question (<em>has_*</em> or <em>is_*</em>):</dt>
%%   <dd><em>boolean()</em></dd>
%% </dl>
%%
%% Exceptions include <em>chunk/2</em> which always returns <em>'ok'</em>,
%% <em>to_list/1</em> which returns a list of key/values,
%% and <em>transport/1</em> which returns <em>{ok, Transport, Socket}</em>.
%%
%% Also note that all body reading functions perform actions, as Cowboy
%% doesn't read the request body until they are called.
%%
%% Whenever <em>Req</em> is returned, it should always be kept in place of
%% the one given as argument in your function call, because it keeps
%% track of the request and response state. Doing so allows Cowboy to do
%% some lazy evaluation and cache results when possible.
-module(cowboy_req).

%% Request API.
-export([new/9]).
-export([method/1]).
-export([version/1]).
-export([peer/1]).
-export([peer_addr/1]).
-export([host/1]).
-export([host_info/1]).
-export([port/1]).
-export([path/1]).
-export([path_info/1]).
-export([qs/1]).
-export([qs_val/2]).
-export([qs_val/3]).
-export([qs_vals/1]).
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
-export([cookie/2]).
-export([cookie/3]).
-export([cookies/1]).
-export([meta/2]).
-export([meta/3]).
-export([set_meta/3]).

%% Request body API.
-export([has_body/1]).
-export([body_length/1]).
-export([init_stream/4]).
-export([stream_body/1]).
-export([skip_body/1]).
-export([body/1]).
-export([body/2]).
-export([body_qs/1]).
-export([multipart_data/1]).
-export([multipart_skip/1]).

%% Response API.
-export([set_resp_cookie/4]).
-export([set_resp_header/3]).
-export([set_resp_body/2]).
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
-export([ensure_response/2]).

%% Private setter/getter API.
-export([set_host/4]).
-export([set_connection/2]).
-export([add_header/3]).
-export([set_buffer/2]).
-export([set_bindings/4]).
-export([get_resp_state/1]).
-export([get_buffer/1]).
-export([get_connection/1]).

%% Misc API.
-export([compact/1]).
-export([lock/1]).
-export([to_list/1]).
-export([transport/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(http_req, {
	%% Transport.
	socket = undefined :: undefined | inet:socket(),
	transport = undefined :: undefined | module(),
	connection = keepalive :: keepalive | close,

	%% Request.
	pid = undefined :: pid(),
	method = 'GET' :: cowboy_http:method(),
	version = {1, 1} :: cowboy_http:version(),
	peer = undefined :: undefined | {inet:ip_address(), inet:port_number()},
	host = undefined :: undefined | binary(),
	host_info = undefined :: undefined | cowboy_dispatcher:tokens(),
	port = undefined :: undefined | inet:port_number(),
	path = undefined :: binary(),
	path_info = undefined :: undefined | cowboy_dispatcher:tokens(),
	qs = undefined :: binary(),
	qs_vals = undefined :: undefined | list({binary(), binary() | true}),
	bindings = undefined :: undefined | cowboy_dispatcher:bindings(),
	headers = [] :: cowboy_http:headers(),
	p_headers = [] :: [any()], %% @todo Improve those specs.
	cookies = undefined :: undefined | [{binary(), binary()}],
	meta = [] :: [{atom(), any()}],

	%% Request body.
	body_state = waiting :: waiting | done | {stream, fun(), any(), fun()},
	multipart = undefined :: undefined | {non_neg_integer(), fun()},
	buffer = <<>> :: binary(),

	%% Response.
	resp_state = waiting :: locked | waiting | chunks | done,
	resp_headers = [] :: cowboy_http:headers(),
	resp_body = <<>> :: iodata()
		| {non_neg_integer(), fun(() -> {sent, non_neg_integer()})},

	%% Functions.
	onresponse = undefined :: undefined | cowboy_protocol:onresponse_fun(),
	urldecode :: {fun((binary(), T) -> binary()), T}
}).

-opaque req() :: #http_req{}.
-export_type([req/0]).

%% Request API.

%% @doc Create a new HTTP Req object.
%%
%% This function takes care of setting the owner's pid to self().
%% @private
-spec new(inet:socket(), module(), keepalive | close,
	cowboy_http:method(), cowboy_http:version(), binary(), binary(),
	undefined | fun(), undefined | {fun(), atom()})
	-> req().
new(Socket, Transport, Connection, Method, Version, Path, Qs,
		OnResponse, URLDecode) ->
	#http_req{socket=Socket, transport=Transport, connection=Connection,
		pid=self(), method=Method, version=Version, path=Path, qs=Qs,
		onresponse=OnResponse, urldecode=URLDecode}.

%% @doc Return the HTTP method of the request.
-spec method(Req) -> {cowboy_http:method(), Req} when Req::req().
method(Req) ->
	{Req#http_req.method, Req}.

%% @doc Return the HTTP version used for the request.
-spec version(Req) -> {cowboy_http:version(), Req} when Req::req().
version(Req) ->
	{Req#http_req.version, Req}.

%% @doc Return the peer address and port number of the remote host.
-spec peer(Req)
	-> {{inet:ip_address(), inet:port_number()}, Req} when Req::req().
peer(Req=#http_req{socket=Socket, transport=Transport, peer=undefined}) ->
	{ok, Peer} = Transport:peername(Socket),
	{Peer, Req#http_req{peer=Peer}};
peer(Req) ->
	{Req#http_req.peer, Req}.

%% @doc Returns the peer address calculated from headers.
-spec peer_addr(Req) -> {inet:ip_address(), Req} when Req::req().
peer_addr(Req = #http_req{}) ->
	{RealIp, Req1} = header(<<"X-Real-Ip">>, Req),
	{ForwardedForRaw, Req2} = header(<<"X-Forwarded-For">>, Req1),
	{{PeerIp, _PeerPort}, Req3} = peer(Req2),
	ForwardedFor = case ForwardedForRaw of
		undefined ->
			undefined;
		ForwardedForRaw ->
			case re:run(ForwardedForRaw, "^(?<first_ip>[^\\,]+)",
					[{capture, [first_ip], binary}]) of
				{match, [FirstIp]} -> FirstIp;
				_Any -> undefined
			end
	end,
	{ok, PeerAddr} = if
		is_binary(RealIp) -> inet_parse:address(binary_to_list(RealIp));
		is_binary(ForwardedFor) -> inet_parse:address(binary_to_list(ForwardedFor));
		true -> {ok, PeerIp}
	end,
	{PeerAddr, Req3}.

%% @doc Return the host binary string.
-spec host(Req) -> {binary(), Req} when Req::req().
host(Req) ->
	{Req#http_req.host, Req}.

%% @doc Return the extra host information obtained from partially matching
%% the hostname using <em>'...'</em>.
-spec host_info(Req)
	-> {cowboy_dispatcher:tokens() | undefined, Req} when Req::req().
host_info(Req) ->
	{Req#http_req.host_info, Req}.

%% @doc Return the port used for this request.
-spec port(Req) -> {inet:port_number(), Req} when Req::req().
port(Req) ->
	{Req#http_req.port, Req}.

%% @doc Return the path binary string.
-spec path(Req) -> {binary(), Req} when Req::req().
path(Req) ->
	{Req#http_req.path, Req}.

%% @doc Return the extra path information obtained from partially matching
%% the patch using <em>'...'</em>.
-spec path_info(Req)
	-> {cowboy_dispatcher:tokens() | undefined, Req} when Req::req().
path_info(Req) ->
	{Req#http_req.path_info, Req}.

%% @doc Return the raw query string directly taken from the request.
-spec qs(Req) -> {binary(), Req} when Req::req().
qs(Req) ->
	{Req#http_req.qs, Req}.

%% @equiv qs_val(Name, Req, undefined)
-spec qs_val(binary(), Req)
	-> {binary() | true | undefined, Req} when Req::req().
qs_val(Name, Req) when is_binary(Name) ->
	qs_val(Name, Req, undefined).

%% @doc Return the query string value for the given key, or a default if
%% missing.
-spec qs_val(binary(), Req, Default)
	-> {binary() | true | Default, Req} when Req::req(), Default::any().
qs_val(Name, Req=#http_req{qs=RawQs, qs_vals=undefined,
		urldecode={URLDecFun, URLDecArg}}, Default) when is_binary(Name) ->
	QsVals = cowboy_http:x_www_form_urlencoded(
		RawQs, fun(Bin) -> URLDecFun(Bin, URLDecArg) end),
	qs_val(Name, Req#http_req{qs_vals=QsVals}, Default);
qs_val(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.qs_vals) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of query string values.
-spec qs_vals(Req) -> {list({binary(), binary() | true}), Req} when Req::req().
qs_vals(Req=#http_req{qs=RawQs, qs_vals=undefined,
		urldecode={URLDecFun, URLDecArg}}) ->
	QsVals = cowboy_http:x_www_form_urlencoded(
		RawQs, fun(Bin) -> URLDecFun(Bin, URLDecArg) end),
	qs_vals(Req#http_req{qs_vals=QsVals});
qs_vals(Req=#http_req{qs_vals=QsVals}) ->
	{QsVals, Req}.

%% @doc Return the request URL as a binary without the path and query string.
%%
%% The URL includes the scheme, host and port only.
%% @see cowboy_req:url/1
-spec host_url(Req) -> {binary(), Req} when Req::req().
host_url(Req=#http_req{transport=Transport, host=Host, port=Port}) ->
	TransportName = Transport:name(),
	Secure = case TransportName of
		ssl -> <<"s">>;
		_ -> <<>>
	end,
	PortBin = case {TransportName, Port} of
		{ssl, 443} -> <<>>;
		{tcp, 80} -> <<>>;
		_ -> << ":", (list_to_binary(integer_to_list(Port)))/binary >>
	end,
	{<< "http", Secure/binary, "://", Host/binary, PortBin/binary >>, Req}.

%% @doc Return the full request URL as a binary.
%%
%% The URL includes the scheme, host, port, path and query string.
-spec url(Req) -> {binary(), Req} when Req::req().
url(Req=#http_req{path=Path, qs=QS}) ->
	{HostURL, Req2} = host_url(Req),
	QS2 = case QS of
		<<>> -> <<>>;
		_ -> << "?", QS/binary >>
	end,
	{<< HostURL/binary, Path/binary, QS2/binary >>, Req2}.

%% @equiv binding(Name, Req, undefined)
-spec binding(atom(), Req) -> {binary() | undefined, Req} when Req::req().
binding(Name, Req) when is_atom(Name) ->
	binding(Name, Req, undefined).

%% @doc Return the binding value for the given key obtained when matching
%% the host and path against the dispatch list, or a default if missing.
-spec binding(atom(), Req, Default)
	-> {binary() | Default, Req} when Req::req(), Default::any().
binding(Name, Req, Default) when is_atom(Name) ->
	case lists:keyfind(Name, 1, Req#http_req.bindings) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of binding values.
-spec bindings(Req) -> {list({atom(), binary()}), Req} when Req::req().
bindings(Req) ->
	{Req#http_req.bindings, Req}.

%% @equiv header(Name, Req, undefined)
-spec header(atom() | binary(), Req)
	-> {binary() | undefined, Req} when Req::req().
header(Name, Req) when is_atom(Name) orelse is_binary(Name) ->
	header(Name, Req, undefined).

%% @doc Return the header value for the given key, or a default if missing.
-spec header(atom() | binary(), Req, Default)
	-> {binary() | Default, Req} when Req::req(), Default::any().
header(Name, Req, Default) when is_atom(Name) orelse is_binary(Name) ->
	case lists:keyfind(Name, 1, Req#http_req.headers) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of headers.
-spec headers(Req) -> {cowboy_http:headers(), Req} when Req::req().
headers(Req) ->
	{Req#http_req.headers, Req}.

%% @doc Semantically parse headers.
%%
%% When the value isn't found, a proper default value for the type
%% returned is used as a return value.
%% @see parse_header/3
-spec parse_header(cowboy_http:header(), Req)
	-> {ok, any(), Req} | {undefined, binary(), Req}
	| {error, badarg} when Req::req().
parse_header(Name, Req=#http_req{p_headers=PHeaders}) ->
	case lists:keyfind(Name, 1, PHeaders) of
		false -> parse_header(Name, Req, parse_header_default(Name));
		{Name, Value} -> {ok, Value, Req}
	end.

%% @doc Default values for semantic header parsing.
-spec parse_header_default(cowboy_http:header()) -> any().
parse_header_default('Connection') -> [];
parse_header_default('Transfer-Encoding') -> [<<"identity">>];
parse_header_default(_Name) -> undefined.

%% @doc Semantically parse headers.
%%
%% When the header is unknown, the value is returned directly without parsing.
-spec parse_header(cowboy_http:header(), Req, any())
	-> {ok, any(), Req} | {undefined, binary(), Req}
	| {error, badarg} when Req::req().
parse_header(Name, Req, Default) when Name =:= 'Accept' ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:list(Value, fun cowboy_http:media_range/2)
		end);
parse_header(Name, Req, Default) when Name =:= 'Accept-Charset' ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:nonempty_list(Value, fun cowboy_http:conneg/2)
		end);
parse_header(Name, Req, Default) when Name =:= 'Accept-Encoding' ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:list(Value, fun cowboy_http:conneg/2)
		end);
parse_header(Name, Req, Default) when Name =:= 'Accept-Language' ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:nonempty_list(Value, fun cowboy_http:language_range/2)
		end);
parse_header(Name, Req, Default) when Name =:= 'Connection' ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:nonempty_list(Value, fun cowboy_http:token_ci/2)
		end);
parse_header(Name, Req, Default) when Name =:= 'Content-Length' ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:digits(Value)
		end);
parse_header(Name, Req, Default) when Name =:= 'Content-Type' ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:content_type(Value)
		end);
parse_header(Name, Req, Default) when Name =:= <<"Expect">> ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:nonempty_list(Value, fun cowboy_http:expectation/2)
		end);
parse_header(Name, Req, Default)
		when Name =:= 'If-Match'; Name =:= 'If-None-Match' ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:entity_tag_match(Value)
		end);
parse_header(Name, Req, Default)
		when Name =:= 'If-Modified-Since'; Name =:= 'If-Unmodified-Since' ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:http_date(Value)
		end);
%% @todo Extension parameters.
parse_header(Name, Req, Default) when Name =:= 'Transfer-Encoding' ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:nonempty_list(Value, fun cowboy_http:token_ci/2)
		end);
parse_header(Name, Req, Default) when Name =:= 'Upgrade' ->
	parse_header(Name, Req, Default,
		fun (Value) ->
			cowboy_http:nonempty_list(Value, fun cowboy_http:token_ci/2)
		end);
parse_header(Name, Req, Default) ->
	{Value, Req2} = header(Name, Req, Default),
	{undefined, Value, Req2}.

parse_header(Name, Req=#http_req{p_headers=PHeaders}, Default, Fun) ->
	case header(Name, Req) of
		{undefined, Req2} ->
			{ok, Default, Req2#http_req{p_headers=[{Name, Default}|PHeaders]}};
		{Value, Req2} ->
			case Fun(Value) of
				{error, badarg} ->
					{error, badarg};
				P ->
					{ok, P, Req2#http_req{p_headers=[{Name, P}|PHeaders]}}
			end
	end.

%% @equiv cookie(Name, Req, undefined)
-spec cookie(binary(), Req)
	-> {binary() | true | undefined, Req} when Req::req().
cookie(Name, Req) when is_binary(Name) ->
	cookie(Name, Req, undefined).

%% @doc Return the cookie value for the given key, or a default if
%% missing.
-spec cookie(binary(), Req, Default)
	-> {binary() | true | Default, Req} when Req::req(), Default::any().
cookie(Name, Req=#http_req{cookies=undefined}, Default) when is_binary(Name) ->
	case header('Cookie', Req) of
		{undefined, Req2} ->
			{Default, Req2#http_req{cookies=[]}};
		{RawCookie, Req2} ->
			Cookies = cowboy_cookies:parse_cookie(RawCookie),
			cookie(Name, Req2#http_req{cookies=Cookies}, Default)
	end;
cookie(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.cookies) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of cookie values.
-spec cookies(Req) -> {list({binary(), binary() | true}), Req} when Req::req().
cookies(Req=#http_req{cookies=undefined}) ->
	case header('Cookie', Req) of
		{undefined, Req2} ->
			{[], Req2#http_req{cookies=[]}};
		{RawCookie, Req2} ->
			Cookies = cowboy_cookies:parse_cookie(RawCookie),
			cookies(Req2#http_req{cookies=Cookies})
	end;
cookies(Req=#http_req{cookies=Cookies}) ->
	{Cookies, Req}.

%% @equiv meta(Name, Req, undefined)
-spec meta(atom(), Req) -> {any() | undefined, Req} when Req::req().
meta(Name, Req) ->
	meta(Name, Req, undefined).

%% @doc Return metadata information about the request.
%%
%% Metadata information varies from one protocol to another. Websockets
%% would define the protocol version here, while REST would use it to
%% indicate which media type, language and charset were retained.
-spec meta(atom(), Req, any()) -> {any(), Req} when Req::req().
meta(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.meta) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Set metadata information.
%%
%% You can use this function to attach information about the request.
%%
%% If the value already exists it will be overwritten.
-spec set_meta(atom(), any(), Req) -> Req when Req::req().
set_meta(Name, Value, Req=#http_req{meta=Meta}) ->
	Req#http_req{meta=[{Name, Value}|lists:keydelete(Name, 1, Meta)]}.

%% Request Body API.

%% @doc Return whether the request message has a body.
-spec has_body(Req) -> {boolean(), Req} when Req::req().
has_body(Req) ->
	Has = lists:keymember('Content-Length', 1, Req#http_req.headers) orelse
		lists:keymember('Transfer-Encoding', 1, Req#http_req.headers),
	{Has, Req}.

%% @doc Return the request message body length, if known.
%%
%% The length may not be known if Transfer-Encoding is not identity,
%% and the body hasn't been read at the time of the call.
-spec body_length(Req) -> {undefined | non_neg_integer(), Req} when Req::req().
body_length(Req) ->
	case lists:keymember('Transfer-Encoding', 1, Req#http_req.headers) of
		true ->
			{undefined, Req};
		false ->
			{ok, Length, Req2} = parse_header('Content-Length', Req, 0),
			{Length, Req2}
	end.

%% @doc Initialize body streaming and set custom decoding functions.
%%
%% Calling this function is optional. It should only be used if you
%% need to override the default behavior of Cowboy. Otherwise you
%% should call stream_body/1 directly.
%%
%% Two decodings happen. First a decoding function is applied to the
%% transferred data, and then another is applied to the actual content.
%%
%% Transfer encoding is generally used for chunked bodies. The decoding
%% function uses a state to keep track of how much it has read, which is
%% also initialized through this function.
%%
%% Content encoding is generally used for compression.
%%
%% Standard encodings can be found in cowboy_http.
-spec init_stream(fun(), any(), fun(), Req) -> {ok, Req} when Req::req().
init_stream(TransferDecode, TransferState, ContentDecode, Req) ->
	{ok, Req#http_req{body_state=
		{stream, TransferDecode, TransferState, ContentDecode}}}.

%% @doc Stream the request's body.
%%
%% This is the most low level function to read the request body.
%%
%% In most cases, if they weren't defined before using stream_body/4,
%% this function will guess which transfer and content encodings were
%% used for building the request body, and configure the decoding
%% functions that will be used when streaming.
%%
%% It then starts streaming the body, returning {ok, Data, Req}
%% for each streamed part, and {done, Req} when it's finished streaming.
-spec stream_body(Req) -> {ok, binary(), Req}
	| {done, Req} | {error, atom()} when Req::req().
stream_body(Req=#http_req{body_state=waiting,
		version=Version, transport=Transport, socket=Socket}) ->
	case parse_header(<<"Expect">>, Req) of
		{ok, [<<"100-continue">>], Req1} ->
			HTTPVer = cowboy_http:version_to_binary(Version),
			Transport:send(Socket,
				<< HTTPVer/binary, " ", (status(100))/binary, "\r\n\r\n" >>);
		{ok, undefined, Req1} ->
			ok
	end,
	case parse_header('Transfer-Encoding', Req1) of
		{ok, [<<"chunked">>], Req2} ->
			stream_body(Req2#http_req{body_state=
				{stream, fun cowboy_http:te_chunked/2, {0, 0},
				 fun cowboy_http:ce_identity/1}});
		{ok, [<<"identity">>], Req2} ->
			{Length, Req3} = body_length(Req2),
			case Length of
				0 ->
					{done, Req3#http_req{body_state=done}};
				Length ->
					stream_body(Req3#http_req{body_state=
						{stream, fun cowboy_http:te_identity/2, {0, Length},
						 fun cowboy_http:ce_identity/1}})
			end
	end;
stream_body(Req=#http_req{buffer=Buffer, body_state={stream, _, _, _}})
		when Buffer =/= <<>> ->
	transfer_decode(Buffer, Req#http_req{buffer= <<>>});
stream_body(Req=#http_req{body_state={stream, _, _, _}}) ->
	stream_body_recv(Req);
stream_body(Req=#http_req{body_state=done}) ->
	{done, Req}.

-spec stream_body_recv(Req)
	-> {ok, binary(), Req} | {error, atom()} when Req::req().
stream_body_recv(Req=#http_req{
		transport=Transport, socket=Socket, buffer=Buffer}) ->
	%% @todo Allow configuring the timeout.
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} -> transfer_decode(<< Buffer/binary, Data/binary >>, Req);
		{error, Reason} -> {error, Reason}
	end.

-spec transfer_decode(binary(), Req)
	-> {ok, binary(), Req} | {error, atom()} when Req::req().
transfer_decode(Data, Req=#http_req{
		body_state={stream, TransferDecode, TransferState, ContentDecode}}) ->
	case TransferDecode(Data, TransferState) of
		{ok, Data2, TransferState2} ->
			content_decode(ContentDecode, Data2, Req#http_req{body_state=
				{stream, TransferDecode, TransferState2, ContentDecode}});
		{ok, Data2, Rest, TransferState2} ->
			content_decode(ContentDecode, Data2, Req#http_req{
				buffer=Rest, body_state=
				{stream, TransferDecode, TransferState2, ContentDecode}});
		%% @todo {header(s) for chunked
		more ->
			stream_body_recv(Req#http_req{buffer=Data});
		{done, Length, Rest} ->
			Req2 = transfer_decode_done(Length, Rest, Req),
			{done, Req2};
		{done, Data2, Length, Rest} ->
			Req2 = transfer_decode_done(Length, Rest, Req),
			content_decode(ContentDecode, Data2, Req2);
		{error, Reason} ->
			{error, Reason}
	end.

-spec transfer_decode_done(non_neg_integer(), binary(), Req)
	-> Req when Req::req().
transfer_decode_done(Length, Rest, Req=#http_req{
		headers=Headers, p_headers=PHeaders}) ->
	Headers2 = lists:keystore('Content-Length', 1, Headers,
		{'Content-Length', list_to_binary(integer_to_list(Length))}),
	%% At this point we just assume TEs were all decoded.
	Headers3 = lists:keydelete('Transfer-Encoding', 1, Headers2),
	PHeaders2 = lists:keystore('Content-Length', 1, PHeaders,
		{'Content-Length', Length}),
	PHeaders3 = lists:keydelete('Transfer-Encoding', 1, PHeaders2),
	Req#http_req{buffer=Rest, body_state=done,
		headers=Headers3, p_headers=PHeaders3}.

%% @todo Probably needs a Rest.
-spec content_decode(fun(), binary(), Req)
	-> {ok, binary(), Req} | {error, atom()} when Req::req().
content_decode(ContentDecode, Data, Req) ->
	case ContentDecode(Data) of
		{ok, Data2} -> {ok, Data2, Req};
		{error, Reason} -> {error, Reason}
	end.

%% @doc Return the full body sent with the request.
-spec body(Req) -> {ok, binary(), Req} | {error, atom()} when Req::req().
body(Req) ->
	read_body(infinity, Req, <<>>).

%% @doc Return the full body sent with the request as long as the body
%% length doesn't go over MaxLength.
%%
%% This is most useful to quickly be able to get the full body while
%% avoiding filling your memory with huge request bodies when you're
%% not expecting it.
-spec body(non_neg_integer() | infinity, Req)
	-> {ok, binary(), Req} | {error, atom()} when Req::req().
body(MaxLength, Req) ->
	read_body(MaxLength, Req, <<>>).

-spec read_body(non_neg_integer() | infinity, Req, binary())
	-> {ok, binary(), Req} | {error, atom()} when Req::req().
read_body(MaxLength, Req, Acc) when MaxLength > byte_size(Acc) ->
	case stream_body(Req) of
		{ok, Data, Req2} ->
			read_body(MaxLength, Req2, << Acc/binary, Data/binary >>);
		{done, Req2} ->
			{ok, Acc, Req2};
		{error, Reason} ->
			{error, Reason}
	end.

-spec skip_body(Req) -> {ok, Req} | {error, atom()} when Req::req().
skip_body(Req) ->
	case stream_body(Req) of
		{ok, _, Req2} -> skip_body(Req2);
		{done, Req2} -> {ok, Req2};
		{error, Reason} -> {error, Reason}
	end.

%% @doc Return the full body sent with the reqest, parsed as an
%% application/x-www-form-urlencoded string. Essentially a POST query string.
%% @todo We need an option to limit the size of the body for QS too.
-spec body_qs(Req)
	-> {ok, [{binary(), binary() | true}], Req} | {error, atom()}
	when Req::req().
body_qs(Req=#http_req{urldecode={URLDecFun, URLDecArg}}) ->
	case body(Req) of
		{ok, Body, Req2} ->
			{ok, cowboy_http:x_www_form_urlencoded(
				Body, fun(Bin) -> URLDecFun(Bin, URLDecArg) end), Req2};
		{error, Reason} ->
			{error, Reason}
	end.

%% Multipart Request API.

%% @doc Return data from the multipart parser.
%%
%% Use this function for multipart streaming. For each part in the request,
%% this function returns <em>{headers, Headers}</em> followed by a sequence of
%% <em>{body, Data}</em> tuples and finally <em>end_of_part</em>. When there
%% is no part to parse anymore, <em>eof</em> is returned.
%%
%% If the request Content-Type is not a multipart one, <em>{error, badarg}</em>
%% is returned.
-spec multipart_data(Req)
	-> {headers, cowboy_http:headers(), Req} | {body, binary(), Req}
		| {end_of_part | eof, Req} when Req::req().
multipart_data(Req=#http_req{body_state=waiting}) ->
	{ok, {<<"multipart">>, _SubType, Params}, Req2} =
		parse_header('Content-Type', Req),
	{_, Boundary} = lists:keyfind(<<"boundary">>, 1, Params),
	{ok, Length, Req3} = parse_header('Content-Length', Req2),
	multipart_data(Req3, Length, {more, cowboy_multipart:parser(Boundary)});
multipart_data(Req=#http_req{multipart={Length, Cont}}) ->
	multipart_data(Req, Length, Cont());
multipart_data(Req=#http_req{body_state=done}) ->
	{eof, Req}.

%% @todo Typespecs.
multipart_data(Req, Length, {headers, Headers, Cont}) ->
	{headers, Headers, Req#http_req{multipart={Length, Cont}}};
multipart_data(Req, Length, {body, Data, Cont}) ->
	{body, Data, Req#http_req{multipart={Length, Cont}}};
multipart_data(Req, Length, {end_of_part, Cont}) ->
	{end_of_part, Req#http_req{multipart={Length, Cont}}};
multipart_data(Req, 0, eof) ->
	{eof, Req#http_req{body_state=done, multipart=undefined}};
multipart_data(Req=#http_req{socket=Socket, transport=Transport},
		Length, eof) ->
	%% We just want to skip so no need to stream data here.
	{ok, _Data} = Transport:recv(Socket, Length, 5000),
	{eof, Req#http_req{body_state=done, multipart=undefined}};
multipart_data(Req, Length, {more, Parser}) when Length > 0 ->
	case stream_body(Req) of
		{ok, << Data:Length/binary, Buffer/binary >>, Req2} ->
			multipart_data(Req2#http_req{buffer=Buffer}, 0, Parser(Data));
		{ok, Data, Req2} ->
			multipart_data(Req2, Length - byte_size(Data), Parser(Data))
	end.

%% @doc Skip a part returned by the multipart parser.
%%
%% This function repeatedly calls <em>multipart_data/1</em> until
%% <em>end_of_part</em> or <em>eof</em> is parsed.
-spec multipart_skip(Req) -> {ok, Req} when Req::req().
multipart_skip(Req) ->
	case multipart_data(Req) of
		{end_of_part, Req2} -> {ok, Req2};
		{eof, Req2} -> {ok, Req2};
		{_, _, Req2} -> multipart_skip(Req2)
	end.

%% Response API.

%% @doc Add a cookie header to the response.
-spec set_resp_cookie(binary(), binary(),
	[cowboy_cookies:cookie_option()], Req) -> Req when Req::req().
set_resp_cookie(Name, Value, Options, Req) ->
	{HeaderName, HeaderValue} = cowboy_cookies:cookie(Name, Value, Options),
	set_resp_header(HeaderName, HeaderValue, Req).

%% @doc Add a header to the response.
-spec set_resp_header(cowboy_http:header(), iodata(), Req)
	-> Req when Req::req().
set_resp_header(Name, Value, Req=#http_req{resp_headers=RespHeaders}) ->
	NameBin = header_to_binary(Name),
	Req#http_req{resp_headers=[{NameBin, Value}|RespHeaders]}.

%% @doc Add a body to the response.
%%
%% The body set here is ignored if the response is later sent using
%% anything other than reply/2 or reply/3. The response body is expected
%% to be a binary or an iolist.
-spec set_resp_body(iodata(), Req) -> Req when Req::req().
set_resp_body(Body, Req) ->
	Req#http_req{resp_body=Body}.

%% @doc Add a body function to the response.
%%
%% The response body may also be set to a content-length - stream-function pair.
%% If the response body is of this type normal response headers will be sent.
%% After the response headers has been sent the body function is applied.
%% The body function is expected to write the response body directly to the
%% socket using the transport module.
%%
%% If the body function crashes while writing the response body or writes fewer
%% bytes than declared the behaviour is undefined. The body set here is ignored
%% if the response is later sent using anything other than `reply/2' or
%% `reply/3'.
%%
%% @see cowboy_req:transport/1.
-spec set_resp_body_fun(non_neg_integer(),
	fun(() -> {sent, non_neg_integer()}), Req) -> Req when Req::req().
set_resp_body_fun(StreamLen, StreamFun, Req) ->
	Req#http_req{resp_body={StreamLen, StreamFun}}.

%% @doc Return whether the given header has been set for the response.
-spec has_resp_header(cowboy_http:header(), req()) -> boolean().
has_resp_header(Name, #http_req{resp_headers=RespHeaders}) ->
	NameBin = header_to_binary(Name),
	lists:keymember(NameBin, 1, RespHeaders).

%% @doc Return whether a body has been set for the response.
-spec has_resp_body(req()) -> boolean().
has_resp_body(#http_req{resp_body={Length, _}}) ->
	Length > 0;
has_resp_body(#http_req{resp_body=RespBody}) ->
	iolist_size(RespBody) > 0.

%% Remove a header previously set for the response.
-spec delete_resp_header(cowboy_http:header(), Req)
	-> Req when Req::req().
delete_resp_header(Name, Req=#http_req{resp_headers=RespHeaders}) ->
	RespHeaders2 = lists:keydelete(Name, 1, RespHeaders),
	Req#http_req{resp_headers=RespHeaders2}.

%% @equiv reply(Status, [], [], Req)
-spec reply(cowboy_http:status(), Req) -> {ok, Req} when Req::req().
reply(Status, Req=#http_req{resp_body=Body}) ->
	reply(Status, [], Body, Req).

%% @equiv reply(Status, Headers, [], Req)
-spec reply(cowboy_http:status(), cowboy_http:headers(), Req)
	-> {ok, Req} when Req::req().
reply(Status, Headers, Req=#http_req{resp_body=Body}) ->
	reply(Status, Headers, Body, Req).

%% @doc Send a reply to the client.
-spec reply(cowboy_http:status(), cowboy_http:headers(), iodata(), Req)
	-> {ok, Req} when Req::req().
reply(Status, Headers, Body, Req=#http_req{socket=Socket, transport=Transport,
		version=Version, connection=Connection,
		method=Method, resp_state=waiting, resp_headers=RespHeaders}) ->
	RespConn = response_connection(Headers, Connection),
	ContentLen = case Body of {CL, _} -> CL; _ -> iolist_size(Body) end,
	HTTP11Headers = case Version of
		{1, 1} -> [{<<"Connection">>, atom_to_connection(Connection)}];
		_ -> []
	end,
	{ReplyType, Req2} = response(Status, Headers, RespHeaders,  [
		{<<"Content-Length">>, integer_to_list(ContentLen)},
		{<<"Date">>, cowboy_clock:rfc1123()},
		{<<"Server">>, <<"Cowboy">>}
	|HTTP11Headers], Req),
	if	Method =:= 'HEAD' -> ok;
		ReplyType =:= hook -> ok; %% Hook replied for us, stop there.
		true ->
			case Body of
				{_, StreamFun} -> StreamFun();
				_ -> Transport:send(Socket, Body)
			end
	end,
	{ok, Req2#http_req{connection=RespConn, resp_state=done,
		resp_headers=[], resp_body= <<>>}}.

%% @equiv chunked_reply(Status, [], Req)
-spec chunked_reply(cowboy_http:status(), Req) -> {ok, Req} when Req::req().
chunked_reply(Status, Req) ->
	chunked_reply(Status, [], Req).

%% @doc Initiate the sending of a chunked reply to the client.
%% @see cowboy_req:chunk/2
-spec chunked_reply(cowboy_http:status(), cowboy_http:headers(), Req)
	-> {ok, Req} when Req::req().
chunked_reply(Status, Headers, Req=#http_req{
		version=Version, connection=Connection,
		resp_state=waiting, resp_headers=RespHeaders}) ->
	RespConn = response_connection(Headers, Connection),
	HTTP11Headers = case Version of
		{1, 1} -> [
			{<<"Connection">>, atom_to_connection(Connection)},
			{<<"Transfer-Encoding">>, <<"chunked">>}];
		_ -> []
	end,
	{_, Req2} = response(Status, Headers, RespHeaders, [
		{<<"Date">>, cowboy_clock:rfc1123()},
		{<<"Server">>, <<"Cowboy">>}
	|HTTP11Headers], Req),
	{ok, Req2#http_req{connection=RespConn, resp_state=chunks,
		resp_headers=[], resp_body= <<>>}}.

%% @doc Send a chunk of data.
%%
%% A chunked reply must have been initiated before calling this function.
-spec chunk(iodata(), req()) -> ok | {error, atom()}.
chunk(_Data, #http_req{socket=_Socket, transport=_Transport, method='HEAD'}) ->
	ok;
chunk(Data, #http_req{socket=Socket, transport=Transport, version={1, 0}}) ->
	Transport:send(Socket, Data);
chunk(Data, #http_req{socket=Socket, transport=Transport, resp_state=chunks}) ->
	Transport:send(Socket, [integer_to_list(iolist_size(Data), 16),
		<<"\r\n">>, Data, <<"\r\n">>]).

%% @doc Send an upgrade reply.
%% @private
-spec upgrade_reply(cowboy_http:status(), cowboy_http:headers(), Req)
	-> {ok, Req} when Req::req().
upgrade_reply(Status, Headers, Req=#http_req{
		resp_state=waiting, resp_headers=RespHeaders}) ->
	{_, Req2} = response(Status, Headers, RespHeaders, [
		{<<"Connection">>, <<"Upgrade">>}
	], Req),
	{ok, Req2#http_req{resp_state=done, resp_headers=[], resp_body= <<>>}}.

%% @doc Ensure the response has been sent fully.
%% @private
-spec ensure_response(req(), cowboy_http:status()) -> ok.
%% The response has already been fully sent to the client.
ensure_response(#http_req{resp_state=done}, _) ->
	ok;
%% No response has been sent but everything apparently went fine.
%% Reply with the status code found in the second argument.
ensure_response(Req=#http_req{resp_state=waiting}, Status) ->
	_ = reply(Status, [], [], Req),
	ok;
%% Terminate the chunked body for HTTP/1.1 only.
ensure_response(#http_req{method='HEAD', resp_state=chunks}, _) ->
	ok;
ensure_response(#http_req{version={1, 0}, resp_state=chunks}, _) ->
	ok;
ensure_response(#http_req{socket=Socket, transport=Transport,
		resp_state=chunks}, _) ->
	Transport:send(Socket, <<"0\r\n\r\n">>),
	ok.

%% Private setter/getter API.

%% @private
-spec set_host(binary(), inet:port_number(), binary(), Req)
	-> Req when Req::req().
set_host(Host, Port, RawHost, Req=#http_req{headers=Headers}) ->
	Req#http_req{host=Host, port=Port, headers=[{'Host', RawHost}|Headers]}.

%% @private
-spec set_connection(binary(), Req) -> Req when Req::req().
set_connection(RawConnection, Req=#http_req{headers=Headers}) ->
	Req2 = Req#http_req{headers=[{'Connection', RawConnection}|Headers]},
	{ok, ConnTokens, Req3} = parse_header('Connection', Req2),
	ConnAtom = cowboy_http:connection_to_atom(ConnTokens),
	Req3#http_req{connection=ConnAtom}.

%% @private
-spec add_header(cowboy_http:header(), binary(), Req)
	-> Req when Req::req().
add_header(Name, Value, Req=#http_req{headers=Headers}) ->
	Req#http_req{headers=[{Name, Value}|Headers]}.

%% @private
-spec set_buffer(binary(), Req) -> Req when Req::req().
set_buffer(Buffer, Req) ->
	Req#http_req{buffer=Buffer}.

%% @private
-spec set_bindings(cowboy_dispatcher:tokens(), cowboy_dispatcher:tokens(),
	cowboy_dispatcher:bindings(), Req) -> Req when Req::req().
set_bindings(HostInfo, PathInfo, Bindings, Req) ->
	Req#http_req{host_info=HostInfo, path_info=PathInfo,
		bindings=Bindings}.

%% @private
-spec get_resp_state(req()) -> locked | waiting | chunks | done.
get_resp_state(#http_req{resp_state=RespState}) ->
	RespState.

%% @private
-spec get_buffer(req()) -> binary().
get_buffer(#http_req{buffer=Buffer}) ->
	Buffer.

%% @private
-spec get_connection(req()) -> keepalive | close.
get_connection(#http_req{connection=Connection}) ->
	Connection.

%% Misc API.

%% @doc Compact the request data by removing all non-system information.
%%
%% This essentially removes the host and path info, query string, bindings,
%% headers and cookies.
%%
%% Use it when you really need to save up memory, for example when having
%% many concurrent long-running connections.
-spec compact(Req) -> Req when Req::req().
compact(Req) ->
	Req#http_req{host_info=undefined,
		path_info=undefined, qs_vals=undefined,
		bindings=undefined, headers=[],
		p_headers=[], cookies=[]}.

%% @doc Prevent any further responses.
%% @private
-spec lock(Req) -> Req when Req::req().
lock(Req) ->
	Req#http_req{resp_state=locked}.

%% @doc Convert the Req object to a list of key/values.
-spec to_list(req()) -> [{atom(), any()}].
to_list(Req) ->
	lists:zip(record_info(fields, http_req), tl(tuple_to_list(Req))).

%% @doc Return the transport module and socket associated with a request.
%%
%% This exposes the same socket interface used internally by the HTTP protocol
%% implementation to developers that needs low level access to the socket.
%%
%% It is preferred to use this in conjuction with the stream function support
%% in `set_resp_body_fun/3' if this is used to write a response body directly
%% to the socket. This ensures that the response headers are set correctly.
-spec transport(req()) -> {ok, module(), inet:socket()}.
transport(#http_req{transport=Transport, socket=Socket}) ->
	{ok, Transport, Socket}.

%% Internal.

-spec response(cowboy_http:status(), cowboy_http:headers(),
	cowboy_http:headers(), cowboy_http:headers(), Req)
	-> {normal | hook, Req} when Req::req().
response(Status, Headers, RespHeaders, DefaultHeaders, Req=#http_req{
		socket=Socket, transport=Transport, version=Version,
		pid=ReqPid, onresponse=OnResponse}) ->
	FullHeaders = response_merge_headers(Headers, RespHeaders, DefaultHeaders),
	Req2 = case OnResponse of
		undefined -> Req;
		OnResponse -> OnResponse(Status, FullHeaders,
			%% Don't call 'onresponse' from the hook itself.
			Req#http_req{resp_headers=[], resp_body= <<>>,
				onresponse=undefined})
	end,
	ReplyType = case Req2#http_req.resp_state of
		waiting ->
			HTTPVer = cowboy_http:version_to_binary(Version),
			StatusLine = << HTTPVer/binary, " ",
				(status(Status))/binary, "\r\n" >>,
			HeaderLines = [[Key, <<": ">>, Value, <<"\r\n">>]
				|| {Key, Value} <- FullHeaders],
			Transport:send(Socket, [StatusLine, HeaderLines, <<"\r\n">>]),
			ReqPid ! {?MODULE, resp_sent},
			normal;
		_ ->
			hook
	end,
	{ReplyType, Req2}.

-spec response_connection(cowboy_http:headers(), keepalive | close)
	-> keepalive | close.
response_connection([], Connection) ->
	Connection;
response_connection([{Name, Value}|Tail], Connection) ->
	case Name of
		'Connection' -> response_connection_parse(Value);
		Name when is_atom(Name) -> response_connection(Tail, Connection);
		Name ->
			Name2 = cowboy_bstr:to_lower(Name),
			case Name2 of
				<<"connection">> -> response_connection_parse(Value);
				_Any -> response_connection(Tail, Connection)
			end
	end.

-spec response_connection_parse(binary()) -> keepalive | close.
response_connection_parse(ReplyConn) ->
	Tokens = cowboy_http:nonempty_list(ReplyConn, fun cowboy_http:token/2),
	cowboy_http:connection_to_atom(Tokens).

-spec response_merge_headers(cowboy_http:headers(), cowboy_http:headers(),
	cowboy_http:headers()) -> cowboy_http:headers().
response_merge_headers(Headers, RespHeaders, DefaultHeaders) ->
	Headers2 = [{header_to_binary(Key), Value} || {Key, Value} <- Headers],
	merge_headers(
		merge_headers(Headers2, RespHeaders),
		DefaultHeaders).

-spec merge_headers(cowboy_http:headers(), cowboy_http:headers())
	-> cowboy_http:headers().
merge_headers(Headers, []) ->
	Headers;
merge_headers(Headers, [{Name, Value}|Tail]) ->
	Headers2 = case lists:keymember(Name, 1, Headers) of
		true -> Headers;
		false -> Headers ++ [{Name, Value}]
	end,
	merge_headers(Headers2, Tail).

-spec atom_to_connection(keepalive) -> <<_:80>>;
						(close) -> <<_:40>>.
atom_to_connection(keepalive) ->
	<<"keep-alive">>;
atom_to_connection(close) ->
	<<"close">>.

-spec status(cowboy_http:status()) -> binary().
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

-spec header_to_binary(cowboy_http:header()) -> binary().
header_to_binary('Cache-Control') -> <<"Cache-Control">>;
header_to_binary('Connection') -> <<"Connection">>;
header_to_binary('Date') -> <<"Date">>;
header_to_binary('Pragma') -> <<"Pragma">>;
header_to_binary('Transfer-Encoding') -> <<"Transfer-Encoding">>;
header_to_binary('Upgrade') -> <<"Upgrade">>;
header_to_binary('Via') -> <<"Via">>;
header_to_binary('Accept') -> <<"Accept">>;
header_to_binary('Accept-Charset') -> <<"Accept-Charset">>;
header_to_binary('Accept-Encoding') -> <<"Accept-Encoding">>;
header_to_binary('Accept-Language') -> <<"Accept-Language">>;
header_to_binary('Authorization') -> <<"Authorization">>;
header_to_binary('From') -> <<"From">>;
header_to_binary('Host') -> <<"Host">>;
header_to_binary('If-Modified-Since') -> <<"If-Modified-Since">>;
header_to_binary('If-Match') -> <<"If-Match">>;
header_to_binary('If-None-Match') -> <<"If-None-Match">>;
header_to_binary('If-Range') -> <<"If-Range">>;
header_to_binary('If-Unmodified-Since') -> <<"If-Unmodified-Since">>;
header_to_binary('Max-Forwards') -> <<"Max-Forwards">>;
header_to_binary('Proxy-Authorization') -> <<"Proxy-Authorization">>;
header_to_binary('Range') -> <<"Range">>;
header_to_binary('Referer') -> <<"Referer">>;
header_to_binary('User-Agent') -> <<"User-Agent">>;
header_to_binary('Age') -> <<"Age">>;
header_to_binary('Location') -> <<"Location">>;
header_to_binary('Proxy-Authenticate') -> <<"Proxy-Authenticate">>;
header_to_binary('Public') -> <<"Public">>;
header_to_binary('Retry-After') -> <<"Retry-After">>;
header_to_binary('Server') -> <<"Server">>;
header_to_binary('Vary') -> <<"Vary">>;
header_to_binary('Warning') -> <<"Warning">>;
header_to_binary('Www-Authenticate') -> <<"Www-Authenticate">>;
header_to_binary('Allow') -> <<"Allow">>;
header_to_binary('Content-Base') -> <<"Content-Base">>;
header_to_binary('Content-Encoding') -> <<"Content-Encoding">>;
header_to_binary('Content-Language') -> <<"Content-Language">>;
header_to_binary('Content-Length') -> <<"Content-Length">>;
header_to_binary('Content-Location') -> <<"Content-Location">>;
header_to_binary('Content-Md5') -> <<"Content-Md5">>;
header_to_binary('Content-Range') -> <<"Content-Range">>;
header_to_binary('Content-Type') -> <<"Content-Type">>;
header_to_binary('Etag') -> <<"Etag">>;
header_to_binary('Expires') -> <<"Expires">>;
header_to_binary('Last-Modified') -> <<"Last-Modified">>;
header_to_binary('Accept-Ranges') -> <<"Accept-Ranges">>;
header_to_binary('Set-Cookie') -> <<"Set-Cookie">>;
header_to_binary('Set-Cookie2') -> <<"Set-Cookie2">>;
header_to_binary('X-Forwarded-For') -> <<"X-Forwarded-For">>;
header_to_binary('Cookie') -> <<"Cookie">>;
header_to_binary('Keep-Alive') -> <<"Keep-Alive">>;
header_to_binary('Proxy-Connection') -> <<"Proxy-Connection">>;
header_to_binary(B) when is_binary(B) -> B.

%% Tests.

-ifdef(TEST).

url_test() ->
	{<<"http://localhost/path">>, _ } =
		url(#http_req{transport=ranch_tcp, host= <<"localhost">>, port=80,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	{<<"http://localhost:443/path">>, _} =
		url(#http_req{transport=ranch_tcp, host= <<"localhost">>, port=443,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	{<<"http://localhost:8080/path">>, _} =
		url(#http_req{transport=ranch_tcp, host= <<"localhost">>, port=8080,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	{<<"http://localhost:8080/path?dummy=2785">>, _} =
		url(#http_req{transport=ranch_tcp, host= <<"localhost">>, port=8080,
			path= <<"/path">>, qs= <<"dummy=2785">>, pid=self()}),
	{<<"https://localhost/path">>, _} =
		url(#http_req{transport=ranch_ssl, host= <<"localhost">>, port=443,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	{<<"https://localhost:8443/path">>, _} =
		url(#http_req{transport=ranch_ssl, host= <<"localhost">>, port=8443,
			path= <<"/path">>, qs= <<>>, pid=self()}),
	{<<"https://localhost:8443/path?dummy=2785">>, _} =
		url(#http_req{transport=ranch_ssl, host= <<"localhost">>, port=8443,
			path= <<"/path">>, qs= <<"dummy=2785">>, pid=self()}),
	ok.

-endif.
