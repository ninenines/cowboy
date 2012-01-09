%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
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
%% Almost all functions in this module return a new <em>Req</em> variable.
%% It should always be used instead of the one used in your function call
%% because it keeps the state of the request. It also allows Cowboy to do
%% some lazy evaluation and cache results where possible.
-module(cowboy_http_req).

-export([
	method/1, version/1, peer/1, peer_addr/1,
	host/1, host_info/1, raw_host/1, port/1,
	path/1, path_info/1, raw_path/1,
	qs_val/2, qs_val/3, qs_vals/1, raw_qs/1,
	binding/2, binding/3, bindings/1,
	header/2, header/3, headers/1,
	parse_header/2, parse_header/3,
	cookie/2, cookie/3, cookies/1,
	meta/2, meta/3
]). %% Request API.

-export([
	body_length/1, body/1, body/2, body_qs/1
]). %% Request Body API.

-export([
	set_resp_cookie/4, set_resp_header/3, set_resp_body/2,
	set_resp_body_fun/3, has_resp_header/2, has_resp_body/1,
	reply/2, reply/3, reply/4,
	chunked_reply/2, chunked_reply/3, chunk/2,
	upgrade_reply/3
]). %% Response API.

-export([
	compact/1, transport/1
]). %% Misc API.

-include("include/http.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Request API.

%% @doc Return the HTTP method of the request.
-spec method(#http_req{}) -> {http_method(), #http_req{}}.
method(Req) ->
	{Req#http_req.method, Req}.

%% @doc Return the HTTP version used for the request.
-spec version(#http_req{}) -> {http_version(), #http_req{}}.
version(Req) ->
	{Req#http_req.version, Req}.

%% @doc Return the peer address and port number of the remote host.
-spec peer(#http_req{}) -> {{inet:ip_address(), inet:ip_port()}, #http_req{}}.
peer(Req=#http_req{socket=Socket, transport=Transport, peer=undefined}) ->
	{ok, Peer} = Transport:peername(Socket),
	{Peer, Req#http_req{peer=Peer}};
peer(Req) ->
	{Req#http_req.peer, Req}.

%% @doc Returns the peer address calculated from headers.
-spec peer_addr(#http_req{}) -> {inet:ip_address(), #http_req{}}.
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

%% @doc Return the tokens for the hostname requested.
-spec host(#http_req{}) -> {cowboy_dispatcher:tokens(), #http_req{}}.
host(Req) ->
	{Req#http_req.host, Req}.

%% @doc Return the extra host information obtained from partially matching
%% the hostname using <em>'...'</em>.
-spec host_info(#http_req{})
	-> {cowboy_dispatcher:tokens() | undefined, #http_req{}}.
host_info(Req) ->
	{Req#http_req.host_info, Req}.

%% @doc Return the raw host directly taken from the request.
-spec raw_host(#http_req{}) -> {binary(), #http_req{}}.
raw_host(Req) ->
	{Req#http_req.raw_host, Req}.

%% @doc Return the port used for this request.
-spec port(#http_req{}) -> {inet:ip_port(), #http_req{}}.
port(Req) ->
	{Req#http_req.port, Req}.

%% @doc Return the path segments for the path requested.
%%
%% Following RFC2396, this function may return path segments containing any
%% character, including <em>/</em> if, and only if, a <em>/</em> was escaped
%% and part of a path segment in the path requested.
-spec path(#http_req{}) -> {cowboy_dispatcher:tokens(), #http_req{}}.
path(Req) ->
	{Req#http_req.path, Req}.

%% @doc Return the extra path information obtained from partially matching
%% the patch using <em>'...'</em>.
-spec path_info(#http_req{})
	-> {cowboy_dispatcher:tokens() | undefined, #http_req{}}.
path_info(Req) ->
	{Req#http_req.path_info, Req}.

%% @doc Return the raw path directly taken from the request.
-spec raw_path(#http_req{}) -> {binary(), #http_req{}}.
raw_path(Req) ->
	{Req#http_req.raw_path, Req}.

%% @equiv qs_val(Name, Req, undefined)
-spec qs_val(binary(), #http_req{})
	-> {binary() | true | undefined, #http_req{}}.
qs_val(Name, Req) when is_binary(Name) ->
	qs_val(Name, Req, undefined).

%% @doc Return the query string value for the given key, or a default if
%% missing.
-spec qs_val(binary(), #http_req{}, Default)
	-> {binary() | true | Default, #http_req{}} when Default::any().
qs_val(Name, Req=#http_req{raw_qs=RawQs, qs_vals=undefined,
		urldecode={URLDecFun, URLDecArg}}, Default) when is_binary(Name) ->
	QsVals = parse_qs(RawQs, fun(Bin) -> URLDecFun(Bin, URLDecArg) end),
	qs_val(Name, Req#http_req{qs_vals=QsVals}, Default);
qs_val(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.qs_vals) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of query string values.
-spec qs_vals(#http_req{}) -> {list({binary(), binary() | true}), #http_req{}}.
qs_vals(Req=#http_req{raw_qs=RawQs, qs_vals=undefined,
		urldecode={URLDecFun, URLDecArg}}) ->
	QsVals = parse_qs(RawQs, fun(Bin) -> URLDecFun(Bin, URLDecArg) end),
	qs_vals(Req#http_req{qs_vals=QsVals});
qs_vals(Req=#http_req{qs_vals=QsVals}) ->
	{QsVals, Req}.

%% @doc Return the raw query string directly taken from the request.
-spec raw_qs(#http_req{}) -> {binary(), #http_req{}}.
raw_qs(Req) ->
	{Req#http_req.raw_qs, Req}.

%% @equiv binding(Name, Req, undefined)
-spec binding(atom(), #http_req{}) -> {binary() | undefined, #http_req{}}.
binding(Name, Req) when is_atom(Name) ->
	binding(Name, Req, undefined).

%% @doc Return the binding value for the given key obtained when matching
%% the host and path against the dispatch list, or a default if missing.
-spec binding(atom(), #http_req{}, Default)
	-> {binary() | Default, #http_req{}} when Default::any().
binding(Name, Req, Default) when is_atom(Name) ->
	case lists:keyfind(Name, 1, Req#http_req.bindings) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of binding values.
-spec bindings(#http_req{}) -> {list({atom(), binary()}), #http_req{}}.
bindings(Req) ->
	{Req#http_req.bindings, Req}.

%% @equiv header(Name, Req, undefined)
-spec header(atom() | binary(), #http_req{})
	-> {binary() | undefined, #http_req{}}.
header(Name, Req) when is_atom(Name) orelse is_binary(Name) ->
	header(Name, Req, undefined).

%% @doc Return the header value for the given key, or a default if missing.
-spec header(atom() | binary(), #http_req{}, Default)
	-> {binary() | Default, #http_req{}} when Default::any().
header(Name, Req, Default) when is_atom(Name) orelse is_binary(Name) ->
	case lists:keyfind(Name, 1, Req#http_req.headers) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% @doc Return the full list of headers.
-spec headers(#http_req{}) -> {http_headers(), #http_req{}}.
headers(Req) ->
	{Req#http_req.headers, Req}.

%% @doc Semantically parse headers.
%%
%% When the value isn't found, a proper default value for the type
%% returned is used as a return value.
%% @see parse_header/3
-spec parse_header(http_header(), #http_req{})
	-> {any(), #http_req{}} | {error, badarg}.
parse_header(Name, Req=#http_req{p_headers=PHeaders}) ->
	case lists:keyfind(Name, 1, PHeaders) of
		false -> parse_header(Name, Req, parse_header_default(Name));
		{Name, Value} -> {Value, Req}
	end.

%% @doc Default values for semantic header parsing.
-spec parse_header_default(http_header()) -> any().
parse_header_default('Connection') -> [];
parse_header_default(_Name) -> undefined.

%% @doc Semantically parse headers.
%%
%% When the header is unknown, the value is returned directly without parsing.
-spec parse_header(http_header(), #http_req{}, any())
	-> {any(), #http_req{}} | {error, badarg}.
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
			{Default, Req2#http_req{p_headers=[{Name, Default}|PHeaders]}};
		{Value, Req2} ->
			case Fun(Value) of
				{error, badarg} ->
					{error, badarg};
				P ->
					{P, Req2#http_req{p_headers=[{Name, P}|PHeaders]}}
			end
	end.

%% @equiv cookie(Name, Req, undefined)
-spec cookie(binary(), #http_req{})
	-> {binary() | true | undefined, #http_req{}}.
cookie(Name, Req) when is_binary(Name) ->
	cookie(Name, Req, undefined).

%% @doc Return the cookie value for the given key, or a default if
%% missing.
-spec cookie(binary(), #http_req{}, Default)
	-> {binary() | true | Default, #http_req{}} when Default::any().
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
-spec cookies(#http_req{}) -> {list({binary(), binary() | true}), #http_req{}}.
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
-spec meta(atom(), #http_req{}) -> {any() | undefined, #http_req{}}.
meta(Name, Req) ->
	meta(Name, Req, undefined).

%% @doc Return metadata information about the request.
%%
%% Metadata information varies from one protocol to another. Websockets
%% would define the protocol version here, while REST would use it to
%% indicate which media type, language and charset were retained.
-spec meta(atom(), #http_req{}, any()) -> {any(), #http_req{}}.
meta(Name, Req, Default) ->
	case lists:keyfind(Name, 1, Req#http_req.meta) of
		{Name, Value} -> {Value, Req};
		false -> {Default, Req}
	end.

%% Request Body API.

%% @doc Return the length of body sent with the request, or <em>{error, badarg}</em>
%% if no <em>Content-Length</em> is available.
-spec body_length(#http_req{}) -> {ok, non_neg_integer(), #http_req{}} | {error, atom()}.
body_length(Req) ->
	{Length, Req2} = cowboy_http_req:header('Content-Length', Req),
	case Length of
		undefined -> {error, badarg};
		_Any ->
			Length2 = list_to_integer(binary_to_list(Length)),
            {ok, Length2, Req2}
	end.

%% @doc Return the full body sent with the request, or <em>{error, badarg}</em>
%% if no <em>Content-Length</em> is available.
%% @todo We probably want to allow a max length.
-spec body(#http_req{}) -> {ok, binary(), #http_req{}} | {error, atom()}.
body(Req) ->
	{Length, Req2} = cowboy_http_req:parse_header('Content-Length', Req),
	case Length of
		undefined -> {error, badarg};
		{error, badarg} -> {error, badarg};
		_Any ->
			body(Length, Req2)
	end.

%% @doc Return <em>Length</em> bytes of the request body.
%%
%% You probably shouldn't be calling this function directly, as it expects the
%% <em>Length</em> argument to be the full size of the body, and will consider
%% the body to be fully read from the socket.
%% @todo We probably want to configure the timeout.
-spec body(non_neg_integer(), #http_req{})
	-> {ok, binary(), #http_req{}} | {error, atom()}.
body(Length, Req=#http_req{body_state=waiting, buffer=Buffer})
		when is_integer(Length) andalso Length =< byte_size(Buffer) ->
	<< Body:Length/binary, Rest/bits >> = Buffer,
	{ok, Body, Req#http_req{body_state=done, buffer=Rest}};
body(Length, Req=#http_req{socket=Socket, transport=Transport,
		body_state=waiting, buffer=Buffer}) ->
	case Transport:recv(Socket, Length - byte_size(Buffer), 5000) of
		{ok, Body} -> {ok, << Buffer/binary, Body/binary >>,
			Req#http_req{body_state=done, buffer= <<>>}};
		{error, Reason} -> {error, Reason}
	end.

%% @doc Return the full body sent with the reqest, parsed as an
%% application/x-www-form-urlencoded string. Essentially a POST query string.
-spec body_qs(#http_req{}) -> {list({binary(), binary() | true}), #http_req{}}.
body_qs(Req=#http_req{urldecode={URLDecFun, URLDecArg}}) ->
	{ok, Body, Req2} = body(Req),
	{parse_qs(Body, fun(Bin) -> URLDecFun(Bin, URLDecArg) end), Req2}.

%% Response API.

%% @doc Add a cookie header to the response.
-spec set_resp_cookie(binary(), binary(), [cowboy_cookies:cookie_option()],
	#http_req{}) -> {ok, #http_req{}}.
set_resp_cookie(Name, Value, Options, Req) ->
	{HeaderName, HeaderValue} = cowboy_cookies:cookie(Name, Value, Options),
	set_resp_header(HeaderName, HeaderValue, Req).

%% @doc Add a header to the response.
-spec set_resp_header(http_header(), iodata(), #http_req{})
	-> {ok, #http_req{}}.
set_resp_header(Name, Value, Req=#http_req{resp_headers=RespHeaders}) ->
	NameBin = header_to_binary(Name),
	{ok, Req#http_req{resp_headers=[{NameBin, Value}|RespHeaders]}}.

%% @doc Add a body to the response.
%%
%% The body set here is ignored if the response is later sent using
%% anything other than reply/2 or reply/3. The response body is expected
%% to be a binary or an iolist.
-spec set_resp_body(iodata(), #http_req{}) -> {ok, #http_req{}}.
set_resp_body(Body, Req) ->
	{ok, Req#http_req{resp_body=Body}}.


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
%% @see cowboy_http_req:transport/1.
-spec set_resp_body_fun(non_neg_integer(), fun(() -> {sent, non_neg_integer()}),
		#http_req{}) -> {ok, #http_req{}}.
set_resp_body_fun(StreamLen, StreamFun, Req) ->
	{ok, Req#http_req{resp_body={StreamLen, StreamFun}}}.


%% @doc Return whether the given header has been set for the response.
-spec has_resp_header(http_header(), #http_req{}) -> boolean().
has_resp_header(Name, #http_req{resp_headers=RespHeaders}) ->
	NameBin = header_to_binary(Name),
	lists:keymember(NameBin, 1, RespHeaders).

%% @doc Return whether a body has been set for the response.
-spec has_resp_body(#http_req{}) -> boolean().
has_resp_body(#http_req{resp_body={Length, _}}) ->
	Length > 0;
has_resp_body(#http_req{resp_body=RespBody}) ->
	iolist_size(RespBody) > 0.

%% @equiv reply(Status, [], [], Req)
-spec reply(http_status(), #http_req{}) -> {ok, #http_req{}}.
reply(Status, Req=#http_req{resp_body=Body}) ->
	reply(Status, [], Body, Req).

%% @equiv reply(Status, Headers, [], Req)
-spec reply(http_status(), http_headers(), #http_req{}) -> {ok, #http_req{}}.
reply(Status, Headers, Req=#http_req{resp_body=Body}) ->
	reply(Status, Headers, Body, Req).

%% @doc Send a reply to the client.
-spec reply(http_status(), http_headers(), iodata(), #http_req{})
	-> {ok, #http_req{}}.
reply(Status, Headers, Body, Req=#http_req{socket=Socket,
		transport=Transport, connection=Connection, pid=ReqPid,
		method=Method, resp_state=waiting, resp_headers=RespHeaders}) ->
	RespConn = response_connection(Headers, Connection),
	ContentLen = case Body of {CL, _} -> CL; _ -> iolist_size(Body) end,
	Head = response_head(Status, Headers, RespHeaders, [
		{<<"Connection">>, atom_to_connection(Connection)},
		{<<"Content-Length">>, integer_to_list(ContentLen)},
		{<<"Date">>, cowboy_clock:rfc1123()},
		{<<"Server">>, <<"Cowboy">>}
	]),
	case {Method, Body} of
		{'HEAD', _} -> Transport:send(Socket, Head);
		{_, {_, StreamFun}} -> Transport:send(Socket, Head), StreamFun();
		{_, _} -> Transport:send(Socket, [Head, Body])
	end,
	ReqPid ! {?MODULE, resp_sent},
	{ok, Req#http_req{connection=RespConn, resp_state=done,
		resp_headers=[], resp_body= <<>>}}.

%% @equiv chunked_reply(Status, [], Req)
-spec chunked_reply(http_status(), #http_req{}) -> {ok, #http_req{}}.
chunked_reply(Status, Req) ->
	chunked_reply(Status, [], Req).

%% @doc Initiate the sending of a chunked reply to the client.
%% @see cowboy_http_req:chunk/2
-spec chunked_reply(http_status(), http_headers(), #http_req{})
	-> {ok, #http_req{}}.
chunked_reply(Status, Headers, Req=#http_req{socket=Socket,
		transport=Transport, connection=Connection, pid=ReqPid,
		resp_state=waiting, resp_headers=RespHeaders}) ->
	RespConn = response_connection(Headers, Connection),
	Head = response_head(Status, Headers, RespHeaders, [
		{<<"Connection">>, atom_to_connection(Connection)},
		{<<"Transfer-Encoding">>, <<"chunked">>},
		{<<"Date">>, cowboy_clock:rfc1123()},
		{<<"Server">>, <<"Cowboy">>}
	]),
	Transport:send(Socket, Head),
	ReqPid ! {?MODULE, resp_sent},
	{ok, Req#http_req{connection=RespConn, resp_state=chunks,
		resp_headers=[], resp_body= <<>>}}.

%% @doc Send a chunk of data.
%%
%% A chunked reply must have been initiated before calling this function.
-spec chunk(iodata(), #http_req{}) -> ok | {error, atom()}.
chunk(_Data, #http_req{socket=_Socket, transport=_Transport, method='HEAD'}) ->
	ok;
chunk(Data, #http_req{socket=Socket, transport=Transport, resp_state=chunks}) ->
	Transport:send(Socket, [integer_to_list(iolist_size(Data), 16),
		<<"\r\n">>, Data, <<"\r\n">>]).

%% @doc Send an upgrade reply.
%% @private
-spec upgrade_reply(http_status(), http_headers(), #http_req{})
	-> {ok, #http_req{}}.
upgrade_reply(Status, Headers, Req=#http_req{socket=Socket, transport=Transport,
		pid=ReqPid, resp_state=waiting, resp_headers=RespHeaders}) ->
	Head = response_head(Status, Headers, RespHeaders, [
		{<<"Connection">>, <<"Upgrade">>}
	]),
	Transport:send(Socket, Head),
	ReqPid ! {?MODULE, resp_sent},
	{ok, Req#http_req{resp_state=done, resp_headers=[], resp_body= <<>>}}.

%% Misc API.

%% @doc Compact the request data by removing all non-system information.
%%
%% This essentially removes the host, path, query string, bindings and headers.
%% Use it when you really need to save up memory, for example when having
%% many concurrent long-running connections.
-spec compact(#http_req{}) -> #http_req{}.
compact(Req) ->
	Req#http_req{host=undefined, host_info=undefined, path=undefined,
		path_info=undefined, qs_vals=undefined,
		bindings=undefined, headers=[],
		p_headers=[], cookies=[]}.

%% @doc Return the transport module and socket associated with a request.
%%
%% This exposes the same socket interface used internally by the HTTP protocol
%% implementation to developers that needs low level access to the socket.
%%
%% It is preferred to use this in conjuction with the stream function support
%% in `set_resp_body_fun/3' if this is used to write a response body directly
%% to the socket. This ensures that the response headers are set correctly.
-spec transport(#http_req{}) -> {ok, module(), inet:socket()}.
transport(#http_req{transport=Transport, socket=Socket}) ->
	{ok, Transport, Socket}.

%% Internal.

-spec parse_qs(binary(), fun((binary()) -> binary())) ->
		list({binary(), binary() | true}).
parse_qs(<<>>, _URLDecode) ->
	[];
parse_qs(Qs, URLDecode) ->
	Tokens = binary:split(Qs, <<"&">>, [global, trim]),
	[case binary:split(Token, <<"=">>) of
		[Token] -> {URLDecode(Token), true};
		[Name, Value] -> {URLDecode(Name), URLDecode(Value)}
	end || Token <- Tokens].

-spec response_connection(http_headers(), keepalive | close)
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

-spec response_head(http_status(), http_headers(), http_headers(),
	http_headers()) -> iolist().
response_head(Status, Headers, RespHeaders, DefaultHeaders) ->
	StatusLine = <<"HTTP/1.1 ", (status(Status))/binary, "\r\n">>,
	Headers2 = [{header_to_binary(Key), Value} || {Key, Value} <- Headers],
	Headers3 = merge_headers(
		merge_headers(Headers2, RespHeaders),
		DefaultHeaders),
	Headers4 = [[Key, <<": ">>, Value, <<"\r\n">>]
		|| {Key, Value} <- Headers3],
	[StatusLine, Headers4, <<"\r\n">>].

-spec merge_headers(http_headers(), http_headers()) -> http_headers().
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

-spec status(http_status()) -> binary().
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
status(500) -> <<"500 Internal Server Error">>;
status(501) -> <<"501 Not Implemented">>;
status(502) -> <<"502 Bad Gateway">>;
status(503) -> <<"503 Service Unavailable">>;
status(504) -> <<"504 Gateway Timeout">>;
status(505) -> <<"505 HTTP Version Not Supported">>;
status(506) -> <<"506 Variant Also Negotiates">>;
status(507) -> <<"507 Insufficient Storage">>;
status(510) -> <<"510 Not Extended">>;
status(B) when is_binary(B) -> B.

-spec header_to_binary(http_header()) -> binary().
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

parse_qs_test_() ->
	%% {Qs, Result}
	Tests = [
		{<<"">>, []},
		{<<"a=b">>, [{<<"a">>, <<"b">>}]},
		{<<"aaa=bbb">>, [{<<"aaa">>, <<"bbb">>}]},
		{<<"a&b">>, [{<<"a">>, true}, {<<"b">>, true}]},
		{<<"a=b&c&d=e">>, [{<<"a">>, <<"b">>},
			{<<"c">>, true}, {<<"d">>, <<"e">>}]},
		{<<"a=b=c=d=e&f=g">>, [{<<"a">>, <<"b=c=d=e">>}, {<<"f">>, <<"g">>}]},
		{<<"a+b=c+d">>, [{<<"a b">>, <<"c d">>}]}
	],
	URLDecode = fun cowboy_http:urldecode/1,
	[{Qs, fun() -> R = parse_qs(Qs, URLDecode) end} || {Qs, R} <- Tests].

-endif.
