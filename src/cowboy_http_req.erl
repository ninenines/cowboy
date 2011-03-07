%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
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

-module(cowboy_http_req).
-export([
	listener/1, method/1, version/1, peer/1,
	host/1, raw_host/1,
	path/1, raw_path/1,
	qs_val/2, qs_val/3, qs_vals/1, raw_qs/1,
	binding/2, binding/3, bindings/1,
	header/2, header/3, headers/1
%%	cookie/2, cookie/3, cookies/1 @todo
]). %% API.

-include("include/types.hrl").
-include("include/http.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API.

-spec listener(Req::#http_req{}) -> {Listener::atom(), Req::#http_req{}}.
listener(Req) ->
	{Req#http_req.listener, Req}.

-spec method(Req::#http_req{}) -> {Method::http_method(), Req::#http_req{}}.
method(Req) ->
	{Req#http_req.method, Req}.

-spec version(Req::#http_req{}) -> {Version::http_version(), Req::#http_req{}}.
version(Req) ->
	{Req#http_req.version, Req}.

-spec peer(Req::#http_req{})
	-> {{Address::ip_address(), Port::port_number()}, Req::#http_req{}}.
peer(Req) ->
	{Req#http_req.peer, Req}.

-spec host(Req::#http_req{}) -> {Host::path_tokens(), Req::#http_req{}}.
host(Req) ->
	{Req#http_req.host, Req}.

-spec raw_host(Req::#http_req{}) -> {RawHost::string(), Req::#http_req{}}.
raw_host(Req) ->
	{Req#http_req.raw_host, Req}.

-spec path(Req::#http_req{}) -> {Path::path_tokens(), Req::#http_req{}}.
path(Req) ->
	{Req#http_req.path, Req}.

-spec raw_path(Req::#http_req{}) -> {RawPath::string(), Req::#http_req{}}.
raw_path(Req) ->
	{Req#http_req.raw_path, Req}.

-spec qs_val(Name::atom(), Req::#http_req{})
	-> {Value::string(), Req::#http_req{}}.
qs_val(Name, Req=#http_req{raw_qs=RawQs, qs_vals=undefined}) ->
	QsVals = parse_qs(RawQs),
	qs_val(Name, Req#http_req{qs_vals=QsVals});
qs_val(Name, Req) ->
	{Name, Value} = lists:keyfind(Name, 1, Req#http_req.qs_vals),
	{Value, Req}.

-spec qs_val(Name::atom(), Default::term(), Req::#http_req{})
	-> {Value::string() | term(), Req::#http_req{}}.
qs_val(Name, Default, Req=#http_req{raw_qs=RawQs, qs_vals=undefined}) ->
	QsVals = parse_qs(RawQs),
	qs_val(Name, Default, Req#http_req{qs_vals=QsVals});
qs_val(Name, Default, Req) ->
	Value = proplists:get_value(Name, Req#http_req.qs_vals, Default),
	{Value, Req}.

-spec qs_vals(Req::#http_req{}) -> list({Name::atom(), Value::string()}).
qs_vals(Req=#http_req{raw_qs=RawQs, qs_vals=undefined}) ->
	QsVals = parse_qs(RawQs),
	qs_vals(Req#http_req{qs_vals=QsVals});
qs_vals(Req=#http_req{qs_vals=QsVals}) ->
	{QsVals, Req}.

-spec raw_qs(Req::#http_req{}) -> {RawQs::string(), Req::#http_req{}}.
raw_qs(Req) ->
	{Req#http_req.raw_qs, Req}.

-spec binding(Name::atom(), Req::#http_req{})
	-> {Value::string(), Req::#http_req{}}.
binding(Name, Req) ->
	{Name, Value} = lists:keyfind(Name, 1, Req#http_req.bindings),
	{Value, Req}.

-spec binding(Name::atom(), Default::term(), Req::#http_req{})
	-> {Value::string() | term(), Req::#http_req{}}.
binding(Name, Default, Req) ->
	Value = proplists:get_value(Name, Req#http_req.bindings, Default),
	{Value, Req}.

-spec bindings(Req::#http_req{})
	-> {list({Name::atom(), Value::string()}), Req::#http_req{}}.
bindings(Req) ->
	{Req#http_req.bindings, Req}.

-spec header(Name::atom() | string(), Req::#http_req{})
	-> {Value::string(), Req::#http_req{}}.
header(Name, Req) ->
	{Name, Value} = lists:keyfind(Name, 1, Req#http_req.headers),
	{Value, Req}.

-spec header(Name::atom() | string(), Default::term(), Req::#http_req{})
	-> {Value::string() | term(), Req::#http_req{}}.
header(Name, Default, Req) ->
	Value = proplists:get_value(Name, Req#http_req.headers, Default),
	{Value, Req}.

-spec headers(Req::#http_req{})
	-> {list({Name::atom() | string(), Value::string()}), Req::#http_req{}}.
headers(Req) ->
	{Req#http_req.headers, Req}.

%% Internal.

-spec parse_qs(Qs::string()) -> list({Name::string(), Value::string()}).
parse_qs(Qs) ->
	Tokens = string:tokens(Qs, "&"),
	[case string:chr(Token, $=) of
		0 ->
			{Token, true};
		N ->
			{Name, [$=|Value]} = lists:split(N - 1, Token),
			{Name, Value}
	end || Token <- Tokens].

%% Tests.

-ifdef(TEST).

parse_qs_test_() ->
	%% {Qs, Result}
	Tests = [
		{"", []},
		{"a=b", [{"a", "b"}]},
		{"aaa=bbb", [{"aaa", "bbb"}]},
		{"a&b", [{"a", true}, {"b", true}]},
		{"a=b&c&d=e", [{"a", "b"}, {"c", true}, {"d", "e"}]},
		{"a=b=c=d=e&f=g", [{"a", "b=c=d=e"}, {"f", "g"}]}
	],
	[{Qs, fun() -> R = parse_qs(Qs) end} || {Qs, R} <- Tests].

-endif.
