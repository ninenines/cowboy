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

-type http_method() :: 'OPTIONS' | 'GET' | 'HEAD'
	| 'POST' | 'PUT' | 'DELETE' | 'TRACE' | string().
-type http_uri() :: '*' | {absoluteURI, http | https, Host::string(),
	Port::integer() | undefined, Path::string()}
	| {scheme, Scheme::string(), string()}
	| {abs_path, string()} | string().
-type http_version() :: {Major::integer(), Minor::integer()}.
-type http_header() :: 'Cache-Control' | 'Connection' | 'Date' | 'Pragma'
	| 'Transfer-Encoding' | 'Upgrade' | 'Via' | 'Accept' | 'Accept-Charset'
	| 'Accept-Encoding' | 'Accept-Language' | 'Authorization' | 'From' | 'Host'
	| 'If-Modified-Since' | 'If-Match' | 'If-None-Match' | 'If-Range'
	| 'If-Unmodified-Since' | 'Max-Forwards' | 'Proxy-Authorization' | 'Range'
	| 'Referer' | 'User-Agent' | 'Age' | 'Location' | 'Proxy-Authenticate'
	| 'Public' | 'Retry-After' | 'Server' | 'Vary' | 'Warning'
	| 'Www-Authenticate' | 'Allow' | 'Content-Base' | 'Content-Encoding'
	| 'Content-Language' | 'Content-Length' | 'Content-Location'
	| 'Content-Md5' | 'Content-Range' | 'Content-Type' | 'Etag'
	| 'Expires' | 'Last-Modified' | 'Accept-Ranges' | 'Set-Cookie'
	| 'Set-Cookie2' | 'X-Forwarded-For' | 'Cookie' | 'Keep-Alive'
	| 'Proxy-Connection' | string().
-type http_headers() :: list({http_header(), string()}).
%% -type http_cookies() :: term(). %% @todo
-type http_status() :: non_neg_integer() | string().

-record(http_req, {
	%% Transport.
	socket     = undefined :: undefined | inet:socket(),
	transport  = undefined :: undefined | module(),
	connection = keepalive :: keepalive | close,

	%% Request.
	method     = 'GET'     :: http_method(),
	version    = {1, 1}    :: http_version(),
	peer       = undefined :: undefined | {Address::inet:ip_address(), Port::port_number()},
	host       = undefined :: undefined | path_tokens(),
	raw_host   = undefined :: undefined | string(),
	path       = undefined :: undefined | '*' | path_tokens(),
	raw_path   = undefined :: undefined | string(),
	qs_vals    = undefined :: undefined | list({Name::string(), Value::string() | true}),
	raw_qs     = undefined :: undefined | string(),
	bindings   = undefined :: undefined | bindings(),
	headers    = []        :: http_headers(),
%%	cookies    = undefined :: undefined | http_cookies() %% @todo

	%% Request body.
	body_state = waiting   :: waiting | done,

	%% Response.
	resp_state = locked    :: locked | waiting | done
}).
