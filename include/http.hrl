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

-type http_method() :: 'OPTIONS' | 'GET' | 'HEAD'
	| 'POST' | 'PUT' | 'DELETE' | 'TRACE' | binary().
-type http_uri() :: '*' | {absoluteURI, http | https, Host::binary(),
	Port::integer() | undefined, Path::binary()}
	| {scheme, Scheme::binary(), binary()}
	| {abs_path, binary()} | binary().
-type http_version() :: {Major::non_neg_integer(), Minor::non_neg_integer()}.
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
	| 'Proxy-Connection' | binary().
-type http_headers() :: list({http_header(), iodata()}).
-type http_cookies() :: list({binary(), binary()}).
-type http_status() :: non_neg_integer() | binary().
-type http_resp_body() :: iodata() | {non_neg_integer(),
		fun(() -> {sent, non_neg_integer()})}.

-record(http_req, {
	%% Transport.
	socket     = undefined :: undefined | inet:socket(),
	transport  = undefined :: undefined | module(),
	connection = keepalive :: keepalive | close,

	%% Request.
	pid        = undefined :: pid(),
	method     = 'GET'     :: http_method(),
	version    = {1, 1}    :: http_version(),
	peer       = undefined :: undefined | {inet:ip_address(), inet:ip_port()},
	host       = undefined :: undefined | cowboy_dispatcher:tokens(),
	host_info  = undefined :: undefined | cowboy_dispatcher:tokens(),
	raw_host   = undefined :: undefined | binary(),
	port       = undefined :: undefined | inet:ip_port(),
	path       = undefined :: undefined | '*' | cowboy_dispatcher:tokens(),
	path_info  = undefined :: undefined | cowboy_dispatcher:tokens(),
	raw_path   = undefined :: undefined | binary(),
	qs_vals    = undefined :: undefined | list({binary(), binary() | true}),
	raw_qs     = undefined :: undefined | binary(),
	bindings   = undefined :: undefined | cowboy_dispatcher:bindings(),
	headers    = []        :: http_headers(),
	p_headers  = []        :: [any()], %% @todo Improve those specs.
	cookies    = undefined :: undefined | http_cookies(),
	meta       = []        :: [{atom(), any()}],

	%% Request body.
	body_state = waiting   :: waiting | done,
	buffer     = <<>>      :: binary(),

	%% Response.
	resp_state = waiting   :: locked | waiting | chunks | done,
	resp_headers = []      :: http_headers(),
	resp_body  = <<>>      :: http_resp_body(),

	%% Functions.
	urldecode :: {fun((binary(), T) -> binary()), T}
}).
