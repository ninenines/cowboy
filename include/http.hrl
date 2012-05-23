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

-record(http_req, {
	%% Transport.
	socket     = undefined :: undefined | inet:socket(),
	transport  = undefined :: undefined | module(),
	connection = keepalive :: keepalive | close,

	%% Request.
	pid        = undefined :: pid(),
	method     = 'GET'     :: cowboy_http:method(),
	version    = {1, 1}    :: cowboy_http:version(),
	peer       = undefined :: undefined |
								{inet:ip_address(), inet:port_number()},
	host       = undefined :: undefined | cowboy_dispatcher:tokens(),
	host_info  = undefined :: undefined | cowboy_dispatcher:tokens(),
	raw_host   = undefined :: undefined | binary(),
	port       = undefined :: undefined | inet:port_number(),
	path       = undefined :: undefined | '*' | cowboy_dispatcher:tokens(),
	path_info  = undefined :: undefined | cowboy_dispatcher:tokens(),
	raw_path   = undefined :: undefined | binary(),
	qs_vals    = undefined :: undefined | list({binary(), binary() | true}),
	raw_qs     = undefined :: undefined | binary(),
	bindings   = undefined :: undefined | cowboy_dispatcher:bindings(),
	headers    = []        :: cowboy_http:headers(),
	p_headers  = []        :: [any()], %% @todo Improve those specs.
	cookies    = undefined :: undefined | [{binary(), binary()}],
	meta       = []        :: [{atom(), any()}],

	%% Request body.
	body_state = waiting   :: waiting | done | {stream, fun(), any(), fun()}
								| {multipart, non_neg_integer(), fun()},
	buffer     = <<>>      :: binary(),

	%% Response.
	resp_state = waiting   :: locked | waiting | chunks | done,
	resp_headers = []      :: cowboy_http:headers(),
	resp_body  = <<>>      :: iodata() | {non_neg_integer(),
								fun(() -> {sent, non_neg_integer()})},

	%% Functions.
	onresponse = undefined :: undefined | fun((cowboy_http:status(),
		cowboy_http:headers(), #http_req{}) -> #http_req{}),
	urldecode :: {fun((binary(), T) -> binary()), T}
}).
