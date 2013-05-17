cowboy_protocol
===============

The `cowboy_protocol` module implements HTTP/1.1 and HTTP/1.0 
as a Ranch protocol.

Types
-----

### opts() = [{compress, boolean()}
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
	| {timeout, timeout()}]

> Configuration for the HTTP protocol handler.
>
> This configuration is passed to Cowboy when starting listeners
> using `cowboy:start_http/4` or `cowboy:start_https/4` functions.
>
> It can be updated without restarting listeners using the
> Ranch functions `ranch:get_protocol_options/1` and
> `ranch:set_protocol_options/2`.

Option descriptions
-------------------

The default value is given next to the option name.

 -  compress (false)
   -  When enabled, Cowboy will attempt to compress the response body.
 -  env ([{listener, Ref}])
   -  Initial middleware environment.
 -  max_empty_lines (5)
   -  Maximum number of empty lines before a request.
 -  max_header_name_length (64)
   -  Maximum length of header names.
 -  max_header_value_length (4096)
   -  Maximum length of header values.
 -  max_headers (100)
   -  Maximum number of headers allowed per request.
 -  max_keepalive (100)
   -  Maximum number of requests allowed per connection.
 -  max_request_line_length (4096)
   -  Maximum length of the request line.
 -  middlewares ([cowboy_router, cowboy_handler])
   -  List of middlewares to execute for every requests.
 -  onrequest (undefined)
   -  Fun called every time a request is received.
 -  onresponse (undefined)
   -  Fun called every time a response is sent.
 -  timeout (5000)
   -  Time in ms with no requests before Cowboy closes the connection.

Exports
-------

None.
