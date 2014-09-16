cowboy_spdy
===========

The `cowboy_spdy` module implements SPDY/3 as a Ranch protocol.

Types
-----

### opts() = [{env, cowboy_middleware:env()}
	| {idle_timeout, non_neg_integer()}
	| {middlewares, [module()]}
	| {onrequest, cowboy:onrequest_fun()}
	| {onresponse, cowboy:onresponse_fun()}
	| {ping_interval, non_neg_integer()}]

> Configuration for the SPDY protocol handler.
>
> This configuration is passed to Cowboy when starting listeners
> using the `cowboy:start_spdy/4` function.
>
> It can be updated without restarting listeners using the
> Ranch functions `ranch:get_protocol_options/1` and
> `ranch:set_protocol_options/2`.

Option descriptions
-------------------

The default value is given next to the option name.

 -  env ([{listener, Ref}])
   -  Initial middleware environment.
 -  idle_timeout (60000)
   -  Milliseconds before idle connections are closed.
 -  middlewares ([cowboy_router, cowboy_handler])
   -  List of middlewares to execute for every requests.
 -  onrequest (undefined)
   -  Fun called every time a request is received.
 -  onresponse (undefined)
   -  Fun called every time a response is sent.
 -  ping_interval (undefined)
   - Milliseconds between PING frames sent to the client.

Exports
-------

None.
