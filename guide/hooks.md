Hooks
=====

On request
----------

The `onrequest` hook is called as soon as Cowboy finishes fetching
the request headers. It occurs before any other processing, including
routing. It can be used to perform any modification needed on the
request object before continuing with the processing. If a reply is
sent inside this hook, then Cowboy will move on to the next request,
skipping any subsequent handling.

This hook is a function that takes a request object as argument,
and returns a request object. This function MUST NOT crash. Cowboy
will not send any reply if a crash occurs in this function.

You can specify the `onrequest` hook when creating the listener,
inside the request options.

``` erlang
cowboy:start_http(my_http_listener, 100,
    [{port, 8080}],
    [
        {env, [{dispatch, Dispatch}]},
        {onrequest, fun ?MODULE:debug_hook/1}
    ]
).
```

The following hook function prints the request object everytime a
request is received. This can be useful for debugging, for example.

``` erlang
debug_hook(Req) ->
    erlang:display(Req),
    Req.
```

Make sure to always return the last request object obtained.

On response
-----------

The `onresponse` hook is called right before sending the response
to the socket. It can be used for the purposes of logging responses,
or for modifying the response headers or body. The best example is
providing custom error pages.

Note that like the `onrequest` hook, this function MUST NOT crash.
Cowboy may or may not send a reply if this function crashes. If a reply
is sent, the hook MUST explicitly provide all headers that are needed.

You can specify the `onresponse` hook when creating the listener also.

``` erlang
cowboy:start_http(my_http_listener, 100,
    [{port, 8080}],
    [
        {env, [{dispatch, Dispatch}]},
        {onresponse, fun ?MODULE:custom_404_hook/4}
    ]
).
```

The following hook function will provide a custom body for 404 errors
when it has not been provided before, and will let Cowboy proceed with
the default response otherwise.

``` erlang
custom_404_hook(404, Headers, <<>>, Req) ->
    Body = <<"404 Not Found.">>,
    Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
        {<<"content-length">>, integer_to_list(byte_size(Body))}),
    {ok, Req2} = cowboy_req:reply(404, Headers2, Body, Req),
    Req2;
custom_404_hook(_, _, _, Req) ->
    Req.
```

Again, make sure to always return the last request object obtained.
