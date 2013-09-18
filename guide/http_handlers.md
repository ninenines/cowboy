Handling plain HTTP requests
============================

The simplest way to handle a request is by writing a
plain HTTP handler. It is modeled after Erlang/OTP's
gen_server behaviour, although simplified, as Cowboy
will simply call the three callbacks sequentially.

Initialization
--------------

The first callback, `init/3`, is common to all handlers,
as it is used to identify the type of handler. Plain
HTTP handlers just return `ok`.

``` erlang
init(_Type, Req, _Opts) ->
    {ok, Req, no_state}.
```

This function receives the name of the transport and
protocol modules used for processing the request.
They can be used to quickly dismiss requests. For
example the following handler will crash when accessed
using TCP instead of SSL.

``` erlang
init({ssl, _}, Req, _Opts) ->
    {ok, Req, no_state}.
```

This function also receives the options associated with
this route that you configured previously. If your
handler does not use options, then it is recommended
you match the value `[]` directly to quickly detect
configuration errors.

``` erlang
init(_Type, Req, []) ->
    {ok, Req, no_state}.
```

You do not need to validate the options unless they
are user configured. If they are, and there's a
configuration error, you may choose to crash. For
example, this will crash if the required `lang`
option is not found.

``` erlang
init(_Type, Req, Opts) ->
    {_, _Lang} = lists:keyfind(lang, 1, Opts),
    {ok, Req, no_state}.
```

If your users are unlikely to figure out the issue
without explanations, then you should send a more
meaningful error back to the user. Since we already
replied to the user, there's no need for us to
continue with the handler code, so we use the
`shutdown` return value to stop early.

``` erlang
init(_Type, Req, Opts) ->
    case lists:keyfind(lang, 1, Opts) of
        false ->
            {ok, Req2} = cowboy_req:reply(500, [
                {<<"content-type">>, <<"text/plain">>}
            ], "Missing option 'lang'.", Req),
            {shutdown, Req2, no_state};
        _ ->
            {ok, Req, no_state}
    end.
```

Once the options have been validated, we can use them
safely. So we need to pass them onward to the rest of
the handler. That's what the third element of the return
tuple, the state, is for.

We recommend that you create a state record for this.
The record will make your handler code clearer and
will allow you to better use Dialyzer for type checking.

``` erlang
-record(state, {
    lang :: en | fr
    %% More fields here.
}).

init(_Type, Req, Opts) ->
    {_, Lang} = lists:keyfind(lang, 1, Opts),
    {ok, Req, #state{lang=Lang}}.
```

Handling the request
--------------------

The second callback, `handle/2`, is specific to plain HTTP
handlers. It's where you, wait for it, handle the request.

A handle function that does nothing would look like this:

``` erlang
handle(Req, State) ->
    {ok, Req, State}.
```

There's no other return value. To obtain information about
the request, or send a response, you would use the Req object
here. The Req object is documented in its own chapter.

The following handle function will send a fairly original response.

``` erlang
handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], <<"Hello World!">>, Req),
    {ok, Req2, State}.
```

Cleaning up
-----------

The third and last callback, `terminate/3`, will most likely
be empty in your handler.

``` erlang
terminate(_Reason, Req, State) ->
    ok.
```

This callback is strictly reserved for any required cleanup.
You cannot send a response from this function. There is no
other return value.

If you used the process dictionary, timers, monitors or may
be receiving messages, then you can use this function to clean
them up, as Cowboy might reuse the process for the next
keep-alive request.

The chances of any of this happening in your handler are pretty
thin however. The use of the process dictionary is discouraged
in Erlang code in general. And if you need to use timers, monitors
or to receive messages, you are better off with a loop handler,
a different kind of handler meant specifically for this use.

This function is still available should you need it. It will
always be called.
