REST handlers
=============

Purpose
-------

REST is a set of constraints that, when applied to HTTP, dictates how
resources must behave. It is the recommended way to handle requests
with Cowboy.

REST is implemented in Cowboy as a protocol upgrade. Once upgraded,
the request is handled as a state machine with many optional callbacks
describing the resource and modifying the machine's behavior.

As the REST handler is still subject to change, the documentation is
still thin. This state of affair will be improved in the coming weeks.

Usage
-----

Like Websocket, REST is a sub-protocol of HTTP. It therefore
requires a protocol upgrade.

``` erlang
init({tcp, http}, Req, Opts) ->
    {upgrade, protocol, cowboy_rest}.
```

Cowboy will then switch to the REST protocol and start executing
the flow diagram, starting from `rest_init/2` if it's defined,
and ending with `rest_terminate/2` also if defined.

Flow diagram
------------

Not done yet. Feel free to use the one that is currently being worked on.

 *  https://github.com/extend/cowboy/pull/364

Methods
-------

The REST component has code for handling the following HTTP methods:
HEAD, GET, POST, PATCH, PUT, DELETE and OPTIONS.

Other methods can be accepted, however they have no specific callback
defined for them at this time.

Callbacks
---------

All callbacks are optional. Some may become mandatory depending
on what other defined callbacks return. The flow diagram should
be a pretty good resource to determine which callbacks you need.

When the request starts being processed, Cowboy will call the
`rest_init/2` function if it is defined, with the Req object
and the handler options as arguments. This function must return
`{ok, Req, State}` where `State` is the handler's state that all
subsequent callbacks will receive.

At the end of every request, the special callback `rest_terminate/2`
will be called if it is defined. It cannot be used to send a reply,
and must always return `ok`.

All other callbacks are resource callbacks. They all take two
arguments, the Req object and the State, and return a three-element
tuple of the form `{Value, Req, State}`.

The following table summarizes the callbacks and their default values.
If the callback isn't defined, then the default value will be used.
Please look at the flow diagram to find out the result of each return
value.

All callbacks can also return `{halt, Req, State}` to stop execution
of the request, at which point `rest_terminate/2` will be called.

In the following table, "skip" means the callback is entirely skipped
if it is undefined, moving directly to the next step. Similarly, an
empty column means there is no default value for this callback.

| Callback name          | Default value             |
| ---------------------- | ------------------------- |
| allowed_methods        | `[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>]` |
| allow_missing_post     | `true`                    |
| charsets_provided      | skip                      |
| content_types_accepted |                           |
| content_types_provided | `[{{<<"text">>, <<"html">>, '*'}, to_html}] ` |
| delete_completed       | `true`                    |
| delete_resource        | `false`                   |
| expires                | `undefined`               |
| forbidden              | `false`                   |
| generate_etag          | `undefined`               |
| is_authorized          | `true`                    |
| is_conflict            | `false`                   |
| known_content_type     | `true`                    |
| known_methods          | `[<<"GET">>, <<"HEAD">>, <<"POST">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>]` |
| languages_provided     | skip                      |
| last_modified          | `undefined`               |
| malformed_request      | `false`                   |
| moved_permanently      | `false`                   |
| moved_temporarily      | `false`                   |
| multiple_choices       | `false`                   |
| options                | `ok`                      |
| previously_existed     | `false`                   |
| resource_exists        | `true`                    |
| service_available      | `true`                    |
| uri_too_long           | `false`                   |
| valid_content_headers  | `true`                    |
| valid_entity_length    | `true`                    |
| variances              | `[]`                      |

As you can see, Cowboy tries to move on with the request whenever
possible by using well thought out default values.

In addition to these, there can be any number of user-defined
callbacks that are specified through `content_types_accepted/2`
and `content_types_provided/2`. They can take any name, however
it is recommended to use a separate prefix for the callbacks of
each function. For example, `from_html` and `to_html` indicate
in the first case that we're accepting a resource given as HTML,
and in the second case that we send one as HTML.

Meta data
---------

Cowboy will set informative meta values at various points of the
execution. You can retrieve them using `cowboy_req:meta/{2,3}`.
The values are defined in the following table.

| Meta key   | Details                                              |
| -----------| ---------------------------------------------------- |
| media_type | The content-type negotiated for the response entity. |
| language   | The language negotiated for the response entity.     |
| charset    | The charset negotiated for the response entity.      |

They can be used to send a proper body with the response to a
request that used a method other than HEAD or GET.

Response headers
----------------

Cowboy will set response headers automatically over the execution
of the REST code. They are listed in the following table.

| Header name      | Details                                            |
| ---------------- | -------------------------------------------------- |
| content-language | Language used in the response body                 |
| content-type     | Media type and charset of the response body        |
| etag             | Etag of the resource                               |
| expires          | Expiration date of the resource                    |
| last-modified    | Last modification date for the resource            |
| location         | Relative or absolute URI to the requested resource |
| vary             | List of headers that may change the representation of the resource |
