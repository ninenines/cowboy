cowboy_handler
==============

The `cowboy_handler` middleware executes the handler passed
through the environment values `handler` and `handler_opts`,
and add the result of this execution to the environment as
the value `result`, indicating that the request has been
handled and received a response.

Environment input:
 *  handler = module()
 *  handler_opts = any()

Environment output:
 *  result = ok

Types
-----

None.

Exports
-------

None.
