cowboy_router
=============

The `cowboy_router` middleware maps the requested host and
path to the handler to be used for processing the request.
It uses the dispatch rules compiled from the routes given
to the `compile/1` function for this purpose. It adds the
handler name and options to the environment as the values
`handler` and `handler_opts` respectively.

Environment input:
 *  dispatch = dispatch_rules()

Environment output:
 *  handler = module()
 *  handler_opts = any()

Types
-----

### bindings() = [{atom(), binary()}]

> List of bindings found during routing.

### constraints() = [IntConstraint | FunConstraint]

> Types:
>  *  IntConstraint = {atom(), int}
>  *  FunConstraint = {atom(), function, Fun}
>  *  Fun = fun((binary()) -> true | {true, any()} | false)
>
> List of constraints to apply to the bindings.
>
> The int constraint will convert the binding to an integer.
> The fun constraint allows writing custom code for checking
> the bindings. Returning a new value from that fun allows
> replacing the current binding with a new value.

### dispatch_rules() - opaque to the user

> Rules for dispatching request used by Cowboy.

### routes() = [{Host, Paths} | {Host, constraints(), Paths}]

> Types:
>  *  Host = Path = '_' | iodata()
>  *  Paths = [{Path, Handler, Opts} | {Path, constraints(), Handler, Opts}]
>  *  Handler = module()
>  *  Opts = any()
>
> Human readable list of routes mapping hosts and paths to handlers.
>
> The syntax for routes is defined in the user guide.

### tokens() = [binary()]

> List of host_info and path_info tokens found during routing.

Exports
-------

### compile(Routes) -> Dispatch

> Types:
>  *  Routes = routes()
>  *  Dispatch = dispatch_rules()
>
> Compile the routes for use by Cowboy.
