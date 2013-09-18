Routing
=======

Cowboy does nothing by default.

To make Cowboy useful, you need to map URLs to Erlang modules that will
handle the requests. This is called routing.

When Cowboy receives a request, it tries to match the requested host and
path to the resources given in the dispatch rules. If it matches, then
the associated Erlang code will be executed.

Routing rules are given per host. Cowboy will first match on the host,
and then try to find a matching path.

Routes need to be compiled before they can be used by Cowboy.

Structure
---------

The general structure for the routes is defined as follow.

``` erlang
Routes = [Host1, Host2, ... HostN].
```

Each host contains matching rules for the host along with optional
constraints, and a list of routes for the path component.

``` erlang
Host1 = {HostMatch, PathsList}.
Host2 = {HostMatch, Constraints, PathsList}.
```

The list of routes for the path component is defined similar to the
list of hosts.

``` erlang
PathsList = [Path1, Path2, ... PathN].
```

Finally, each path contains matching rules for the path along with
optional constraints, and gives us the handler module to be used
along with options that will be given to it on initialization.

``` erlang
Path1 = {PathMatch, Handler, Opts}.
Path2 = {PathMatch, Constraints, Handler, Opts}.
```

Continue reading to learn more about the match syntax and the optional
constraints.

Match syntax
------------

The match syntax is used to associate host names and paths with their
respective handlers.

The match syntax is the same for host and path with a few subtleties.
Indeed, the segments separator is different, and the host is matched
starting from the last segment going to the first. All examples will
feature both host and path match rules and explain the differences
when encountered.

Excluding special values that we will explain at the end of this section,
the simplest match value is a host or a path. It can be given as either
a `string()` or a `binary()`.

``` erlang
PathMatch1 = "/".
PathMatch2 = "/path/to/resource".

HostMatch1 = "cowboy.example.org".
```

As you can see, all paths defined this way must start with a slash
character. Note that these two paths are identical as far as routing
is concerned.

``` erlang
PathMatch2 = "/path/to/resource".
PathMatch3 = "/path/to/resource/".
```

Hosts with and without a trailing dot are equivalent for routing.
Similarly, hosts with and without a leading dot are also equivalent.

``` erlang
HostMatch1 = "cowboy.example.org".
HostMatch2 = "cowboy.example.org.".
HostMatch3 = ".cowboy.example.org".
```

It is possible to extract segments of the host and path and to store
the values in the `Req` object for later use. We call these kind of
values bindings.

The syntax for bindings is very simple. A segment that begins with
the `:` character means that what follows until the end of the segment
is the name of the binding in which the segment value will be stored.

``` erlang
PathMatch = "/hats/:name/prices".
HostMatch = ":subdomain.example.org".
```

If these two end up matching when routing, you will end up with two
bindings defined, `subdomain` and `name`, each containing the
segment value where they were defined. For example, the URL
`http://test.example.org/hats/wild_cowboy_legendary/prices` will
result in having the value `test` bound to the name `subdomain`
and the value `wild_cowboy_legendary` bound to the name `name`.
They can later be retrieved using `cowboy_req:binding/{2,3}`. The
binding name must be given as an atom.

There is a special binding name you can use to mimic the underscore
variable in Erlang. Any match against the `_` binding will succeed
but the data will be discarded. This is especially useful for
matching against many domain names in one go.

``` erlang
HostMatch = "ninenines.:_".
```

Similarly, it is possible to have optional segments. Anything
between brackets is optional.

``` erlang
PathMatch = "/hats/[page/:number]".
HostMatch = "[www.]ninenines.eu".
```

You can also have imbricated optional segments.

``` erlang
PathMatch = "/hats/[page/[:number]]".
```

You can retrieve the rest of the host or path using `[...]`.
In the case of hosts it will match anything before, in the case
of paths anything after the previously matched segments. It is
a special case of optional segments, in that it can have
zero, one or many segments. You can then find the segments using
`cowboy_req:host_info/1` and `cowboy_req:path_info/1` respectively.
They will be represented as a list of segments.

``` erlang
PathMatch = "/hats/[...]".
HostMatch = "[...]ninenines.eu".
```

If a binding appears twice in the routing rules, then the match
will succeed only if they share the same value. This copies the
Erlang pattern matching behavior.

``` erlang
PathMatch = "/hats/:name/:name".
```

This is also true when an optional segment is present. In this
case the two values must be identical only if the segment is
available.

``` erlang
PathMatch = "/hats/:name/[:name]".
```

If a binding is defined in both the host and path, then they must
also share the same value.

``` erlang
PathMatch = "/:user/[...]".
HostMatch = ":user.github.com".
```

Finally, there are two special match values that can be used. The
first is the atom `'_'` which will match any host or path.

``` erlang
PathMatch = '_'.
HostMatch = '_'.
```

The second is the special host match `"*"` which will match the
wildcard path, generally used alongside the `OPTIONS` method.

``` erlang
HostMatch = "*".
```

Constraints
-----------

After the matching has completed, the resulting bindings can be tested
against a set of constraints. Constraints are only tested when the
binding is defined. They run in the order you defined them. The match
will succeed only if they all succeed.

They are always given as a two or three elements tuple, where the first
element is the name of the binding, the second element is the constraint's
name, and the optional third element is the constraint's arguments.

The following constraints are currently defined:

 *  {Name, int}
 *  {Name, function, fun ((Value) -> true | {true, NewValue} | false)}

The `int` constraint will check if the binding is a binary string
representing an integer, and if it is, will convert the value to integer.

The `function` constraint will pass the binding value to a user specified
function that receives the binary value as its only argument and must
return whether it fulfills the constraint, optionally modifying the value.
The value thus returned can be of any type.

Note that constraint functions SHOULD be pure and MUST NOT crash.

Compilation
-----------

The structure defined in this chapter needs to be compiled before it is
passed to Cowboy. This allows Cowboy to efficiently lookup the correct
handler to run instead of having to parse the routes repeatedly.

This can be done with a simple call to `cowboy_router:compile/1`.

``` erlang
Dispatch = cowboy_router:compile([
    %% {HostMatch, list({PathMatch, Handler, Opts})}
    {'_', [{'_', my_handler, []}]}
]),
%% Name, NbAcceptors, TransOpts, ProtoOpts
cowboy:start_http(my_http_listener, 100,
    [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
).
```

Note that this function will return `{error, badarg}` if the structure
given is incorrect.

Live update
-----------

You can use the `cowboy:set_env/3` function for updating the dispatch
list used by routing. This will apply to all new connections accepted
by the listener.

``` erlang
cowboy:set_env(my_http_listener, dispatch,
    cowboy_router:compile(Dispatch)).
```

Note that you need to compile the routes before updating.
