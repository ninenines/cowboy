Routing
=======

@todo Note that this documentation is for the new routing interface
not available in master at this point.

Purpose
-------

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

```
Path1 = {PathMatch, Handler, Module}.
Path2 = {PathMatch, Constraints, Handler, Module}.
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

``` erlang
HostMatch1 = "cowboy.example.org".
HostMatch2 = "cowboy.example.org.".
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
bindings defined, `subdomain` and `hat_name`, each containing the
segment value where they were defined. For example, the URL
`http://test.example.org/hats/wild_cowboy_legendary/prices` will
result in having the value `test` bound to the name `subdomain`
and the value `wild_cowboy_legendary` bound to the name `hat_name`.
They can later be retrieved using `cowboy_req:binding/{2,3}`.

@todo special binding `'_'`
@todo optional path or segments
@todo same binding twice (+ optional + host/path)

Constraints
-----------

@todo Describe constraints.

Compilation
-----------

The structure defined in this chapter needs to be compiled before it is
passed to Cowboy. This allows Cowboy to efficiently lookup the correct
handler to run instead of having to parse the routes repeatedly.

This can be done with a simple call to `cowboy_routing:compile/1`.

@todo Note that the `routes` option will be specified slightly differently
when middleware support gets in.

``` erlang
{ok, Routes} = cowboy_routing:compile([
    %% {URIHost, list({URIPath, Handler, Opts})}
    {'_', [{'_', my_handler, []}]}
]),
%% Name, NbAcceptors, TransOpts, ProtoOpts
cowboy:start_http(my_http_listener, 100,
    [{port, 8080}],
    [{routes, Routes}]
).
```

Note that this function will return `{error, badarg}` if the structure
given is incorrect.
