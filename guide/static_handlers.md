Static handler
==============

The static handler is a built-in REST handler for serving files.
It is available as a convenience and provides a quick solution
for serving files during development.

For systems in production, consider using one of the many
Content Distribution Network (CDN) available on the market,
as they are the best solution for serving files. They are
covered in the next chapter. If you decide against using a
CDN solution, then please look at the chapter after that,
as it explains how to efficiently serve static files on
your own.

The static handler can serve either one file or all files
from a given directory. It can also send etag headers for
client-side caching.

To use the static file handler, simply add routes for it
with the appropriate options.

Serve one file
--------------

You can use the static handler to serve one specific file
from an application's private directory. This is particularly
useful to serve an `index.html` file when the client requests
the `/` path, for example. The path configured is relative
to the given application's private directory.

The following rule will serve the file `static/index.html`
from the application `my_app`'s priv directory whenever the
path `/` is accessed.

``` erlang
{"/", cowboy_static, {priv_file, my_app, "static/index.html"}}
```

You can also specify the absolute path to a file, or the
path to the file relative to the current directory.

``` erlang
{"/", cowboy_static, {file, "/var/www/index.html"}}
```

Serve all files from a directory
--------------------------------

You can also use the static handler to serve all files that
can be found in the configured directory. The handler will
use the `path_info` information to resolve the file location,
which means that your route must end with a `[...]` pattern
for it to work. All files are served, including the ones that
may be found in subfolders.

You can specify the directory relative to an application's
private directory.

The following rule will serve any file found in the application
`my_app`'s priv directory inside the `static/assets` folder
whenever the requested path begins with `/assets/`.

``` erlang
{"/assets/[...]", cowboy_static, {priv_dir, my_app, "static/assets"}}
```

You can also specify the absolute path to the directory or
set it relative to the current directory.

``` erlang
{"/assets/[...]", cowboy_static, {dir, "/var/www/assets"}}
```

Customize the mimetype detection
--------------------------------

By default, Cowboy will attempt to recognize the mimetype
of your static files by looking at the extension.

You can override the function that figures out the mimetype
of the static files. It can be useful when Cowboy is missing
a mimetype you need to handle, or when you want to reduce
the list to make lookups faster. You can also give a
hard-coded mimetype that will be used unconditionally.

Cowboy comes with two functions built-in. The default
function only handles common file types used when building
Web applications. The other function is an extensive list
of hundreds of mimetypes that should cover almost any need
you may have. You can of course create your own function.

To use the default function, you should not have to configure
anything, as it is the default. If you insist, though, the
following will do the job.

``` erlang
{"/assets/[...]", cowboy_static, {priv_dir, my_app, "static/assets",
    [{mimetypes, cow_mimetypes, web}]}}
```

As you can see, there is an optional field that may contain
a list of less used options, like mimetypes or etag. All option
types have this optional field.

To use the function that will detect almost any mimetype,
the following configuration will do.

``` erlang
{"/assets/[...]", cowboy_static, {priv_dir, my_app, "static/assets",
    [{mimetypes, cow_mimetypes, all}]}}
```

You probably noticed the pattern by now. The configuration
expects a module and a function name, so you can use any
of your own functions instead.

``` erlang
{"/assets/[...]", cowboy_static, {priv_dir, my_app, "static/assets",
    [{mimetypes, Module, Function}]}}
```

The function that performs the mimetype detection receives
a single argument that is the path to the file on disk. It
is recommended to return the mimetype in tuple form, although
a binary string is also allowed (but will require extra
processing). If the function can't figure out the mimetype,
then it should return `{<<"application">>, <<"octet-stream">>, []}`.

When the static handler fails to find the extension in the
list, it will send the file as `application/octet-stream`.
A browser receiving such file will attempt to download it
directly to disk.

Finally, the mimetype can be hard-coded for all files.
This is especially useful in combination with the `file`
and `priv_file` options as it avoids needless computation.

``` erlang
{"/", cowboy_static, {priv_file, my_app, "static/index.html",
    [{mimetypes, {<<"text">>, <<"html">>, []}}]}}
```

Generate an etag
----------------

By default, the static handler will generate an etag header
value based on the size and modified time. This solution
can not be applied to all systems though. It would perform
rather poorly over a cluster of nodes, for example, as the
file metadata will vary from server to server, giving a
different etag on each server.

You can however change the way the etag is calculated.

``` erlang
{"/assets/[...]", cowboy_static, {priv_dir, my_app, "static/assets",
    [{etag, Module, Function}]}}
```

This function will receive three arguments: the path to the
file on disk, the size of the file and the last modification
time. In a distributed setup, you would typically use the
file path to retrieve an etag value that is identical across
all your servers.

You can also completely disable etag handling.

``` erlang
{"/assets/[...]", cowboy_static, {priv_dir, my_app, "static/assets",
    [{etag, false}]}}
```
