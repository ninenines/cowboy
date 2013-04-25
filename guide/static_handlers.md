Static handlers
===============

Purpose
-------

Static handlers are a built-in REST handler for serving files. They
are available as a convenience and provide fast file serving with
proper cache handling.

It is recommended to use a Content Distribution Network (CDN) or at
least a dedicated file server running on a dedicated cookie-less
hostname for serving your application's static files in production.

Usage
-----

Static handlers are pre-written REST handlers. They only need
to be specified in the routing information with the proper options.

The following example routing serves all files found in the
`priv_dir/static/` directory of the application `my_app`.

``` erlang
Dispatch = [
	{'_', [
		{"/[...]", cowboy_static, [
			{directory, {priv_dir, my_app, [<<"static">>]}},
			{mimetypes, {fun mimetypes:path_to_mimes/2, default}}
		]}
	]}
].
```

You can also serve a single file specifically. A common example
would be an `index.html` file to be served when the path `/`
is requested. The following example will serve the `priv/index.html`
file from the application `my_app`.

``` erlang
Dispatch = [
	{'_', [
		{"/", cowboy_static, [
			{directory, {priv_dir, my_app, []}},
			{file, "index.html"},
			{mimetypes, {fun mimetypes:path_to_mimes/2, default}}
		]}
	]}
].
```

MIME type
---------

Cowboy does not provide any default for MIME types. This means
that unless you specify the `mimetypes` option, all files will
be sent as `application/octet-stream`, which the browser will
not try to interpret, instead trying to make you download it.

In the examples above we used the
[mimetypes application](https://github.com/spawngrid/mimetypes)
to find the MIME type from the file's extension.
