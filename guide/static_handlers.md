Static handlers
===============

Purpose
-------

Static handlers are a built-in REST handler for serving files. They
are available as a convenience and provide fast file serving with
proper cache handling.

Usage
-----

Static handlers are pre-written REST handlers. They only need
to be specified in the routing information with the proper options.

The following example routing serves all files found in the
`priv_dir/static/` directory of the application. It uses a
mimetypes library to figure out the files' content types.

``` erlang
Dispatch = [
	{'_', [
		{"/[...]", cowboy_static, [
			{directory, {priv_dir, static, []}},
			{mimetypes, {fun mimetypes:path_to_mimes/2, default}}
		]}
	]}
].
```
