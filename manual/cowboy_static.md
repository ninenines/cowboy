cowboy_static
=============

The `cowboy_static` module implements file serving capabilities
by using the REST semantics provided by `cowboy_rest`.

Types
-----

### opts() = {priv_file, atom(), string() | binary()}
	| {priv_file, atom(), string() | binary(), extra()}
	| {file, string() | binary()}
	| {file, string() | binary(), extra()}
	| {priv_dir, atom(), string() | binary()}
	| {priv_dir, atom(), string() | binary(), extra()}
	| {dir, atom(), string() | binary()}
	| {dir, atom(), string() | binary(), extra()}

> Configuration for the static handler.
>
> The handler can be configured for sending either one file or
> a directory (including its subdirectories).
>
> Extra options allow you to define how the etag should be calculated
> and how the mimetype of files should be detected. They are defined
> as follow, but do note that these types are not exported, only the
> `opts/0` type is public.

### extra() = [extra_etag() | extra_mimetypes()]

### extra_etag() = {etag, module(), function()} | {etag, false}

### extra_mimetypes() = {mimetypes, module(), function()}
	| {mimetypes, binary() | {binary(), binary(), [{binary(), binary()}]}}
