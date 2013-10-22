Middleware example
==================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/hello_world_example console
```

Then point your browser at
[http://localhost:8080/video.html](http://localhost:8080/video.html).

Cowboy will serve all the files you put in the `priv` directory.
If you request a `.html` file that has a corresponding `.md` file
that has been modified more recently than the `.html` file, the
Markdown file will be converted to HTML and served by Cowboy.
