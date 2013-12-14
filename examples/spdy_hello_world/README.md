Hello world example(SPDY)
=========================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/spdy_hello_world_example console
```

Then point your browser at [https://localhost:8443](https://localhost:8443).
You will need to temporarily trust the root certificate authority,
which can also be found in `priv/spdy/cowboy-ca.crt`.
