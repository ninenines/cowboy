Elixir Websocket
================

This is an example of running Cowboy with [Elixir](http://elixir-lang.org).

You need Elixir installed
([instructions here](http://elixir-lang.org/getting_started/1.html))
to run this example. After installing Elixir, you should have both
`elixir` and `mix` executables available.

You also need [rebar](https://github.com/rebar/rebar) in your PATH
to compile dependencies.

Then type the following command:

```
mix deps.get
```

The command above will fetch all dependencies and compile them.

You can then start the Erlang node with the following command:

```
mix run --no-halt
```

Then point your browser to localhost:8080.
