# AGENTS.md

Cowboy is a small, fast and modern HTTP server for Erlang/OTP.

## Commands
- `make` - build
- `make ct` - integration tests
- `make eunit` - unit tests
- `make proper` - property tests
- `make dialyzer` - static analysis

Always use `make` to build, even if wanting to build a single module.

To run a single test suite: `make ct-$SUITE` where `$SUITE`
is the test suite's name minus `_SUITE` (so `make ct-req`
runs the `req_SUITE`).

## Experimental features
HTTP/3 and WebTransport are currently experimental and are
hidden behind compile flags. Set `COWBOY_QUICER=1` and
`GUN_QUICER=1` to enable but only if working on HTTP/3
or WebTransport.

## Layout
- `src/` - .erl files
- `test/` - Common Test .erl test suites and test handlers
- Unit tests live inside modules under `-ifdef(TEST).`
- `doc/src/` - documentation of public interface (in Asciidoc)

## Modules
- `cowboy` - Listener management
- `cowboy_app` - OTP application callback module
- `cowboy_bstr` - Binary string helpers
- `cowboy_children` - Manager of request processes
- `cowboy_clear` - Entry-point for clear-text connections
- `cowboy_clock` - Builds a valid Date string every second
- `cowboy_compress_h` - Stream handler for automatic response compression
- `cowboy_constraints` - Implementation for constraints (used for matching and validation)
- `cowboy_decompress_h` - Stream handler for automatic request decompression
- `cowboy_handler` - Plain HTTP handlers
- `cowboy_http` - HTTP/1.1 connections
- `cowboy_http2` - HTTP/2 connections
- `cowboy_http3` - HTTP/3 connections
- `cowboy_loop` - Loop handler
- `cowboy_metrics_h` - Stream handler gathering metrics
- `cowboy_middleware` - Middleware interface
- `cowboy_req` - Request object provided to handlers, also used for responses
- `cowboy_rest` - REST handlers
- `cowboy_router` - Router middleware
- `cowboy_static` - Built-in handler to serve static files
- `cowboy_stream` - Stream handlers interface and chaining logic
- `cowboy_stream_h` - Stream handler creating and managing a request process
- `cowboy_sub_protocol` - Sub-protocol interface
- `cowboy_sup` - OTP supervisor callback module
- `cowboy_tls` - Entry-point for TLS connections
- `cowboy_tracer_h` - Stream handler for tracing requests
- `cowboy_websocket` - Websocket connections
- `cowboy_webtransport` - WebTransport connections

## When doing any sort of development
Don't include unnecessary comments. Comments are only useful
when the code is not obvious.

Do not remove existing comments.

### When implementing features
Always include tests for both normal cases (including available
examples if implementing from a design document such as an RFC)
and for edge cases.

Do not include documentation unless requested.

### When fixing bugs
Always write one or more tests before modifying the code. There
must be at least one test that fails before and succeeds after.

If you are not able to write a failing test, abort and tell
the user about it.

### When writing tests
We want to test both success and failure conditions.

Make sure the tests are not only naive tests. It's OK to have
naive tests, but there must be other more subtle tests.

Make sure to be thorough. We want tests that cover all possible
scenarios, not just a handful.

There should be tests that exercise limits but are expected
to pass. For example if limiting a component to at most 128
characters, there must tests for 128 and 129 characters at a
minimum. If a value ranges from 0 to 128, there must be tests
around the boundaries (-1, 0, 128, 129 are good candidates)
with success expected for values within and failures otherwise.

Always write integration tests except when fixing a module that
already has unit tests and they are enough to answer the project
as a whole is working as intended.

### When optimising performance
Same as for fixing bugs, first write horse tests and then ensure
they run faster after doing the changes. As performance tests are
fiddle a few test runs may be necessary to be sure.

### When you think you are done
Review the changes and ensure all changes are necessary.
Discard any *change* that is not then run the tests again.
You can review changes with `git diff`.

## When doing code analysis

Always perform code analysis on Cowboy and all its dependencies
as a unit. A code analysis of Cowboy alone is likely to miss
things.

Cowboy uses Cowlib for most of its parsing.

Cowlib is typically strict at parsing but lax at building
protocol output. It is the responsibility of the caller to
sanitize the data given to Cowlib to build protocol output.
The same applies to data given to Cowboy, regardless of
Cowboy having functionality to prevent certain issues.
