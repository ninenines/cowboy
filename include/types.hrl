%% Copyright (c) 2011, Loïc Hoguin <essen@dev-extend.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-type application_start_type() :: normal |
	{takeover, Node::node()} | {failover, Node::node()}.

-type posix() :: atom().
-type port_number() :: 0..65535.

-type bindings() :: list({Key::atom(), Value::string()}).
-type path_tokens() :: list(nonempty_string()).
-type match() :: '_' | '*' | list(string() | '_' | atom()).

-type dispatch_rule() :: {Host::match(), list({Path::match(),
	Handler::module(), Opts::term()})}.
-type dispatch() :: list(dispatch_rule()).
