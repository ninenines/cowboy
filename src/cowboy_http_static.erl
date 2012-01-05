%% Copyright (c) 2011, Magnus Klaar <magnus.klaar@gmail.com>
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

%% @doc Static resource handler.
%%
%% This built in HTTP handler provides a simple file serving capability for
%% cowboy applications. It should be considered an experimental feature because
%% of it's dependency on the experimental REST handler. It's recommended to be
%% used for small or temporary environments where it is not preferrable to set
%% up a second server just to serve files.
%%
%% If this handler is used the Erlang node running the cowboy application must
%% be configured to use an async thread pool. This is configured by adding the
%% `+A $POOL_SIZE' argument to the `erl' command used to start the node. See
%% <a href="http://erlang.org/pipermail/erlang-bugs/2012-January/002720.html">
%% this reply</a> from the OTP team to erlang-bugs
%%
%% == Base configuration ==
%%
%% The handler must be configured with a request path prefix to serve files
%% under and the path to a directory to read files from. The request path prefix
%% is defined in the path pattern of the cowboy dispatch rule for the handler.
%% The request path pattern must end with a ``'...''' token.
%% The directory path can be set to either an absolute or relative path in the
%% form of a list or binary string representation of a file system path. A list
%% of binary path segments, as is used throughout cowboy, is also a valid
%% directory path.
%%
%% The directory path can also be set to a relative path within the `priv/'
%% directory of an application. This is configured by setting the value of the
%% directory option to a tuple of the form `{priv_dir, Application, Relpath}'.
%%
%% ==== Examples ====
%% ```
%% %% Serve files from /var/www/ under http://example.com/static/
%% {[<<"static">>, '...'], cowboy_http_static,
%%     [{directory, "/var/www"}]}
%%
%% %% Serve files from the current working directory under http://example.com/static/
%% {[<<"static">>, '...'], cowboy_http_static,
%%     [{directory, <<"./">>}]}
%%
%% %% Serve files from cowboy/priv/www under http://example.com/
%% {['...'], cowboy_http_static,
%%     [{directory, {priv_dir, cowboy, [<<"www">>]}}]}
%% '''
%%
%% == Content type configuration  ==
%%
%% By default the content type of all static resources will be set to
%% `application/octet-stream'. This can be overriden by supplying a list
%% of filename extension to mimetypes pairs in the `mimetypes' option.
%% The filename extension should be a binary string including the leading dot.
%% The mimetypes must be of a type that the `cowboy_http_rest' protocol can
%% handle.
%%
%% The <a href="https://github.com/spawngrid/mimetypes">spawngrid/mimetypes</a>
%% application, or an arbitrary function accepting the path to the file being
%% served, can also be used to generate the list of content types for a static
%% file resource. The function used must accept an additional argument after
%% the file path argument.
%%
%% ==== Example ====
%% ```
%% %% Use a static list of content types.
%% {[<<"static">>, '...'], cowboy_http_static,
%%     [{directory, {priv_dir, cowboy, []}},
%%      {mimetypes, [
%%          {<<".css">>, [<<"text/css">>]},
%%          {<<".js">>, [<<"application/javascript">>]}]}]}
%%
%% %% Use the default database in the mimetypes application.
%% {[<<"static">>, '...', cowboy_http_static,
%%     [{directory, {priv_dir, cowboy, []}},
%%      {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]]}
%% '''
-module(cowboy_http_static).

%% include files
-include("http.hrl").
-include_lib("kernel/include/file.hrl").

%% cowboy_http_protocol callbacks
-export([init/3]).

%% cowboy_http_rest callbacks
-export([rest_init/2, allowed_methods/2, malformed_request/2, resource_exists/2,
	forbidden/2, last_modified/2, content_types_provided/2, file_contents/2]).

%% internal
-export([path_to_mimetypes/2]).

%% types
-type dirpath() :: string() | binary() | [binary()].
-type dirspec() :: dirpath() | {priv, atom(), dirpath()}.
-type mimedef() :: {binary(), binary(), [{binary(), binary()}]}.

%% handler state
-record(state, {
	filepath  :: binary() | error,
	fileinfo  :: {ok, #file_info{}} | {error, _} | error,
	mimetypes :: {fun((binary(), T) -> [mimedef()]), T} | undefined}).


%% @private Upgrade from HTTP handler to REST handler.
init({_Transport, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_http_rest}.


%% @private Set up initial state of REST handler.
-spec rest_init(#http_req{}, list()) -> {ok, #http_req{}, #state{}}.
rest_init(Req, Opts) ->
	Directory = proplists:get_value(directory, Opts),
	Directory1 = directory_path(Directory),
	Mimetypes = proplists:get_value(mimetypes, Opts, []),
	Mimetypes1 = case Mimetypes of
		{_, _} -> Mimetypes;
		[] -> {fun path_to_mimetypes/2, []};
		[_|_] -> {fun path_to_mimetypes/2, Mimetypes}
	end,
	{Filepath, Req1} = cowboy_http_req:path_info(Req),
	State = case check_path(Filepath) of
		error ->
			#state{filepath=error, fileinfo=error, mimetypes=undefined};
		ok ->
			Filepath1 = join_paths(Directory1, Filepath),
			Fileinfo = file:read_file_info(Filepath1),
			#state{filepath=Filepath1, fileinfo=Fileinfo, mimetypes=Mimetypes1}
	end,
	{ok, Req1, State}.


%% @private Only allow GET and HEAD requests on files.
-spec allowed_methods(#http_req{}, #state{}) ->
		{[atom()], #http_req{}, #state{}}.
allowed_methods(Req, State) ->
	{['GET', 'HEAD'], Req, State}.

%% @private
-spec malformed_request(#http_req{}, #state{}) ->
		{boolean(), #http_req{}, #state{}}.
malformed_request(Req, #state{filepath=error}=State) ->
	{true, Req, State};
malformed_request(Req, State) ->
	{false, Req, State}.


%% @private Check if the resource exists under the document root.
-spec resource_exists(#http_req{}, #state{}) ->
		{boolean(), #http_req{}, #state{}}.
resource_exists(Req, #state{fileinfo={error, _}}=State) ->
	{false, Req, State};
resource_exists(Req, #state{fileinfo={ok, Fileinfo}}=State) ->
	{Fileinfo#file_info.type =:= regular, Req, State}.


%% @private
%% Access to a file resource is forbidden if it exists and the local node does
%% not have permission to read it. Directory listings are always forbidden.
-spec forbidden(#http_req{}, #state{}) -> {boolean(), #http_req{}, #state{}}.
forbidden(Req, #state{fileinfo={_, #file_info{type=directory}}}=State) ->
	{true, Req, State};
forbidden(Req, #state{fileinfo={error, eacces}}=State) ->
	{true, Req, State};
forbidden(Req, #state{fileinfo={error, _}}=State) ->
	{false, Req, State};
forbidden(Req, #state{fileinfo={ok, #file_info{access=Access}}}=State) ->
	{not (Access =:= read orelse Access =:= read_write), Req, State}.


%% @private Read the time a file system system object was last modified.
-spec last_modified(#http_req{}, #state{}) ->
		{cowboy_clock:datetime(), #http_req{}, #state{}}.
last_modified(Req, #state{fileinfo={ok, #file_info{mtime=Modified}}}=State) ->
	{Modified, Req, State}.


%% @private Return the content type of a file.
-spec content_types_provided(#http_req{}, #state{}) -> tuple().
content_types_provided(Req, #state{filepath=Filepath,
		mimetypes={MimetypesFun, MimetypesData}}=State) ->
	Mimetypes = [{T, file_contents}
		|| T <- MimetypesFun(Filepath, MimetypesData)],
	{Mimetypes, Req, State}.


%% @private Return a function that writes a file directly to the socket.
-spec file_contents(#http_req{}, #state{}) -> tuple().
file_contents(Req, #state{filepath=Filepath,
		fileinfo={ok, #file_info{size=Filesize}}}=State) ->
	{ok, Transport, Socket} = cowboy_http_req:transport(Req),
	Writefile = content_function(Transport, Socket, Filepath),
	{{stream, Filesize, Writefile}, Req, State}.


%% @private Return a function writing the contents of a file to a socket.
%% The function returns the number of bytes written to the socket to enable
%% the calling function to determine if the expected number of bytes were
%% written to the socket.
-spec content_function(module(), inet:socket(), binary()) ->
	fun(() -> {sent, non_neg_integer()}).
content_function(Transport, Socket, Filepath) ->
	%% `file:sendfile/2' will only work with the `cowboy_tcp_transport'
	%% transport module. SSL or future SPDY transports that require the
	%% content to be encrypted or framed as the content is sent.
	case erlang:function_exported(file, sendfile, 2) of
		false ->
			fun() -> sfallback(Transport, Socket, Filepath) end;
		_ when Transport =/= cowboy_tcp_transport ->
			fun() -> sfallback(Transport, Socket, Filepath) end;
		true ->
			fun() -> sendfile(Socket, Filepath) end
	end.


%% @private Sendfile fallback function.
-spec sfallback(module(), inet:socket(), binary()) -> {sent, non_neg_integer()}.
sfallback(Transport, Socket, Filepath) ->
	{ok, File} = file:open(Filepath, [read,binary,raw]),
	sfallback(Transport, Socket, File, 0).

-spec sfallback(module(), inet:socket(), file:io_device(),
		non_neg_integer()) -> {sent, non_neg_integer()}.
sfallback(Transport, Socket, File, Sent) ->
	case file:read(File, 16#1FFF) of
		eof ->
			ok = file:close(File),
			{sent, Sent};
		{ok, Bin} ->
			ok = Transport:send(Socket, Bin),
			sfallback(Transport, Socket, File, Sent + byte_size(Bin))
	end.


%% @private Wrapper for sendfile function.
-spec sendfile(inet:socket(), binary()) -> {sent, non_neg_integer()}.
sendfile(Socket, Filepath) ->
	{ok, Sent} = file:sendfile(Filepath, Socket),
	{sent, Sent}.

-spec directory_path(dirspec()) -> dirpath().
directory_path({priv_dir, App, []}) ->
	priv_dir_path(App);
directory_path({priv_dir, App, [H|_]=Path}) when is_integer(H) ->
	filename:join(priv_dir_path(App), Path);
directory_path({priv_dir, App, [H|_]=Path}) when is_binary(H) ->
	filename:join(filename:split(priv_dir_path(App)) ++ Path);
directory_path({priv_dir, App, Path}) when is_binary(Path) ->
	filename:join(priv_dir_path(App), Path);
directory_path(Path) ->
	Path.


%% @private Validate a request path for unsafe characters.
%% There is no way to escape special characters in a filesystem path.
-spec check_path(Path::[binary()]) -> ok | error.
check_path([]) -> ok;
check_path([<<"">>|_T]) -> error;
check_path([<<".">>|_T]) -> error;
check_path([<<"..">>|_T]) -> error;
check_path([H|T]) ->
	case binary:match(H, <<"/">>) of
		{_, _} -> error;
		nomatch -> check_path(T)
	end.


%% @private Join the the directory and request paths.
-spec join_paths(dirpath(), [binary()]) -> binary().
join_paths([H|_]=Dirpath, Filepath) when is_integer(H) ->
	filename:join(filename:split(Dirpath) ++ Filepath);
join_paths([H|_]=Dirpath, Filepath) when is_binary(H) ->
	filename:join(Dirpath ++ Filepath);
join_paths(Dirpath, Filepath) when is_binary(Dirpath) ->
	filename:join([Dirpath] ++ Filepath);
join_paths([], Filepath) ->
	filename:join(Filepath).


%% @private Return the path to the priv/ directory of an application.
-spec priv_dir_path(atom()) -> string().
priv_dir_path(App) ->
	case code:priv_dir(App) of
		{error, bad_name} -> priv_dir_mod(App);
		Dir -> Dir
	end.

-spec priv_dir_mod(atom()) -> string().
priv_dir_mod(Mod) ->
	case code:which(Mod) of
		File when not is_list(File) -> "../priv";
		File -> filename:join([filename:dirname(File),"../priv"])
	end.


%% @private Use application/octet-stream as the default mimetype.
%% If a list of extension - mimetype pairs are provided as the mimetypes
%% an attempt to find the mimetype using the file extension. If no match
%% is found the default mimetype is returned.
-spec path_to_mimetypes(binary(), [{binary(), [mimedef()]}]) ->
		[mimedef()].
path_to_mimetypes(Filepath, Extensions) when is_binary(Filepath) ->
	Ext = filename:extension(Filepath),
	case Ext of
		<<>> -> default_mimetype();
		_Ext -> path_to_mimetypes_(Ext, Extensions)
	end.

-spec path_to_mimetypes_(binary(), [{binary(), [mimedef()]}]) -> [mimedef()].
path_to_mimetypes_(Ext, Extensions) ->
	case lists:keyfind(Ext, 1, Extensions) of
		{_, MTs} -> MTs;
		_Unknown -> default_mimetype()
	end.

-spec default_mimetype() -> [mimedef()].
default_mimetype() ->
	[{<<"application">>, <<"octet-stream">>, []}].


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(_eq(E, I), ?_assertEqual(E, I)).

check_path_test_() ->
	C = fun check_path/1,
	[?_eq(error, C([<<>>])),
	 ?_eq(ok, C([<<"abc">>])),
	 ?_eq(error, C([<<".">>])),
	 ?_eq(error, C([<<"..">>])),
	 ?_eq(error, C([<<"/">>]))
	].

join_paths_test_() ->
	P = fun join_paths/2,
	[?_eq(<<"a">>, P([], [<<"a">>])),
	 ?_eq(<<"a/b/c">>, P(<<"a/b">>, [<<"c">>])),
	 ?_eq(<<"a/b/c">>, P("a/b", [<<"c">>])),
	 ?_eq(<<"a/b/c">>, P([<<"a">>, <<"b">>], [<<"c">>]))
	].

directory_path_test_() ->
	P = fun directory_path/1,
	PL = fun(I) -> length(filename:split(P(I))) end,
	Base = PL({priv_dir, cowboy, []}),
	[?_eq(Base + 1, PL({priv_dir, cowboy, "a"})),
	 ?_eq(Base + 1, PL({priv_dir, cowboy, <<"a">>})),
	 ?_eq(Base + 1, PL({priv_dir, cowboy, [<<"a">>]})),
	 ?_eq(Base + 2, PL({priv_dir, cowboy, "a/b"})),
	 ?_eq(Base + 2, PL({priv_dir, cowboy, <<"a/b">>})),
	 ?_eq(Base + 2, PL({priv_dir, cowboy, [<<"a">>, <<"b">>]})),
	 ?_eq("a/b", P("a/b"))
	].


-endif.
