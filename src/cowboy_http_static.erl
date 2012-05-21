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
%% The request path pattern must end with a `...' token.
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
%%
%% == ETag Header Function ==
%%
%% The default behaviour of the static file handler is to not generate ETag
%% headers. This is because generating ETag headers based on file metadata
%% causes different servers in a cluster to generate different ETag values for
%% the same file unless the metadata is also synced. Generating strong ETags
%% based on the contents of a file is currently out of scope for this module.
%%
%% The default behaviour can be overridden to generate an ETag header based on
%% a combination of the file path, file size, inode and mtime values. If the
%% option value is a non-empty list of attribute names tagged with `attributes'
%% a hex encoded checksum of each attribute specified is included in the value
%% of the the ETag header. If the list of attribute names is empty no ETag
%% header is generated.
%%
%% If a strong ETag is required a user defined function for generating the
%% header value can be supplied. The function must accept a proplist of the
%% file attributes as the first argument and a second argument containing any
%% additional data that the function requires. The function must return a term
%% of the type `{weak | strong, binary()}' or `undefined'.
%%
%% ====  Examples ====
%% ```
%% %% A value of default is equal to not specifying the option.
%% {[<<"static">>, '...', cowboy_http_static,
%%     [{directory, {priv_dir, cowboy, []}},
%%      {etag, default}]]}
%%
%% %% Use all avaliable ETag function arguments to generate a header value.
%% {[<<"static">>, '...', cowboy_http_static,
%%     [{directory, {priv_dir, cowboy, []}},
%%      {etag, {attributes, [filepath, filesize, inode, mtime]}}]]}
%%
%% %% Use a user defined function to generate a strong ETag header value.
%% {[<<"static">>, '...', cowboy_http_static,
%%     [{directory, {priv_dir, cowboy, []}},
%%      {etag, {fun generate_strong_etag/2, strong_etag_extra}}]]}
%%
%% generate_strong_etag(Arguments, strong_etag_extra) ->
%%     {_, Filepath} = lists:keyfind(filepath, 1, Arguments),
%%     {_, _Filesize} = lists:keyfind(filesize, 1, Arguments),
%%     {_, _INode} = lists:keyfind(inode, 1, Arguments),
%%     {_, _Modified} = lists:keyfind(mtime, 1, Arguments),
%%     ChecksumCommand = lists:flatten(io_lib:format("sha1sum ~s", [Filepath])),
%%     [Checksum|_] = string:tokens(os:cmd(ChecksumCommand), " "),
%%     {strong, iolist_to_binary(Checksum)}.
%% '''
%%
%% == File configuration ==
%%
%% If the file system path being served does not share a common suffix with
%% the request path it is possible to override the file path using the `file'
%% option. The value of this option is expected to be a relative path within
%% the static file directory specified using the `directory' option.
%% The path must be in the form of a list or binary string representation of a
%% file system path. A list of binary path segments, as is used throughout
%% cowboy, is also a valid.
%%
%% When the `file' option is used the same file will be served for all requests
%% matching the cowboy dispatch fule for the handler. It is not necessary to
%% end the request path pattern with a `...' token because the request path
%% will not be used to determine which file to serve from the static directory.
%%
%% === Examples ===
%%
%% ```
%% %% Serve cowboy/priv/www/index.html as http://example.com/
%% {[], cowboy_http_static,
%%     [{directory, {priv_dir, cowboy, [<<"www">>]}}
%%      {file, <<"index.html">>}]}
%%
%% %% Serve cowboy/priv/www/page.html under http://example.com/*/page
%% {['*', <<"page">>], cowboy_http_static,
%%     [{directory, {priv_dir, cowboy, [<<"www">>]}}
%%      {file, <<"page.html">>}]}.
%%
%% %% Always serve cowboy/priv/www/other.html under http://example.com/other
%% {[<<"other">>, '...'], cowboy_http_static,
%%     [{directory, {priv_dir, cowboy, [<<"www">>]}}
%%      {file, "other.html"}]}
%% '''
-module(cowboy_http_static).

%% include files
-include("http.hrl").
-include_lib("kernel/include/file.hrl").

%% cowboy_http_protocol callbacks
-export([init/3]).

%% cowboy_http_rest callbacks
-export([rest_init/2, allowed_methods/2, malformed_request/2,
	resource_exists/2, forbidden/2, last_modified/2, generate_etag/2,
	content_types_provided/2, file_contents/2]).

%% internal
-export([path_to_mimetypes/2]).

%% types
-type dirpath() :: string() | binary() | [binary()].
-type dirspec() :: dirpath() | {priv, atom(), dirpath()}.
-type mimedef() :: {binary(), binary(), [{binary(), binary()}]}.
-type etagarg() :: {filepath, binary()} | {mtime, calendar:datetime()}
	| {inode, non_neg_integer()} | {filesize, non_neg_integer()}.

%% handler state
-record(state, {
	filepath  :: binary() | error,
	fileinfo  :: {ok, #file_info{}} | {error, _} | error,
	mimetypes :: {fun((binary(), T) -> [mimedef()]), T} | undefined,
	etag_fun  :: {fun(([etagarg()], T) ->
		undefined | {strong | weak, binary()}), T}}).


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
	ETagFunction = case proplists:get_value(etag, Opts) of
		default -> {fun no_etag_function/2, undefined};
		undefined -> {fun no_etag_function/2, undefined};
		{attributes, []} -> {fun no_etag_function/2, undefined};
		{attributes, Attrs} -> {fun attr_etag_function/2, Attrs};
		{_, _}=ETagFunction1 -> ETagFunction1
	end,
	{Filepath, Req1} = case lists:keyfind(file, 1, Opts) of
		{_, Filepath2} -> {filepath_path(Filepath2), Req};
		false -> cowboy_http_req:path_info(Req)
	end,
	State = case check_path(Filepath) of
		error ->
			#state{filepath=error, fileinfo=error, mimetypes=undefined,
				etag_fun=ETagFunction};
		ok ->
			Filepath1 = join_paths(Directory1, Filepath),
			Fileinfo = file:read_file_info(Filepath1),
			#state{filepath=Filepath1, fileinfo=Fileinfo, mimetypes=Mimetypes1,
				etag_fun=ETagFunction}
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
		{calendar:datetime(), #http_req{}, #state{}}.
last_modified(Req, #state{fileinfo={ok, #file_info{mtime=Modified}}}=State) ->
	{Modified, Req, State}.


%% @private Generate the ETag header value for this file.
%% The ETag header value is only generated if the resource is a file that
%% exists in document root.
-spec generate_etag(#http_req{}, #state{}) ->
	{undefined | binary(), #http_req{}, #state{}}.
generate_etag(Req, #state{fileinfo={_, #file_info{type=regular, inode=INode,
		mtime=Modified, size=Filesize}}, filepath=Filepath,
		etag_fun={ETagFun, ETagData}}=State) ->
	ETagArgs = [
		{filepath, Filepath}, {filesize, Filesize},
		{inode, INode}, {mtime, Modified}],
	{ETagFun(ETagArgs, ETagData), Req, State};
generate_etag(Req, State) ->
	{undefined, Req, State}.


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
			case Transport:send(Socket, Bin) of
				ok -> sfallback(Transport, Socket, File, Sent + byte_size(Bin));
				{error, closed} -> {sent, Sent}
			end
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

%% @private Ensure that a file path is of the same type as a request path.
-spec filepath_path(dirpath()) -> Path::[binary()].
filepath_path([H|_]=Path) when is_integer(H) ->
	filename:split(list_to_binary(Path));
filepath_path(Path) when is_binary(Path) ->
	filename:split(Path);
filepath_path([H|_]=Path) when is_binary(H) ->
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


%% @private Do not send ETag headers in the default configuration.
-spec no_etag_function([etagarg()], undefined) -> undefined.
no_etag_function(_Args, undefined) ->
	undefined.

%% @private A simple alternative is to send an ETag based on file attributes.
-type fileattr() :: filepath | filesize | mtime | inode.
-spec attr_etag_function([etagarg()], [fileattr()]) -> {strong, binary()}.
attr_etag_function(Args, Attrs) ->
	[[_|H]|T] = [begin
		{_,Pair} = {_,{_,_}} = {Attr,lists:keyfind(Attr, 1, Args)},
		[$-|integer_to_list(erlang:phash2(Pair, 1 bsl 32), 16)]
	end || Attr <- Attrs],
	{strong, list_to_binary([H|T])}.


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

filepath_path_test_() ->
	P = fun filepath_path/1,
	[?_eq([<<"a">>], P("a")),
	 ?_eq([<<"a">>], P(<<"a">>)),
	 ?_eq([<<"a">>], P([<<"a">>])),
	 ?_eq([<<"a">>, <<"b">>], P("a/b")),
	 ?_eq([<<"a">>, <<"b">>], P(<<"a/b">>)),
	 ?_eq([<<"a">>, <<"b">>], P([<<"a">>, <<"b">>]))
	].

-endif.
