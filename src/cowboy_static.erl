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
%% cowboy applications. It is provided as a convenience for small or temporary
%% environments where it is not preferrable to set up a second server just
%% to serve files. It is recommended to use a CDN instead for efficiently
%% handling static files, preferrably on a cookie-less domain name.
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
%%
%% The directory path can be set to either an absolute or relative path in the
%% form of a list or binary string representation of a file system path. A list
%% of binary path segments is also a valid directory path.
%%
%% The directory path can also be set to a relative path within the `priv/'
%% directory of an application. This is configured by setting the value of the
%% directory option to a tuple of the form `{priv_dir, Application, Relpath}'.
%%
%% ==== Examples ====
%% ```
%% %% Serve files from /var/www/ under http://example.com/static/
%% {"/static/[...]", cowboy_static,
%%     [{directory, "/var/www"}]}
%%
%% %% Serve files from the current working directory under http://example.com/static/
%% {"/static/[...]", cowboy_static,
%%     [{directory, <<"./">>}]}
%%
%% %% Serve files from cowboy/priv/www under http://example.com/
%% {"/[...]", cowboy_static,
%%     [{directory, {priv_dir, cowboy, [<<"www">>]}}]}
%% '''
%%
%% == Content type configuration  ==
%%
%% By default the content type of all static resources will be set to
%% `application/octet-stream'. This can be overriden by supplying a list
%% of filename extension to mimetypes pairs in the `mimetypes' option.
%% The filename extension should be a binary string including the leading dot.
%% The mimetypes must be of a type that the `cowboy_rest' protocol can
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
%% {"/static/[...]", cowboy_static,
%%     [{directory, {priv_dir, cowboy, []}},
%%      {mimetypes, [
%%          {<<".css">>, [<<"text/css">>]},
%%          {<<".js">>, [<<"application/javascript">>]}]}]}
%%
%% %% Use the default database in the mimetypes application.
%% {"/static/[...]", cowboy_static,
%%     [{directory, {priv_dir, cowboy, []}},
%%      {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}
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
%% header value can be supplied. The function must accept a list of key/values
%% of the file attributes as the first argument and a second argument
%% containing any additional data that the function requires. The function
%% must return a term of the type `{weak | strong, binary()}' or `undefined'.
%%
%% ====  Examples ====
%% ```
%% %% A value of default is equal to not specifying the option.
%% {"static/[...]", cowboy_static,
%%     [{directory, {priv_dir, cowboy, []}},
%%      {etag, default}]}
%%
%% %% Use all avaliable ETag function arguments to generate a header value.
%% {"static/[...]", cowboy_static,
%%     [{directory, {priv_dir, cowboy, []}},
%%      {etag, {attributes, [filepath, filesize, inode, mtime]}}]}
%%
%% %% Use a user defined function to generate a strong ETag header value.
%% {"static/[...]", cowboy_static,
%%     [{directory, {priv_dir, cowboy, []}},
%%      {etag, {fun generate_strong_etag/2, strong_etag_extra}}]}
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
%% {"/", cowboy_static,
%%     [{directory, {priv_dir, cowboy, [<<"www">>]}},
%%      {file, <<"index.html">>}]}
%%
%% %% Serve cowboy/priv/www/page.html under http://example.com/*/page
%% {"/:_/page", cowboy_static,
%%     [{directory, {priv_dir, cowboy, [<<"www">>]}},
%%      {file, <<"page.html">>}]}.
%%
%% %% Always serve cowboy/priv/www/other.html under http://example.com/other
%% {"/other/[...]", cowboy_static,
%%     [{directory, {priv_dir, cowboy, [<<"www">>]}},
%%      {file, "other.html"}]}
%% '''
-module(cowboy_static).

%% include files
-include_lib("kernel/include/file.hrl").

%% cowboy_protocol callbacks
-export([init/3]).

%% cowboy_rest callbacks
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([malformed_request/2]).
-export([resource_exists/2]).
-export([forbidden/2]).
-export([last_modified/2]).
-export([generate_etag/2]).
-export([content_types_provided/2]).
-export([file_contents/2]).

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
		undefined | {strong | weak, binary()}), T}
}).

%% @private Upgrade from HTTP handler to REST handler.
init({_Transport, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

%% @private Set up initial state of REST handler.
-spec rest_init(Req, list()) -> {ok, Req, #state{}} when Req::cowboy_req:req().
rest_init(Req, Opts) ->
	{_, DirectoryOpt} = lists:keyfind(directory, 1, Opts),
	Directory = fullpath(filename:absname(directory_path(DirectoryOpt))),
	case lists:keyfind(file, 1, Opts) of
		false ->
			{PathInfo, Req2} = cowboy_req:path_info(Req),
			Filepath = filename:join([Directory|PathInfo]),
			Len = byte_size(Directory),
			case fullpath(Filepath) of
				<< Directory:Len/binary, $/, _/binary >> ->
					rest_init(Req2, Opts, Filepath);
				_ ->
					{ok, Req2, #state{filepath=error, fileinfo=error,
						mimetypes=undefined, etag_fun=undefined}}
			end;
		{_, FileOpt} ->
			Filepath = filepath_path(FileOpt),
			Filepath2 = << Directory/binary, $/, Filepath/binary >>,
			rest_init(Req, Opts, Filepath2)
	end.

rest_init(Req, Opts, Filepath) ->
	Fileinfo = file:read_file_info(Filepath),
	Mimetypes = case lists:keyfind(mimetypes, 1, Opts) of
		false -> {fun path_to_mimetypes/2, []};
		{_, {{M, F}, E}} -> {fun M:F/2, E};
		{_, Mtypes} when is_tuple(Mtypes) -> Mtypes;
		{_, Mtypes} when is_list(Mtypes) -> {fun path_to_mimetypes/2, Mtypes}
	end,
	EtagFun = case lists:keyfind(etag, 1, Opts) of
		false -> {fun no_etag_function/2, undefined};
		{_, default} -> {fun no_etag_function/2, undefined};
		{_, {attributes, []}} -> {fun no_etag_function/2, undefined};
		{_, {attributes, Attrs}} -> {fun attr_etag_function/2, Attrs};
		{_, EtagOpt} -> EtagOpt
	end,
	{ok, Req, #state{filepath=Filepath, fileinfo=Fileinfo,
		mimetypes=Mimetypes, etag_fun=EtagFun}}.

%% @private Only allow GET and HEAD requests on files.
-spec allowed_methods(Req, #state{})
	-> {[binary()], Req, #state{}} when Req::cowboy_req:req().
allowed_methods(Req, State) ->
	{[<<"GET">>, <<"HEAD">>], Req, State}.

%% @private
-spec malformed_request(Req, #state{})
	-> {boolean(), Req, #state{}} when Req::cowboy_req:req().
malformed_request(Req, #state{filepath=error}=State) ->
	{true, Req, State};
malformed_request(Req, State) ->
	{false, Req, State}.

%% @private Check if the resource exists under the document root.
-spec resource_exists(Req, #state{})
	-> {boolean(), Req, #state{}} when Req::cowboy_req:req().
resource_exists(Req, #state{fileinfo={error, _}}=State) ->
	{false, Req, State};
resource_exists(Req, #state{fileinfo={ok, Fileinfo}}=State) ->
	{Fileinfo#file_info.type =:= regular, Req, State}.

%% @private
%% Access to a file resource is forbidden if it exists and the local node does
%% not have permission to read it. Directory listings are always forbidden.
-spec forbidden(Req, #state{})
	-> {boolean(), Req, #state{}} when Req::cowboy_req:req().
forbidden(Req, #state{fileinfo={_, #file_info{type=directory}}}=State) ->
	{true, Req, State};
forbidden(Req, #state{fileinfo={error, eacces}}=State) ->
	{true, Req, State};
forbidden(Req, #state{fileinfo={error, _}}=State) ->
	{false, Req, State};
forbidden(Req, #state{fileinfo={ok, #file_info{access=Access}}}=State) ->
	{not (Access =:= read orelse Access =:= read_write), Req, State}.

%% @private Read the time a file system system object was last modified.
-spec last_modified(Req, #state{})
	-> {calendar:datetime(), Req, #state{}} when Req::cowboy_req:req().
last_modified(Req, #state{fileinfo={ok, #file_info{mtime=Modified}}}=State) ->
	{erlang:localtime_to_universaltime(Modified), Req, State}.

%% @private Generate the ETag header value for this file.
%% The ETag header value is only generated if the resource is a file that
%% exists in document root.
-spec generate_etag(Req, #state{})
	-> {undefined | binary(), Req, #state{}} when Req::cowboy_req:req().
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
-spec content_types_provided(cowboy_req:req(), #state{}) -> tuple().
content_types_provided(Req, #state{filepath=Filepath,
		mimetypes={MimetypesFun, MimetypesData}}=State) ->
	Mimetypes = [{T, file_contents}
		|| T <- MimetypesFun(Filepath, MimetypesData)],
	{Mimetypes, Req, State}.

%% @private Return a function that writes a file directly to the socket.
-spec file_contents(cowboy_req:req(), #state{}) -> tuple().
file_contents(Req, #state{filepath=Filepath,
		fileinfo={ok, #file_info{size=Filesize}}}=State) ->
	Writefile = fun(Socket, Transport) ->
		%% Transport:sendfile/2 may return {error, closed}
		%% if the connection is closed while sending the file.
		case Transport:sendfile(Socket, Filepath) of
			{ok, _} -> ok;
			{error, closed} -> ok;
			{error, etimedout} -> ok
		end
	end,
	{{stream, Filesize, Writefile}, Req, State}.

%% Internal.

-spec directory_path(dirspec()) -> dirpath().
directory_path({priv_dir, App, []}) ->
	priv_dir_path(App);
directory_path({priv_dir, App, [H|_]=Path}) when is_binary(H) ->
	filename:join(priv_dir_path(App), filename:join(Path));
directory_path({priv_dir, App, Path}) ->
	filename:join(priv_dir_path(App), Path);
directory_path([H|_]=Path) when is_binary(H) ->
	filename:join(Path);
directory_path([H|_]=Path) when is_integer(H) ->
	list_to_binary(Path);
directory_path(Path) when is_binary(Path) ->
	Path.

%% @private Return the path to the priv/ directory of an application.
-spec priv_dir_path(atom()) -> string().
priv_dir_path(App) ->
	case code:priv_dir(App) of
		{error, bad_name} -> priv_dir_mod(App);
		Dir -> list_to_binary(Dir)
	end.

-spec priv_dir_mod(atom()) -> string().
priv_dir_mod(Mod) ->
	case code:which(Mod) of
		File when not is_list(File) -> <<"../priv">>;
		File -> filename:join(filename:dirname(File), <<"../priv">>)
	end.

%% @private Ensure that a file path is of the same type as a request path.
filepath_path(Path) when is_binary(Path) ->
	Path;
filepath_path([H|_]=Path) when is_binary(H) ->
	filename:join(Path);
filepath_path([H|_]=Path) when is_integer(H) ->
	list_to_binary(Path).

fullpath(Path) when is_binary(Path) ->
	fullpath(filename:split(Path), []).
fullpath([], Acc) ->
	filename:join(lists:reverse(Acc));
fullpath([<<".">>|Tail], Acc) ->
	fullpath(Tail, Acc);
fullpath([<<"..">>|Tail], Acc=[_]) ->
	fullpath(Tail, Acc);
fullpath([<<"..">>|Tail], [_|Acc]) ->
	fullpath(Tail, Acc);
fullpath([Segment|Tail], Acc) ->
	fullpath(Tail, [Segment|Acc]).

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
	case lists:keyfind(cowboy_bstr:to_lower(Ext), 1, Extensions) of
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

directory_path_test_() ->
	PL = fun(D) -> length(filename:split(directory_path(D))) end,
	Base = PL({priv_dir, cowboy, []}),
	LengthTests = [
		Base + 1, {priv_dir, cowboy, "a"},
		Base + 1, {priv_dir, cowboy, <<"a">>},
		Base + 1, {priv_dir, cowboy, [<<"a">>]},
		Base + 2, {priv_dir, cowboy, "a/b"},
		Base + 2, {priv_dir, cowboy, <<"a/b">>},
		Base + 2, {priv_dir, cowboy, [<<"a">>, <<"b">>]}
	],
	TypeTests = [
		{priv_dir, cowboy, []},
		{priv_dir, cowboy, "a"},
		{priv_dir, cowboy, <<"a">>},
		{priv_dir, cowboy, [<<"a">>]},
		"a",
		<<"a">>,
		[<<"a">>]
	],
	[{lists:flatten(io_lib:format("~p", [D])),
		fun() -> R = PL(D) end} || {R, D} <- LengthTests]
	++ [{lists:flatten(io_lib:format("~p", [D])),
		fun() -> is_binary(directory_path(D)) end} || D <- TypeTests].

filepath_path_test_() ->
	Tests = [
		{<<"a">>, "a"},
		{<<"a">>, <<"a">>},
		{<<"a">>, [<<"a">>]},
		{<<"a/b">>, "a/b"},
		{<<"a/b">>, <<"a/b">>},
		{<<"a/b">>, [<<"a">>, <<"b">>]}
	],
	[{lists:flatten(io_lib:format("~p", [F])),
		fun() -> R = filepath_path(F) end} || {R, F} <- Tests].

fullpath_test_() ->
	Tests = [
		{<<"/home/cowboy">>, <<"/home/cowboy">>},
		{<<"/home/cowboy">>, <<"/home/cowboy/">>},
		{<<"/home/cowboy">>, <<"/home/cowboy/./">>},
		{<<"/home/cowboy">>, <<"/home/cowboy/./././././.">>},
		{<<"/home/cowboy">>, <<"/home/cowboy/abc/..">>},
		{<<"/home/cowboy">>, <<"/home/cowboy/abc/../">>},
		{<<"/home/cowboy">>, <<"/home/cowboy/abc/./../.">>},
		{<<"/">>, <<"/home/cowboy/../../../../../..">>},
		{<<"/etc/passwd">>, <<"/home/cowboy/../../etc/passwd">>}
	],
	[{P, fun() -> R = fullpath(P) end} || {R, P} <- Tests].

good_path_check_test_() ->
	Tests = [
		<<"/home/cowboy/file">>,
		<<"/home/cowboy/file/">>,
		<<"/home/cowboy/./file">>,
		<<"/home/cowboy/././././././file">>,
		<<"/home/cowboy/abc/../file">>,
		<<"/home/cowboy/abc/../file">>,
		<<"/home/cowboy/abc/./.././file">>
	],
	[{P, fun() ->
		case fullpath(P) of
			<< "/home/cowboy/", _/binary >> -> ok
		end
	end} || P <- Tests].

bad_path_check_test_() ->
	Tests = [
		<<"/home/cowboy/../../../../../../file">>,
		<<"/home/cowboy/../../etc/passwd">>
	],
	[{P, fun() ->
		error = case fullpath(P) of
			<< "/home/cowboy/", _/binary >> -> ok;
			_ -> error
		end
	end} || P <- Tests].

good_path_win32_check_test_() ->
	Tests = case os:type() of
		{unix, _} ->
			[];
		{win32, _} ->
			[
				<<"c:/home/cowboy/file">>,
				<<"c:/home/cowboy/file/">>,
				<<"c:/home/cowboy/./file">>,
				<<"c:/home/cowboy/././././././file">>,
				<<"c:/home/cowboy/abc/../file">>,
				<<"c:/home/cowboy/abc/../file">>,
				<<"c:/home/cowboy/abc/./.././file">>
			]
	end,
	[{P, fun() ->
		case fullpath(P) of
			<< "c:/home/cowboy/", _/binary >> -> ok
		end
	end} || P <- Tests].

bad_path_win32_check_test_() ->
	Tests = case os:type() of
		{unix, _} ->
			[];
		{win32, _} ->
			[
				<<"c:/home/cowboy/../../secretfile.bat">>,
				<<"c:/home/cowboy/c:/secretfile.bat">>,
				<<"c:/home/cowboy/..\\..\\secretfile.bat">>,
				<<"c:/home/cowboy/c:\\secretfile.bat">>
			]
	end,
	[{P, fun() ->
		error = case fullpath(P) of
			<< "c:/home/cowboy/", _/binary >> -> ok;
			_ -> error
		end
	end} || P <- Tests].

-endif.
