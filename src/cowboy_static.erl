%% Copyright (c) 2013-2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_static).

-export([init/2]).
-export([malformed_request/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([ranges_provided/2]).
-export([resource_exists/2]).
-export([last_modified/2]).
-export([generate_etag/2]).
-export([get_file/2]).

-type extra_charset() :: {charset, module(), function()} | {charset, binary()}.
-type extra_etag() :: {etag, module(), function()} | {etag, false}.
-type extra_mimetypes() :: {mimetypes, module(), function()}
	| {mimetypes, binary() | {binary(), binary(), [{binary(), binary()}]}}.
-type extra() :: [extra_charset() | extra_etag() | extra_mimetypes()].
-type opts() :: {file | dir, string() | binary()}
	| {file | dir, string() | binary(), extra()}
	| {priv_file | priv_dir, atom(), string() | binary()}
	| {priv_file | priv_dir, atom(), string() | binary(), extra()}.
-export_type([opts/0]).

-include_lib("kernel/include/file.hrl").

-type state() :: {binary(), {direct | archive, #file_info{}}
	| {error, atom()}, extra()}.

%% Resolve the file that will be sent and get its file information.
%% If the handler is configured to manage a directory, check that the
%% requested file is inside the configured directory.

-spec init(Req, opts()) -> {cowboy_rest, Req, error | state()} when Req::cowboy_req:req().
init(Req, {Name, Path}) ->
	init_opts(Req, {Name, Path, []});
init(Req, {Name, App, Path})
		when Name =:= priv_file; Name =:= priv_dir ->
	init_opts(Req, {Name, App, Path, []});
init(Req, Opts) ->
	init_opts(Req, Opts).

init_opts(Req, {priv_file, App, Path, Extra}) ->
	{PrivPath, HowToAccess} = priv_path(App, Path),
	init_info(Req, absname(PrivPath), HowToAccess, Extra);
init_opts(Req, {file, Path, Extra}) ->
	init_info(Req, absname(Path), direct, Extra);
init_opts(Req, {priv_dir, App, Path, Extra}) ->
	{PrivPath, HowToAccess} = priv_path(App, Path),
	init_dir(Req, PrivPath, HowToAccess, Extra);
init_opts(Req, {dir, Path, Extra}) ->
	init_dir(Req, Path, direct, Extra).

priv_path(App, Path) ->
	case code:priv_dir(App) of
		{error, bad_name} ->
			error({badarg, "Can't resolve the priv_dir of application "
				++ atom_to_list(App)});
		PrivDir when is_list(Path) ->
			{
				PrivDir ++ "/" ++ Path,
				how_to_access_app_priv(PrivDir)
			};
		PrivDir when is_binary(Path) ->
			{
				<< (list_to_binary(PrivDir))/binary, $/, Path/binary >>,
				how_to_access_app_priv(PrivDir)
			}
	end.

how_to_access_app_priv(PrivDir) ->
	%% If the priv directory is not a directory, it must be
	%% inside an Erlang application .ez archive. We call
	%% how_to_access_app_priv1() to find the corresponding archive.
	case filelib:is_dir(PrivDir) of
		true  -> direct;
		false -> how_to_access_app_priv1(PrivDir)
	end.

how_to_access_app_priv1(Dir) ->
	%% We go "up" by one path component at a time and look for a
	%% regular file.
	Archive = filename:dirname(Dir),
	case Archive of
		Dir ->
			%% filename:dirname() returned its argument:
			%% we reach the root directory. We found no
			%% archive so we return 'direct': the given priv
			%% directory doesn't exist.
			direct;
		_ ->
			case filelib:is_regular(Archive) of
				true  -> {archive, Archive};
				false -> how_to_access_app_priv1(Archive)
			end
	end.

absname(Path) when is_list(Path) ->
	filename:absname(list_to_binary(Path));
absname(Path) when is_binary(Path) ->
	filename:absname(Path).

init_dir(Req, Path, HowToAccess, Extra) when is_list(Path) ->
	init_dir(Req, list_to_binary(Path), HowToAccess, Extra);
init_dir(Req, Path, HowToAccess, Extra) ->
	Dir = fullpath(filename:absname(Path)),
	case cowboy_req:path_info(Req) of
		%% When dir/priv_dir are used and there is no path_info
		%% this is a configuration error and we abort immediately.
		undefined ->
			{ok, cowboy_req:reply(500, Req), error};
		PathInfo ->
			case validate_reserved(PathInfo) of
				error ->
					{cowboy_rest, Req, error};
				ok ->
					Filepath = filename:join([Dir|PathInfo]),
					Len = byte_size(Dir),
					case fullpath(Filepath) of
						<< Dir:Len/binary, $/, _/binary >> ->
							init_info(Req, Filepath, HowToAccess, Extra);
						<< Dir:Len/binary >> ->
							init_info(Req, Filepath, HowToAccess, Extra);
						_ ->
							{cowboy_rest, Req, error}
					end
			end
	end.

validate_reserved([]) ->
	ok;
validate_reserved([P|Tail]) ->
	case validate_reserved1(P) of
		ok -> validate_reserved(Tail);
		error -> error
	end.

%% We always reject forward slash, backward slash and NUL as
%% those have special meanings across the supported platforms.
%% We could support the backward slash on some platforms but
%% for the sake of consistency and simplicity we don't.
validate_reserved1(<<>>) ->
	ok;
validate_reserved1(<<$/, _/bits>>) ->
	error;
validate_reserved1(<<$\\, _/bits>>) ->
	error;
validate_reserved1(<<0, _/bits>>) ->
	error;
validate_reserved1(<<_, Rest/bits>>) ->
	validate_reserved1(Rest).

fullpath(Path) ->
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

init_info(Req, Path, HowToAccess, Extra) ->
	Info = read_file_info(Path, HowToAccess),
	{cowboy_rest, Req, {Path, Info, Extra}}.

read_file_info(Path, direct) ->
	case file:read_file_info(Path, [{time, universal}]) of
		{ok, Info} -> {direct, Info};
		Error      -> Error
	end;
read_file_info(Path, {archive, Archive}) ->
	case file:read_file_info(Archive, [{time, universal}]) of
		{ok, ArchiveInfo} ->
			%% The Erlang application archive is fine.
			%% Now check if the requested file is in that
			%% archive. We also need the file_info to merge
			%% them with the archive's one.
			PathS = binary_to_list(Path),
			case erl_prim_loader:read_file_info(PathS) of
				{ok, ContainedFileInfo} ->
					Info = fix_archived_file_info(
						ArchiveInfo,
						ContainedFileInfo),
					{archive, Info};
				error ->
					{error, enoent}
			end;
		Error ->
			Error
	end.

fix_archived_file_info(ArchiveInfo, ContainedFileInfo) ->
	%% We merge the archive and content #file_info because we are
	%% interested by the timestamps of the archive, but the type and
	%% size of the contained file/directory.
	%%
	%% We reset the access to 'read', because we won't rewrite the
	%% archive.
	ArchiveInfo#file_info{
		size = ContainedFileInfo#file_info.size,
		type = ContainedFileInfo#file_info.type,
		access = read
	}.

-ifdef(TEST).
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
			<< "/home/cowboy/", _/bits >> -> ok
		end
	end} || P <- Tests].

bad_path_check_test_() ->
	Tests = [
		<<"/home/cowboy/../../../../../../file">>,
		<<"/home/cowboy/../../etc/passwd">>
	],
	[{P, fun() ->
		error = case fullpath(P) of
			<< "/home/cowboy/", _/bits >> -> ok;
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
			<< "c:/home/cowboy/", _/bits >> -> ok
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
			<< "c:/home/cowboy/", _/bits >> -> ok;
			_ -> error
		end
	end} || P <- Tests].
-endif.

%% Reject requests that tried to access a file outside
%% the target directory, or used reserved characters.

-spec malformed_request(Req, State)
	-> {boolean(), Req, State}.
malformed_request(Req, State) ->
	{State =:= error, Req, State}.

%% Directories, files that can't be accessed at all and
%% files with no read flag are forbidden.

-spec forbidden(Req, State)
	-> {boolean(), Req, State}
	when State::state().
forbidden(Req, State={_, {_, #file_info{type=directory}}, _}) ->
	{true, Req, State};
forbidden(Req, State={_, {error, eacces}, _}) ->
	{true, Req, State};
forbidden(Req, State={_, {_, #file_info{access=Access}}, _})
		when Access =:= write; Access =:= none ->
	{true, Req, State};
forbidden(Req, State) ->
	{false, Req, State}.

%% Detect the mimetype of the file.

-spec content_types_provided(Req, State)
	-> {[{binary(), get_file}], Req, State}
	when State::state().
content_types_provided(Req, State={Path, _, Extra}) when is_list(Extra) ->
	case lists:keyfind(mimetypes, 1, Extra) of
		false ->
			{[{cow_mimetypes:web(Path), get_file}], Req, State};
		{mimetypes, Module, Function} ->
			{[{Module:Function(Path), get_file}], Req, State};
		{mimetypes, Type} ->
			{[{Type, get_file}], Req, State}
	end.

%% Detect the charset of the file.

-spec charsets_provided(Req, State)
	-> {[binary()], Req, State}
	when State::state().
charsets_provided(Req, State={Path, _, Extra}) ->
	case lists:keyfind(charset, 1, Extra) of
		%% We simulate the callback not being exported.
		false ->
			no_call;
		{charset, Module, Function} ->
			{[Module:Function(Path)], Req, State};
		{charset, Charset} when is_binary(Charset) ->
			{[Charset], Req, State}
	end.

%% Enable support for range requests.

-spec ranges_provided(Req, State)
	-> {[{binary(), auto}], Req, State}
	when State::state().
ranges_provided(Req, State) ->
	{[{<<"bytes">>, auto}], Req, State}.

%% Assume the resource doesn't exist if it's not a regular file.

-spec resource_exists(Req, State)
	-> {boolean(), Req, State}
	when State::state().
resource_exists(Req, State={_, {_, #file_info{type=regular}}, _}) ->
	{true, Req, State};
resource_exists(Req, State) ->
	{false, Req, State}.

%% Generate an etag for the file.

-spec generate_etag(Req, State)
	-> {{strong | weak, binary()}, Req, State}
	when State::state().
generate_etag(Req, State={Path, {_, #file_info{size=Size, mtime=Mtime}},
		Extra}) ->
	case lists:keyfind(etag, 1, Extra) of
		false ->
			{generate_default_etag(Size, Mtime), Req, State};
		{etag, Module, Function} ->
			{Module:Function(Path, Size, Mtime), Req, State};
		{etag, false} ->
			{undefined, Req, State}
	end.

generate_default_etag(Size, Mtime) ->
	{strong, integer_to_binary(erlang:phash2({Size, Mtime}, 16#ffffffff))}.

%% Return the time of last modification of the file.

-spec last_modified(Req, State)
	-> {calendar:datetime(), Req, State}
	when State::state().
last_modified(Req, State={_, {_, #file_info{mtime=Modified}}, _}) ->
	{Modified, Req, State}.

%% Stream the file.

-spec get_file(Req, State)
	-> {{sendfile, 0, non_neg_integer(), binary()}, Req, State}
	when State::state().
get_file(Req, State={Path, {direct, #file_info{size=Size}}, _}) ->
	{{sendfile, 0, Size, Path}, Req, State};
get_file(Req, State={Path, {archive, _}, _}) ->
	PathS = binary_to_list(Path),
	{ok, Bin, _} = erl_prim_loader:get_file(PathS),
	{Bin, Req, State}.
