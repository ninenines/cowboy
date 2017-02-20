%% Feel free to use, reuse and abuse the code in this file.

-module(directory_lister).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env=#{handler := cowboy_static}) ->
	redirect_directory(Req, Env);
execute(Req, Env) ->
	{ok, Req, Env}.

redirect_directory(Req, Env=#{handler_opts := {_, _, _, Extra}}) ->
	Path = cowboy_req:path_info(Req),
	Path1 = << <<S/binary, $/>> || S <- Path >>,
	{dir_handler, DirHandler} = lists:keyfind(dir_handler, 1, Extra),
	FullPath = resource_path(Path1),
	case valid_path(Path) and filelib:is_dir(FullPath) of
		true -> handle_directory(Req, Env, Path1, FullPath, DirHandler);
		false -> {ok, Req, Env}
	end.

handle_directory(Req, Env, Prefix, Path, DirHandler) ->
	{ok, Req, Env#{handler => DirHandler, handler_opts => {Prefix, Path}}}.

valid_path([]) -> true;
valid_path([<<"..">> | _T]) -> false;
valid_path([<<"/", _/binary>> | _T]) -> false;
valid_path([_H | Rest]) -> valid_path(Rest).

resource_path(Path) ->
	filename:join([code:priv_dir(file_server), Path]).
