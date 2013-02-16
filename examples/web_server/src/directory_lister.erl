%% Feel free to use, reuse and abuse the code in this file.

-module(directory_lister).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
	case lists:keyfind(handler, 1, Env) of
		{handler, cowboy_static} -> redirect_directory(Req, Env);
		_H -> {ok, Req, Env}
	end.

redirect_directory(Req, Env) ->
	{Path, Req1} = cowboy_req:path_info(Req),
	Path1 = << <<S/binary, $/>> || S <- Path >>,
	{handler_opts, StaticOpts} = lists:keyfind(handler_opts, 1, Env),
	{dir_handler, DirHandler} = lists:keyfind(dir_handler, 1, StaticOpts),
	FullPath = resource_path(Path1),
	case valid_path(Path) and filelib:is_dir(FullPath) of
		true -> handle_directory(Req1, Env, Path1, FullPath, DirHandler);
		false -> {ok, Req1, Env}
	end.

handle_directory(Req, Env, Prefix, Path, DirHandler) ->
	Env1 = lists:keydelete(handler, 1,
		lists:keydelete(handler_opts, 1, Env)),
	{ok, Req, [{handler, DirHandler}, {handler_opts, {Prefix, Path}} | Env1]}.

valid_path([]) -> true;
valid_path([<<"..">> | _T]) -> false;
valid_path([<<"/", _/binary>> | _T]) -> false;
valid_path([_H | Rest]) -> valid_path(Rest).

resource_path(Path) ->
	{ok, Cwd} = file:get_cwd(),
	filename:join([Cwd, "priv", Path]).
