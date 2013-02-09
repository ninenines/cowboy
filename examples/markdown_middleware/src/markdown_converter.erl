%% Feel free to use, reuse and abuse the code in this file.

-module(markdown_converter).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
	{[Path], Req1} = cowboy_req:path_info(Req),
	case filename:extension(Path) of
		<<".html">> -> maybe_generate_markdown(resource_path(Path));
		_Ext -> ok
	end,
	{ok, Req1, Env}.

maybe_generate_markdown(Path) ->
	ModifiedAt = filelib:last_modified(source_path(Path)),
	GeneratedAt = filelib:last_modified(Path),
	case ModifiedAt > GeneratedAt of
		true -> erlmarkdown:conv_file(source_path(Path), Path);
		false -> ok
	end.

resource_path(Path) ->
	{ok, Cwd} = file:get_cwd(),
	filename:join([Cwd, "priv", Path]).

source_path(Path) ->
	<< (filename:rootname(Path))/binary, ".md" >>.
