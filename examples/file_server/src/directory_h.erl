%% Feel free to use, reuse and abuse the code in this file.

%% @doc Directory handler.
-module(directory_h).

%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).

%% Callback Callbacks
-export([list_json/2]).
-export([list_html/2]).

init(Req, Paths) ->
	{cowboy_rest, Req, Paths}.

allowed_methods(Req, State) ->
	{[<<"GET">>], Req, State}.

resource_exists(Req, {ReqPath, FilePath}) ->
	case file:list_dir(FilePath) of
		{ok, Fs} -> {true, Req, {ReqPath, lists:sort(Fs)}};
		_Err -> {false, Req, {ReqPath, FilePath}}
	end.

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"html">>, []}, list_html},
		{{<<"application">>, <<"json">>, []}, list_json}
	], Req, State}.

list_json(Req, {Path, Fs}) ->
	Files = [ <<(unicode_to_iolist(F))/binary>> || F <- Fs ],
	{jsx:encode(Files), Req, Path}.

list_html(Req, {Path, Fs}) ->
	Body = [[ links(Path, unicode_to_iolist(F)) || F <- [".."|Fs] ]],
	HTML = [<<"<!DOCTYPE html><html><head><meta charset='utf-8'<title>Index</title></head>",
		"<body>">>, Body, <<"</body></html>\n">>],
	{HTML, Req, Path}.

links(<<>>, "..") ->
	"<a href='/..'>..</a><br>\n";
links(Prefix, "..") ->
	Tokens = string:tokens(binary_to_list(Prefix), "/"),
	Back = lists:join("/", lists:reverse(tl(lists:reverse(Tokens)))),
	["<a href='/../", Back, "'>..</a><br>\n"];
links(<<>>, File) ->
	["<a href='/", File, "'>", File, "</a><br>\n"];
links(Prefix, File) ->
	["<a href='/", Prefix, File, "'>", File, "</a><br>\n"].


unicode_to_iolist(UniString) ->
        binary_to_list(unicode:characters_to_binary(UniString)). 
