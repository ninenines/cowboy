%% Feel free to use, reuse and abuse the code in this file.

%% @doc Directory handler.
-module(directory_handler).

%% REST Callbacks
-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).

%% Callback Callbacks
-export([list_json/2]).
-export([list_html/2]).

init(_Transport, _Req, _Paths) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, Paths) ->
	{ok, Req, Paths}.

allowed_methods(Req, State) ->
	{[<<"GET">>], Req, State}.

resource_exists(Req, {ReqPath, FilePath}) ->
	case file:list_dir(FilePath) of
		{ok, Fs} -> {true, Req, {ReqPath, lists:sort(Fs)}};
		_Err -> {false, Req, {ReqPath, FilePath}}
	end.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, list_json},
		{{<<"text">>, <<"html">>, []}, list_html}
	], Req, State}.

list_json(Req, {Path, Fs}) ->
	Files = [[ <<(list_to_binary(F))/binary>> || F <- Fs ]],
	{jsx:encode(Files), Req, Path}.

list_html(Req, {Path, Fs}) ->
	Body = [[ links(Path, F) || F <- [".."|Fs] ]],
	HTML = [<<"<!DOCTYPE html><html><head><title>Index</title></head>",
		"<body>">>, Body, <<"</body></html>\n">>],
	{HTML, Req, Path}.

links(<<>>, File) ->
	["<a href='/", File, "'>", File, "</a><br>\n"];
links(Prefix, File) ->
	["<a href='/", Prefix, $/, File, "'>", File, "</a><br>\n"].
