%% Feel free to use, reuse and abuse the code in this file.

-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	Html = get_html(),
	{ok, Req2} = cowboy_req:reply(200,
		[{<<"content-type">>, <<"text/html">>}],
		Html, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

get_html() ->
	{ok, Cwd} = file:get_cwd(),
	Filename =filename:join([Cwd, "priv", "html_ws_client.html"]),
	{ok, Binary} = file:read_file(Filename),
	Binary.
