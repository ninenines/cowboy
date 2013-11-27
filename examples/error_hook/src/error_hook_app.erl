%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(error_hook_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', []}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]},
		{onresponse, fun error_hook/4}
	]),
	error_hook_sup:start_link().

stop(_State) ->
	ok.

error_hook(404, Headers, <<>>, Req) ->
	{Path, Req2} = cowboy_req:path(Req),
	Body = ["404 Not Found: \"", Path,
		"\" is not the path you are looking for.\n"],
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, integer_to_list(iolist_size(Body))}),
	{ok, Req3} = cowboy_req:reply(404, Headers2, Body, Req2),
	Req3;
error_hook(Code, Headers, <<>>, Req) when is_integer(Code), Code >= 400 ->
	Body = ["HTTP Error ", integer_to_list(Code), $\n],
	Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
		{<<"content-length">>, integer_to_list(iolist_size(Body))}),
	{ok, Req2} = cowboy_req:reply(Code, Headers2, Body, Req),
	Req2;
error_hook(_Code, _Headers, _Body, Req) ->
	Req.
