%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(regexp_router_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_regexp_router:compile([
    	{".*", [
			{"^/hello/(?<user>[^/]+)/?$", [{user, function, fun constraint/1}], hello_handler, []},
			{"^/$", toppage_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{regexp_dispatch, Dispatch}]},
		{middlewares, [cowboy_regexp_router, cowboy_handler]}
	]),
	regexp_router_sup:start_link().

constraint(<<"Tom">> = User) ->
	{true, <<"dear ", User/binary>>};
constraint(_) ->
	true.

stop(_State) ->
        ok.
