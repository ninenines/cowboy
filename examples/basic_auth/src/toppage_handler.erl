%% Feel free to use, reuse and abuse the code in this file.

%% @doc Basic authorization Hello world handler.
-module(toppage_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([hello_to_text/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.


is_authorized(Req, S) ->
	{ok, Auth, Req1} = cowboy_req:parse_header(<<"authorization">>, Req),
	case Auth of
		{<<"basic">>, {User = <<"Alladin">>, <<"open sesame">>}} ->
			{true, Req1, User};
		_ ->
			{{false, <<"Basic realm=\"cowboy\"">>}, Req1, S}
	end.

content_types_provided(Req, State) ->
	{[
		{<<"text/plain">>, hello_to_text}
	], Req, State}.


hello_to_text(Req, User) ->
	{<< <<"Hello, ">>/binary, User/binary, <<"!\n">>/binary >>, Req, User}.

