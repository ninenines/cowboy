-module(accept_callback_missing_h).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
	{[<<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
	ct_helper_error_h:ignore(cowboy_rest, process_content_type, 3),
	{[{<<"text/plain">>, accept}], Req, State}.
