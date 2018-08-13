-module(provide_callback_missing_h).

-export([init/2]).
-export([content_types_provided/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	ct_helper_error_h:ignore(cowboy_rest, set_resp_body, 2),
	{[{<<"text/plain">>, provide}], Req, State}.
