%% Feel free to use, reuse and abuse the code in this file.

-module(http_errors).

-export([init/2]).

-spec init(_, _) -> no_return().
init(Req, _Opts) ->
	#{'case' := Case} = cowboy_req:match_qs(['case'], Req),
    case_init(Case, Req).

-spec case_init(_, _) -> no_return().
case_init(<<"init_before_reply">> = Case, _Req) ->
	ct_helper_error_h:ignore(?MODULE, case_init, 2),
    error(Case);
case_init(<<"init_after_reply">> = Case, Req) ->
	ct_helper_error_h:ignore(?MODULE, case_init, 2),
    _ = cowboy_req:reply(200, #{}, "http_handler_crashes", Req),
    error(Case).
