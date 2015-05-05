%% This module crashes on request input data
%% depending on the given option.

-module(input_crash_h).

-export([init/2]).

init(Req, content_length) ->
	ct_helper_error_h:ignore(erlang, binary_to_integer, 1),
	cowboy_req:parse_header(<<"content-length">>, Req).
