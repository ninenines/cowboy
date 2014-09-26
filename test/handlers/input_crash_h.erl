%% This module crashes on request input data
%% depending on the given option.

-module(input_crash_h).

-export([init/2]).

init(Req, content_length) ->
	cowboy_error_h:ignore(cow_http_hd, number, 2),
	cowboy_req:parse_header(<<"content-length">>, Req).
