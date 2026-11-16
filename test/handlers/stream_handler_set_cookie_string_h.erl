%% This module sends a character list as set-cookie, or a list
%% whose element is not iodata. Neither is a list of cookies.

-module(stream_handler_set_cookie_string_h).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

init(_StreamID, #{path := Path}, _Opts) ->
	Headers = case Path of
		<<"/set_cookie_string">> ->
			#{<<"set-cookie">> => "err=1; Path=/"};
		<<"/set_cookie_bad_element">> ->
			#{<<"set-cookie">> => [<<"ok=1">>, not_iodata]}
	end,
	{[{response, 200, Headers, <<"OK">>}], undefined}.

data(_StreamID, _IsFin, _Data, State) ->
	{[], State}.

%% cowboy_http sends the 500 through info/3 when the response is rejected.
info(_StreamID, Response={response, _, _, _}, State) ->
	{[Response], State};
info(_StreamID, _Info, State) ->
	{[], State}.

terminate(_StreamID, _Reason, _State) ->
	ok.

early_error(_StreamID, _Reason, _PartialReq, Resp, _Opts) ->
	Resp.
