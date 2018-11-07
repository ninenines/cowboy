%% This module returns stop based on the query string.
%% Success is indicated via a 248 status code in the response.

-module(stop_handler_h).

-export([init/2]).

-export([allowed_methods/2]).
-export([allow_missing_post/2]).
-export([charsets_provided/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_completed/2]).
-export([delete_resource/2]).
-export([forbidden/2]).
-export([is_authorized/2]).
-export([is_conflict/2]).
-export([known_methods/2]).
-export([languages_provided/2]).
-export([malformed_request/2]).
-export([moved_permanently/2]).
-export([moved_temporarily/2]).
-export([multiple_choices/2]).
-export([options/2]).
-export([previously_existed/2]).
-export([range_satisfiable/2]).
-export([ranges_provided/2]).
-export([rate_limited/2]).
-export([resource_exists/2]).
-export([service_available/2]).
-export([uri_too_long/2]).
-export([valid_content_headers/2]).
-export([valid_entity_length/2]).

-export([accept/2]).
-export([provide/2]).
-export([provide_range/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

allow_missing_post(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

charsets_provided(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

content_types_accepted(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

content_types_provided(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

delete_completed(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

delete_resource(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

forbidden(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

is_authorized(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

is_conflict(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

known_methods(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

languages_provided(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

malformed_request(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

moved_permanently(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

moved_temporarily(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

multiple_choices(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

options(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

previously_existed(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

range_satisfiable(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

ranges_provided(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

rate_limited(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

resource_exists(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

service_available(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

uri_too_long(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

valid_content_headers(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

valid_entity_length(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

accept(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

provide(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

provide_range(Req, State) ->
	maybe_stop_handler(Req, State, ?FUNCTION_NAME).

maybe_stop_handler(Req=#{qs := Qs}, State, StateName) ->
	case atom_to_binary(StateName, latin1) of
		Qs -> do_stop_handler(Req, State);
		_ -> do_default(Req, State, StateName)
	end.

%% These are all the methods necessary to reach all callbacks.
do_default(Req, State, allowed_methods) ->
	{[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State};
%% We need to accept/provide media types to reach these callbacks.
do_default(Req, State, content_types_accepted) ->
	{[{<<"text/plain">>, accept}], Req, State};
do_default(Req, State, content_types_provided) ->
	{[{<<"text/plain">>, provide}], Req, State};
%% We need to accept ranges to reach these callbacks.
do_default(Req=#{qs := <<"range_satisfiable">>}, State, ranges_provided) ->
	{[{<<"bytes">>, provide_range}], Req, State};
do_default(Req=#{qs := <<"provide_range">>}, State, ranges_provided) ->
	{[{<<"bytes">>, provide_range}], Req, State};
%% We need resource_exists to return false to reach these callbacks.
do_default(Req=#{qs := <<"allow_missing_post">>}, State, resource_exists) ->
	{false, Req, State};
do_default(Req=#{qs := <<"moved_permanently">>}, State, resource_exists) ->
	{false, Req, State};
do_default(Req=#{qs := <<"moved_temporarily">>}, State, resource_exists) ->
	{false, Req, State};
do_default(Req=#{qs := <<"previously_existed">>}, State, resource_exists) ->
	{false, Req, State};
%% We need previously_existed to return true to reach these callbacks.
do_default(Req=#{qs := <<"moved_permanently">>}, State, previously_existed) ->
	{true, Req, State};
do_default(Req=#{qs := <<"moved_temporarily">>}, State, previously_existed) ->
	{true, Req, State};
%% We need the DELETE to suceed to reach this callback.
do_default(Req=#{qs := <<"delete_completed">>}, State, delete_resource) ->
	{true, Req, State};
%% We should never reach these callbacks.
do_default(Req, State, accept) ->
	{false, Req, State};
do_default(Req, State, provide) ->
	{<<"This is REST!">>, Req, State};
do_default(Req, State, provide_range) ->
	{<<"This is ranged REST!">>, Req, State};
%% Simulate the callback being missing in any other cases.
do_default(_, _, _) ->
	no_call.

do_stop_handler(Req0, State) ->
	Req = cowboy_req:reply(<<"248 REST handler stopped!">>, #{}, <<>>, Req0),
	{stop, Req, State}.
