%% This module adds custom fields to the Req object.
%% It is only meant to be checked by Dialyzer.

-module(custom_req_fields_h).

-export([init/2]).

-spec init(Req, Opts) -> {ok, Req, Opts} when Req::cowboy_req:req().
init(Req, Opts) ->
	{ok, Req#{'_myapp_auth_method' => pubkey}, Opts}.
