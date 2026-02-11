%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(cowboy_rewrite_h).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

%% Helpers.
-export([has_rewrites/1]).
-export([record_rewrite/2]).

%% Predefined rewrites.
-export([index/2]).
-export([slash/2]).

%% The outcome determines what happens after a rewrite
%% occurred. The rewrite can either be internal and lead
%% to further rewrites or to stop processing rewrites.
%% When the rewrite is external (redirect) processing
%% immediately stop and a redirect response is issued.

-type outcome() :: continue | stop | {redirect, 300..399}.
-export_type([outcome/0]).

-type predefined_rewrites() :: index | slash.

-type rewrites() :: [
	{predefined_rewrites(), map()} |
	{module(), atom(), map()} |
	{fun((cowboy_req:req(), map()) -> {outcome(), cowboy_req:req()}), map()}
].
-export_type([rewrites/0]).

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
	-> {cowboy_stream:commands(), any()}.

init(StreamID, Req0, Opts) ->
	Rewrites = maps:get(rewrite, Opts, []),
	case rewrite(Rewrites, Req0) of
		{ok, Req} ->
			%% Our job is done, we don't need to keep state, so our state will
			%% be the state of the next handler and we only pass everything
			%% forward in all other callbacks.
			cowboy_stream:init(StreamID, Req, Opts);
		{redirect, Status, Req} ->
			{[{response, Status, #{<<"location">> => cowboy_req:uri(Req)}, <<>>}], undefined}
	end.

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), any())
	-> {cowboy_stream:commands(), any()}.

data(StreamID, IsFin, Data, State) ->
	cowboy_stream:data(StreamID, IsFin, Data, State).

-spec info(cowboy_stream:streamid(), any(), any())
	-> {cowboy_stream:commands(), any()}.

info(StreamID, Info, State) ->
	cowboy_stream:info(StreamID, Info, State).

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), any()) -> ok.

terminate(StreamID, Reason, State) ->
	cowboy_stream:terminate(StreamID, Reason, State).

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(),
	cowboy_stream:partial_req(), Resp, cowboy:opts()) -> Resp
	when Resp::cowboy_stream:resp_command().

early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
	cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

%% Helpers.

%% Whether rewrites have occurred.

-spec has_rewrites(cowboy_req:req()) -> boolean().

has_rewrites(Req) ->
	maps:is_key(rewrites, Req).

%% Record a rewrite event in the Req. This is optional
%% but very useful for debugging rewrites.

-spec record_rewrite(any(), cowboy_req:req()) -> cowboy_req:req().

record_rewrite(Event, Req) ->
	Rewrites = maps:get(rewrites, Req, []),
	Req#{rewrites => [Event|Rewrites]}.

%% Rewrite logic.

%% @todo Mark the Req as being modified so we can detect it was.

rewrite([], Req) ->
	{ok, Req};
rewrite([{F, Opts}|Tail], Req0) when is_atom(F) ->
	{Outcome, Req} = ?MODULE:F(Req0, Opts),
	rewrite_outcome(Tail, Req, Outcome);
rewrite([{M, F, Opts}|Tail], Req0) ->
	{Outcome, Req} = M:F(Req0, Opts),
	rewrite_outcome(Tail, Req, Outcome);
rewrite([{Fun, Opts}|Tail], Req0) when is_function(Fun) ->
	{Outcome, Req} = Fun(Req0, Opts),
	rewrite_outcome(Tail, Req, Outcome).

rewrite_outcome(Tail, Req, continue) ->
	rewrite(Tail, Req);
rewrite_outcome(_, Req, stop) ->
	{ok, Req};
rewrite_outcome(_, Req, {redirect, Status}) ->
	{redirect, Status, Req}.

%% Predefined rewrites.
%%
%% They are exported so they can be used for composition.
%% For example one may want to apply the index rewrite
%% only to a path prefix. A fun could match the prefix
%% and then defer to the index rewrite. Though that
%% scenario is covered by the if_path_prefix rewrite.

%% @todo if_path_prefix

%% Add a filename at the end of the request path if this
%% path ends with a slash (otherwise it is assumed there
%% already is a filename).

-spec index(cowboy_req:req(), #{filename => binary(), outcome => outcome()})
	-> {outcome(), cowboy_req:req()}.

index(Req=#{path := Path0}, Opts) ->
	Filename = maps:get(filename, Opts, <<"index.html">>),
	case binary:last(Path0) of
		$/ ->
			Path = <<Path0/binary, Filename/binary>>,
			{maps:get(outcome, Opts, continue),
				record_rewrite({path, Path0, Path}, Req#{path => Path})};
		_ ->
			{continue, Req}
	end.

%% @todo regex

%% Ensure there is a slash at the end of the path.

-spec slash(cowboy_req:req(), #{outcome => outcome()})
	-> {outcome(), cowboy_req:req()}.

slash(Req=#{path := Path0}, Opts) ->
	case binary:last(Path0) of
		$/ ->
			{continue, Req};
		_ ->
			Path = <<Path0/binary, $/>>,
			{maps:get(outcome, Opts, continue),
				record_rewrite({path, Path0, Path}, Req#{path => Path})}
	end.
