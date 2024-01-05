%% Copyright (c) 2024, jdamanalo <joshuadavid.agustin@manalo.ph>
%% Copyright (c) 2024, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_decompress_h).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

-record(state, {
	next :: any(),
	enabled = true :: boolean(),
	ratio_limit :: non_neg_integer() | undefined,
	compress = undefined :: undefined | gzip,
	inflate = undefined :: undefined | zlib:zstream(),
	is_reading = false :: boolean(),

	%% We use a list of binaries to avoid doing unnecessary
	%% memory allocations when inflating. We convert to binary
	%% when we propagate the data. The data must be reversed
	%% before converting to binary or inflating: this is done
	%% via the buffer_to_binary/buffer_to_iovec functions.
	read_body_buffer = [] :: [binary()],
	read_body_is_fin = nofin :: nofin | {fin, non_neg_integer()}
}).

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
	-> {cowboy_stream:commands(), #state{}}.
init(StreamID, Req0, Opts) ->
	Enabled = maps:get(decompress_enabled, Opts, true),
	RatioLimit = maps:get(decompress_ratio_limit, Opts, 20),
	{Req, State} = check_and_update_req(Req0),
	Inflate = case State#state.compress of
		undefined ->
			undefined;
		gzip ->
			Z = zlib:open(),
			zlib:inflateInit(Z, 31),
			Z
	end,
	{Commands, Next} = cowboy_stream:init(StreamID, Req, Opts),
	fold(Commands, State#state{next=Next, enabled=Enabled,
		ratio_limit=RatioLimit, inflate=Inflate}).

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State)
	-> {cowboy_stream:commands(), State} when State::#state{}.
data(StreamID, IsFin, Data, State=#state{next=Next0, inflate=undefined}) ->
	{Commands, Next} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
	fold(Commands, State#state{next=Next, read_body_is_fin=IsFin});
data(StreamID, IsFin, Data, State=#state{next=Next0, enabled=false, read_body_buffer=Buffer}) ->
	{Commands, Next} = cowboy_stream:data(StreamID, IsFin,
		buffer_to_binary([Data|Buffer]), Next0),
	fold(Commands, State#state{next=Next, read_body_is_fin=IsFin});
data(StreamID, IsFin, Data, State0=#state{next=Next0, ratio_limit=RatioLimit,
		inflate=Z, is_reading=true, read_body_buffer=Buffer}) ->
	case inflate(Z, RatioLimit, buffer_to_iovec([Data|Buffer])) of
		{error, ErrorType} ->
			zlib:close(Z),
			Status = case ErrorType of
				data_error -> 400;
				size_error -> 413
			end,
			Commands = [
				{error_response, Status, #{<<"content-length">> => <<"0">>}, <<>>},
				stop
			],
			fold(Commands, State0#state{inflate=undefined, read_body_buffer=[]});
		{ok, Inflated} ->
			State = case IsFin of
				nofin ->
					State0;
				fin ->
					zlib:close(Z),
					State0#state{inflate=undefined}
			end,
			{Commands, Next} = cowboy_stream:data(StreamID, IsFin, Inflated, Next0),
			fold(Commands, State#state{next=Next, read_body_buffer=[],
				read_body_is_fin=IsFin})
	end;
data(_, IsFin, Data, State=#state{read_body_buffer=Buffer}) ->
	{[], State#state{read_body_buffer=[Data|Buffer], read_body_is_fin=IsFin}}.

-spec info(cowboy_stream:streamid(), any(), State)
	-> {cowboy_stream:commands(), State} when State::#state{}.
info(StreamID, Info, State=#state{next=Next0, inflate=undefined}) ->
	{Commands, Next} = cowboy_stream:info(StreamID, Info, Next0),
	fold(Commands, State#state{next=Next});
info(StreamID, Info={CommandTag, _, _, _, _}, State=#state{next=Next0, read_body_is_fin=IsFin})
		when CommandTag =:= read_body; CommandTag =:= read_body_timeout ->
	{Commands0, Next1} = cowboy_stream:info(StreamID, Info, Next0),
	{Commands, Next} = data(StreamID, IsFin, <<>>, State#state{next=Next1, is_reading=true}),
	fold(Commands ++ Commands0, Next);
info(StreamID, Info={set_options, Opts}, State0=#state{next=Next0,
		enabled=Enabled0, ratio_limit=RatioLimit0, is_reading=IsReading}) ->
	Enabled = maps:get(decompress_enabled, Opts, Enabled0),
	RatioLimit = maps:get(decompress_ratio_limit, Opts, RatioLimit0),
	{Commands, Next} = cowboy_stream:info(StreamID, Info, Next0),
	%% We can't change the enabled setting after we start reading,
	%% otherwise the data becomes garbage. Changing the setting
	%% is not treated as an error, it is just ignored.
	State = case IsReading of
		true -> State0;
		false -> State0#state{enabled=Enabled}
	end,
	fold(Commands, State#state{next=Next, ratio_limit=RatioLimit});
info(StreamID, Info, State=#state{next=Next0}) ->
	{Commands, Next} = cowboy_stream:info(StreamID, Info, Next0),
	fold(Commands, State#state{next=Next}).

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), #state{}) -> any().
terminate(StreamID, Reason, #state{next=Next, inflate=Z}) ->
	case Z of
		undefined -> ok;
		_ -> zlib:close(Z)
	end,
	cowboy_stream:terminate(StreamID, Reason, Next).

-spec early_error(cowboy_stream:streamid(), cowboy_stream:reason(),
	cowboy_stream:partial_req(), Resp, cowboy:opts()) -> Resp
	when Resp::cowboy_stream:resp_command().
early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
	cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

%% Internal.

%% Check whether the request needs content decoding, and if it does
%% whether it fits our criteria for decoding. We also update the
%% Req to indicate whether content was decoded.
%%
%% We always set the content_decoded value in the Req because it
%% indicates whether content decoding was attempted.
%%
%% A malformed content-encoding header results in no decoding.
check_and_update_req(Req=#{headers := Headers}) ->
	ContentDecoded = maps:get(content_decoded, Req, []),
	try cowboy_req:parse_header(<<"content-encoding">>, Req) of
		%% We only automatically decompress when gzip is the only
		%% encoding used. Since it's the only encoding used, we
		%% can remove the header entirely before passing the Req
		%% forward.
		[<<"gzip">>] ->
			{Req#{
				headers => maps:remove(<<"content-encoding">>, Headers),
				content_decoded => [<<"gzip">>|ContentDecoded]
			}, #state{compress=gzip}};
		_ ->
			{Req#{content_decoded => ContentDecoded},
				#state{compress=undefined}}
	catch _:_ ->
		{Req#{content_decoded => ContentDecoded},
			#state{compress=undefined}}
	end.

buffer_to_iovec(Buffer) ->
	lists:reverse(Buffer).

buffer_to_binary(Buffer) ->
	iolist_to_binary(lists:reverse(Buffer)).

fold(Commands, State) ->
	fold(Commands, State, []).

fold([], State, Acc) ->
	{lists:reverse(Acc), State};
fold([{response, Status, Headers0, Body}|Tail], State=#state{enabled=true}, Acc) ->
	Headers = add_accept_encoding(Headers0),
	fold(Tail, State, [{response, Status, Headers, Body}|Acc]);
fold([{headers, Status, Headers0} | Tail], State=#state{enabled=true}, Acc) ->
	Headers = add_accept_encoding(Headers0),
	fold(Tail, State, [{headers, Status, Headers}|Acc]);
fold([Command|Tail], State, Acc) ->
	fold(Tail, State, [Command|Acc]).

add_accept_encoding(Headers=#{<<"accept-encoding">> := AcceptEncoding}) ->
	try cow_http_hd:parse_accept_encoding(iolist_to_binary(AcceptEncoding)) of
		List ->
			case lists:keyfind(<<"gzip">>, 1, List) of
				%% gzip is excluded but this handler is enabled; we replace.
				{_, 0} ->
					Replaced = lists:keyreplace(<<"gzip">>, 1, List, {<<"gzip">>, 1000}),
					Codings = build_accept_encoding(Replaced),
					Headers#{<<"accept-encoding">> => Codings};
				{_, _} ->
					Headers;
				false ->
					case lists:keyfind(<<"*">>, 1, List) of
						%% Others are excluded along with gzip; we add.
						{_, 0} ->
							WithGzip = [{<<"gzip">>, 1000} | List],
							Codings = build_accept_encoding(WithGzip),
							Headers#{<<"accept-encoding">> => Codings};
						{_, _} ->
							Headers;
						false ->
							Headers#{<<"accept-encoding">> => [AcceptEncoding, <<", gzip">>]}
					end
			end
	catch _:_ ->
		%% The accept-encoding header is invalid. Probably empty. We replace it with ours.
		Headers#{<<"accept-encoding">> => <<"gzip">>}
	end;
add_accept_encoding(Headers) ->
	Headers#{<<"accept-encoding">> => <<"gzip">>}.

%% @todo From cowlib, maybe expose?
qvalue_to_iodata(0) -> <<"0">>;
qvalue_to_iodata(Q) when Q < 10 -> [<<"0.00">>, integer_to_binary(Q)];
qvalue_to_iodata(Q) when Q < 100 -> [<<"0.0">>, integer_to_binary(Q)];
qvalue_to_iodata(Q) when Q < 1000 -> [<<"0.">>, integer_to_binary(Q)];
qvalue_to_iodata(1000) -> <<"1">>.

%% @todo Should be added to Cowlib.
build_accept_encoding([{ContentCoding, Q}|Tail]) ->
	Weight = iolist_to_binary(qvalue_to_iodata(Q)),
	Acc = <<ContentCoding/binary, ";q=", Weight/binary>>,
	do_build_accept_encoding(Tail, Acc).

do_build_accept_encoding([{ContentCoding, Q}|Tail], Acc0) ->
	Weight = iolist_to_binary(qvalue_to_iodata(Q)),
	Acc = <<Acc0/binary, ", ", ContentCoding/binary, ";q=", Weight/binary>>,
	do_build_accept_encoding(Tail, Acc);
do_build_accept_encoding([], Acc) ->
	Acc.

inflate(Z, RatioLimit, Data) ->
	try
		{Status, Output} = zlib:safeInflate(Z, Data),
		Size = iolist_size(Output),
		do_inflate(Z, Size, iolist_size(Data) * RatioLimit, Status, [Output])
	catch
		error:data_error ->
			{error, data_error}
	end.

do_inflate(_, Size, Limit, _, _) when Size > Limit ->
	{error, size_error};
do_inflate(Z, Size0, Limit, continue, Acc) ->
	{Status, Output} = zlib:safeInflate(Z, []),
	Size = Size0 + iolist_size(Output),
	do_inflate(Z, Size, Limit, Status, [Output | Acc]);
do_inflate(_, _, _, finished, Acc) ->
	{ok, iolist_to_binary(lists:reverse(Acc))}.
