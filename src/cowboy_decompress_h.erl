-module(cowboy_decompress_h).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

-record(state, {
	next :: any(),
	ratio_limit :: non_neg_integer() | undefined,
	ignore = false :: boolean(),
	compress = undefined :: undefined | gzip,
	inflate = undefined :: undefined | zlib:zstream(),
	is_reading = false :: boolean(),
	read_body_buffer = <<>> :: binary(),
	read_body_is_fin = nofin :: nofin | {fin, non_neg_integer()}
}).

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
	-> {cowboy_stream:commands(), #state{}}.
init(StreamID, Req, Opts) ->
	RatioLimit = maps:get(decompress_ratio_limit, Opts, 20),
	Ignore = maps:get(decompress_ignore, Opts, false),
	State = check_req(Req),
	Inflate = case State#state.compress of
		undefined ->
			undefined;
		gzip ->
			Z = zlib:open(),
			zlib:inflateInit(Z, 31),
			Z
	end,
	{Commands, Next} = cowboy_stream:init(StreamID, Req, Opts),
	fold(Commands, State#state{next=Next, ratio_limit=RatioLimit, ignore=Ignore,
		inflate=Inflate}).

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State)
	-> {cowboy_stream:commands(), State} when State::#state{}.
data(StreamID, IsFin, Data, State=#state{next=Next0, inflate=undefined}) ->
	{Commands, Next} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
	fold(Commands, State#state{next=Next, read_body_is_fin=IsFin});
data(StreamID, IsFin, Data, State=#state{next=Next0, ignore=true, read_body_buffer=Buffer}) ->
	{Commands, Next} = cowboy_stream:data(StreamID, IsFin,
		<< Buffer/binary, Data/binary >>, Next0),
	fold(Commands, State#state{next=Next, read_body_is_fin=IsFin});
data(StreamID, IsFin, Data, State0=#state{next=Next0, ratio_limit=RatioLimit,
		inflate=Z, is_reading=true, read_body_buffer=Buffer0}) ->
	Buffer = << Buffer0/binary, Data/binary >>,
	case inflate(Z, RatioLimit, Buffer) of
		{error, Type} ->
			Status = case Type of
				data -> 400;
				size -> 413
			end,
			Commands = [
				{error_response, Status, #{<<"content-length">> => <<"0">>}, <<>>},
				stop
			],
			fold(Commands, State0#state{inflate=undefined});
		{ok, Inflated} ->
			State = case IsFin of
				nofin ->
					State0;
				fin ->
					zlib:inflateEnd(Z),
					zlib:close(Z),
					State0#state{inflate=undefined}
			end,
			{Commands, Next} = cowboy_stream:data(StreamID, IsFin, Inflated, Next0),
			fold(Commands, State#state{next=Next, read_body_buffer= <<>>,
				read_body_is_fin=IsFin})
	end;
data(_, IsFin, Data, State=#state{read_body_buffer=Buffer0}) ->
	Buffer = << Buffer0/binary, Data/binary >>,
	{[], State#state{read_body_buffer=Buffer, read_body_is_fin=IsFin}}.

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
info(StreamID, Info={set_options, Opts}, State=#state{next=Next0,
		ignore=Ignore0, ratio_limit=RatioLimit0}) ->
	Ignore = maps:get(decompress_ignore, Opts, Ignore0),
	RatioLimit = maps:get(decompress_ratio_limit, Opts, RatioLimit0),
	{Commands, Next} = cowboy_stream:info(StreamID, Info, Next0),
	fold(Commands, State#state{next=Next, ignore=Ignore, ratio_limit=RatioLimit});
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

check_req(Req) ->
	try cowboy_req:parse_header(<<"content-encoding">>, Req) of
		undefined ->
			#state{compress=undefined};
		Encodings ->
			case [E || E=(<<"gzip">>) <- Encodings] of
				[] ->
					#state{compress=undefined};
				_ ->
					#state{compress=gzip}
			end
	catch
		_:_ ->
			#state{compress=undefined}
	end.

fold(Commands, State) ->
	fold(Commands, State, []).

fold([], State, Acc) ->
	{lists:reverse(Acc), State};
fold([{response, Status, Headers0, Body}|Tail], State=#state{ignore=false}, Acc) ->
	Headers = add_accept_encoding(Headers0),
	fold(Tail, State, [{response, Status, Headers, Body}|Acc]);
fold([{headers, Status, Headers0} | Tail], State=#state{ignore=false}, Acc) ->
	Headers = add_accept_encoding(Headers0),
	fold(Tail, State, [{headers, Status, Headers}|Acc]);
fold([Command|Tail], State, Acc) ->
	fold(Tail, State, [Command|Acc]).

add_accept_encoding(Headers=#{<<"accept-encoding">> := AcceptEncoding}) ->
	try cow_http_hd:parse_accept_encoding(iolist_to_binary(AcceptEncoding)) of
		List ->
			case lists:keyfind(<<"gzip">>, 1, List) of
				%% gzip is excluded but this handler is not ignored; we replace.
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
		Headers#{<<"accept-encoding">> => <<"gzip">>}
	end;
add_accept_encoding(Headers) ->
	Headers#{<<"accept-encoding">> => <<"gzip">>}.

%% From cowlib, maybe expose?
qvalue_to_iodata(0) -> <<"0">>;
qvalue_to_iodata(Q) when Q < 10 -> [<<"0.00">>, integer_to_binary(Q)];
qvalue_to_iodata(Q) when Q < 100 -> [<<"0.0">>, integer_to_binary(Q)];
qvalue_to_iodata(Q) when Q < 1000 -> [<<"0.">>, integer_to_binary(Q)];
qvalue_to_iodata(1000) -> <<"1">>.

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
		do_inflate(Z, Size, byte_size(Data) * RatioLimit, Status, [Output])
	catch
		error:data_error ->
			zlib:close(Z),
			{error, data}
	end.

do_inflate(Z, Size, Limit, Status, _) when Size > Limit ->
	case Status of
		continue -> ok;
		finished -> zlib:inflateEnd(Z)
	end,
	zlib:close(Z),
	{error, size};
do_inflate(Z, Size0, Limit, continue, Acc) ->
	{Status, Output} = zlib:safeInflate(Z, []),
	Size = Size0 + iolist_size(Output),
	do_inflate(Z, Size, Limit, Status, [Output | Acc]);
do_inflate(_, _, _, finished, Acc) ->
	{ok, iolist_to_binary(lists:reverse(Acc))}.
