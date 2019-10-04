%% Copyright (c) 2017, Loïc Hoguin <essen@ninenines.eu>
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

-module(cowboy_compress_h).
-behavior(cowboy_stream).

-export([init/3]).
-export([data/4]).
-export([info/3]).
-export([terminate/3]).
-export([early_error/5]).

-record(state, {
	next :: any(),
	threshold :: non_neg_integer() | undefined,
	compress = undefined :: undefined | gzip,
	deflate = undefined :: undefined | zlib:zstream(),
	deflate_flush = sync :: none | sync
}).

-spec init(cowboy_stream:streamid(), cowboy_req:req(), cowboy:opts())
	-> {cowboy_stream:commands(), #state{}}.
init(StreamID, Req, Opts) ->
	State0 = check_req(Req),
	CompressThreshold = maps:get(compress_threshold, Opts, 300),
	DeflateFlush = buffering_to_zflush(maps:get(compress_buffering, Opts, false)),
	{Commands0, Next} = cowboy_stream:init(StreamID, Req, Opts),
	fold(Commands0, State0#state{next=Next,
		threshold=CompressThreshold,
		deflate_flush=DeflateFlush}).

-spec data(cowboy_stream:streamid(), cowboy_stream:fin(), cowboy_req:resp_body(), State)
	-> {cowboy_stream:commands(), State} when State::#state{}.
data(StreamID, IsFin, Data, State0=#state{next=Next0}) ->
	{Commands0, Next} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
	fold(Commands0, State0#state{next=Next}).

-spec info(cowboy_stream:streamid(), any(), State)
	-> {cowboy_stream:commands(), State} when State::#state{}.
info(StreamID, Info, State0=#state{next=Next0}) ->
	{Commands0, Next} = cowboy_stream:info(StreamID, Info, Next0),
	fold(Commands0, State0#state{next=Next}).

-spec terminate(cowboy_stream:streamid(), cowboy_stream:reason(), #state{}) -> any().
terminate(StreamID, Reason, #state{next=Next, deflate=Z}) ->
	%% Clean the zlib:stream() in case something went wrong.
	%% In the normal scenario the stream is already closed.
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

%% Check if the client supports decoding of gzip responses.
check_req(Req) ->
	%% @todo Probably shouldn't unconditionally crash on failure.
	case cowboy_req:parse_header(<<"accept-encoding">>, Req) of
		%% Client doesn't support any compression algorithm.
		undefined ->
			#state{compress=undefined};
		Encodings ->
			%% We only support gzip so look for it specifically.
			%% @todo A recipient SHOULD consider "x-gzip" to be
			%% equivalent to "gzip". (RFC7230 4.2.3)
			case [E || E={<<"gzip">>, Q} <- Encodings, Q =/= 0] of
				[] ->
					#state{compress=undefined};
				_ ->
					#state{compress=gzip}
			end
	end.

%% Do not compress responses that contain the content-encoding header.
check_resp_headers(#{<<"content-encoding">> := _}, State) ->
	State#state{compress=undefined};
check_resp_headers(_, State) ->
	State.

fold(Commands, State=#state{compress=undefined}) ->
	{Commands, State};
fold(Commands, State) ->
	fold(Commands, State, []).

fold([], State, Acc) ->
	{lists:reverse(Acc), State};
%% We do not compress full sendfile bodies.
fold([Response={response, _, _, {sendfile, _, _, _}}|Tail], State, Acc) ->
	fold(Tail, State, [Response|Acc]);
%% We compress full responses directly, unless they are lower than
%% the configured threshold or we find we are not able to by looking at the headers.
fold([Response0={response, _, Headers, Body}|Tail],
		State0=#state{threshold=CompressThreshold}, Acc) ->
	case check_resp_headers(Headers, State0) of
		State=#state{compress=undefined} ->
			fold(Tail, State, [Response0|Acc]);
		State1 ->
			BodyLength = iolist_size(Body),
			if
				BodyLength =< CompressThreshold ->
					fold(Tail, State1, [Response0|Acc]);
				true ->
					{Response, State} = gzip_response(Response0, State1),
					fold(Tail, State, [Response|Acc])
			end
	end;
%% Check headers and initiate compression...
fold([Response0={headers, _, Headers}|Tail], State0, Acc) ->
	case check_resp_headers(Headers, State0) of
		State=#state{compress=undefined} ->
			fold(Tail, State, [Response0|Acc]);
		State1 ->
			{Response, State} = gzip_headers(Response0, State1),
			fold(Tail, State, [Response|Acc])
	end;
%% then compress each data commands individually.
fold([Data0={data, _, _}|Tail], State0=#state{compress=gzip}, Acc) ->
	{Data, State} = gzip_data(Data0, State0),
	fold(Tail, State, [Data|Acc]);
%% When trailers are sent we need to end the compression.
%% This results in an extra data command being sent.
fold([Trailers={trailers, _}|Tail], State0=#state{compress=gzip}, Acc) ->
	{{data, fin, Data}, State} = gzip_data({data, fin, <<>>}, State0),
	fold(Tail, State, [Trailers, {data, nofin, Data}|Acc]);
%% All the options from this handler can be updated for the current stream.
%% The set_options command must be propagated as-is regardless.
fold([SetOptions={set_options, Opts}|Tail], State=#state{
		threshold=CompressThreshold0, deflate_flush=DeflateFlush0}, Acc) ->
	CompressThreshold = maps:get(compress_threshold, Opts, CompressThreshold0),
	DeflateFlush = case Opts of
		#{compress_buffering := CompressBuffering} ->
			buffering_to_zflush(CompressBuffering);
		_ ->
			DeflateFlush0
	end,
	fold(Tail, State#state{threshold=CompressThreshold, deflate_flush=DeflateFlush},
		[SetOptions|Acc]);
%% Otherwise, we have an unrelated command or compression is disabled.
fold([Command|Tail], State, Acc) ->
	fold(Tail, State, [Command|Acc]).

buffering_to_zflush(true) -> none;
buffering_to_zflush(false) -> sync.

gzip_response({response, Status, Headers, Body}, State) ->
	%% We can't call zlib:gzip/1 because it does an
	%% iolist_to_binary(GzBody) at the end to return
	%% a binary(). Therefore the code here is largely
	%% a duplicate of the code of that function.
	Z = zlib:open(),
	GzBody = try
		%% 31 = 16+?MAX_WBITS from zlib.erl
		%% @todo It might be good to allow them to be configured?
		zlib:deflateInit(Z, default, deflated, 31, 8, default),
		Gz = zlib:deflate(Z, Body, finish),
		zlib:deflateEnd(Z),
		Gz
	after
		zlib:close(Z)
	end,
	{{response, Status, vary(Headers#{
		<<"content-length">> => integer_to_binary(iolist_size(GzBody)),
		<<"content-encoding">> => <<"gzip">>
	}), GzBody}, State}.

gzip_headers({headers, Status, Headers0}, State) ->
	Z = zlib:open(),
	%% We use the same arguments as when compressing the body fully.
	%% @todo It might be good to allow them to be configured?
	zlib:deflateInit(Z, default, deflated, 31, 8, default),
	Headers = maps:remove(<<"content-length">>, Headers0),
	{{headers, Status, vary(Headers#{
		<<"content-encoding">> => <<"gzip">>
	})}, State#state{deflate=Z}}.

%% We must add content-encoding to vary if it's not already there.
vary(Headers=#{<<"vary">> := Vary}) ->
	try cow_http_hd:parse_vary(iolist_to_binary(Vary)) of
		'*' -> Headers;
		List ->
			case lists:member(<<"accept-encoding">>, List) of
				true -> Headers;
				false -> Headers#{<<"vary">> => [Vary, <<", accept-encoding">>]}
			end
	catch _:_ ->
		%% The vary header is invalid. Probably empty. We replace it with ours.
		Headers#{<<"vary">> => <<"accept-encoding">>}
	end;
vary(Headers) ->
	Headers#{<<"vary">> => <<"accept-encoding">>}.

%% It is not possible to combine zlib and the sendfile
%% syscall as far as I can tell, because the zlib format
%% includes a checksum at the end of the stream. We have
%% to read the file in memory, making this not suitable for
%% large files.
gzip_data({data, nofin, Sendfile={sendfile, _, _, _}},
		State=#state{deflate=Z, deflate_flush=Flush}) ->
	{ok, Data0} = read_file(Sendfile),
	Data = zlib:deflate(Z, Data0, Flush),
	{{data, nofin, Data}, State};
gzip_data({data, fin, Sendfile={sendfile, _, _, _}}, State=#state{deflate=Z}) ->
	{ok, Data0} = read_file(Sendfile),
	Data = zlib:deflate(Z, Data0, finish),
	zlib:deflateEnd(Z),
	zlib:close(Z),
	{{data, fin, Data}, State#state{deflate=undefined}};
gzip_data({data, nofin, Data0}, State=#state{deflate=Z, deflate_flush=Flush}) ->
	Data = zlib:deflate(Z, Data0, Flush),
	{{data, nofin, Data}, State};
gzip_data({data, fin, Data0}, State=#state{deflate=Z}) ->
	Data = zlib:deflate(Z, Data0, finish),
	zlib:deflateEnd(Z),
	zlib:close(Z),
	{{data, fin, Data}, State#state{deflate=undefined}}.

read_file({sendfile, Offset, Bytes, Path}) ->
	{ok, IoDevice} = file:open(Path, [read, raw, binary]),
	try
		_ = case Offset of
			0 -> ok;
			_ -> file:position(IoDevice, {bof, Offset})
		end,
		file:read(IoDevice, Bytes)
	after
		file:close(IoDevice)
	end.
