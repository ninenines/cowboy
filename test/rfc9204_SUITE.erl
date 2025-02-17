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

-module(rfc9204_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

-ifdef(COWBOY_QUICER).

-include_lib("quicer/include/quicer.hrl").

all() ->
	[{group, h3}].

groups() ->
	%% @todo Enable parallel tests but for this issues in the
	%% QUIC accept loop need to be figured out (can't connect
	%% concurrently somehow, no backlog?).
	[{h3, [], ct_helper:all(?MODULE)}].

init_per_group(Name = h3, Config) ->
	cowboy_test:init_http3(Name, #{
		env => #{dispatch => cowboy_router:compile(init_routes(Config))}
	}, Config).

end_per_group(Name, _) ->
	cowboy_test:stop_group(Name).

init_routes(_) -> [
	{"localhost", [
		{"/", hello_h, []}
	]}
].

%% Encoder.

%% 2.1
%% QPACK preserves the ordering of field lines within
%% each field section. An encoder MUST emit field
%% representations in the order they appear in the
%% input field section.

%% 2.1.1
%% If the dynamic table does not contain enough room
%% for a new entry without evicting other entries,
%% and the entries that would be evicted are not evictable,
%% the encoder MUST NOT insert that entry into the dynamic
%% table (including duplicates of existing entries).
%% In order to avoid this, an encoder that uses the
%% dynamic table has to keep track of each dynamic
%% table entry referenced by each field section until
%% those representations are acknowledged by the decoder;
%% see Section 4.4.1.

%% 2.1.2
%% The decoder specifies an upper bound on the number
%% of streams that can be blocked using the
%% SETTINGS_QPACK_BLOCKED_STREAMS setting; see Section 5.
%% An encoder MUST limit the number of streams that could
%% become blocked to the value of SETTINGS_QPACK_BLOCKED_STREAMS
%% at all times. If a decoder encounters more blocked streams
%% than it promised to support, it MUST treat this as a
%% connection error of type QPACK_DECOMPRESSION_FAILED.

%% 2.1.3
%% To avoid these deadlocks, an encoder SHOULD NOT
%% write an instruction unless sufficient stream and
%% connection flow-control credit is available for
%% the entire instruction.

%% Decoder.

%% 2.2
%% The decoder MUST emit field lines in the order their
%% representations appear in the encoded field section.

%% 2.2.1
%% While blocked, encoded field section data SHOULD
%% remain in the blocked stream's flow-control window.

%% If it encounters a Required Insert Count smaller than
%% expected, it MUST treat this as a connection error of
%% type QPACK_DECOMPRESSION_FAILED; see Section 2.2.3.

%% If it encounters a Required Insert Count larger than
%% expected, it MAY treat this as a connection error of
%% type QPACK_DECOMPRESSION_FAILED.

%% After the decoder finishes decoding a field section
%% encoded using representations containing dynamic table
%% references, it MUST emit a Section Acknowledgment
%% instruction (Section 4.4.1).

%% 2.2.2.2
%% A decoder with a maximum dynamic table capacity
%% (Section 3.2.3) equal to zero MAY omit sending Stream
%% Cancellations, because the encoder cannot have any
%% dynamic table references.

%% 2.2.3
%% If the decoder encounters a reference in a field line
%% representation to a dynamic table entry that has already
%% been evicted or that has an absolute index greater than
%% or equal to the declared Required Insert Count (Section 4.5.1),
%% it MUST treat this as a connection error of type
%% QPACK_DECOMPRESSION_FAILED.

%% If the decoder encounters a reference in an encoder
%% instruction to a dynamic table entry that has already
%% been evicted, it MUST treat this as a connection error
%% of type QPACK_ENCODER_STREAM_ERROR.

%% Static table.

%% 3.1
%% When the decoder encounters an invalid static table index
%% in a field line representation, it MUST treat this as a
%% connection error of type QPACK_DECOMPRESSION_FAILED.
%%
%% If this index is received on the encoder stream, this
%% MUST be treated as a connection error of type
%% QPACK_ENCODER_STREAM_ERROR.

%% Dynamic table.

%% 3.2
%% The dynamic table can contain duplicate entries
%% (i.e., entries with the same name and same value).
%% Therefore, duplicate entries MUST NOT be treated
%% as an error by the decoder.

%% 3.2.2
%% The encoder MUST NOT cause a dynamic table entry to be
%% evicted unless that entry is evictable; see Section 2.1.1.

%% It is an error if the encoder attempts to add an entry
%% that is larger than the dynamic table capacity; the
%% decoder MUST treat this as a connection error of type
%% QPACK_ENCODER_STREAM_ERROR.

%% 3.2.3
%% The encoder MUST NOT set a dynamic table capacity that
%% exceeds this maximum, but it can choose to use a lower
%% dynamic table capacity; see Section 4.3.1.

%% When the client's 0-RTT value of the SETTING is zero,
%% the server MAY set it to a non-zero value in its SETTINGS
%% frame. If the remembered value is non-zero, the server
%% MUST send the same non-zero value in its SETTINGS frame.
%% If it specifies any other value, or omits
%% SETTINGS_QPACK_MAX_TABLE_CAPACITY from SETTINGS,
%% the encoder must treat this as a connection error of
%% type QPACK_DECODER_STREAM_ERROR.

%% When the maximum table capacity is zero, the encoder
%% MUST NOT insert entries into the dynamic table and
%% MUST NOT send any encoder instructions on the encoder stream.

%% Wire format.

%% 4.1.1
%% QPACK implementations MUST be able to decode integers
%% up to and including 62 bits long.

%% Encoder and decoder streams.

decoder_reject_multiple(Config) ->
	doc("Endpoints must not create multiple decoder streams. (RFC9204 4.2)"),
	rfc9114_SUITE:do_critical_reject_multiple(Config, <<3>>).

encoder_reject_multiple(Config) ->
	doc("Endpoints must not create multiple encoder streams. (RFC9204 4.2)"),
	rfc9114_SUITE:do_critical_reject_multiple(Config, <<2>>).

%% 4.2
%% The sender MUST NOT close either of these streams,
%% and the receiver MUST NOT request that the sender close
%% either of these streams. Closure of either unidirectional
%% stream type MUST be treated as a connection error of type
%% H3_CLOSED_CRITICAL_STREAM.

decoder_local_closed_abort(Config) ->
	doc("Endpoints must not close the decoder stream. (RFC9204 4.2)"),
	rfc9114_SUITE:do_critical_local_closed_abort(Config, <<3>>).

decoder_local_closed_graceful(Config) ->
	doc("Endpoints must not close the decoder stream. (RFC9204 4.2)"),
	rfc9114_SUITE:do_critical_local_closed_graceful(Config, <<3>>).

decoder_remote_closed_abort(Config) ->
	doc("Endpoints must not close the decoder stream. (RFC9204 4.2)"),
	#{conn := Conn} = rfc9114_SUITE:do_connect(Config, #{peer_unidi_stream_count => 3}),
	{ok, #{decoder := StreamRef}} = do_wait_unidi_streams(Conn, #{}),
	%% Close the control stream.
	quicer:async_shutdown_stream(StreamRef, ?QUIC_STREAM_SHUTDOWN_FLAG_ABORT, 0),
	%% The connection should have been closed.
	#{reason := h3_closed_critical_stream} = rfc9114_SUITE:do_wait_connection_closed(Conn),
	ok.

encoder_local_closed_abort(Config) ->
	doc("Endpoints must not close the encoder stream. (RFC9204 4.2)"),
	rfc9114_SUITE:do_critical_local_closed_abort(Config, <<2>>).

encoder_local_closed_graceful(Config) ->
	doc("Endpoints must not close the encoder stream. (RFC9204 4.2)"),
	rfc9114_SUITE:do_critical_local_closed_graceful(Config, <<2>>).

encoder_remote_closed_abort(Config) ->
	doc("Endpoints must not close the encoder stream. (RFC9204 4.2)"),
	#{conn := Conn} = rfc9114_SUITE:do_connect(Config, #{peer_unidi_stream_count => 3}),
	{ok, #{encoder := StreamRef}} = do_wait_unidi_streams(Conn, #{}),
	%% Close the control stream.
	quicer:async_shutdown_stream(StreamRef, ?QUIC_STREAM_SHUTDOWN_FLAG_ABORT, 0),
	%% The connection should have been closed.
	#{reason := h3_closed_critical_stream} = rfc9114_SUITE:do_wait_connection_closed(Conn),
	ok.

do_wait_unidi_streams(_, Acc=#{decoder := _, encoder := _}) ->
	{ok, Acc};
do_wait_unidi_streams(Conn, Acc) ->
	receive
		{quic, new_stream, StreamRef, #{flags := Flags}} ->
			ok = quicer:setopt(StreamRef, active, true),
			true = quicer:is_unidirectional(Flags),
			receive {quic, <<TypeValue>>, StreamRef, _} ->
				Type = case TypeValue of
					2 -> encoder;
					3 -> decoder
				end,
				do_wait_unidi_streams(Conn, Acc#{Type => StreamRef})
			after 5000 ->
				{error, timeout}
			end
	after 5000 ->
		{error, timeout}
	end.

%% An endpoint MAY avoid creating an encoder stream if it will
%% not be used (for example, if its encoder does not wish to
%% use the dynamic table or if the maximum size of the dynamic
%% table permitted by the peer is zero).

%% An endpoint MAY avoid creating a decoder stream if its
%% decoder sets the maximum capacity of the dynamic table to zero.

%% An endpoint MUST allow its peer to create an encoder stream
%% and a decoder stream even if the connection's settings
%% prevent their use.

%% Encoder instructions.

%% 4.3.1
%% The new capacity MUST be lower than or equal to the limit
%% described in Section 3.2.3. In HTTP/3, this limit is the
%% value of the SETTINGS_QPACK_MAX_TABLE_CAPACITY parameter
%% (Section 5) received from the decoder. The decoder MUST
%% treat a new dynamic table capacity value that exceeds this
%% limit as a connection error of type QPACK_ENCODER_STREAM_ERROR.

%% Reducing the dynamic table capacity can cause entries to be
%% evicted; see Section 3.2.2. This MUST NOT cause the eviction
%% of entries that are not evictable; see Section 2.1.1.

%% Decoder instructions.

%% 4.4.1
%% If an encoder receives a Section Acknowledgment instruction
%% referring to a stream on which every encoded field section
%% with a non-zero Required Insert Count has already been
%% acknowledged, this MUST be treated as a connection error
%% of type QPACK_DECODER_STREAM_ERROR.

%% 4.4.3
%% An encoder that receives an Increment field equal to zero,
%% or one that increases the Known Received Count beyond what
%% the encoder has sent, MUST treat this as a connection error
%% of type QPACK_DECODER_STREAM_ERROR.

%% Field line representation.

%% 4.5.1.1
%% If the decoder encounters a value of EncodedInsertCount that
%% could not have been produced by a conformant encoder, it MUST
%% treat this as a connection error of type QPACK_DECOMPRESSION_FAILED.

%% 4.5.1.2
%% The value of Base MUST NOT be negative. Though the protocol
%% might operate correctly with a negative Base using post-Base
%% indexing, it is unnecessary and inefficient. An endpoint MUST
%% treat a field block with a Sign bit of 1 as invalid if the
%% value of Required Insert Count is less than or equal to the
%% value of Delta Base.

%% 4.5.4
%% When the 'N' bit is set, the encoded field line MUST always
%% be encoded with a literal representation. In particular,
%% when a peer sends a field line that it received represented
%% as a literal field line with the 'N' bit set, it MUST use a
%% literal representation to forward this field line. This bit
%% is intended for protecting field values that are not to be
%% put at risk by compressing them; see Section 7.1 for more details.

%% Configuration.

%% 5
%% SETTINGS_QPACK_MAX_TABLE_CAPACITY
%% SETTINGS_QPACK_BLOCKED_STREAMS

%% Security considerations.

%% 7.1.2
%% (security if used as a proxy merging many connections into one)
%% An ideal solution segregates access to the dynamic table
%% based on the entity that is constructing the message.
%% Field values that are added to the table are attributed
%% to an entity, and only the entity that created a particular
%% value can extract that value.

%% 7.1.3
%% An intermediary MUST NOT re-encode a value that uses a
%% literal representation with the 'N' bit set with another
%% representation that would index it. If QPACK is used for
%% re-encoding, a literal representation with the 'N' bit set
%% MUST be used. If HPACK is used for re-encoding, the
%% never-indexed literal representation (see Section 6.2.3
%% of [RFC7541]) MUST be used.

%% 7.4
%% An implementation has to set a limit for the values it
%% accepts for integers, as well as for the encoded length;
%% see Section 4.1.1. In the same way, it has to set a limit
%% to the length it accepts for string literals; see Section 4.1.2.
%% These limits SHOULD be large enough to process the largest
%% individual field the HTTP implementation can be configured
%% to accept.

%% If an implementation encounters a value larger than it is
%% able to decode, this MUST be treated as a stream error of
%% type QPACK_DECOMPRESSION_FAILED if on a request stream or
%% a connection error of the appropriate type if on the
%% encoder or decoder stream.

-endif.
