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

%% These functions are common to cowboy_http, cowboy_http2 and
%% cowboy_websocket. It requires the options and the state
%% to use the same field names.

%% Experiments have shown that the size of the 'buffer' can greatly
%% impact performance: a buffer too small leads to more messages
%% being handled and typically more binary appends; and a buffer
%% too large results in inefficient use of memory which in turn
%% reduces the throughput, presumably because large binary appends
%% are not as efficient as smaller ones, and because while the
%% buffer gets allocated only when there is data, the allocated
%% size remains until the binary is GC and so under-use hurts.
%%
%% The performance of a given 'buffer' size will also depend on
%% how the client is sending data, and on the protocol. For example,
%% HTTP/1.1 doesn't need a very large 'buffer' size for reading
%% request headers, but it does need one for reading large request
%% bodies. At the same time, HTTP/2 performs best reading large
%% request bodies when the 'buffer' size is about half that of
%% HTTP/1.1.
%%
%% It therefore becomes important to resize the buffer dynamically
%% depending on what is currently going on. We do this based on
%% the size of data packets we received from the transport. We
%% maintain a moving average and when that moving average is
%% 90% of the current 'buffer' size, we double the 'buffer' size.
%% When things slow down and the moving average falls below
%% 40% of the current 'buffer' size, we halve the 'buffer' size.
%%
%% To calculate the moving average we do (MovAvg + DataLen) div 2.
%% This means that the moving average will change very quickly when
%% DataLen increases or decreases rapidly. That's OK, we want to
%% be reactive, but also setting the buffer size is a pretty fast
%% operation. The formula could be changed to the following if it
%% became a problem: (MovAvg * N + DataLen) div (N + 1).
%%
%% Note that this works best when active,N uses low values of N.
%% We don't want to accumulate too much data because we resize
%% the buffer.

init_dynamic_buffer_size(#{dynamic_buffer_initial_size := DynamicBuffer}) ->
	DynamicBuffer;
init_dynamic_buffer_size(#{dynamic_buffer := {LowDynamicBuffer, _}}) ->
	LowDynamicBuffer;
init_dynamic_buffer_size(_) ->
	false.

maybe_resize_buffer(State=#state{dynamic_buffer_size=false}, _) ->
	State;
maybe_resize_buffer(State=#state{transport=Transport, socket=Socket,
		opts=#{dynamic_buffer := {LowDynamicBuffer, HighDynamicBuffer}},
		dynamic_buffer_size=BufferSize0, dynamic_buffer_moving_average=MovingAvg0}, Data) ->
	DataLen = byte_size(Data),
	MovingAvg = (MovingAvg0 + DataLen) div 2,
	if
		BufferSize0 < HighDynamicBuffer andalso MovingAvg > BufferSize0 * 0.9 ->
			BufferSize = min(BufferSize0 * 2, HighDynamicBuffer),
			ok = maybe_socket_error(State, Transport:setopts(Socket, [{buffer, BufferSize}])),
			State#state{dynamic_buffer_moving_average=MovingAvg, dynamic_buffer_size=BufferSize};
		BufferSize0 > LowDynamicBuffer andalso MovingAvg < BufferSize0 * 0.4 ->
			BufferSize = max(BufferSize0 div 2, LowDynamicBuffer),
			ok = maybe_socket_error(State, Transport:setopts(Socket, [{buffer, BufferSize}])),
			State#state{dynamic_buffer_moving_average=MovingAvg, dynamic_buffer_size=BufferSize};
		true ->
			State#state{dynamic_buffer_moving_average=MovingAvg}
	end.
