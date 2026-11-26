%% Handler for the data_delivery_n tests.

-module(ws_delivery_n_h).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	N = binary_to_integer(cowboy_req:binding(n, Req)),
	Mode = case cowboy_req:binding(mode, Req) of
		undefined -> mailbox;
		<<"passive">> -> passive;
		<<"empty">> -> empty
	end,
	{cowboy_websocket, Req, Mode, Opts#{data_delivery_n => N}}.

websocket_init(passive) ->
	{[{text, pid_to_list(self())}], passive};
websocket_init(State) ->
	{[], State}.

%% Block until the test releases us, then report how many relay
%% data messages are already waiting in the mailbox.
websocket_handle({binary, Bin}, mailbox) ->
	{TestPid, Expect} = binary_to_term(Bin),
	TestPid ! {blocked, self()},
	receive
		release ->
			ok
	after 5000 ->
		error(timeout)
	end,
	{messages, Messages} = process_info(self(), messages),
	InMailbox = length([1 || {'$cowboy_relay_data', _, _, _} <- Messages]),
	Text = case InMailbox of
		Expect -> <<"ok">>;
		Other -> integer_to_binary(Other)
	end,
	{[{text, Text}], mailbox};
websocket_handle({text, Data}, mailbox) ->
	{[{text, Data}], mailbox};
%% Empty DATA is not relayed. A non-empty frame sent while
%% waiting is already in the mailbox before the acknowledgement.
websocket_handle({binary, Bin}, empty) ->
	{TestPid, Expect} = binary_to_term(Bin),
	case Expect of
		before ->
			TestPid ! {blocked, self(), undefined},
			receive release -> ok after 10000 -> error(timeout) end,
			{messages, Messages} = process_info(self(), messages),
			[] = [1 || {'$cowboy_relay_data', _, _, <<>>} <- Messages],
			[] = [1 || {'$cowboy_relay_data', _, need_ack} <- Messages],
			{[{text, <<"ok">>}], empty};
		_ ->
			receive
				{'$cowboy_relay_data', {ConnPid, StreamID}, need_ack} ->
					TestPid ! {blocked, self(), ConnPid},
					receive release -> ok after 10000 -> error(timeout) end,
					{messages, Waiting} = process_info(self(), messages),
					[] = [1 || {'$cowboy_relay_data', _, _, <<>>} <- Waiting],
					case Expect of
						forward ->
							true = lists:any(fun
								({'$cowboy_relay_data', _, nofin, Data})
										when byte_size(Data) > 0 ->
									true;
								(_) ->
									false
							end, Waiting);
						_ ->
							ok
					end,
					ConnPid ! {'$cowboy_relay_command', {ConnPid, StreamID}, ack},
					case Expect of
						fin ->
							receive
								{'$cowboy_relay_data', _, _, <<>>} ->
									error(empty_relayed)
							after 1000 ->
								ok
							end;
						_ ->
							ok
					end,
					{[{text, <<"ok">>}], empty}
			after 1000 ->
				error(timeout)
			end
	end;
websocket_handle({text, Data}, empty) ->
	{[{text, Data}], empty};
websocket_handle({text, <<"one">>}, passive) ->
	{[{active, false}, {text, <<"one">>}], passive};
websocket_handle({text, Data}, passive) ->
	{[{text, Data}], passive};
websocket_handle(Frame, State) ->
	{[Frame], State}.

websocket_info(go_active, passive) ->
	{[{active, true}], passive};
websocket_info(_, State) ->
	{[], State}.
