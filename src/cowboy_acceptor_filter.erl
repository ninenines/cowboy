-module(cowboy_acceptor_filter).

-callback approve_connection(Socket :: inet:socket()) -> ok | {reject, Reason :: term()}.
