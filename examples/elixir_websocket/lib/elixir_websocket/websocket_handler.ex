defmodule ElixirWebsocket.WebsocketHandler do
  	@behaviour :cowboy_websocket_handler


	def init({:tcp, :http}, _req, _opts) do
		{:upgrade, :protocol, :cowboy_websocket}
	end

	def websocket_init(_transportName, req, _opts) do
		:erlang.start_timer(1000, self, <<"Hello!">>)
		{:ok, req, :undefined_state}
	end

	def websocket_handle({:text, msg}, req, state) do
		{:reply, {:text, << "That's what she said! ">> <> msg}, req, state}
	end
	def websocket_handle(_data, req, state) do
		{:ok, req, state}
	end

	def websocket_info({:timeout, _ref, msg}, req, state) do
		:erlang.start_timer(1000, self, <<"How' you doin'?">>)
		{:reply, {:text, msg}, req, state}
	end
	def websocket_info(_info, req, state) do
		{:ok, req, state}
	end

	def websocket_terminate(_reason, _req, _state) do
		:ok
	end
end
