defmodule ElixirWebsocket do
  use Application.Behaviour

  def start(_type, _args) do
    dispatch = :cowboy_router.compile([
                 {:_, [
                        {"/", ElixirWebsocket.TopPageHandler, []},
                        {"/websocket", ElixirWebsocket.WebsocketHandler, []},
                        {"/static/[...]", :cowboy_static, [
                          {:directory, {:priv_dir, :websocket, [<<"static">>]}},
                          {:mimetypes, {function(:mimetypes, :path_to_mimes, 2), :default}}
                        ]}
                      ]}
               ])
    {:ok, _} = :cowboy.start_http(:http, 100,
                                  [port: 8080],
                                  [env: [dispatch: dispatch]])
    ElixirWebsocket.Supervisor.start_link
  end
end
