defmodule ElixirHelloWorld do
  use Application.Behaviour

  def start(_type, _args) do
    dispatch = :cowboy_router.compile([
                 {:_, [{"/", ElixirHelloWorld.TopPageHandler, []}]}
               ])
    {:ok, _} = :cowboy.start_http(:http, 100,
                                  [port: 8080],
                                  [env: [dispatch: dispatch]])
    ElixirHelloWorld.Supervisor.start_link
  end
end
